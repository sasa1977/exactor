defmodule ExActor.Operations do
  @moduledoc """
  Macros that can be used for simpler definition of `GenServer` operations
  such as casts or calls.

  For example:

      defcall request(x, y), state: state do
        set_and_reply(state + x + y, :ok)
      end

  will generate two functions:

      def request(server, x, y) do
        GenServer.call(server, {:request, x, y})
      end

      def handle_call({:request, x, y}, _, state) do
        {:reply, :ok, state + x + y}
      end

  There are various helper macros available for specifying responses. For more details
  see `ExActor.Responders`.

  ## Request format (passed to `handle_call/3` and `handle_cast/2`)

  - no arguments -> `:my_request`
  - one arguments -> `{:my_request, x}`
  - more arguments -> `{:my_request, x, y, ...}`

  ## Common options

  - `:when` - specifies guards (see __Pattern matching__ below for details)
  - `:export` - applicable in `defcall/3` and `defcast/3`. If provided, specifies
  the server alias. In this case, interface functions will not accept the server
  as the first argument, and will insted use the provided alias. The alias
  can be an atom (for locally registered processes), `{:global, global_alias}` or
  a via tuple (`{:via, registration_module, alias}`).

  ## Pattern matching

      defcall a(1), do: ...
      defcall a(x), when: x > 1, do: ...
      defcall a(x), when: [interface: x > 1, handler: x < state], do: ...
      defcall a(x), state: 1, do: ...
      defcall a(_), state: state, do: ...

  ### Details

  `defcall` and other similar constructs usually define a clause for two
  functions: the interface function and the handler function. If you're writing
  multi-clauses, the following rules apply:

  - Arguments are pattern-matched in the interface and in the handler function.
  - The `:state` pattern is used in the handler function.
  - The `:when` option by default applies to both, the interface and the handler function.
    You can however specify separate guards with `when: [interface: ..., handler: ...]`.
    It's not necessary to provide both options to `when`.

  `ExActor` will try to be smart to some extent, and defer from generating the
  interface clause if it's not needed.

  For example:

      defcall foo(_, _), state: nil, do: ...
      defcall foo(x, y), state: state, do: ...

  will generate only a single interface function that always matches its arguments
  and sends them to the server process. There will be of course two `handle_call`
  clauses.

  The same holds for more elaborate pattern-matches:

      defcall foo(1, 2), ...
      defcall foo(x, y), when: x > y, ...
      defcall foo(_, _), state: nil, do: ...
      defcall foo(x, y), state: state, do: ...

  The example above will generate three interface clauses:

  - `def foo(1, 2)`
  - `def foo(x, y) when x > y`
  - `def foo(x, y)`

  Of course, there will be four `handle_call` clauses, each with the corresponding
  body provided via `do` option.

  ### Separating interface and handler clauses

  If you want to be more explicit about pattern matching, you can use a body-less
  construct:

      defcall foo(x, y)

  This will generate only the interface clause that issues a call (or a cast in
  the case of `defcast`) to the server process.

  You can freely use multiple `defcall` body-less clauses if you need to pattern
  match arguments.

  To generate handler clauses you can use `defhandlecall/3`:

      defhandlecall foo(_, _), state: nil, do: ...
      defhandlecall foo(x, y), state: state, do: ...

  This approach requires some more typing, but it's more explicit. If you need to
  perform a complex combination of pattern matches on arguments and the state, it's
  probably better to use this technique as it gives you more control over what is
  matched at which point.
  """


  @doc """
  Defines the starter function and initializer body.

      # defines and export start/2
      defstart start(x, y) do
        # runs in init/1 callback
        initial_state(x + y)
      end

      # defines and export start_link/2
      defstart start_link(x, y) do
        # runs in init/1 callback
        initial_state(x + y)
      end

  You can also provide additional `GenServer` options via `:gen_server_opts` option.

      defstart start(x, y), gen_server_opts: [spawn_opts: [min_heap_size: 10000]], do: ...

  If you need to  set `GenServer` options at runtime, use `gen_server_opts: :runtime` and
  then the starter function will receive one more argument where you can pass options:

      defstart start(x, y), gen_server_opts: :runtime do
        ...
      end

      ...

      MyServer.start(x, y, name: :foo, spawn_opts: [min_heap_size: 10000])

  Body can be omitted. In this case, just the interface function is generated.
  This can be useful if you want to define both `start` and `start_link`:

      defstart start(x, y)
      defstart start_link(x, y) do
        # runs for both cases
      end

  Keep in mind that generated `init/1` matches on the number of arguments, so this won't work:

      defstart start_link(x)
      defstart start_link(x, y) do
        # doesn't handle start_link(x)
      end

  If you want to handle various versions, you can just define start heads without the body,
  and then use `definit/2` or just implement `init/1`.

  ## Other notes

  - If the `export` option is set while using `ExActor`, it will be used in starters, and
  the server process will be registered under a given alias.
  - For each specified clause, there will be one corresponding interface function clause.

  ### Request format (arg passed to `init/1`)

  - no arguments -> `nil`
  - one arguments -> `{x}`
  - more arguments -> `{x, y, ...}`
  """
  defmacro defstart(definition, opts \\ [], body \\ []) do
    {fun, args} = Macro.decompose_call(definition)
    define_starter(false, fun, args, opts ++ body)
  end

  @doc """
  Same as `defstart/2` but the interface function is private.

  Can be useful when you need to do pre/post processing in the caller process.

      defmodule MyServer do
        def start_link(x, y) do
          ...

          do_start_link(x, y)

          ...
        end

        defstartp do_start_link(x, y), link: true do
          ...
        end
      end
  """
  defmacro defstartp(definition, options \\ [], body \\ []) do
    {fun, args} = Macro.decompose_call(definition)
    define_starter(true, fun, args, options ++ body)
  end

  defp define_starter(private, fun, args, options) do
    quote bind_quoted: [
      private: private,
      fun: Macro.escape(fun, unquote: true),
      args: Macro.escape(args || [], unquote: true),
      options: escape_options(options)
    ] do
      {interface_matches, payload, match_pattern} = ExActor.Operations.start_args(args)

      {arity, interface_matches, gen_server_fun, gen_server_opts} =
        ExActor.Operations.prepare_start_interface(fun, interface_matches, options, @exactor_global_options)

      unless private do
        case ExActor.Operations.guard(options, :interface) do
          nil ->
            def unquote(fun)(unquote_splicing(interface_matches)) do
              GenServer.unquote(gen_server_fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
          guard ->
            def unquote(fun)(unquote_splicing(interface_matches)) when unquote(guard) do
              GenServer.unquote(gen_server_fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
        end
      else
        case ExActor.Operations.guard(options, :interface) do
          nil ->
            defp unquote(fun)(unquote_splicing(interface_matches)) do
              GenServer.unquote(gen_server_fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
          guard ->
            defp unquote(fun)(unquote_splicing(interface_matches)) when unquote(guard) do
              GenServer.unquote(gen_server_fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
          end
      end

      if options[:do] do
        definit(
          unquote(match_pattern),
          unquote(Keyword.take(options, [:when]) ++ [do: options[:do]])
        )
      end
    end
  end

  @doc false
  def extract_args(args) do
    arg_names =
      for {arg, index} <- Enum.with_index(args), do: extract_arg(arg, index)

    interface_matches = for {arg, arg_name} <- Enum.zip(args, arg_names) do
      case arg do
        {:\\, context, [match, default]} ->
          {:\\, context, [quote(do: unquote(match) = unquote(arg_name)), default]}
        match -> quote(do: unquote(match) = unquote(arg_name))
      end
    end

    args = for arg <- args do
      case arg do
        {:\\, _, [match, _]} -> match
        _ -> arg
      end
    end
    {arg_names, interface_matches, args}
  end

  defmacrop var_name?(arg_name) do
    quote do
      is_atom(unquote(arg_name)) and not (unquote(arg_name) in [:_, :\\, :=, :%, :%{}, :{}, :<<>>])
    end
  end

  defp extract_arg({:\\, _, [inner_arg, _]}, index),
    do: extract_arg(inner_arg, index)
  defp extract_arg({:=, _, [{arg_name, _, _} = arg, _]}, _index) when var_name?(arg_name),
    do: arg
  defp extract_arg({:=, _, [_, {arg_name, _, _} = arg]}, _index) when var_name?(arg_name),
    do: arg
  defp extract_arg({:=, _, [_, {:=, _, _} = submatch]}, index),
    do: extract_arg(submatch, index)
  defp extract_arg({arg_name, _, _} = arg, _index) when var_name?(arg_name),
    do: arg
  defp extract_arg(_, index),
    do: Macro.var(:"arg#{index}", __MODULE__)

  @doc false
  def start_args(args) do
    {arg_names, interface_matches, args} = extract_args(args)

    {payload, match_pattern} =
      case args do
        [] -> {nil, nil}
        [_|_] ->
          {
            quote(do: {unquote_splicing(arg_names)}),
            quote(do: {unquote_splicing(args)})
          }
      end

    {interface_matches, payload, match_pattern}
  end

  @doc false
  def prepare_start_interface(fun, interface_matches, options, global_options) do
    interface_matches =
      unless options[:gen_server_opts] == :runtime do
        interface_matches
      else
        interface_matches ++ [quote(do: unquote(Macro.var(:gen_server_opts, __MODULE__)) \\ [])]
      end

    arity = length(interface_matches)

    gen_server_fun = case (options[:link]) do
      true -> :start_link
      false -> :start
      nil ->
        if fun in [:start, :start_link] do
          fun
        else
          raise "Function name must be either start or start_link. If you need another name, provide explicit :link option."
        end
    end

    gen_server_opts =
      unless options[:gen_server_opts] == :runtime do
        case global_options[:export] do
          default when default in [nil, false] -> []
          name -> [name: Macro.escape(name)]
        end ++ (options[:gen_server_opts] || [])
      else
        Macro.var(:gen_server_opts, __MODULE__)
      end

    {arity, interface_matches, gen_server_fun, gen_server_opts}
  end



  @doc """
  Similar to `defstart/3` but generates just the `init` clause.

  Note: keep in mind that `defstart` wraps arguments in a tuple. If you want to
  handle `defstart start(x)`, you need to define `definit {x}`
  """
  defmacro definit(arg \\ quote(do: _), opts), do: do_definit([{:arg, arg} | opts])

  defp do_definit(opts) do
    quote bind_quoted: [opts: Macro.escape(opts, unquote: true)] do
      case ExActor.Operations.guard(opts, :handler) do
        nil ->
          def init(unquote_splicing([opts[:arg]])), do: unquote(opts[:do])
        guard ->
          def init(unquote_splicing([opts[:arg]])) when unquote(guard), do: unquote(opts[:do])
      end
    end
  end


  @doc """
  Defines the cast callback clause and the corresponding interface fun.
  """
  defmacro defcast(req_def, options \\ [], body \\ []) do
    generate_funs(:defcast, req_def, options ++ body)
  end

  @doc """
  Same as `defcast/3` but the interface function is private.

  Can be useful when you need to do pre/post processing in the caller process.

      def exported_interface(...) do
        # do some client side preprocessing here
        my_request(...)
        # do some client side post processing here
      end

      # Not available outside of this module
      defcastp my_request(...), do: ...
  """
  defmacro defcastp(req_def, options \\ [], body \\ []) do
    generate_funs(:defcast, req_def, [{:private, true} | options] ++ body)
  end


  @doc """
  Defines the call callback clause and the corresponding interface fun.

  Call-specific options:

    - `:timeout` - specifies the timeout used in `GenServer.call` (see below for
    details)
    - `:from` - matches the caller in `handle_call`.

  ## Timeout

      defcall long_call, state: state, timeout: :timer.seconds(10), do: ...

  You can also make the timeout parameterizable

      defcall long_call(...), timeout: some_variable, do: ...

  This will generate the interface function as:

      def long_call(..., some_variable)

  where `some_variable` will be used as the timeout in `GenServer.call`. You
  won't have the access to this variable in your body though, since the body
  specifies the handler function. Default timeout value can also be provided via
  standard `\\\\` syntax.
  """
  defmacro defcall(req_def, options \\ [], body \\ []) do
    generate_funs(:defcall, req_def, options ++ body)
  end

  @doc """
  Same as `defcall/3` but the interface function is private.

  Can be useful when you need to do pre/post processing in the caller process.

      def exported_interface(...) do
        # do some client side preprocessing here
        my_request(...)
        # do some client side post processing here
      end

      # Not available outside of this module
      defcallp my_request(...), do: ...
  """
  defmacro defcallp(req_def, options \\ [], body \\ []) do
    generate_funs(:defcall, req_def, [{:private, true} | options] ++ body)
  end

  @doc """
  Similar to `defcall/3`, but generates just the `handle_call` clause,
  without creating the interface function.
  """
  defmacro defhandlecall(req_def, options \\ [], body \\ []) do
    generate_request_def(:defcall, req_def, options ++ body)
  end

  @doc """
  Similar to `defcast/3`, but generates just the `handle_call` clause,
  without creating the interface function.
  """
  defmacro defhandlecast(req_def, options \\ [], body \\ []) do
    generate_request_def(:defcast, req_def, options ++ body)
  end



  # Generation of call/cast functions. Essentially, this is just
  # deferred to be evaluated in the module context.
  defp generate_funs(type, req_def, options) do
    quote bind_quoted: [
      type: type,
      req_def: Macro.escape(req_def, unquote: true),
      options: escape_options(options)
    ] do
      ExActor.Operations.def_request(type, req_def, Keyword.merge(options, @exactor_global_options))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  @doc false
  def guard(options, type) do
    case options[:when] do
      nil -> nil
      list when is_list(list) -> list[type]
      other -> other
    end
  end

  @doc false
  def def_request(type, req_def, options) do
    {req_name, interface_matches, payload, _} = req_args(req_def)

    quote do
      req_id = unquote(Macro.escape(req_id(req_def, options)))
      unless MapSet.member?(@generated_funs, req_id) do
        unquote(define_interface(type, req_name, interface_matches, payload, options))
        @generated_funs MapSet.put(@generated_funs, req_id)
      end

      unquote(if options[:do] do
        implement_request(type, req_def, options)
      end)
    end
  end

  defp req_id({_, _, _} = definition, options) do
    {req_name, args} = Macro.decompose_call(definition)
    {
      req_name,
      Enum.map(
        strip_context(args || []),
        fn
          {var_name, _, scope} when is_atom(var_name) and is_atom(scope) -> :matchall
          other -> other
        end
      ),
      strip_context(guard(options, :interface))
    }
  end

  defp req_id(req_name, options) when is_atom(req_name) do
    req_id({req_name, [], []}, options)
  end

  defp strip_context(ast) do
    Macro.prewalk(ast,
      fn
        {a, _context, b} -> {a, [], b}
        other -> other
      end
    )
  end

  defp generate_request_def(type, req_def, options) do
    quote bind_quoted: [
      type: type,
      req_def: Macro.escape(req_def, unquote: true),
      options: escape_options(options)
    ] do
      ExActor.Operations.implement_request(type, req_def, Keyword.merge(options, @exactor_global_options))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  @doc false
  def implement_request(type, req_def, options) do
    {_, _, _, match_pattern} = req_args(req_def)

    quote do
      unquote(implement_handler(type, options, match_pattern))
    end
  end


  defp req_args(req_def) do
    {req_name, args} = parse_req_def(req_def)
    {arg_names, interface_matches, args} = extract_args(args)

    {payload, match_pattern} =
      case args do
        [] -> {req_name, req_name}
        [_|_] ->
          {
            quote(do: {unquote_splicing([req_name | arg_names])}),
            quote(do: {unquote_splicing([req_name | args])})
          }
      end

    {req_name, interface_matches, payload, match_pattern}
  end

  defp parse_req_def(req_name) when is_atom(req_name), do: {req_name, []}
  defp parse_req_def({_, _, _} = definition) do
    Macro.decompose_call(definition)
  end

  # Defines the interface function to call/cast
  defp define_interface(type, req_name, interface_matches, payload, options) do
    quote bind_quoted: [
      private: options[:private],
      type: type,
      req_name: req_name,
      server_fun: server_fun(type),
      interface_args: Macro.escape(interface_args(interface_matches, options), unquote: true),
      gen_server_args: Macro.escape(gen_server_args(options, type, payload), unquote: true),
      guard: Macro.escape(guard(options, :interface), unquote: true)
    ] do
      {interface_args, gen_server_args} =
        unless type in [:multicall, :abcast] do
          {interface_args, gen_server_args}
        else
          {
            [quote(do: nodes \\ [node() | :erlang.nodes()]) | interface_args],
            [quote(do: nodes) | gen_server_args]
          }
        end

      arity = length(interface_args)
      unless private do
        if guard do
          def unquote(req_name)(unquote_splicing(interface_args))
            when unquote(guard)
          do
            GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
          end
        else
          def unquote(req_name)(unquote_splicing(interface_args)) do
            GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
          end
        end
      else
        if guard do
          defp unquote(req_name)(unquote_splicing(interface_args))
            when unquote(guard)
          do
            GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
          end
        else
          defp unquote(req_name)(unquote_splicing(interface_args)) do
            GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
          end
        end
      end
    end
  end

  defp server_fun(:defcast), do: :cast
  defp server_fun(:defcall), do: :call
  defp server_fun(:multicall), do: :multi_call
  defp server_fun(:abcast), do: :abcast

  defp interface_args(args, options) do
    server_match(options[:export]) ++ args ++ timeout_match(options[:timeout])
  end

  defp server_match(export) when export == nil or export == true, do: [quote(do: server)]
  defp server_match(_), do: []

  defp timeout_match(nil), do: []
  defp timeout_match(:infinity), do: []
  defp timeout_match(timeout) when is_integer(timeout), do: []
  defp timeout_match(pattern), do: [pattern]

  defp gen_server_args(options, type, msg) do
    [server_ref(options, type), msg] ++ timeout_arg(options, type)
  end

  defp server_ref(options, op) when op in [:multicall, :abcast] do
    case options[:export] do
      local when is_atom(local) and local != nil and local != false -> local
      {:local, local} -> local
      _ -> quote(do: server)
    end
  end

  defp server_ref(options, _) do
    case options[:export] do
      default when default in [nil, false, true] -> quote(do: server)
      local when is_atom(local) -> local
      {:local, local} -> local
      {:global, _} = global -> global
      {:via, _, _} = via -> Macro.escape(via)
    end
  end

  defp timeout_arg(options, type) when type in [:defcall, :multicall] do
    case options[:timeout] do
      {:\\, _, [var, _default]} ->
        [var]
      timeout when timeout != nil ->
        [timeout]
      _ -> []
    end
  end

  defp timeout_arg(_, _), do: []


  @doc false
  # Implements the handler function (handle_call, handle_cast, handle_timeout)
  def implement_handler(type, options, msg) do
    state_arg = get_state_identifier(Keyword.fetch(options, :state))
    {handler_name, handler_args} = handler_sig(type, options, msg, state_arg)

    quote bind_quoted: [
      type: type,
      handler_name: handler_name,
      handler_args: Macro.escape(handler_args, unquote: true),
      guard: Macro.escape(guard(options, :handler), unquote: true),
      body: Macro.escape(options[:do], unquote: true)
    ] do
      if guard do
        def unquote(handler_name)(unquote_splicing(handler_args))
          when unquote(guard),
          do: unquote(body)
      else
        def unquote(handler_name)(unquote_splicing(handler_args)),
          do: unquote(body)
      end
    end
  end

  defp get_state_identifier({:ok, match}),
    do: quote(do: unquote(match) = unquote(ExActor.Helper.state_var))
  defp get_state_identifier(:error), do: get_state_identifier({:ok, quote(do: _)})

  defp handler_sig(:defcall, options, msg, state_arg),
    do: {:handle_call, [msg, options[:from] || quote(do: _from), state_arg]}
  defp handler_sig(:defcast, _, msg, state_arg),
    do: {:handle_cast, [msg, state_arg]}
  defp handler_sig(:definfo, _, msg, state_arg),
    do: {:handle_info, [msg, state_arg]}



  @doc """
  Defines the info callback clause. Responses work just like with casts.

      defhandleinfo :some_message, do: ...
      defhandleinfo :another_message, state: ..., do:
  """
  defmacro defhandleinfo(msg, opts \\ [], body) do
    impl_defhandleinfo(msg, opts ++ body)
  end

  # Implements handle_info
  defp impl_defhandleinfo(msg, options) do
    quote bind_quoted: [
      msg: Macro.escape(msg, unquote: true),
      options: escape_options(options)
    ] do
      options = Keyword.merge(options, @exactor_global_options)

      ExActor.Operations.implement_handler(:definfo, options, msg)
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  @doc """
  Defines a multicall operation.

      defmulticall my_request(x, y), do: ...

      ...

      # If the process is locally registered via `:export` option
      MyServer.my_request(2, 3)
      MyServer.my_request(nodes, 2, 3)

      # The process is not locally registered via `:export` option
      MyServer.my_request(:local_alias, 2, 3)
      MyServer.my_request(nodes, :local_alias, 2, 3)

  Request format is the same as in `defcall/3`. Timeout option works just like
  with `defcall/3`.
  """
  defmacro defmulticall(req_def, options \\ [], body \\ []) do
    do_defmulticall(req_def, options ++ body)
  end

  @doc """
  Same as `defmulticall/3` but the interface function is private.
  """
  defmacro defmulticallp(req_def, options \\ [], body \\ []) do
    do_defmulticall(req_def, [{:private, true} | options] ++ body)
  end

  defp do_defmulticall(req_def, options) do
    quote bind_quoted: [
      req_def: Macro.escape(req_def, unquote: true),
      options: escape_options(options)
    ] do
      options = Keyword.merge(options, @exactor_global_options)

      ExActor.Operations.implement_request(:defcall, req_def, options)
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)

      ExActor.Operations.def_request(:multicall, req_def, Keyword.drop(options, [:do]))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end


  @doc """
  Defines an abcast operation.

      defabcast my_request(x, y), do: ...

      ...

      # If the process is locally registered via `:export` option
      MyServer.my_request(2, 3)
      MyServer.my_request(nodes, 2, 3)

      # The process is not locally registered via `:export` option
      MyServer.my_request(:local_alias, 2, 3)
      MyServer.my_request(nodes, :local_alias, 2, 3)
  """
  defmacro defabcast(req_def, options \\ [], body \\ []) do
    do_defabcast(req_def, options ++ body)
  end

  @doc """
  Same as `defabcast/3` but the interface function is private.
  """
  defmacro defabcastp(req_def, options \\ [], body \\ []) do
    do_defabcast(req_def, [{:private, true} | options] ++ body)
  end

  defp do_defabcast(req_def, options) do
    quote bind_quoted: [
      req_def: Macro.escape(req_def, unquote: true),
      options: escape_options(options)
    ] do
      options = Keyword.merge(options, @exactor_global_options)

      ExActor.Operations.implement_request(:defcast, req_def, options)
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)

      ExActor.Operations.def_request(:abcast, req_def, Keyword.drop(options, [:do]))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  defp escape_options(options) do
    Enum.map(options,
      fn
        {:export, export} -> {:export, export}
        other -> Macro.escape(other, unquote: true)
      end
    )
  end
end
