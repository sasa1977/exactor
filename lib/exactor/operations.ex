defmodule ExActor.Operations do
  @moduledoc """
  Macros that can be used for simpler definition of `gen_server` operations
  such as casts or calls.
  """

  @doc """
  Defines the starter function and initializer body.

  Examples:

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

      defstart start(x, y), gen_server_opts: [min_heap_size: 10000], do: ...

  If you need to  set `GenServer` options at runtime, use `gen_server_opts: :runtime` and
  then the starter function will receive one more argument where you can pass options:

      defstart start(x, y), gen_server_opts: :runtime do
        ...
      end

      ...

      MyActor.start(x, y, name: :foo, spawn_opts: [min_heap_size: 10000])

  Other notes:

  - If the `export` option is set while using `ExActor`, it will be honored in starters.
  - You can use patterns in arguments. Pattern matching is done on `init/1`.
    There will be just one `start` or `start_link` function for the given arity.
  - You can provide additional guard via `:when` option. The guard applies to the `init/1`.
  - Body can be omitted. In this case, just the interface function is generated, and you
    need to implement `init/1` yourself (or use `definit/2`). The initializer function
    will receive arguments in form of `{arg1, arg2, ...}` function.
  """
  defmacro defstart({fun, _, args}, opts \\ [], body \\ []) when fun in [:start, :start_link] do
    define_starter(false, fun, args, opts ++ body)
  end

  @doc """
  Same as `defstart/2` but generates a private starter function. This can be useful if you
  need to do some of your own pre- or post- processing:

      defmodule MyActor do
        # start is not exported
        defstartp start(x, y) do
          ...
        end

        def my_start do
          ...

          start(x, y)

          ...
        end
      end
  """
  defmacro defstartp({fun, _, args}, options \\ [], body \\ []) when fun in [:start, :start_link] do
    define_starter(true, fun, args, options ++ body)
  end

  defp define_starter(private, fun, args, options) do
    quote bind_quoted: [
      private: private,
      fun: Macro.escape(fun, unquote: true),
      args: Macro.escape(args || [], unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do
      named_args =
        for {arg, index} <- Enum.with_index(args) do
          Macro.var(:"arg#{index}", __MODULE__)
        end

      unless options[:export] == false do
        interface_args =
          unless options[:gen_server_opts] == :runtime do
            named_args
          else
            named_args ++ [Macro.var(:gen_server_opts, __MODULE__)]
          end

        arity = length(interface_args)

        {payload, match_pattern} = case args do
          [] -> {nil, nil}
          [_|_] ->
            {
              quote(do: {unquote_splicing(named_args)}),
              quote(do: {unquote_splicing(args)})
            }
        end

        unless HashSet.member?(@exported, {fun, arity}) do
          gen_server_opts =
            unless options[:gen_server_opts] == :runtime do
              case Module.get_attribute(__MODULE__, :exactor_global_options)[:export] do
                default when default in [nil, false] -> []
                name -> [name: name]
              end ++ (options[:gen_server_opts] || [])
            else
              Macro.var(:gen_server_opts, __MODULE__)
            end

          unless private do
            def unquote(fun)(unquote_splicing(interface_args)) do
              GenServer.unquote(fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
          else
            defp unquote(fun)(unquote_splicing(named_args)) do
              GenServer.unquote(fun)(__MODULE__, unquote(payload), unquote(gen_server_opts))
            end
          end

          @exported HashSet.put(@exported, {fun, arity})
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



  @doc """
  Defines the initializer callback.

  Examples:

      # ignoring the input argument
      definit do: HashSet.new

      # using the input argument
      definit x do
        x + 1
      end

      # pattern matching
      definit x, when: ..., do: ...
  """
  defmacro definit(arg \\ quote(do: _), opts), do: do_definit([{:arg, arg} | opts])

  defp do_definit(opts) do
    quote bind_quoted: [opts: Macro.escape(opts, unquote: true)] do
      if (opts[:when]) do
        def init(unquote_splicing([opts[:arg]]))
          when unquote(opts[:when]),
          do: unquote(opts[:do])
      else
        def init(unquote_splicing([opts[:arg]])),
          do: unquote(opts[:do])
      end
    end
  end


  @doc """
  Defines the cast callback clause and a corresponding interface fun.

  Examples:

      defcast operation, do: noreply
      defcast inc(x), state: state, do: new_state(state + x)

      # omitting interface fun
      defcast operation, export: false, do: ...

      # pattern matching
      defcast a(1), do: ...
      defcast a(2), do: ...
      defcast a(x), state: 1, do: ...
      defcast a(x), when: x > 1, do: ...
      defcast a(x), state: state, when: state > 1, do: ...
      defcast a(_), do: ...
  """
  defmacro defcast(req_def, options \\ [], body \\ []) do
    generate_funs(:defcast, req_def, options ++ body)
  end

  @doc """
  Same as `defcast/3` but the interface function is private. Can be useful when
  you need to do pre/post processing in the caller process.

  Examples:

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
  Defines the call callback clause and a corresponding interface fun.

  Examples:

      defcall operation, do: reply(response)
      defcall get, state: state, do: reply(state)
      defcall inc, state: state, do: set_and_reply(state + 1, response)

      # timeout option
      defcall long_call, state: state, timeout: :timer.seconds(10), do: ...

      # omitting interface fun
      defcall operation, export: false, do: ...

      # pattern matching
      defcall a(1), do: ...
      defcall a(2), do: ...
      defcall a(x), state: 1, do: ...
      defcall a(x), when: x > 1, do: ...
      defcall a(x), state: state, when: state > 1, do: ...
      defcall a(_), do: ...
  """
  defmacro defcall(req_def, options \\ [], body \\ []) do
    generate_funs(:defcall, req_def, options ++ body)
  end

  @doc """
  Same as `defcall/3` but the interface function is private. Can be useful when
  you need to do pre/post processing in the caller process.

  Examples:

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



  # Generation of call/cast functions. Essentially, this is just
  # deferred to be evaluated in the module context.
  defp generate_funs(type, req_def, options) do
    quote bind_quoted: [
      type: type,
      req_def: Macro.escape(req_def, unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do
      options = Keyword.merge(
        options,
        Module.get_attribute(__MODULE__, :exactor_global_options)
      )

      ExActor.Operations.def_request(type, req_def, options)
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  @doc false
  def def_request(type, req_def, options) do
    {req_name, args} = parse_req_def(req_def)
    quote do
      unquote(define_interface(type, req_name, args, options))
      unquote(
        if options[:do] do
          implement_handler(type, options, msg_payload(req_name, args))
        end
      )
    end
  end

  defp parse_req_def(req_name) when is_atom(req_name), do: {req_name, []}
  defp parse_req_def({req_name, _, args}), do: {req_name, args || []}

  defp msg_payload(req_name, nil), do: req_name
  defp msg_payload(req_name, []), do: req_name
  defp msg_payload(req_name, args), do: quote(do: {unquote_splicing([req_name | args])})


  # Defines the interface function to call/cast
  defp define_interface(type, req_name, args, options) do
    unless options[:export] == false do
      passthrough_args = stub_args(args)
      quote bind_quoted: [
        private: options[:private],
        type: type,
        req_name: req_name,
        server_fun: server_fun(type),
        interface_args: Macro.escape(interface_args(passthrough_args, options), unquote: true),
        gen_server_args: Macro.escape(gen_server_args(options, type, msg_payload(req_name, passthrough_args)), unquote: true)
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
        unless HashSet.member?(@exported, {req_name, arity}) do
          unless private do
            def unquote(req_name)(unquote_splicing(interface_args)) do
              GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
            end
          else
            defp unquote(req_name)(unquote_splicing(interface_args)) do
              GenServer.unquote(server_fun)(unquote_splicing(gen_server_args))
            end
          end

          @exported HashSet.put(@exported, {req_name, arity})
        end
      end
    end
  end

  defp server_fun(:defcast), do: :cast
  defp server_fun(:defcall), do: :call
  defp server_fun(:multicall), do: :multi_call
  defp server_fun(:abcast), do: :abcast

  defp interface_args(passthrough_args, options) do
    case options[:export] do
      nil -> [quote(do: server) | passthrough_args]
      true -> [quote(do: server) | passthrough_args]
      _registered -> passthrough_args
    end
  end

  defp stub_args([]), do: []
  defp stub_args(args) do
    for index <- 1..length(args) do
      Macro.var(:"arg#{index}", __MODULE__)
    end
  end

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
      {:{}, _, [:via, _, _]} = via -> via
    end
  end

  defp timeout_arg(options, type) do
    case {type, options[:timeout]} do
      {:defcall, timeout} when timeout != nil ->
        [timeout]
      _ -> []
    end
  end


  @doc false
  # Implements the handler function (handle_call, handle_cast, handle_timeout)
  def implement_handler(type, options, msg) do
    state_arg = get_state_identifier(options[:state])
    {handler_name, handler_args} = handler_sig(type, options, msg, state_arg)

    quote bind_quoted: [
      type: type,
      handler_name: handler_name,
      handler_args: Macro.escape(handler_args, unquote: true),
      guard: Macro.escape(options[:when], unquote: true),
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

  defp get_state_identifier(nil), do: get_state_identifier(quote(do: _))
  defp get_state_identifier(any),
    do: quote(do: unquote(any) = unquote(ExActor.Helper.state_var))

  defp handler_sig(:defcall, options, msg, state_arg),
    do: {:handle_call, [msg, options[:from] || quote(do: _from), state_arg]}
  defp handler_sig(:defcast, _, msg, state_arg),
    do: {:handle_cast, [msg, state_arg]}
  defp handler_sig(:definfo, _, msg, state_arg),
    do: {:handle_info, [msg, state_arg]}



  @doc """
  Defines the info callback clause. Responses work just like with casts.

  Examples:

      definfo :some_message, do: ...
      definfo :another_message, state: ..., do:
  """
  defmacro definfo(msg, opts \\ [], body) do
    impl_definfo(msg, opts ++ body)
  end

  # Implements handle_info
  defp impl_definfo(msg, options) do
    quote bind_quoted: [
      msg: Macro.escape(msg, unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do
      options = Keyword.merge(
        options,
        Module.get_attribute(__MODULE__, :exactor_global_options)
      )

      ExActor.Operations.implement_handler(:definfo, options, msg)
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end

  @doc """
  Defines a multicall operation.

  Examples:

      defmulticall my_request(x, y), do: ...

      ...

      # If the actor is locally registered via `:export` option
      MyActor.my_request(2, 3)
      MyActor.my_request(nodes, 2, 3)

      # The actor is not locally registered via `:export` option
      MyActor.my_request(:local_alias, 2, 3)
      MyActor.my_request(nodes, :local_alias, 2, 3)
  """
  defmacro defmulticall(req_def, options \\ [], body \\ []) do
    do_defmulticall(req_def, options ++ body)
  end

  @doc """
  Like `defmulticall/3` but the interface function is private.
  """
  defmacro defmulticallp(req_def, options \\ [], body \\ []) do
    do_defmulticall(req_def, [{:private, true} | options] ++ body)
  end

  defp do_defmulticall(req_def, options) do
    quote bind_quoted: [
      req_def: Macro.escape(req_def, unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do
      options = Keyword.merge(
        options,
        Module.get_attribute(__MODULE__, :exactor_global_options)
      )

      ExActor.Operations.def_request(:defcall, req_def, Keyword.put(options, :export, false))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)

      ExActor.Operations.def_request(:multicall, req_def, Keyword.drop(options, [:do]))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end


  @doc """
  Defines an abcast operation.

  Examples:

      defabcast my_request(x, y), do: ...

      ...

      # If the actor is locally registered via `:export` option
      MyActor.my_request(2, 3)
      MyActor.my_request(nodes, 2, 3)

      # The actor is not locally registered via `:export` option
      MyActor.my_request(:local_alias, 2, 3)
      MyActor.my_request(nodes, :local_alias, 2, 3)
  """
  defmacro defabcast(req_def, options \\ [], body \\ []) do
    do_defabcast(req_def, options ++ body)
  end

  @doc """
  Like `defabcast/3` but the interface function is private.
  """
  defmacro defabcastp(req_def, options \\ [], body \\ []) do
    do_defabcast(req_def, [{:private, true} | options] ++ body)
  end

  defp do_defabcast(req_def, options) do
    quote bind_quoted: [
      req_def: Macro.escape(req_def, unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do
      options = Keyword.merge(
        options,
        Module.get_attribute(__MODULE__, :exactor_global_options)
      )

      ExActor.Operations.def_request(:defcast, req_def, Keyword.put(options, :export, false))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)

      ExActor.Operations.def_request(:abcast, req_def, Keyword.drop(options, [:do]))
      |> ExActor.Helper.inject_to_module(__MODULE__, __ENV__)
    end
  end
end