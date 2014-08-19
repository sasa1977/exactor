defmodule ExActor.Operations do
  @moduledoc """
  Macros that can be used for simpler definition of `gen_server` operations
  such as casts or calls.
  """

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
  defmacro defcast(req_def, options \\ [], body) do
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
  defmacro defcastp(req_def, options \\ [], body) do
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
  defmacro defcall(req_def, options \\ [], body) do
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
  defmacro defcallp(req_def, options \\ [], body) do
    generate_funs(:defcall, req_def, [{:private, true} | options] ++ body)
  end

  @doc """
  Defines the info callback clause. Responses work just like with casts.

  Examples:

      definfo :some_message, do: ...
      definfo :another_message, state: ..., do:
  """
  defmacro definfo(msg, opts \\ [], body) do
    impl_definfo(msg, opts ++ body)
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
      unquote(implement_handler(type, options, msg_payload(req_name, args)))
    end
  end

  defp parse_req_def(req_name) when is_atom(req_name), do: {req_name, []}
  defp parse_req_def({req_name, _, args}), do: {req_name, args || []}

  defp msg_payload(req_name, nil), do: req_name
  defp msg_payload(req_name, []), do: req_name
  defp msg_payload(req_name, args), do: quote(do: {unquote_splicing([req_name | args])})


  # Defines the interface function to call/cast
  defp define_interface(type, req_name, args, options) do
    passthrough_args = stub_args(args)

    unless options[:export] == false do
      quote bind_quoted: [
        private: options[:private],
        type: type,
        req_name: req_name,
        server_fun: server_fun(type),
        arity: interface_arity(length(args), options[:export]),
        interface_args: Macro.escape(interface_args(passthrough_args, options), unquote: true),
        gen_server_args: Macro.escape(gen_server_args(options, type, msg_payload(req_name, passthrough_args)), unquote: true)
      ] do
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

  defp interface_arity(args_num, nil), do: args_num + 1
  defp interface_arity(args_num, true), do: args_num + 1
  defp interface_arity(args_num, _), do: args_num

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
      {:"arg#{index}", [], __MODULE__}
    end
  end

  defp gen_server_args(options, type, msg) do
    [server_ref(options), msg] ++ timeout_arg(options, type)
  end

  defp server_ref(options) do
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
    do: quote(do: unquote(any) = var!(___generated_state))

  defp handler_sig(:defcall, options, msg, state_arg),
    do: {:handle_call, [msg, options[:from] || quote(do: _from), state_arg]}
  defp handler_sig(:defcast, _, msg, state_arg),
    do: {:handle_cast, [msg, state_arg]}
  defp handler_sig(:definfo, _, msg, state_arg),
    do: {:handle_info, [msg, state_arg]}


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
end