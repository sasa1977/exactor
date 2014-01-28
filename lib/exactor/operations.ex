defmodule ExActor.Operations do
  defmacro definit(opts), do: do_definit(opts)
  defmacro definit(input, opts), do: do_definit([{:input, input} | opts])

  defp do_definit(opts) do
    quote bind_quoted: [opts: Macro.escape(opts, unquote: true)] do
      if (opts[:when]) do
        def init(unquote_splicing([opts[:input] || quote(do: _)])) when unquote(opts[:when]) do
          initial_state(unquote(opts[:do]))
        end
      else
        def init(unquote_splicing([opts[:input] || quote(do: _)])) do
          initial_state(unquote(opts[:do]))
        end
      end
    end
  end



  defmacro defcast(cast, body) do
    generate_funs(:defcast, cast, body ++ [module: __CALLER__.module])
  end
  
  defmacro defcast(cast, options, body) do
    generate_funs(:defcast, cast, Keyword.from_enum(options ++ body  ++ [module: __CALLER__.module]))
  end
  
  defmacro defcall(call, body) do
    generate_funs(:defcall, call, body ++ [module: __CALLER__.module])
  end
  
  defmacro defcall(call, options, body) do
    generate_funs(:defcall, call, Keyword.from_enum(options ++ body ++ [module: __CALLER__.module]))
  end
  
  defp generate_funs(type, name, options) do
    quote do
      unquote(transfer_options(type, name, options))
      unquote(define_interface(type))
      unquote(define_handler(type))
    end
  end



  defp transfer_options(type, name, options) do
    quote do
      {name, args} = case unquote(Macro.escape(name, unquote: true)) do
        name when is_atom(name) -> {name, []}
        {name, _, args} -> {name, args || []}
      end
      
      options = 
        Module.get_attribute(__MODULE__, :exactor_global_options)
        |> Keyword.merge(unquote(Macro.escape(options, unquote: true)))

      type = unquote(Macro.escape(type, unquote: true))
      msg = ExActor.Helper.msg_payload(name, args)
    end
  end
  

  defp define_interface(type) do
    quote do
      arity = length(args) + case options[:export] do
        nil -> 1
        true -> 1
        _ -> 0
      end

      unless options[:export] == false or HashSet.member?(@exported, {name, arity}) do
        server_fun = unquote(server_fun(type))
        unquote(quoted_interface)

        @exported HashSet.put(@exported, {name, arity})
      end
    end
  end

  defp quoted_interface do
    quote bind_quoted: [] do
      {server_arg, interface_args} = ExActor.Helper.interface_args(args, options)
      send_msg = ExActor.Helper.msg_payload(name, interface_args)
      interface_args = case server_arg do
        nil -> interface_args
        _ -> [server_arg | interface_args]
      end

      def unquote(name)(unquote_splicing(interface_args)) do
        :gen_server.unquote(server_fun)(
          unquote_splicing(ExActor.Helper.server_args(options, type, send_msg))
        )
      end
    end
  end

  
  defp server_fun(:defcast), do: :cast
  defp server_fun(:defcall), do: :call
  
  
  defp define_handler(type) do
    quote bind_quoted: [type: type, wrapped_type: wrapper(type)] do
      {handler_name, handler_args, state_identifier} = ExActor.Helper.handler_sig(type, options, msg)
      guard = options[:when]
      
      handler_body = ExActor.Helper.wrap_handler_body(wrapped_type, state_identifier, options[:do])

      if guard do
        def unquote(handler_name)(
          unquote_splicing(handler_args)
        ) when unquote(guard), do: unquote(handler_body)
      else
        def unquote(handler_name)(
          unquote_splicing(handler_args)
        ), do: unquote(handler_body)
      end
    end
  end

  
  defp wrapper(:defcast), do: :handle_cast_response
  defp wrapper(:defcall), do: :handle_call_response



  defmacro definfo(msg, options), do: impl_definfo(msg, options)
  defmacro definfo(msg, opts1, opts2) do
    impl_definfo(msg, opts1 ++ opts2)
  end

  defp impl_definfo(msg, options) do
    quote bind_quoted: [
      msg: Macro.escape(msg, unquote: true), 
      options: Macro.escape(options, unquote: true)
    ] do
      
      {state_arg, state_identifier} = ExActor.Helper.get_state_identifier(
        options[:state] || quote(do: _)
      )
      
      handler_body = ExActor.Helper.wrap_handler_body(:handle_cast_response, state_identifier, options[:do])
      if options[:when] do
        def handle_info(unquote(msg), unquote(state_arg)) when unquote(options[:when]) do
          unquote(handler_body)
        end
      else
        def handle_info(unquote(msg), unquote(state_arg)) do
          unquote(handler_body)
        end
      end
    end
  end
end