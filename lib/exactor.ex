defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors. See README.md for details.
  """
  
  defmacro __using__(opts) do
    Module.put_attribute(__CALLER__.module, :exactor_global_options, opts || [])
    
    quote do
      use GenServer.Behaviour
      import ExActor, only: [
        definit: 1, definit: 2, 
        defcall: 2, defcall: 3,
        defcast: 2, defcast: 3,
        definfo: 2, definfo: 3,
        handle_call_response: 2, handle_cast_response: 2,
        initial_state: 1, new_state: 1, reply: 2, set_and_reply: 2,
        delegate_to: 2
      ]
      unquote(interface_funs(__CALLER__))
      
      @exported HashSet.new
    end
  end
  
  defp interface_funs(caller) do
    quote do
      def start(args // nil, options // []) do
        :gen_server.start(unquote_splicing(start_args(caller)))
      end
      
      def start_link(args // nil, options // []) do
        :gen_server.start_link(unquote_splicing(start_args(caller)))
      end

      unquote(def_initializer(caller))
    end
  end

  defp start_args(caller) do
    defargs = [quote(do: __MODULE__), quote(do: args), quote(do: options)]
    case Module.get_attribute(caller.module, :exactor_global_options)[:export] do
      default when default in [nil, false, true] -> defargs
      
      local_name when is_atom(local_name) ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:local, local_name} ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:global, global_name} ->
        [quote(do: {:global, unquote(global_name)}) | defargs]
      
      _ -> defargs
    end
  end

  defp def_initializer(caller) do
    case Module.get_attribute(caller.module, :exactor_global_options)[:initial_state] do
      nil -> nil
      state ->
        quote do
          def init(_), do: initial_state(unquote(state))
        end
    end
  end

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
      
      if options[:when] do
        def handle_info(unquote(msg), unquote(state_arg)) when unquote(options[:when]) do
          (unquote(options[:do])) 
          |> handle_cast_response(unquote(state_identifier))
        end
      else
        def handle_info(unquote(msg), unquote(state_arg)) do
          (unquote(options[:do])) 
          |> handle_cast_response(unquote(state_identifier))
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
  
  defmacro reply(response, new_state) do 
    IO.puts "reply/2 is deprecated. Please use set_and_reply/2"

    quote do
      {:reply, unquote(response), unquote(new_state)} 
    end
  end

  defmacro set_and_reply(new_state, response) do
    quote do
      {:reply, unquote(response), unquote(new_state)}
    end
  end

  defmacro initial_state(state) do 
    quote do
      {:ok, unquote(state)}
    end
  end

  defmacro new_state(state) do
    quote do
      {:noreply, unquote(state)}
    end
  end

  def handle_call_response({:reply, _, _} = r, _) do r end
  def handle_call_response({:reply, _, _, _} = r, _) do r end
  def handle_call_response({:noreply, _} = r, _) do r end
  def handle_call_response({:noreply, _, _} = r, _) do r end
  def handle_call_response({:stop, _, _} = r, _) do r end
  def handle_call_response({:stop, _, _, _} = r, _) do r end
  def handle_call_response(reply, state) do {:reply, reply, state} end

  def handle_cast_response({:noreply, _} = r, _) do r end
  def handle_cast_response({:noreply, _, _} = r, _) do r end
  def handle_cast_response({:stop, _, _} = r, _) do r end
  def handle_cast_response(_, state) do {:noreply, state} end


  defmacro delegate_to(target_module, opts) do
    statements(opts[:do])
    |> Enum.map(&(parse_instruction(target_module, &1)))
  end

  defp statements({:__block__, _, statements}), do: statements
  defp statements(statement), do: [statement]


  defp parse_instruction(target_module, {:init, _, _}) do
    quote do
      definit do
        unquote(target_module).new
      end
    end
  end

  defp parse_instruction(target_module, {:query, _, [{:/, _, [{fun, _, _}, arity]}]}) do
    make_delegate(:defcall, fun, arity, forward_call(target_module, fun, arity))
  end

  defp parse_instruction(target_module, {:trans, _, [{:/, _, [{fun, _, _}, arity]}]}) do
    make_delegate(:defcast, fun, arity, 
      quote do
        unquote(forward_call(target_module, fun, arity))
        |> new_state
      end
    )
  end


  defp make_delegate(type, fun, arity, code) do
    quote do
      unquote(type)(
        unquote(fun)(unquote_splicing(make_args(arity))), 
        state: state, 
        do: unquote(code)
      )
    end
  end


  defp forward_call(target_module, fun, arity) do
    full_args = [quote(do: state) | make_args(arity)]

    quote do
      unquote(target_module).unquote(fun)(unquote_splicing(full_args))
    end
  end


  defp make_args(arity) when arity > 0 do
    1..arity
    |> Enum.map(&{:"arg#{&1}", [], nil})
    |> tl
  end
end
