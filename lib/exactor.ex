defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors.

  Examples:
    defmodule Calculator do
      use ExActor
      
      defcast inc(x), state: state, do: new_state(state + x)
      defcast dec(x), state: state, do: new_state(state - x)
      defcall get, state: state, do: state
    end
    
    {:ok, calculator} = Calculator.start(0)
    Calculator.inc(calculator, 10)
    Calculator.dec(calculator, 5)
    IO.puts(Calculator.get(calculator))
    
  OO approach (based on tuple modules):
    defmodule Calculator do
      use ExActor
      
      defcast inc(x), state: state, do: new_state(state + x)
      defcast dec(x), state: state, do: new_state(state - x)
      defcall get, state: state, do: state
    end
    
    calculator = Actor.actor_start_link(0)
    IO.puts(calculator.inc(10).dec(5).get)
  """
  
  defmacro __using__(opts) do
    Module.put_attribute(__CALLER__.module, :exactor_global_options, opts || [])
    
    quote do
      use GenServer.Behaviour
      import ExActor.Privates
      unquote(interface_funs(__CALLER__))
    end
  end
  
  def interface_funs(caller) do
    quote do
      def start(args // nil, options // []) do
        :gen_server.start(unquote_splicing(start_args(caller)))
      end
      
      def start_link(args // nil, options // []) do
        :gen_server.start_link(unquote_splicing(start_args(caller)))
      end

      def actor_start(args // nil, options // []) do
        start(args, options) |> response_to_actor
      end

      def actor_start_link(args // nil, options // []) do
        start_link(args, options) |> response_to_actor
      end

      defp response_to_actor({:ok, pid}), do: actor(pid)
      defp response_to_actor(any), do: any
      
      def this, do: actor(self)
      def pid({module, pid}) when module === __MODULE__, do: pid
      def actor(pid), do: {__MODULE__, pid}
    end
  end
  
  defp start_args(caller) do
    defargs = [quote(do: __MODULE__), quote(do: args), quote(do: options)]
    case Module.get_attribute(caller.module, :exactor_global_options)[:export] do
      nil -> defargs
      false -> defargs
      true -> defargs
      
      local_name when is_atom(local_name) ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:local, local_name} ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:global, global_name} ->
        [quote(do: {:global, unquote(global_name)}) | defargs]
      
      _ -> defargs
    end
  end
  
  defmodule Privates do
    defmacro defcast(cast, body) do
      wrap_and_delegate(__CALLER__, :defcast, cast, body ++ [module: __CALLER__.module])
    end
    
    defmacro defcast(cast, options, body) do
      wrap_and_delegate(__CALLER__, :defcast, cast, Keyword.from_enum(options ++ body  ++ [module: __CALLER__.module]))
    end
    
    defmacro defcall(call, body) do
      wrap_and_delegate(__CALLER__, :defcall, call, body ++ [module: __CALLER__.module])
    end
    
    defmacro defcall(call, options, body) do
      wrap_and_delegate(__CALLER__, :defcall, call, Keyword.from_enum(options ++ body ++ [module: __CALLER__.module]))
    end
    
    defp wrap_and_delegate(caller, type, {_, _, args} = name, options) do
      {state_arg, state_identifier} = get_state_identifier([], options[:state] || {:_, [], :quoted})
      handler_body = options[:do]
      
      msg = msg_payload(name, args)
      
      options =
        Keyword.merge(Module.get_attribute(caller.module, :exactor_global_options), options) |>
        Keyword.merge(
          state: state_arg,
          do: wrap_handler_body(wrapper(type), state_identifier, handler_body),
          server_module: :gen_server,
          server_fun: server_fun(type),
          handler_name: handler_name(type, msg, state_arg),
          name: name,
          msg: msg
        )

      (quote do
        unquote(make_handler(options))
        unquote(make_interface(options))
      end)
    end
    
    defp msg_payload({function, _, _}, nil), do: function
    defp msg_payload({function, _, _}, []), do: function
    defp msg_payload({function, _, _}, args), do: quote(do: {unquote_splicing([function | args])})
    
    defp server_fun(:defcast), do: :cast
    defp server_fun(:defcall), do: :call
    
    defp handler_name(:defcast, msg, state_arg) do
      quote do
        handle_cast(unquote(msg), unquote(state_arg))
      end
    end
    
    defp handler_name(:defcall, msg, state_arg) do
      quote do
        handle_call(unquote(msg), _from, unquote(state_arg))
      end
    end
    
    defp tupmod?(options), do: options[:tupmod]
    
    defp make_interface(options) do
      if define_interface?(options) do
        interface_defined!(options)
        quote do
          unquote(make_obj_interface(options))
          unquote(make_fun_interface(options))
          defoverridable [{unquote(fun_name(options)), unquote(interface_arity(options))}]
        end
      else
        nil
      end
    end
    
    defp make_obj_interface(options) do
      options = Keyword.put(options, :tupmod, true)
      
      call = quote do
        apply(
          unquote(options[:server_module]),
          unquote(options[:server_fun]),
          [unquote_splicing(server_args(options))]
        )
      end
      
      if options[:server_fun] == :cast do
        call = quote do
          unquote(call)
          actor(pid)
        end
      end
      
      quote do
        def unquote(interface_sig(options)) when module == __MODULE__ do
          unquote(call)
        end
      end
    end
    
    defp make_fun_interface(options) do
      options = Keyword.put(options, :tupmod, false)
      quote do
        def unquote(interface_sig(options)) do
          apply(
            unquote(options[:server_module]),
            unquote(options[:server_fun]),
            [unquote_splicing(server_args(options))]
          )
        end
      end
    end
    
    defp fun_name(options), do: elem(options[:name], 0)
    defp interface_args(options), do: elem(interface_sig(options), 2)
    defp interface_arity(options), do: length(interface_args(options))    
    defp interface_id(options), do: {fun_name(options), interface_arity(options)}
    
    defp interface_sig(options) do
      quote do
        unquote(fun_name(options))(unquote_splicing(full_interface_args(options)))
      end
    end
    
    defp full_interface_args(options) do
      args = elem(options[:name], 2) || []
      case tupmod?(options) do
        true ->
          args ++ obj_server_arg
        _ -> 
          fun_server_arg(options) ++ args
      end
    end
    
    defp fun_server_arg(options) do
      cond do
        (options[:export] || true) == true -> [quote(do: server)]
        true -> []
      end
    end
    
    defp obj_server_arg do
      [quote(do: {module, pid})]
    end
    
    defp exactor_interfaces(options), do: (Module.get_attribute(options[:module], :exactor_interfaces) || HashDict.new)
    
    defp define_interface?(options) do
      should_export?(options) and not interface_defined?(options)
    end
    
    defp interface_defined?(options) do
      exactor_interfaces(options)[interface_id(options)] == true
    end
    
    defp should_export?(options), do: options[:export] != false
    
    defp interface_defined!(options) do
      Module.put_attribute(options[:module], :exactor_interfaces, 
        Dict.put(exactor_interfaces(options), interface_id(options), true)
      )
    end
    
    defp server_args(options) do
      [server_ref(options), options[:msg]] ++ timeout_arg(options)
    end
    
    defp server_ref(options) do
      case tupmod?(options) do
        true -> quote(do: pid)
        _ ->
          case options[:export] do
            nil -> quote(do: server)
            true -> quote(do: server)
            local when is_atom(local) -> local
            {:local, local} -> local
            {:global, _} = global -> global
          end
      end
    end
    
    defp timeout_arg(options) do
      case {options[:server_fun], options[:timeout]} do
        {:call, timeout} when timeout != nil ->
          [timeout]
        _ -> []
      end
    end
    
    
    defp make_handler(options) do
      quote do
        def unquote(options[:handler_name]) do
          unquote(options[:do])
        end
      end
    end
    
    
    defp get_state_identifier(_, {:=, _, [_, state_identifier]} = state_arg) do
      {state_arg, state_identifier}
    end

    defp get_state_identifier(line, any) do
      get_state_identifier(line, {:=, line, [any, {:___generated_state, line, nil}]})
    end
    
    defp wrap_handler_body(handler, state_identifier, body) do
      quote do
        unquote(handler)((fn() -> unquote(body) end).(), unquote(state_identifier))
      end
    end
    
    defp wrapper(:defcast), do: :handle_cast_response
    defp wrapper(:defcall), do: :handle_call_response
    
    
    
    def reply(response, new_state) do {:reply, response, new_state} end
    def initial_state(state) do {:ok, state} end
    def new_state(state) do {:noreply, state} end

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
  end
end
