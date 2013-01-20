defmodule ActorBuilder do
  def common_interface_funs do
    quote do
      def start do start(nil) end
      
      def call(server, msg) do
        actor_invoke(server, :call, msg)
      end
      
      def cast(server, msg) do
        actor_invoke(server, :cast, msg)
      end
    end
  end
  
  def pure_actor_interface_funs do
    quote do
      def start(initial_state) do
        {:ok, pid} = :gen_server.start(__MODULE__, initial_state, [])
        pid
      end
      
      defp actor_invoke(server, type, msg) do
        apply(:gen_server, type, [server, msg])
      end
      
      unquote(common_interface_funs)
    end
  end
  
  def objectified_actor_interface_funs do
    quote do
      require Objectify
      
      def start(initial_state) do
        {:ok, pid} = :gen_server.start(__MODULE__, initial_state, [])
        instance(pid)
      end
      
      defp actor_invoke(server, :cast, msg) do
        apply(:gen_server, :cast, [server, msg])
        instance(server)
      end
      
      defp actor_invoke(server, type, msg) do
        apply(:gen_server, type, [server, msg])
      end
      
      def this do instance(self) end
      defp instance(pid) do Objectify.wrap(__MODULE__, pid) end
      
      unquote(common_interface_funs)
    end
  end
  
  # root of transformation
  def transform({:__block__, line, main_block}) do
    main_block = [
      (quote do
        use GenServer.Behaviour
        import GenX.GenServer
        import ExActor.Privates
      end) | 
      do_transform(main_block)
    ]

    {:__block__, line, main_block}
  end

  def transform(tuple) when is_tuple(tuple) do
    # transform of a single clause block
    transform({:__block__, elem(tuple, 1), [tuple]})
  end

  def transform(any) do any end
  
  defp do_transform(list) when is_list(list) do
    lc element inlist list do do_transform(element) end
  end
  
  defp do_transform(tuple) when is_tuple(tuple) do
    transform_tuple(tuple)
  end
  
  defp do_transform(any) do any end
  
  defp transform_tuple(tuple) do list_to_tuple(do_transform(tuple_to_list(tuple))) end
  
  # Generation of a handle_call clause
  defp implementation_fun({:defcall, line, [{fun_name, _, args}, [do: body]]}) do
    [state_arg | impl_args] = args
    {state_arg, state_identifier} = get_state_identifier(line, state_arg)
    
    def_fun(:handle_call, line, 
      [{:"{}", line, [fun_name | impl_args]}, {:_from, line, :nil}, state_arg],
      exec_and_handle_response(:handle_call_response, state_identifier, body)
    )
  end
  
  # generation of a handle_cast clause
  defp implementation_fun({:defcast, line, [{fun_name, _, args}, [do: body]]}) do
    [state_arg | impl_args] = args
    {state_arg, state_identifier} = get_state_identifier(line, state_arg)

    def_fun(:handle_cast, line, 
      [{:"{}", line, [fun_name | impl_args]}, state_arg],
      exec_and_handle_response(:handle_cast_response, state_identifier, body)
    )
  end
  
  defp def_fun(fun_name, line, args, body) do
    # Manual generation of functions, so I can insert correct line number
    {:def, line, [{fun_name, line, args}, [do: body]]}
  end
  
  defp exec_and_handle_response(handler, state_identifier, body) do
    quote do
      unquote(handler)((fn() -> unquote(body) end).(), unquote(state_identifier))
    end
  end
  
  # Gets or makes a named identifier for a state parameter. This identifier will be used
  # for generic response handling.
  defp get_state_identifier(_, {:=, _, [_, state_identifier]} = state_arg) do
    {state_arg, state_identifier}
  end
  
  defp get_state_identifier(line, any) do
    get_state_identifier(line, {:=, line, [any, {:___generated_state, line, nil}]})
  end
end