defmodule ActorBuilder do
  def pure_actor_interface_funs do
    quote do
      def start do start(nil) end
      
      def start(initial_state) do
        :gen_server.start(__MODULE__, initial_state, [])
      end
      
      defoverridable start: 1
    end
  end
  
  def objectified_actor_interface_funs do
    quote do
      require Objectify
      
      unquote(pure_actor_interface_funs)
      
      def start(initial_state) do
        decorate(super)
      end
      defoverridable start: 1
      
      def this do instance(self) end
      defp instance(pid) do Objectify.wrap(__MODULE__, pid) end
      
      defp decorate({:ok, response}), do: {:ok, instance(response)}
      defp decorate(any), do: any
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
      transform_body(main_block)
    ]

    {:__block__, line, main_block}
  end

  def transform(tuple) when is_tuple(tuple) do
    # transform of a single clause block
    transform({:__block__, elem(tuple, 1), [tuple]})
  end

  def transform(any) do any end
  
  defp transform_body({:defcall, line, [args, opts]}) do
    handle_genx_macro(:defcall, :handle_call_response, line, args, opts)
  end
  
  defp transform_body({:defcast, line, [args, opts]}) do
    handle_genx_macro(:defcast, :handle_cast_response, line, args, opts)
  end
  
  defp transform_body(list) when is_list(list) do
    lc element inlist list do transform_body(element) end
  end
  
  defp transform_body(tuple) when is_tuple(tuple) do
    transform_tuple(tuple)
  end
  
  defp transform_body(any) do any end
  
  defp transform_tuple(tuple) do list_to_tuple(transform_body(tuple_to_list(tuple))) end
  
  # wrapping of genx calls
  defp handle_genx_macro(macro, response_wrapper, line, args, opts) do
    {state_arg, state_identifier} = get_state_identifier(line, opts[:state] || {:_, line, :quoted})
    handler_body = opts[:do]
    
    opts = (opts />
      List.keydelete(:state, 0) />
      List.keydelete(:do, 0)
      ) ++ [state: state_arg, do: wrap_handler_body(response_wrapper, state_identifier, handler_body)]

    {macro, line, [args, opts]}
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
end