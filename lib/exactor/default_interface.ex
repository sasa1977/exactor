defmodule ExActor.DefaultInterface do
  defmacro __using__(_) do
    quote do
      def start(args // nil, options // []) do
        :gen_server.start(unquote_splicing(start_args(__CALLER__)))
      end
      
      def start_link(args // nil, options // []) do
        :gen_server.start_link(unquote_splicing(start_args(__CALLER__)))
      end

      unquote(def_initializer(__CALLER__))
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
    initial_state = 
      Module.get_attribute(caller.module, :exactor_global_options)
      |> Dict.fetch(:initial_state)

    case initial_state do
      :error -> nil
      {:ok, state} ->
        quote do
          def init(_), do: initial_state(unquote(state))
        end
    end
  end
end