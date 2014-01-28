defmodule ExActor.Starters do
  @moduledoc """
  A mixin that provides `start/2` and `start_link/2` interface functions that 
  can be used to start new instances of the gen_server.

  Examples:

      Actor.start   # same as Actor.start(nil)
      Actor.start(init_arg)
      Actor.start(init_arg, gen_server_options)

      Actor.start_link  # same as Actor.start_link(nil)
      Actor.start_link(init_arg)
      Actor.start_link(init_arg, gen_server_options)
  """
  defmacro __using__(_) do
    quote do
      def start(args // nil, options // []) do
        :gen_server.start(unquote_splicing(start_args(__CALLER__)))
      end
      
      def start_link(args // nil, options // []) do
        :gen_server.start_link(unquote_splicing(start_args(__CALLER__)))
      end
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
end