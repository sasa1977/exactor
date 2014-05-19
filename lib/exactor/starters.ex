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

  In `gen_server_options`, you can include `name: registered_name` to set the
  registered name of the server in runtime. Following patterns are allowed:

    - `:some_alias` - registers locally
    - `{:local, :some_alias}` - same as above
    - `{:global, :some_alias}` - registers globally
    - `{:via, Module, :alias}` - uses Module for registration
  """
  defmacro __using__(_) do
    quote do
      def start(args \\ nil, options \\ []) do
        apply(
          :gen_server, :start,
          ExActor.Helper.start_args([unquote_splicing(start_args(__CALLER__))])
        )
      end

      def start_link(args \\ nil, options \\ []) do
        apply(
          :gen_server, :start_link,
          ExActor.Helper.start_args([unquote_splicing(start_args(__CALLER__))])
        )
      end

      for fun <- [:start, :start_link], arity <- [0, 1, 2] do
        {fun, arity}
      end
      |> defoverridable
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
