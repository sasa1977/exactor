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

  All starter functions are overridable, and you can redefine them in your module.
  If you don't want these functions, you can exclude them, by providing
  `starters: false` option. For example:

      use ExActor.GenServer, starters: false
  """
  defmacro __using__(_) do
    case Module.get_attribute(__CALLER__.module, :exactor_global_options)[:starters] do
      nil -> generate_starters(__CALLER__)
      true -> generate_starters(__CALLER__)
      false -> nil
    end
  end

  defp generate_starters(caller) do
    quote do
      def start(args \\ nil, options \\ []) do
        GenServer.start(unquote_splicing(start_args(caller)))
      end

      def start_link(args \\ nil, options \\ []) do
        GenServer.start_link(unquote_splicing(start_args(caller)))
      end

      for fun <- [:start, :start_link], arity <- [0, 1, 2] do
        {fun, arity}
      end
      |> defoverridable
    end
  end

  defp start_args(caller) do
    options = case Module.get_attribute(caller.module, :exactor_global_options)[:export] do
      default when default in [nil, false, true] -> quote(do: options)
      name -> quote(do: [{:name, unquote(name)} | options])
    end

    [quote(do: __MODULE__), quote(do: args), options]
  end
end
