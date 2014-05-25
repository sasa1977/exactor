defmodule ExActor.GenServer do
  @moduledoc """
  Predefine that relies on `GenServer` provided by Elixir standard
  lib. All ExActor macros are imported.

  Example:

      defmodule MyActor do
        use ExActor.GenServer
        ...
      end

      # Setting the initial state:
      use ExActor.GenServer, initial_state: HashDict.new

      # Locally registered name:
      use ExActor.GenServer, export: :some_registered_name

      # Globally registered name:
      use ExActor.GenServer, export: {:global, :global_registered_name}
  """
  defmacro __using__(opts) do
    ExActor.Helper.init_global_options(__CALLER__, opts)

    quote do
      use GenServer

      import ExActor.Operations
      import ExActor.Responders
      use ExActor.Starters

      unquote(ExActor.Helper.def_initializer(__CALLER__))
      unquote(ExActor.Helper.init_exported)
    end
  end
end