defmodule ExActor.Empty do
  @moduledoc """
  Empty predefine. Imports all ExActor macros, but doesn't provide any default 
  implementation. The declaring module must define all required functions
  of the `gen_server` behaviour.

  Example:

      defmodule MyActor do
        use ExActor.Empty

        # define all gen_server required functions
      end
      
      # Setting the initial state:
      use ExActor.Empty, initial_state: HashDict.new

      # Locally registered name:
      use ExActor.Empty, export: :some_registered_name

      # Globally registered name:
      use ExActor.Empty, export: {:global, :global_registered_name}
  """
  defmacro __using__(opts) do
    ExActor.Helper.init_global_options(__CALLER__, opts)

    quote do
      @behaviour :gen_server

      import ExActor.Operations
      import ExActor.Responders
      use ExActor.Starters
      
      unquote(ExActor.Helper.def_initializer(__CALLER__))
      unquote(ExActor.Helper.init_exported)
    end
  end
end