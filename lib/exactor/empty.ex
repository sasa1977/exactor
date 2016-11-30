defmodule ExActor.Empty do
  @moduledoc """
  Empty predefine. Imports all ExActor macros, but doesn't provide any default
  implementation. The declaring module must define all required functions
  of the `gen_server` behaviour.

  Example:

      defmodule MyServer do
        use ExActor.Empty

        # define all gen_server required functions
      end

      # Locally registered name:
      use ExActor.Empty, export: :some_registered_name

      # Globally registered name:
      use ExActor.Empty, export: {:global, :global_registered_name}
  """
  defmacro __using__(opts) do
    quote do
      @behaviour :gen_server

      @generated_funs MapSet.new

      import ExActor.Operations
      import ExActor.Responders

      unquote(ExActor.Helper.init_generation_state(opts))
    end
  end
end
