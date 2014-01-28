defmodule ExActor.Tolerant do
  @moduledoc """
  Predefine that provides tolerant default implementation for `gen_server` 
  required functions. Default implementation will cause the `gen_server` to
  ignore messages (e.g. calls/casts).

  All ExActor macros are imported.

  Example:

      defmodule MyActor do
        use ExActor.Tolerant
        ...
      end

      # Setting the initial state:
      use ExActor.Tolerant, initial_state: HashDict.new

      # Locally registered name:
      use ExActor.Tolerant, export: :some_registered_name

      # Globally registered name:
      use ExActor.Tolerant, export: {:global, :global_registered_name}
  """

  defmacro __using__(opts) do
    ExActor.Helper.init_global_options(__CALLER__, opts)

    quote do
      use ExActor.Behaviour.Tolerant

      import ExActor.Operations
      import ExActor.Responders
      use ExActor.DefaultInterface
      
      unquote(ExActor.Helper.init_exported)
    end
  end
end