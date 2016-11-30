defmodule ExActor.Tolerant do
  @moduledoc """
  Predefine that provides tolerant default implementation for `gen_server`
  required functions. Default implementation will cause the `gen_server` to
  ignore messages (e.g. calls/casts).

  All ExActor macros are imported.

  Example:

      defmodule MyServer do
        use ExActor.Tolerant
        ...
      end

      # Locally registered name:
      use ExActor.Tolerant, export: :some_registered_name

      # Globally registered name:
      use ExActor.Tolerant, export: {:global, :global_registered_name}
  """

  defmacro __using__(opts) do
    quote do
      use ExActor.Behaviour.Tolerant

      @generated_funs MapSet.new

      import ExActor.Operations
      import ExActor.Responders

      unquote(ExActor.Helper.init_generation_state(opts))
    end
  end
end
