defmodule ExActor.Strict do
  @moduledoc """
  Predefine that provides strict default implementation for `gen_server`
  required functions. Default implementation will cause the `gen_server` to
  be stopped. This predefine is useful if you want to need only some parts of
  the server to be implemented, and want to fail for everything else that
  happens on the server.

  All ExActor macros are imported.

  Example:

      defmodule MyServer do
        use ExActor.Strict

        # without this the server can't be started
        definit do: ...

        ...
      end

      # Locally registered name:
      use ExActor.Strict, export: :some_registered_name

      # Globally registered name:
      use ExActor.Strict, export: {:global, :global_registered_name}
  """
  defmacro __using__(opts) do
    quote do
      use ExActor.Behaviour.Strict

      @generated_funs MapSet.new

      import ExActor.Operations
      import ExActor.Responders

      unquote(ExActor.Helper.init_generation_state(opts))
    end
  end
end
