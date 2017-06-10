defmodule ExActor.GenServer do
  @moduledoc """
  Predefine that relies on `GenServer` provided by Elixir standard
  lib. All ExActor macros are imported.

  Example:

      defmodule MyServer do
        use ExActor.GenServer
        ...
      end

      # Locally registered name:
      use ExActor.GenServer, export: :some_registered_name

      # Globally registered name:
      use ExActor.GenServer, export: {:global, :global_registered_name}
  """
  defmacro __using__(opts) do
    quote do
      use GenServer

      @generated_funs MapSet.new

      import ExActor.Operations
      import ExActor.Responders

      unquote(ExActor.Helper.init_generation_state(opts))

      @doc """
      By default, the `server` argument given
      to the different interface functions, is expected to be a process identifier,
      unless overridden by the `export:` option.

      But by providing a custom implementation of `server_pid/1`, you can map an identifier
      to a PID by some other means.
      """
      def server_pid(server_reference) do
        server_reference
      end

      defoverridable [server_pid: 1]
    end
  end
end
