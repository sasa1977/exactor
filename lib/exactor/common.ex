defmodule ExActor.Common do
  @moduledoc false

  # This module adds common functionality to all ExActor behaviours.
  defmacro __using__(_opts) do
    quote do
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
