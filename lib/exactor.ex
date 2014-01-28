defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors. See README.md for details.
  """
  
  defmacro __using__(opts) do
    quote do
      IO.write "use ExActor is deprecated. Please use explicit ExActor predefine (e.g. ExActor.GenServer) instead.\n#{Exception.format_stacktrace}"
      use ExActor.GenServer, unquote(opts)
    end
  end
end
