defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors. For quick start and examples, 
  please check [README.md](https://github.com/sasa1977/exactor/blob/master/README.md). 
  For detailed reference, take a look at the docs for corresponding modules.
  """
  
  defmacro __using__(opts) do
    quote do
      IO.write "use ExActor is deprecated. Please use explicit ExActor predefine (e.g. ExActor.GenServer) instead.\nSee CHANGELOG.md for explanation\n#{Exception.format_stacktrace}"
      use ExActor.GenServer, unquote(opts)
    end
  end
end
