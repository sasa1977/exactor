defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors. See README.md for details.
  """
  
  defmacro __using__(opts) do
    Module.put_attribute(__CALLER__.module, :exactor_global_options, opts || [])
    
    quote do
      use GenServer.Behaviour
      
      import ExActor.Macros
      use ExActor.DefaultInterface
      
      @exported HashSet.new
    end
  end

  # Temporary, to preserve stack
  def propagate(x), do: x
end
