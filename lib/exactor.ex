defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors. See README.md for details.
  """
  
  defmacro __using__(opts) do
    Module.put_attribute(__CALLER__.module, :exactor_global_options, opts || [])
    
    quote do
      use GenServer.Behaviour
      
      import ExActor.Operations
      import ExActor.Responders
      use ExActor.DefaultInterface
      
      @exported HashSet.new
    end
  end
end
