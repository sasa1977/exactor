defmodule ExActor.Strict do
  defmacro __using__(opts) do
    ExActor.Helper.init_global_options(__CALLER__, opts)

    quote do
      use ExActor.Behaviour.Strict

      import ExActor.Operations
      import ExActor.Responders
      use ExActor.DefaultInterface
      
      unquote(ExActor.Helper.init_exported)
    end
  end
end