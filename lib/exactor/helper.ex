defmodule ExActor.Helper do
  @moduledoc false

  def inject_to_module(quoted, module, env) do
    Module.eval_quoted(
      module, quoted, [],
      aliases: env.aliases,
      requires: env.requires,
      functions: env.functions,
      macros: env.macros
    )
  end

  def init_global_options(caller, opts) do
    Module.put_attribute(caller.module, :exactor_global_options, opts || [])
  end

  def init_exported do
    quote do
      @exported HashSet.new
    end
  end

  def def_initializer(caller) do
    initial_state =
      Module.get_attribute(caller.module, :exactor_global_options)
      |> Dict.fetch(:initial_state)

    case initial_state do
      :error -> nil
      {:ok, state} ->
        quote do
          definit do: initial_state(unquote(state))
        end
    end
  end
end