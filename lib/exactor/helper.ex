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

  def init_generation_state(opts) do
    quote do
      @exactor_global_options unquote(opts)
    end
  end

  def state_var do
    Macro.var(:___state_var, ExActor)
  end
end