defmodule ExActor.Helper do
  @moduledoc false

  def inject_to_module(quoted, module, env) do
    Module.eval_quoted(
      module, quoted, [],
      aliases: env.aliases,
      requires: env.requires,
      functions: env.functions,
      macros: env.macros,
      file: env.file,
      line: env.line
    )
  end

  def init_generation_state(opts) do
    quote do
      @exactor_global_options unquote(opts)
    end
  end

  def state_var do
    Macro.var(:state_var, ExActor)
  end
end