defmodule Mix.Tasks.Docs do
  def run(_) do
    [
      "rm -rf docs",
      "elixir -pa _build/shared/lib/*/ebin/ ../ex_doc/bin/ex_doc ExActor '#{Mix.project[:version]}' _build/shared/lib/exactor/ebin/"
    ]
    |> Enum.each(&System.cmd(&1))
  end
end