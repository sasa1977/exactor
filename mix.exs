defmodule ExActor.Mixfile do
  use Mix.Project

  def project do
    [
      project: "ExActor",
      version: "0.2.1",
      elixir: ">= 0.13.0",
      app: :exactor,
      deps: deps(Mix.env)
    ]
  end

  def application, do: []

  defp deps(_), do: []
end
