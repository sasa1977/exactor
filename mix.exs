defmodule ExActor.Mixfile do
  use Mix.Project

  def project do
    [
      project: "ExActor",
      version: "0.3.1",
      elixir: ">= 0.13.0",
      app: :exactor,
      deps: deps(Mix.env),
      package: [
        contributors: ["Saša Jurić"],
        licenses: ["MIT"],
        links: [{"Github", "https://github.com/sasa1977/exactor"}]
      ],
      description: "Simplified creation of gen_server based actors in Elixir."
    ]
  end

  def application, do: []

  defp deps(_), do: []
end
