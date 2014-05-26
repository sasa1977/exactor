defmodule ExActor.Mixfile do
  use Mix.Project

  @version "0.4.0"

  def project do
    [
      project: "ExActor",
      version: @version,
      elixir: "~> 0.13.3",
      app: :exactor,
      deps: deps,
      package: [
        contributors: ["Saša Jurić"],
        licenses: ["MIT"],
        links: [{"Github", "https://github.com/sasa1977/exactor"}]
      ],
      description: "Simplified creation of gen_server based actors in Elixir.",
      docs: [
        readme: true,
        main: "README",
        source_url: "https://github.com/sasa1977/exactor/",
        source_ref: @version
      ]
    ]
  end

  def application, do: []

  defp deps do
    [
      {:ex_doc, github: "elixir-lang/ex_doc", only: :docs}
    ]
  end
end
