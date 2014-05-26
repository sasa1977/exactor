defmodule ExActor.Mixfile do
  use Mix.Project

  def project do
    [
      project: "ExActor",
      version: "0.4.0",
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
        source_ref: "0.4.0"
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
