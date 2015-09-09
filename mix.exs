defmodule ExActor.Mixfile do
  use Mix.Project

  @version "2.2.0"

  def project do
    [
      project: "ExActor",
      version: @version,
      elixir: "~> 1.0",
      app: :exactor,
      deps: deps,
      package: [
        contributors: ["Saša Jurić"],
        licenses: ["MIT"],
        links: %{
          "Github" => "https://github.com/sasa1977/exactor",
          "Docs" => "http://hexdocs.pm/exactor",
          "Changelog" => "https://github.com/sasa1977/exactor/blob/#{@version}/CHANGELOG.md#v#{String.replace(@version, ".", "")}"
        }
      ],
      description: "Simplified creation of GenServer based processes in Elixir.",
      name: "ExActor",
      docs: [
        readme: "README.md",
        main: "ExActor.Operations",
        source_url: "https://github.com/sasa1977/exactor/",
        source_ref: @version
      ]
    ]
  end

  def application, do: [applications: [:logger]]

  defp deps do
    [
      {:ex_doc, "~> 0.7.0", only: :docs}
    ]
  end
end
