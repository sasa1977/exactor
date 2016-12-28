defmodule ExActor.Mixfile do
  use Mix.Project

  @version "2.2.3"

  def project do
    [
      project: "ExActor",
      version: @version,
      elixir: "~> 1.0",
      app: :exactor,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      package: [
        maintainers: ["Saša Jurić"],
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
        extras: ["README.md"],
        main: "ExActor.Operations",
        source_url: "https://github.com/sasa1977/exactor/",
        source_ref: @version
      ]
    ]
  end

  def application, do: [applications: [:logger]]

  defp deps do
    [
      {:ex_doc, "~> 0.14.5", only: :dev}
    ]
  end
end
