defmodule ExActor.Mixfile do
  use Mix.Project 

  def project do
    [
      project: "ExActor", 
      version: "0.2.0", 
      elixir: ">= 0.12.2",
      app: :exactor, 
      deps: deps(Mix.env)
    ] 
  end

  def application, do: []

  defp deps(_), do: []
end