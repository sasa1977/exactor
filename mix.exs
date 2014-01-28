defmodule ExActor.Mixfile do
  use Mix.Project 

  def project do
    [
      project: "ExActor", 
      version: "0.2.0-dev", 
      elixir: ">= 0.12.2",
      app: :exactor, 
      deps: deps
    ] 
  end

  def application, do: []
   
  def deps do
    []
  end
end