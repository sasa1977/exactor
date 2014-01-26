defmodule ExActor.Mixfile do
  use Mix.Project 

  def project do
    [
      project: "ExActor", 
      version: "0.1.1", 
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