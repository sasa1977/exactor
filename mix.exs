defmodule ExActor.Mixfile do
  use Mix.Project 

  def project do
    [
      project: "ExActor", 
      version: "0.1", 
      elixir: ">= 0.10.1",
      app: :exactor, 
      deps: deps
    ] 
  end

  def application, do: []
   
  def deps do
    []
  end
end