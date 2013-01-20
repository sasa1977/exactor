defmodule ExActor.Mixfile do
   use Mix.Project 

   def project do
     [project: "ExActor", version: "0.1", app: :exactor, deps: deps] 
   end

   def application, do: []
   
   def deps do
     [{:genx, "0.1", git: "https://github.com/yrashk/genx.git" }]
   end
end