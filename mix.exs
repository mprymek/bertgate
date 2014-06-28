defmodule BertGate.Mixfile do
  use Mix.Project

  def project do
    [app: :bert_gate,
     version: "0.1.0",
     elixir: "~> 0.14.1",
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:ranch]]
  end

  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps, do: [
      { :ranch, github: "extend/ranch", ref: "3189ef2d47843945efc96c224963380462c33d40" },
  ]
end
