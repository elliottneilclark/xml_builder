defmodule XmlBuilder.Mixfile do
  use Mix.Project

  @app :xml_builder_ex
  @version "3.1.4159"

  def project do
    [
      app: @app,
      version: @version,
      elixir: "~> 1.9",
      prune_code_paths: false,
      deps: deps(),
      docs: docs(),
      package: package(),
      description: description(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        credo: :ci,
        dialyzer: :ci,
        tests: :test,
        "coveralls.json": :test,
        "coveralls.html": :test,
        "quality.ci": :ci
      ],
      dialyzer: [
        plt_file: {:no_warn, ".dialyzer/plts/dialyzer.plt"},
        plt_add_apps: [],
        ignore_warnings: ".dialyzer/ignore.exs"
      ]
    ]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [
      {:credo, "~> 1.4", only: [:ci, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:ci], runtime: false},
      {:excoveralls, "~> 0.14", only: [:test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: [:dev]}
    ]
  end

  defp docs do
    [
      main: "XmlBuilder",
      source_ref: "v#{@version}",
      canonical: "http://hexdocs.pm/#{@app}",
      # logo: "stuff/logo-48x48.png",
      source_url: "https://github.com/am-kantox/xml_builder",
      extras: ["README.md"],
      groups_for_modules: [
        # XmlBuilder,
        # XmlBuilder.Format,
        Formats: [
          XmlBuilder.Format.None,
          XmlBuilder.Format.Indented,
          XmlBuilder.Format.TabIndented
        ]
      ]
    ]
  end

  defp description do
    """
    XML builder for Elixir
    """
  end

  defp package do
    [
      name: @app,
      files: ~w|lib mix.exs README.md|,
      maintainers: ["Aleksei Matiushkin"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/am-kantox/xml_builder",
        "Docs" => "https://hexdocs.pm/#{@app}"
      }
    ]
  end

  defp aliases do
    [
      quality: ["format", "credo --strict", "dialyzer"],
      tests: ["coveralls.html --trace"],
      "quality.ci": [
        "format --check-formatted",
        "credo --strict",
        "dialyzer --halt-exit-status"
      ]
    ]
  end
end
