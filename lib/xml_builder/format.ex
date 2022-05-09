defmodule XmlBuilder.Format do
  @moduledoc ~S"""
  Base behaviour for all the formatters. There are two defauls formatters
    included: `XmlBuilder.Format.None` and `XmlBuilder.Format.Indented`.

  To introduce the new formatter, one should `use XmlBuilder.Format` and
    possibly override `indent/1` and/or `intersperse/0` implementation(s):

  ```elixir
  defmodule MyCuteFormat do
    use XmlBuilder.Format, indent: "  ", intersperse: "\n"
  end
  ```
  """
  @callback intersperse() :: binary() | iodata()
  @callback indent(non_neg_integer()) :: binary() | iodata()

  defmacro __using__(opts) do
    quote do
      @behaviour XmlBuilder.Format

      @indenter unquote(opts[:indent]) || "\t"
      @intersperser unquote(opts[:intersperse]) || "\n"
      @blanker unquote(opts[:blank]) || ""

      @impl XmlBuilder.Format
      def indent(level), do: String.duplicate(@indenter, level)

      @impl XmlBuilder.Format
      def intersperse, do: @intersperser

      defoverridable indent: 1, intersperse: 0
    end
  end
end
