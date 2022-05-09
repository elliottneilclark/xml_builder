defmodule XmlBuilder.Format.TabIndented do
  @moduledoc ~S"""
  Format for `XmlBuilder.generate/2` to produce a human-readable indented XML.
  Like `XmlBuilder.Format.Indented`, but indents the content with tabs instead of spaces.

  **Normally you should not use this formatter explicitly, pass it as an
  optional parameter to `XmlBuilder.generate/2` instead.**
  """
  use XmlBuilder.Format, indent: "\t", intersperse: "\n", blank: ""
end
