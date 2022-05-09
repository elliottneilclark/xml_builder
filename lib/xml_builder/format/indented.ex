defmodule XmlBuilder.Format.Indented do
  @moduledoc ~S"""
  Format for `XmlBuilder.generate/2` to produce a human-readable indented XML.
  Unlike `XmlBuilder.Format.None`, this formatter
    produces a human readable XML, indented with tabs and new lines, like this:
  ```xml
  <person>
    <name id=\"123\">Josh</name>
    <age>21</age>
  </person>
  ```
  **Normally you should not use this formatter explicitly, pass it as an
  optional parameter to `XmlBuilder.generate/2` instead.**
  """
  use XmlBuilder.Format, indent: "  ", intersperse: "\n", blank: ""
end
