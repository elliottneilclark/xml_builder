defmodule XmlBuilder.Format.None do
  @moduledoc ~S"""
  Format for `XmlBuilder.generate/2` to produce a machine-readable aka
    minified XML.
  Unlike `XmlBuilder.Format.Indented`, this formatter does not
    produce a human readable XML, it simply spits out elements one immediately
  after another, like this:
  ```xml
  <person><name id=\"123\">Josh</name><age>21</age></person>
  ```

  **Normally you should not use this formatter explicitly, pass it as an
  optional parameter to `XmlBuilder.generate/2` instead.**
  """
  use XmlBuilder.Format, indent: "", intersperse: "", blank: ""
end
