defmodule XmlBuilder do
  @moduledoc """
  A module for generating XML

  ## Examples

      iex> XmlBuilder.document(:person) |> XmlBuilder.generate
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\\n<person/>"

      iex> XmlBuilder.document(:person, "Josh") |> XmlBuilder.generate
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\\n<person>Josh</person>"

      iex> XmlBuilder.document(:person) |> XmlBuilder.generate(format: :none)
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?><person/>"

      iex> XmlBuilder.element(:person, "Josh") |> XmlBuilder.generate
      "<person>Josh</person>"

      iex> XmlBuilder.element(:person, %{occupation: "Developer"}, "Josh") |> XmlBuilder.generate
      "<person occupation=\\\"Developer\\\">Josh</person>"
  """

  defguardp is_blank_map(map) when is_nil(map) or (is_map(map) and map_size(map) == 0)
  defguardp is_blank_list(list) when is_nil(list) or list == []
  defguardp is_blank_attrs(attrs) when is_blank_map(attrs) or is_blank_list(attrs)

  @doc """
  Generate an XML document.

  Returns a `binary`.

  ## Examples

      iex> XmlBuilder.document(:person) |> XmlBuilder.generate
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\\n<person/>"

      iex> XmlBuilder.document(:person, %{id: 1}) |> XmlBuilder.generate
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\\n<person id=\\\"1\\\"/>"

      iex> XmlBuilder.document(:person, %{id: 1}, "some data") |> XmlBuilder.generate
      "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\\n<person id=\\\"1\\\">some data</person>"
  """
  def document(elements),
    do: [:xml_decl | elements_with_prolog(elements) |> List.wrap()]

  def document(name, attrs_or_content),
    do: [:xml_decl | [element(name, attrs_or_content)]]

  def document(name, attrs, content),
    do: [:xml_decl | [element(name, attrs, content)]]

  @doc false
  def doc(elements) do
    IO.warn("doc/1 is deprecated. Use document/1 with generate/1 instead.")
    [:xml_decl | elements_with_prolog(elements) |> List.wrap()] |> generate
  end

  @doc false
  def doc(name, attrs_or_content) do
    IO.warn("doc/2 is deprecated. Use document/2 with generate/1 instead.")
    [:xml_decl | [element(name, attrs_or_content)]] |> generate
  end

  @doc false
  def doc(name, attrs, content) do
    IO.warn("doc/3 is deprecated. Use document/3 with generate/1 instead.")
    [:xml_decl | [element(name, attrs, content)]] |> generate
  end

  @doc """
  Create an XML element.

  Returns a `tuple` in the format `{name, attributes, content | list}`.

  ## Examples

      iex> XmlBuilder.element(:person)
      {:person, nil, nil}

      iex> XmlBuilder.element(:person, "data")
      {:person, nil, "data"}

      iex> XmlBuilder.element(:person, %{id: 1})
      {:person, %{id: 1}, nil}

      iex> XmlBuilder.element(:person, %{id: 1}, "data")
      {:person, %{id: 1}, "data"}

      iex> XmlBuilder.element(:person, %{id: 1}, [XmlBuilder.element(:first, "Steve"), XmlBuilder.element(:last, "Jobs")])
      {:person, %{id: 1}, [
        {:first, nil, "Steve"},
        {:last, nil, "Jobs"}
      ]}
  """

  @spec element(
          name :: XmlBuilder.Element.name(),
          attrs :: XmlBuilder.Element.attrs(),
          content :: XmlBuilder.Element.contents()
        ) :: XmlBuilder.Element.t() | [XmlBuilder.Element.t()]
  defdelegate element(a1), to: XmlBuilder.Element
  defdelegate element(a1, a2), to: XmlBuilder.Element
  defdelegate element(a1, a2, a3), to: XmlBuilder.Element

  defp elements_with_prolog([first | rest]) when length(rest) > 0,
    do: [first_element(first) | element(rest)]

  defp elements_with_prolog(element_spec),
    do: element(element_spec)

  defp first_element({:doctype, args} = doctype_decl) when is_tuple(args),
    do: doctype_decl

  defp first_element(element_spec),
    do: element(element_spec)

  defp format(content, level, options, name \\ nil)

  defp format(:xml_decl, 0, options, _name) do
    encoding = Keyword.get(options, :encoding, "UTF-8")

    standalone =
      case Keyword.get(options, :standalone, nil) do
        true -> ~s| standalone="yes"|
        false -> ~s| standalone="no"|
        nil -> ""
      end

    ['<?xml version="1.0" encoding="', to_string(encoding), ?", standalone, '?>']
  end

  defp format({:doctype, {:system, name, system}}, 0, _options, _name),
    do: ['<!DOCTYPE ', to_string(name), ' SYSTEM "', to_string(system), '">']

  defp format({:doctype, {:public, name, public, system}}, 0, _options, _name),
    do: [
      '<!DOCTYPE ',
      to_string(name),
      ' PUBLIC "',
      to_string(public),
      '" "',
      to_string(system),
      '">'
    ]

  defp format(string, level, options, name) when is_bitstring(string),
    do: format({nil, nil, string}, level, options, name)

  defp format(list, level, options, name) when is_list(list) do
    formatter = formatter(name, options)
    map_intersperse(list, formatter.intersperse(), &format(&1, level, options, name))
  end

  defp format({nil, nil, content}, level, options, name) when is_bitstring(content) do
    formatter = formatter(name, options)
    [formatter.indent(level), to_string(content)]
  end

  defp format({nil, nil, {:iodata, iodata}}, _level, _options, _name), do: iodata

  defp format({name, attrs, content}, level, options, _name)
       when is_blank_attrs(attrs) and is_blank_list(content) do
    formatter = formatter(name, options)
    [formatter.indent(level), '<', to_string(name), '/>']
  end

  defp format({name, attrs, content}, level, options, _name) when is_blank_list(content) do
    formatter = formatter(name, options)

    [
      formatter.indent(level),
      '<',
      to_string(name),
      ' ',
      format_attributes(attrs),
      '/>'
    ]
  end

  defp format({name, attrs, content}, level, options, _name)
       when is_blank_attrs(attrs) and not is_list(content) do
    formatter = formatter(name, options)

    [
      formatter.indent(level),
      '<',
      to_string(name),
      '>',
      format_content(name, content, level + 1, options),
      '</',
      to_string(name),
      '>'
    ]
  end

  defp format({name, attrs, content}, level, options, _name)
       when is_blank_attrs(attrs) and is_list(content) do
    formatter = formatter(name, options)
    format_char = formatter.intersperse()

    [
      formatter.indent(level),
      '<',
      to_string(name),
      '>',
      format_content(name, content, level + 1, options),
      format_char,
      formatter.indent(level),
      '</',
      to_string(name),
      '>'
    ]
  end

  defp format({name, attrs, content}, level, options, _name)
       when not is_blank_attrs(attrs) and not is_list(content) do
    formatter = formatter(name, options)

    [
      formatter.indent(level),
      '<',
      to_string(name),
      ' ',
      format_attributes(attrs),
      '>',
      format_content(name, content, level + 1, options),
      '</',
      to_string(name),
      '>'
    ]
  end

  defp format({name, attrs, content}, level, options, _name)
       when not is_blank_attrs(attrs) and is_list(content) do
    formatter = formatter(name, options)
    format_char = formatter.intersperse()

    [
      formatter.indent(level),
      '<',
      to_string(name),
      ' ',
      format_attributes(attrs),
      '>',
      format_content(name, content, level + 1, options),
      format_char,
      formatter.indent(level),
      '</',
      to_string(name),
      '>'
    ]
  end

  defp formatter(name, options) do
    case Keyword.get(options, :format) do
      :none ->
        XmlBuilder.Format.None

      default when default in [nil, :indent, :indented] ->
        XmlBuilder.Format.Indented

      custom when is_atom(custom) ->
        if Code.ensure_loaded?(custom), do: custom, else: XmlBuilder.Format.Indented

      custom when is_list(custom) ->
        format = Keyword.get_lazy(custom, name, fn -> Keyword.get(custom, :*, :indent) end)
        formatter(name, Keyword.put(options, :format, format))
    end
  end

  defp format_content(name, children, level, options) when is_list(children) do
    format_char = formatter(name, options).intersperse()
    [format_char, map_intersperse(children, format_char, &format(&1, level, options, name))]
  end

  defp format_content(_name, content, _level, _options),
    do: escape(content)

  defp format_attributes(attrs),
    do:
      map_intersperse(attrs, " ", fn {name, value} ->
        [to_string(name), '=', quote_attribute_value(value)]
      end)

  defp quote_attribute_value(val) when not is_bitstring(val),
    do: val |> to_string() |> quote_attribute_value()

  defp quote_attribute_value(val) do
    escape? = String.contains?(val, ["\"", "&", "<"])

    case escape? do
      true -> [?", escape(val), ?"]
      false -> [?", val, ?"]
    end
  end

  defp escape({:iodata, iodata}), do: iodata
  defp escape({:safe, data}) when is_bitstring(data), do: data
  defp escape({:safe, data}), do: to_string(data)
  defp escape({:cdata, data}), do: ["<![CDATA[", data, "]]>"]

  defp escape(data) when is_binary(data),
    do: data |> escape_string() |> to_string()

  defp escape(data) when not is_bitstring(data),
    do: data |> to_string() |> escape_string() |> to_string()

  defp escape_string(""), do: ""
  defp escape_string(<<"&"::utf8, rest::binary>>), do: escape_entity(rest)
  defp escape_string(<<"<"::utf8, rest::binary>>), do: ["&lt;" | escape_string(rest)]
  defp escape_string(<<">"::utf8, rest::binary>>), do: ["&gt;" | escape_string(rest)]
  defp escape_string(<<"\""::utf8, rest::binary>>), do: ["&quot;" | escape_string(rest)]
  defp escape_string(<<"'"::utf8, rest::binary>>), do: ["&apos;" | escape_string(rest)]
  defp escape_string(<<c::utf8, rest::binary>>), do: [c | escape_string(rest)]

  defp escape_entity(<<"amp;"::utf8, rest::binary>>), do: ["&amp;" | escape_string(rest)]
  defp escape_entity(<<"lt;"::utf8, rest::binary>>), do: ["&lt;" | escape_string(rest)]
  defp escape_entity(<<"gt;"::utf8, rest::binary>>), do: ["&gt;" | escape_string(rest)]
  defp escape_entity(<<"quot;"::utf8, rest::binary>>), do: ["&quot;" | escape_string(rest)]
  defp escape_entity(<<"apos;"::utf8, rest::binary>>), do: ["&apos;" | escape_string(rest)]
  defp escape_entity(rest), do: ["&amp;" | escape_string(rest)]

  # Remove when support for Elixir <v1.10 is dropped
  @compile {:inline, map_intersperse: 3}
  if function_exported?(Enum, :map_intersperse, 3) do
    defp map_intersperse(enumerable, separator, mapper),
      do: Enum.map_intersperse(enumerable, separator, mapper)
  else
    defp map_intersperse(enumerable, separator, mapper),
      do: enumerable |> Enum.map(mapper) |> Enum.intersperse(separator)
  end

  @doc """
  Creates a DOCTYPE declaration with a system or public identifier.

  ## System Example

  Returns a `tuple` in the format `{:doctype, {:system, name, system_identifier}}`.

  ```elixir
  import XmlBuilder

  document([
    doctype("greeting", system: "hello.dtd"),
    element(:person, "Josh")
  ]) |> generate
  ```

  Outputs

  ```xml
  <?xml version="1.0" encoding="UTF-8" ?>
  <!DOCTYPE greeting SYSTEM "hello.dtd">
  <person>Josh</person>
  ```

  ## Public Example

   Returns a `tuple` in the format `{:doctype, {:public, name, public_identifier, system_identifier}}`.

  ```elixir
  import XmlBuilder

  document([
    doctype("html", public: ["-//W3C//DTD XHTML 1.0 Transitional//EN",
                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"]),
    element(:html, "Hello, world!")
  ]) |> generate
  ```

  Outputs

  ```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
  <html>Hello, world!</html>
  ```
  """
  def doctype(name, [{:system, system_identifier}]),
    do: {:doctype, {:system, name, system_identifier}}

  def doctype(name, [{:public, [public_identifier, system_identifier]}]),
    do: {:doctype, {:public, name, public_identifier, system_identifier}}

  @doc """
  Generate a binary from an XML tree. Accepts an optional parameter
    `format: Format.Module.Name` to specify the formatter to use.

  The `format` parameter might be shortened to `:none` and `:indented`
    for built-in formatters.

  Returns a `binary`.

  ## Examples

      iex> XmlBuilder.generate(XmlBuilder.element(:person))
      "<person/>"

      iex> XmlBuilder.generate({:person, %{id: 1}, "Steve Jobs"})
      "<person id=\\\"1\\\">Steve Jobs</person>"

      iex> XmlBuilder.generate({:name, nil, [{:first, nil, "Steve"}]}, format: :none)
      "<name><first>Steve</first></name>"

      iex> XmlBuilder.generate({:name, nil, [{:first, nil, "Steve"}]})
      "<name>\\n  <first>Steve</first>\\n</name>"

      iex> XmlBuilder.generate(:xml_decl, encoding: "ISO-8859-1")
      ~s|<?xml version="1.0" encoding="ISO-8859-1"?>|

      iex> XmlBuilder.generate(
      ...>  [{:person, %{},
      ...>    [{:name, %{id: 123}, "Josh"},
      ...>     {:age, %{}, "21"}]}], format: XmlBuilder.Format.None)
      "<person><name id=\\\"123\\\">Josh</name><age>21</age></person>"

      iex> XmlBuilder.generate(
      ...>  [{:person, %{},
      ...>    [{:name, %{id: 123}, "Josh"},
      ...>     {:age, %{}, "21"}]}], format: :none)
      "<person><name id=\\\"123\\\">Josh</name><age>21</age></person>"
  """
  def generate(any, opts \\ []) do
    any
    |> generate_iodata(opts)
    |> IO.chardata_to_string()
  end

  @doc """
  Similar to `generate/2`, but returns `iodata` instead of a `binary`.

  ## Examples

      iex> XmlBuilder.generate_iodata(XmlBuilder.element(:person))
      ["", '<', "person", '/>']
  """
  def generate_iodata(any, opts \\ []), do: format(any, 0, opts)
end
