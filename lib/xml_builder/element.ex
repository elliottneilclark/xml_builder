defmodule XmlBuilder.Element do
  @moduledoc """
  The internal representation of the _XML_ element,
    responsible for parsing _and_ normalization.

  Generally, you won’t use this internal module.
  """

  @typedoc "Name of the _XML_ element"
  @type name :: bitstring() | binary() | atom()

  @typedoc "Attributes of the _XML_ element"
  @type attrs :: map() | nil

  @typedoc "Single inner subelement of the _XML_ element"
  @type content :: tuple() | String.t()

  @typedoc "Whole inner content of the _XML_ element"
  @type contents :: [content()] | nil

  @typedoc "The internal representation of the _XML_ element"
  @type t :: %__MODULE__{
          name: name(),
          attrs: attrs(),
          content: contents()
        }

  @typedoc "The raw internal representation of the _XML_ element"
  @type as_tuple :: {name(), attrs(), contents()}

  @typedoc "The AST representation of _XML_"
  @type ast :: as_tuple() | [as_tuple()]

  defstruct name: nil, attrs: %{}, content: nil

  alias XmlBuilder.Element, as: E

  @doc false
  @spec tuple(t()) :: as_tuple()
  def tuple(%E{name: name, attrs: attrs, content: content}),
    do: {name, attrs, content}

  @doc false
  @spec element(
          {:iodata, iodata()}
          | bitstring()
          | atom()
          | list()
          | {name()}
          | {name(), attrs() | contents()}
          | {name(), attrs(), contents()}
          | {nil, nil, {:iodata, iodata()}}
          | {nil, nil, bitstring()}
        ) :: as_tuple()
  def element({:iodata, _data} = iodata),
    do: element({nil, nil, iodata})

  def element(name) when is_bitstring(name),
    do: element({nil, nil, name})

  def element(name) when is_atom(name),
    do: element({name})

  def element(list) when is_list(list),
    do: list |> Enum.reject(&is_nil/1) |> Enum.map(&element/1)

  def element({name}),
    do: element({name, nil, nil})

  def element({name, attrs}) when is_map(attrs),
    do: element({name, attrs, nil})

  def element({name, content}),
    do: element({name, nil, content})

  def element({name, attrs, content}) when is_list(content) do
    %E{name: name, attrs: attrs, content: Enum.map(content, &element/1)}
    |> E.tuple()
  end

  def element({name, attrs, content}),
    do: {name, attrs, content}

  @doc false
  @spec element(name(), attrs() | contents()) :: as_tuple()
  def element(name, attrs) when is_map(attrs),
    do: element({name, attrs, nil})

  def element(name, content),
    do: element({name, nil, content})

  @doc false
  @spec element(name(), attrs(), contents()) :: as_tuple()
  def element(name, attrs, content),
    do: element({name, attrs, content})
end
