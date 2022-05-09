defmodule XmlBuilder.Element do
  @moduledoc false

  @type name :: bitstring() | binary() | atom()
  @type attrs :: map() | nil
  @type content :: tuple() | String.t()
  @type contents :: [content()]

  @type t :: %__MODULE__{
          name: name(),
          attrs: attrs(),
          content: content()
        }

  defstruct name: nil, attrs: %{}, content: nil

  def tuple(element), do: {element.name, element.attrs, element.content}

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
    #   do: {name, attrs, element(content)}
    %XmlBuilder.Element{name: name, attrs: attrs, content: Enum.map(content, &element/1)}
    |> XmlBuilder.Element.tuple()
  end

  def element({name, attrs, content}),
    do: {name, attrs, content}

  def element(name, attrs) when is_map(attrs),
    do: element({name, attrs, nil})

  def element(name, content),
    do: element({name, nil, content})

  def element(name, attrs, content),
    do: element({name, attrs, content})
end
