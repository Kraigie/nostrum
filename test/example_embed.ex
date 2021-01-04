defmodule MyApp.MyStruct do
  use Nostrum.Struct.Embed

  defstruct []

  def title(_), do: "craig"
  def description(_), do: "nostrum"
  def url(_), do: "https://google.com/"
  def timestamp(_), do: "2016-05-05T21:04:13.203Z"
  def color(_), do: 431_948

  def fields(_) do
    [
      %Nostrum.Struct.Embed.Field{name: "Field 1", value: "Test"},
      %Nostrum.Struct.Embed.Field{name: "Field 2", value: "More test", inline: true}
    ]
  end
end
