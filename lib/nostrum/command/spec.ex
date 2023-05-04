defmodule Nostrum.Command.Spec do
  @moduledoc """
  Specifies the application command:
    - `name`: localizable (see below) name of the command
    - `desc`: localizable description of the command
    - `default_perms`: default permissions of the command as a bit set (set to
    "0" to restrict the command as admin-only by default)
    - `type`: one of `:chat_input` (slash command), `{:context, :user}` (user
    context menu) or `{:context, :message}` (message context menu)
    - `options`: list of options (see `Nostrum.Command.Spec.Option`)
    - `nsfw`: `true` if age-restricted (`false` by default),
    - `dm`: `true` if can be executed in DMs (`true` by default)
    - `mode`:
      - `:unmanaged` just invokes your handler. You are responsible for replying
      to the interaction. The return value of the handler is ignored.
      - `:managed` invokes your handler and replies to the interaction based on
      what it returns
      - `:ephemeral` acts like `:managed`, but makes the response ephemeral

  "Localizable" fields (`name` and `desc`, along with some others in the `Option`
  struct) are either:
    - `"plain strings"` (no localization is done)
    - `{"fallback string", %{"la-NG" => "trąnśłatioń"}}`
  """

  @type localizable() :: String.t | {fallback :: String.t, %{locale :: String.t() => translation :: String.t}}

  @enforce_keys [:name, :desc]
  defstruct [
    :name,
    :desc,
    :default_perms,
    type: :chat_input,
    options: [],
    nsfw: false,
    dm: true,
    mode: :managed
  ]
  @type t :: %__MODULE__{
    name: localizable(),
    desc: localizable(),
    default_perms: String.t,
    type: :chat_input | {:context, :user | :message},
    options: [__MODULE__.Option.t],
    nsfw: boolean(),
    dm: boolean(),
    mode: :unmanaged | :managed | :ephemeral
  }

  defmodule Option do
    @moduledoc """
    Specifies an option of an application command:
      - `name`: localizable (see `Nostrum.Command.Spec`) name of the option
      - `desc`: localizable description of the option
      - `type`: one of `:subcmd` (subcommand), `:subcmd_group` (subcommand
      group), `:string`, `:integer` (-2^53 to 2^53), `:bool`, `:user`,
      `:channel`, `:role`, `:mentionable`, `:double` (same range as `:integer`) or
      `:attachment`
      - `choices`: list of:
        - `name`: localizable name of the option choice
        - `value`: value of the option choice
      - `options`: list of options if the `type` is `:subcmd` or `:subcmd_group`
      - `channel_types`: list of `:text`, `:dm`, `:voice`, `:group`, `:category`,
      `:announcement`, `{:thread, :announcement}`, `{:thread, :public}`,
      `{:thread, :private}`, `:stage`, `:guild_directory` or `:forum` if the
      `type` is `:channel`
      - `value_range`: range of permissible values if the `type` is `:integer`
      or `:double`
      - `length_range`: range of permissible lengths if the `type` is `:string`
      - `autocomplete`: whether the option supports autocompletion
      (`choices` can't be set if this is true)
      - `required`: whether this option is required (true by default)

    Note that for `value_range` and `length_range` either of the ends can be
    set to `Option.ignore_end` to tell Discord to not enforce that end of the
    range
    """

    @enforce_keys [:name, :desc, :type]
    defstruct [
      :name,
      :desc,
      :type,
      choices: nil,
      options: [],
      channel_types: [],
      value_range: nil,
      length_range: nil,
      autocomplete: false,
      required: true,
    ]
    @type t :: %__MODULE__{
      name: Nostrum.Command.Spec.localizable,
      desc: Nostrum.Command.Spec.localizable,
      type: :subcmd | :subcmd_group | :string | :integer | :bool | :user
          | :channel | :role | :mentionable | :double | :attachment,
      choices: [%{
        name: Nostrum.Command.Spec.localizable,
        value: String.t | number()
      }],
      options: [__MODULE__.t],
      channel_types: [:text | :dm | :voice | :group | :category | :announcement
                    | {:thread, :announcement | :public | :private}
                    | :stage | :guild_directory | :forum],
      value_range: Range.t,
      length_range: Range.t,
      autocomplete: boolean()
    }

    @doc """
    Use this value in place of either end of `value_range` and `length_range`
    to tell Discord that this end should not be enforced:
    ```elixir
    alias Nostrum.Command.Spec.Option
    Option.ignore_end..10
    10..Option.ignore_end
    ```
    """
    def ignore_end, do: 2 ** 54

    def to_app_cmd_struct_option(%__MODULE__{} = spec) do
      struct = %{
        type: Map.get(%{
          subcmd: 1,
          subcmd_group: 2,
          string: 3,
          integer: 4,
          bool: 5,
          user: 6,
          channel: 7,
          role: 8,
          mentionable: 9,
          double: 10,
          attachment: 11
        }, spec.type),
        required: spec.required,
        autocomplete: spec.autocomplete
      }
      |> Nostrum.Command.Spec.add_localizable(spec.name, :name, :name_localizations)
      |> Nostrum.Command.Spec.add_localizable(spec.desc, :description, :description_localizations)
      |> add_range(spec.value_range, :min_value, :max_value)
      |> add_range(spec.length_range, :min_length, :max_length)

      struct = if spec.options do
        options = Enum.map(spec.options, &to_app_cmd_struct_option/1)
        Map.put(struct, :options, options)
      else struct end

      struct = if spec.channel_types do
        channel_types = Enum.map(spec.channel_types, fn type ->
          Map.get(%{
            :text => 0,
            :dm => 1,
            :voice => 2,
            :group => 3,
            :category => 4,
            :announcement => 5,
            {:thread, :announcement} => 10,
            {:thread, :public} => 11,
            {:thread, :private} => 12,
            :stage => 13,
            :guild_directory => 14,
            :forum => 15
          }, type)
        end)
        Map.put(struct, :channel_types, channel_types)
      else struct end

      if spec.choices do
        choices = Enum.map(spec.choices, fn %{name: name, value: value} ->
          %{value: value}
          |> Nostrum.Command.Spec.add_localizable(name, :name, :name_localizations)
        end)
        Map.put(struct, :choices, choices)
      else struct end
    end

    defp add_range(map, nil, _, _), do: map
    defp add_range(map, left..right, key_left, key_right) do
      map = if left != ignore_end() do
        Map.put(map, key_left, left)
      else map end
      if right != ignore_end() do
        Map.put(map, key_right, right)
      else map end
    end
  end

  @doc """
  Converts the `spec` to a `Nostrum.Struct.ApplicationCommand` struct
  """
  def to_application_command_struct(%__MODULE__{} = spec) do
    %{
      default_perms: spec.default_perms,
      type: Map.get(%{
        :chat_input => 1,
        {:context, :user} => 2,
        {:context, :message} => 3
      }, spec.type),
      nsfw: spec.nsfw,
      dm_permission: spec.dm,
      options: Enum.map(spec.options, &__MODULE__.Option.to_app_cmd_struct_option/1)
    }
    |> add_localizable(spec.name, :name, :name_localizations)
    |> add_localizable(spec.desc, :description, :description_localizations)
  end

  def add_localizable(map, <<data::binary>>, key_main, _key_loc) do
    map
    |> Map.put(key_main, data)
  end
  def add_localizable(map, {fallback, loc}, key_main, key_loc) do
    map
    |> Map.put(key_main, fallback)
    |> Map.put(key_loc, loc)
  end
end
