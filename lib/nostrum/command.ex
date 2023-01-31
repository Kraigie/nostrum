defmodule Nostrum.Command do
  alias Nostrum.{Api, Consumer, Struct.Interaction}
  use Consumer
  require Logger

  @moduledoc ~S"""
  Reduces boilerplate when implementing application commands.

  Here's an example module that responds with "pong!" to /ping:
  ```Elixir
  defmodule MyBot.PingCommand do
    use Nostrum.Command, %Nostrum.Command.Spec{
      name: "ping",
      desc: "sends back pong"
    }

    def handle(interaction, _options) do
      %{content: "pong!"}
    end
  end
  ```

  This one calculates the sum of two integers:
  ```Elixir
  defmodule MyBot.SumCommand do
    use Nostrum.Command, %Nostrum.Command.Spec{
      name: "sum",
      desc: "adds two integers together",
      options: [
        %Nostrum.Command.Spec.Option{
          name: "a",
          desc: "first number",
          type: :integer
        },
        %Nostrum.Command.Spec.Option{
          name: "b",
          desc: "second number",
          type: :integer
        }
      ]
    }

    def handle(_interaction, options = %{"a" => a, "b" => b}) do
      %{content: "The sum of #{a} and #{b} is #{a + b}"}
    end
  end
  ```

  And this one evaluates an Elixir expression (WARNING: very unsafe):
  ```Elixir
  defmodule MyBot.EvalCommand do
    use Nostrum.Command, %Nostrum.Command.Spec{
      name: "eval",
      desc: "evaluates an Elixir expression",
      options: [
        %Nostrum.Command.Spec.Option{
          name: "expression",
          desc: "expression to evaluate",
          type: :string
        }
      ]
    }

    def handle(_interaction, options = %{"expression" => expr}) do
      {result, _} = Code.eval_string(expr)
      %{content: "`#{inspect(result)}`"}
    end
  end
  ```

  Note that in order for these commands to work, you should tell Nostrum about
  them:
  ```Elixir
  config :nostrum,
    managed_commands: [
      MyBot.PingCommand,
      MyBot.SumCommand,
      MyBot.EvalCommand
    ]
  ```
  """

  defmacro __using__(specification) do
    quote do
      @behaviour Nostrum.Command
      def spec, do: unquote(specification)
    end
  end

  @doc """
  Should return the specification of the command as a `Nostrum.Command.Spec`
  struct
  """
  @callback spec() :: __MODULE__.Spec.t

  @doc """
  Gets called when the command is invoked. If `mode` in the spec is set to
  `:unmanaged`, the return value is ignored. Other values for this setting
  (`:managed` and `:ephemeral`) do consider the return value:
    - `:ignore` does nothing. The user will continue to see the "Your Bot is
    thinking" message for the next 15 minutes, after which it will be replaced
    with "The application did not respond"
    - `:delete` deletes the "Your Bot is thinking" message
    - `{:post, :delete, post}` deletes the "Your Bot is thinking" message and
    invokes `module.post_handle(post)` ignoring its return values
    - `{:post, data, post}` edits the response to `data` and invokes
    `module.post_handle(post)` ignoring its return values
    - `data` edits the response to `data`
  """
  @callback handle(Interaction.t, %{String.t => String.t | number()}) ::
    :ignore | :delete | map() | {:post, map() | :delete, term()}

  @doc """
  Gets called with the argument `post` after calling `handle` if it returned
  `{:post, _, post}` in `:managed` or `:ephemeral` mode
  """
  @callback post_handle(term()) :: term()

  # TODO: autocomplete
  @callback handle_autocomplete(term()) :: term()
  @optional_callbacks post_handle: 1, handle_autocomplete: 1

  def start_link(), do: Consumer.start_link(__MODULE__)

  def handle_event({:READY, _, _}) do
    Logger.debug("command consumer started")
    {:ok, _} = Supervisor.start_child(Nostrum.Supervisor, __MODULE__.Holder)
  end

  def handle_event({:INTERACTION_CREATE, %Interaction{data: %{name: command}} = interaction, _}) when command != nil do
    Logger.debug("received /#{command} invocation")

    case __MODULE__.Holder.get_command(command) do
      {:ok, {module, mode}} ->
        defer = mode == :managed || mode == :ephemeral
        if defer do
          flags = if mode == :ephemeral do 64 else 0 end
          Api.create_interaction_response!(interaction, %{type: 5, data: %{flags: flags}})
        end

        options = if interaction.data.options do
          Enum.map(interaction.data.options, fn %{name: name, value: value} -> {name, value} end)
          |> Enum.into(%{})
        else [] end

        case module.handle(interaction, options) do
          _ when not defer -> :ok
          :ignore -> :ok

          :delete ->
            Api.delete_interaction_response!(interaction)

          data when is_map(data) ->
            Api.edit_interaction_response!(interaction, data)

          {:post, :delete, post} ->
            Api.delete_interaction_response!(interaction)
            module.post_handle(post)

          {:post, data, post} ->
            Api.edit_interaction_response!(interaction, data)
            module.post_handle(post)
        end

      {:error, :unknown} ->
        Logger.warning("unknown command /#{command} invoked. is it listed under :managed_commands?")
    end
  end

  def handle_event(_), do: :ok
end
