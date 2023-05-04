defmodule Nostrum.Command.Holder do
  use GenServer
  require Logger

  @moduledoc """
  Loads and registers managed slash commands
  """

  def start_link(args), do: GenServer.start_link(__MODULE__, args, name: __MODULE__)

  def init(_args) do
    table = :ets.new(Nostrum.ManagedCommands, [:named_table, :public, :set])

    modules = Application.get_env(:nostrum, :managed_commands)

    commands = for module <- modules do
      spec = module.spec()
      name = case spec.name do
        {fb, _} -> fb
        name -> name
      end
      :ets.insert(table, {name, module, spec.mode})
      Nostrum.Command.Spec.to_application_command_struct(spec)
    end

    {:ok, _} = Nostrum.Api.bulk_overwrite_global_application_commands(commands)
    Logger.info("loaded #{length(commands)} application commands")

    {:ok, table}
  end

  def get_command(name) do
    case :ets.lookup(Nostrum.ManagedCommands, name) do
      [] -> {:error, :unknown}
      [{^name, module, mode}] -> {:ok, {module, mode}}
    end
  end
end
