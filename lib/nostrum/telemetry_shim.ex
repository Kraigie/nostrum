if Code.ensure_loaded?(:telemetry) do
  defmodule Nostrum.TelemetryShim do
    @moduledoc false

    defdelegate execute(event_name, measurements), to: :telemetry

    defdelegate execute(event_name, measurements, metadata), to: :telemetry

    defdelegate span(event_prefix, start_metadata, span_function), to: :telemetry
  end
else
  defmodule Nostrum.TelemetryShim do
    @moduledoc false

    def span(_event, _meta, func) do
      case func.() do
        {result, _meta} ->
          result

        {result, _extra_measurements, _meta} ->
          result
      end
    end

    def execute(_event, _measurements), do: :ok

    def execute(_event, _measurements, _meta), do: :ok
  end
end
