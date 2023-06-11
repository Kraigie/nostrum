defmodule Mix.Tasks.Gh.Docs do
  @moduledoc """
  Task that pushes docs to gh-pages branch on Discord.
  """

  use Mix.Task

  @remote_push_url "https://github.com/Kraigie/nostrum"

  defp run!(command) do
    if Mix.shell().cmd(command) != 0 do
      raise Mix.Error, message: "command `#{command}` failed"
    end

    :ok
  end

  # Taken from https://gist.github.com/jjh42/5737777 with some modifications
  @shortdoc "Mix task to publish gh-pages site."
  @dialyzer {:nowarn_function, run: 1}
  def run(_) do
    File.rm_rf("doc")
    Mix.Task.run("docs")
    File.cd!("doc")
    run!("git init .")
    run!("git add .")
    run!("git commit -a -m \"Generated docs\"")
    run!("git remote add origin #{@remote_push_url}")
    run!("git push origin master:gh-pages --force")
  end
end
