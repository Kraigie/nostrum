defmodule Nostrum.UnsafeHelpers do
  @moduledoc false
  #############################################################################
  ## Taken from [`Unsafe`](https://hex.pm/packages/unsafe)
  ## Copyright (c) 2017 Isaac Whitfield
  ##

  defmacro __using__(options) do
    quote location: :keep do
      @before_compile unquote(__MODULE__)
      @unsafe_options unquote(options)

      Module.register_attribute(__MODULE__, :unsafe, accumulate: true)
    end
  end

  defmacro __before_compile__(%{module: module} = env) do
    binding = Module.get_attribute(module, :unsafe)

    options =
      module
      |> Module.get_attribute(:unsafe_options)
      |> Kernel.||([])

    compile!(env, binding, options)
  end

  @type arities :: arity | [arity]
  @type binding :: {atom, arities} | {atom, arities, handler}
  @type handler :: atom | {atom, atom}

  @spec compile!(Macro.Env.t(), binding | [binding], Keyword.t()) :: Macro.t()
  def compile!(env, bindings, options) when is_list(bindings),
    do: Enum.map(bindings, &compile!(env, &1, options))

  def compile!(env, {name, arity}, options),
    do: compile!(env, {name, arity, options[:handler]}, options)

  def compile!(env, {name, [head | _] = arity, handler}, options)
      when is_integer(head) do
    arity
    |> Enum.map(&{name, &1, handler})
    |> Enum.map(&compile!(env, &1, options))
  end

  def compile!(env, {name, arity, handler}, options) do
    {enum, length, generator} =
      if is_list(arity) do
        {arity, length(arity), &Macro.var(&1, env.module)}
      else
        {0..(arity && arity - 1), arity, &Macro.var(:"arg#{&1}", env.module)}
      end

    params = Enum.map(enum, generator)

    result = quote do: apply(unquote(env.module), unquote(name), unquote(params))

    handle =
      case handler do
        func when is_atom(func) and not is_nil(func) ->
          quote do: unquote(func)(unquote(result))

        {mod, func} ->
          quote do: apply(unquote(mod), unquote(func), [unquote(result)])

        _fail ->
          raise CompileError,
            description: "Invalid handler definition for #{name}/#{length}",
            file: env.file,
            line: env.line
      end

    ex_docs =
      if options[:docs] do
        quote do: @doc("Unsafe proxy definition for `#{unquote(name)}/#{unquote(length)}`.")
      else
        quote do: @doc(false)
      end

    quote do
      unquote(ex_docs)

      def unquote(:"#{name}!")(unquote_splicing(params)) do
        unquote(handle)
      end
    end
  end

  def compile!(env, _invalid, _options),
    do:
      raise(CompileError,
        description: "Invalid function reference provided",
        file: env.file,
        line: env.line
      )
end
