defmodule ExActor.Helper do
  @moduledoc false

  def handler_sig(:defcall, options, msg, state_arg) do
    {:handle_call, [msg, options[:from] || quote(do: _from), state_arg]}
  end

  def handler_sig(:defcast, _, msg, state_arg) do
    {:handle_cast, [msg, state_arg]}
  end

  def handler_sig(:definfo, _, msg, state_arg) do
    {:handle_info, [msg, state_arg]}
  end


  def get_state_identifier(nil), do: get_state_identifier(quote(do: _))
  def get_state_identifier(any) do
    quote do
      unquote(any) = var!(___generated_state)
    end
  end



  def msg_payload(function, nil), do: function
  def msg_payload(function, []), do: function
  def msg_payload(function, args), do: quote(do: {unquote_splicing([function | args])})


  def interface_args(args, options), do: {server_arg(options), stub_args(args)}

  defp server_arg(options) do
    cond do
      (options[:export] || true) == true -> quote(do: server)
      true -> nil
    end
  end

  defp stub_args(args) do
    Enum.reduce(args, {0, []}, fn(_, {index, args}) ->
      {
        index + 1,
        [{:"arg#{index}", [], nil} | args]
      }
    end)
    |> elem(1)
    |> Enum.reverse
  end



  def server_args(options, type, msg) do
    [server_ref(options), msg] ++ timeout_arg(options, type)
  end

  defp server_ref(options) do
    case options[:export] do
      default when default in [nil, false, true] -> quote(do: server)
      local when is_atom(local) -> local
      {:local, local} -> local
      {:global, _} = global -> global
      {:{}, _, [:via, _, _]} = via -> via
    end
  end

  defp timeout_arg(options, type) do
    case {type, options[:timeout]} do
      {:defcall, timeout} when timeout != nil ->
        [timeout]
      _ -> []
    end
  end


  def init_global_options(caller, opts) do
    Module.put_attribute(caller.module, :exactor_global_options, opts || [])
  end

  def init_exported do
    quote do
      @exported HashSet.new
    end
  end

  def def_initializer(caller) do
    initial_state =
      Module.get_attribute(caller.module, :exactor_global_options)
      |> Dict.fetch(:initial_state)

    case initial_state do
      :error -> nil
      {:ok, state} ->
        quote do
          definit do: initial_state(unquote(state))
        end
    end
  end
end