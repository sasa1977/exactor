defmodule ExActor.Responders do
  @moduledoc """
  Helper macros that can be used for simpler responses from init/call/cast/info
  handlers.
  """

  @doc """
  Can be used from `init` or `definit` to return the initial state.
  """
  defmacro initial_state(state) do 
    quote do
      {:ok, unquote(state)}
    end
  end

  @doc """
  Can be used from `handle_call` or `defcall` to reply without changing the
  state
  """
  defmacro reply(response) do
    quote do
      {ExActor, :reply, unquote(response)}
    end
  end

  @doc """
  Can be used from `handle_call` or `defcall` to reply and change the state.
  """
  defmacro set_and_reply(new_state, response) do
    quote do
      {:reply, unquote(response), unquote(new_state)}
    end
  end

  @doc """
  Can be used from `handle_call`, `defcall`, `handle_info`, or `definfo` to
  set the new state.
  """
  defmacro new_state(state) do
    quote do
      {:noreply, unquote(state)}
    end
  end

  @doc """
  Can be used from `handle_call`, `defcall`, `handle_info`, or `definfo` to
  leave the state unchanged.
  """
  defmacro noreply do
    quote do
      {ExActor, :noreply}
    end
  end
end