defmodule ExActor.Responders do
  @moduledoc """
  Helper macros that can be used for simpler responses from init/call/cast/info
  handlers.
  """

  @doc """
  Sets the initial state.

  Applicable in:

  - `ExActor.Operations.defstart/3`
  - `ExActor.Operations.definit/2`
  """
  defmacro initial_state(state) do
    quote do
      {:ok, unquote(state)}
    end
  end

  @doc """
  Replies without changing the state.

  Applicable in:
  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defmulticall/3`
  """
  defmacro reply(response) do
    quote do
      {:reply, unquote(response), unquote(ExActor.Helper.state_var)}
    end
  end

  @doc """
  Replies and sets the new state

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defmulticall/3`
  """
  defmacro set_and_reply(new_state, response) do
    quote do
      {:reply, unquote(response), unquote(new_state)}
    end
  end

  @doc """
  Sets the new state.

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defcast/3`
  - `ExActor.Operations.defabcast/3`
  - `ExActor.Operations.defmulticall/3`
  - `ExActor.Operations.definfo/3`
  """
  defmacro new_state(state) do
    quote do
      {:noreply, unquote(state)}
    end
  end

  @doc """
  Leaves the state unchanged.

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defcast/3`
  - `ExActor.Operations.defabcast/3`
  - `ExActor.Operations.defmulticall/3`
  - `ExActor.Operations.definfo/3`
  """
  defmacro noreply do
    quote do
      {:noreply, unquote(ExActor.Helper.state_var)}
    end
  end
end