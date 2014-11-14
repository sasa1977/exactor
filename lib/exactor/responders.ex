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
      {:ok, unquote(state), Process.get(ExActor.ResponseDecoration) || :infinity}
    end
  end

  @doc """
  When called from `ExActor.Operations.definit/2`, ensures that timeout
  will be included in each return tuple. This works only for return tuples
  made by the macro from this module. If you're creating standard `gen_server`
  response manually, it's your responsibility to include the timeout, or override
  it if you want to.
  """
  defmacro timeout_after(time_ms) do
    quote do
      Process.put(ExActor.ResponseDecoration, unquote(time_ms))
    end
  end

  @doc """
  When called from `ExActor.Operations.definit/2`, ensures that `:hibernate`
  will be included in each return tuple. This works only for return tuples
  made by the macro from this module. If you're creating standard `gen_server`
  response manually, it's your responsibility to include the timeout, or override
  it if you want to.
  """
  defmacro hibernate do
    quote do
      Process.put(ExActor.ResponseDecoration, :hibernate)
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
      {:reply, unquote(response), unquote(ExActor.Helper.state_var), Process.get(ExActor.ResponseDecoration) || :infinity}
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
      {:reply, unquote(response), unquote(new_state), Process.get(ExActor.ResponseDecoration) || :infinity}
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
      {:noreply, unquote(state), Process.get(ExActor.ResponseDecoration) || :infinity}
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
      {:noreply, unquote(ExActor.Helper.state_var), Process.get(ExActor.ResponseDecoration) || :infinity}
    end
  end
end