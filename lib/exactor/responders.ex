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
  defmacro initial_state(state, timeout \\ nil) do
    timeout = timeout || quote(do: Process.get(ExActor.ResponseDecoration) || :infinity)
    quote do
      {:ok, unquote(state), unquote(timeout)}
    end
  end

  @doc """
  Ensures that timeout will be included in each return tuple.

  Must be called from `ExActor.Operations.defstart/2` (or `definit`).

  This works only for return tuples made by macros from this module. If you're
  creating standard `gen_server` response manually, it's your responsibility to
  include the timeout, or override it if you want to.
  """
  defmacro timeout_after(time_ms) do
    quote do
      Process.put(ExActor.ResponseDecoration, unquote(time_ms))
    end
  end

  @doc """
  Ensures that `:hibernate` will be included in each return tuple.

  Must be called from `ExActor.Operations.defstart/2` (or `definit`).

  This works only for return tuples made by macros from this module. If you're
  creating standard `gen_server` response manually, it's your responsibility to
  include the `:hibernate`, or override it if you want to.
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
  defmacro reply(response, timeout \\ nil) do
    timeout = timeout || quote(do: Process.get(ExActor.ResponseDecoration) || :infinity)
    quote do
      {:reply, unquote(response), unquote(ExActor.Helper.state_var), unquote(timeout)}
    end
  end

  @doc """
  Replies and sets the new state

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defmulticall/3`
  """
  defmacro set_and_reply(new_state, response, timeout \\ nil) do
    timeout = timeout || quote(do: Process.get(ExActor.ResponseDecoration) || :infinity)
    quote do
      {:reply, unquote(response), unquote(new_state), unquote(timeout)}
    end
  end

  @doc """
  Sets the new state.

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defcast/3`
  - `ExActor.Operations.defabcast/3`
  - `ExActor.Operations.defmulticall/3`
  - `ExActor.Operations.defhandleinfo/3`
  """
  defmacro new_state(state, timeout \\ nil) do
    timeout = timeout || quote(do: Process.get(ExActor.ResponseDecoration) || :infinity)
    quote do
      {:noreply, unquote(state), unquote(timeout)}
    end
  end

  @doc """
  Leaves the state unchanged.

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defcast/3`
  - `ExActor.Operations.defabcast/3`
  - `ExActor.Operations.defmulticall/3`
  - `ExActor.Operations.defhandleinfo/3`
  """
  defmacro noreply(timeout \\ nil) do
    timeout = timeout || quote(do: Process.get(ExActor.ResponseDecoration) || :infinity)
    quote do
      {:noreply, unquote(ExActor.Helper.state_var), unquote(timeout)}
    end
  end

  @doc """
  Stops the server.

  Applicable in:

  - `ExActor.Operations.defcall/3`
  - `ExActor.Operations.defcast/3`
  - `ExActor.Operations.defabcast/3`
  - `ExActor.Operations.defmulticall/3`
  - `ExActor.Operations.defhandleinfo/3`
  """
  defmacro stop_server(reason) do
    quote do
      {:stop, unquote(reason), unquote(ExActor.Helper.state_var)}
    end
  end
end