defmodule ExActor.Responders do
  defmacro initial_state(state) do 
    quote do
      {:ok, unquote(state)}
    end
  end

  defmacro reply(response) do
    quote do
      {ExActor, :reply, unquote(response)}
    end
  end

  defmacro set_and_reply(new_state, response) do
    quote do
      {:reply, unquote(response), unquote(new_state)}
    end
  end

  defmacro new_state(state) do
    quote do
      {:noreply, unquote(state)}
    end
  end

  defmacro noreply do
    quote do
      {ExActor, :noreply}
    end
  end
end