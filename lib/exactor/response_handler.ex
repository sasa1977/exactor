defmodule ExActor.ResponseHandler do
  @moduledoc false

  def handle_call_response({ExActor, :reply, response}, state), do: {:reply, response, state}
  def handle_call_response({ExActor, :noreply}, state), do: {:noreply, state}
  def handle_call_response(response, state) do
    if proper_call_response(response) do
      response
    else
      IO.write "Implicit reply is deprecated. Please use standard gen_server replies or reply(response)} instead.\n#{Exception.format_stacktrace}"
      {:reply, response, state}
    end
  end

  defp proper_call_response({:reply, _, _}), do: true
  defp proper_call_response({:reply, _, _, _}), do: true
  defp proper_call_response({:noreply, _}), do: true
  defp proper_call_response({:noreply, _, _}), do: true
  defp proper_call_response({:stop, _, _}), do: true
  defp proper_call_response({:stop, _, _, _}), do: true
  defp proper_call_response(_), do: false
  


  def handle_cast_response({ExActor, :noreply}, state), do: {:noreply, state}
  def handle_cast_response(response, state) do
    if proper_cast_response(response) do
      response
    else
      IO.write "Implicit reply is deprecated. Please use gen_server replies or :noreply instead.\n#{Exception.format_stacktrace}"
      {:noreply, state} 
    end
  end

  defp proper_cast_response({:noreply, _}), do: true
  defp proper_cast_response({:noreply, _, _}), do: true
  defp proper_cast_response({:stop, _, _}), do: true
  defp proper_cast_response(_), do: true

  # Temporary, to preserve stack
  def dummy_wrap(x), do: x
end