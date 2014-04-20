defmodule ExActor.ResponseHandler do
  @moduledoc false

  def handle_call_response({ExActor, :reply, response}, state), do: {:reply, response, state}
  def handle_call_response({ExActor, :noreply}, state), do: {:noreply, state}
  def handle_call_response(response, _) do
    response
  end

  def handle_cast_response({ExActor, :noreply}, state), do: {:noreply, state}
  def handle_cast_response(response, _) do
    response
  end
end