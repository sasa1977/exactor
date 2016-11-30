defmodule DynamicTest do
  use ExUnit.Case

  defmodule DynServer do
    use ExActor.Tolerant

    defstart start

    for op <- [:get] do
      defcall unquote(op), state: state do
        reply(state)
      end
    end

    for op <- [:set] do
      defcast unquote(op)(arg) do
        new_state(arg)
      end
    end
  end

  test "dynamic" do
    {:ok, pid} = DynServer.start
    DynServer.set(pid, 1)
    assert DynServer.get(pid) == 1
  end


  defmodule MapServer do
    use ExActor.Tolerant
    import ExActor.Delegator

    defstart start, do: initial_state(Map.new)

    delegate_to Map do
      query get/2
      query size/1
      trans put/3
    end

    defcall normal_call, do: reply(2)
  end

  test "wrapper" do
    {:ok, pid} = MapServer.start

    assert MapServer.get(pid, :a) == nil
    assert MapServer.size(pid) == 0

    MapServer.put(pid, :a, 1)
    assert MapServer.get(pid, :a) == 1
    assert MapServer.size(pid) == 1
    assert MapServer.normal_call(pid) == 2
  end
end
