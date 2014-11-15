defmodule RegistrationTest do
  use ExUnit.Case

  defmodule SingletonServer do
    use ExActor.Tolerant, export: :singleton
    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "singleton" do
    {:ok, _} = SingletonServer.start(0)
    SingletonServer.set(5)
    assert SingletonServer.get == 5
  end


  defmodule GlobalSingletonServer do
    use ExActor.Tolerant, export: {:global, :global_singleton}
    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "global singleton" do
    {:ok, _} = GlobalSingletonServer.start(0)
    GlobalSingletonServer.set(3)
    assert GlobalSingletonServer.get == 3
  end


  defmodule ViaSingletonServer do
    use ExActor.Tolerant, export: {:via, :global, :global_singleton2}

    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "via singleton" do
    {:ok, _} = ViaSingletonServer.start(0)
    ViaSingletonServer.set(4)
    assert ViaSingletonServer.get == 4
  end
end