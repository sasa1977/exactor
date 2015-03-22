defmodule ClusterTest do
  use ExUnit.Case

  defmodule ClusterServer do
    use ExActor.GenServer, export: :cluster_pid

    defstart start, do: initial_state(nil)
    defmulticall sum(x, y), do: reply(x + y)

    defmulticall timeout1, timeout: 10, do: (:timer.sleep(100); reply(:ok))
    defmulticall timeout2, timeout: foo, do: (:timer.sleep(100); reply(:ok))
    defmulticall timeout3, timeout: foo \\ 10, do: (:timer.sleep(100); reply(:ok))

    def diff(x, y), do: do_diff(x, y)
    defmulticallp do_diff(x, y), do: reply(x - y)

    defcall get, state: state, do: reply(state)
    defabcast set(x), do: new_state(x)

    def set2(x), do: do_set2(x)
    defabcastp do_set2(x), do: new_state(2 * x)
  end

  test "cluster pid" do
    ClusterServer.start
    assert ClusterServer.sum(3, 2) == {[{:"nonode@nohost", 5}], []}
    assert ClusterServer.sum([node()], 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert ClusterServer.sum([:unknown_node], 3, 2) == {[], [:unknown_node]}
    assert ClusterServer.sum([], 3, 2) == {[], []}
    assert catch_error(ClusterServer.do_diff(3, 2)) == :undef
    assert ClusterServer.diff(3, 2) == {[{:"nonode@nohost", 1}], []}

    assert ClusterServer.timeout1 == {[], [node]}
    assert ClusterServer.timeout1 == {[], [node]}
    assert ClusterServer.timeout2(10) == {[], [node]}
    assert ClusterServer.timeout2(2000) == {[{node, :ok}], []}
    assert ClusterServer.timeout3 == {[], [node]}
    assert ClusterServer.timeout3([node], 10) == {[], [node]}
    assert ClusterServer.timeout3([node], 2000) == {[{node, :ok}], []}

    assert ClusterServer.set(4) == :abcast
    assert ClusterServer.get == 4

    assert catch_error(ClusterServer.do_set2(3)) == :undef
    assert ClusterServer.set2(4) == :abcast
    assert ClusterServer.get == 8
  end



  defmodule NonRegisteredClusterServer do
    use ExActor.GenServer

    defstart start do
      Process.register(self, :nrca)
      initial_state(nil)
    end

    defmulticall sum(x, y), do: reply(x + y)

    def diff(x, y), do: do_diff(:nrca, x, y)
    defmulticallp do_diff(x, y), do: reply(x - y)

    defcall get, state: state, do: reply(state)
    defabcast set(x), do: new_state(x)

    def set2(x), do: do_set2(:nrca, x)
    defabcastp do_set2(x), do: new_state(2 * x)
  end

  test "non registered cluster pid" do
    NonRegisteredClusterServer.start
    assert NonRegisteredClusterServer.sum(:nrca, 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert NonRegisteredClusterServer.sum([node()], :nrca, 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert NonRegisteredClusterServer.sum([:unknown_node], :nrca, 3, 2) == {[], [:unknown_node]}
    assert NonRegisteredClusterServer.sum([], :nrca, 3, 2) == {[], []}
    assert catch_error(NonRegisteredClusterServer.do_diff(:nrca, 3, 2)) == :undef
    assert NonRegisteredClusterServer.diff(3, 2) == {[{:"nonode@nohost", 1}], []}

    assert NonRegisteredClusterServer.set(:nrca, 4) == :abcast
    assert NonRegisteredClusterServer.get(:nrca) == 4

    assert catch_error(NonRegisteredClusterServer.do_set2(:nrca, 3)) == :undef
    assert NonRegisteredClusterServer.set2(4) == :abcast
    assert NonRegisteredClusterServer.get(:nrca) == 8
  end
end