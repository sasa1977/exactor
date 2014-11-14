defmodule ExActorTest do
  use ExUnit.Case

  defmodule TestActor do
    use ExActor.Tolerant

    defstart start, do: initial_state(nil)
    defstart start(x), do: initial_state(x)
    defstart start(x,y,z), do: initial_state(x+y+z)
    defstart my_start(x,y), link: false, do: initial_state(x+y)

    defstart start_link

    defcast set(x), do: new_state(x)
    defcall get, state: state, do: reply(state)

    defcast pm_set, state: 2, do: new_state(:two)
    defcast pm_set, state: 3, do: new_state(:three)

    defcall timeout, timeout: 10, do: (:timer.sleep(100); noreply)

    defcall unexported, export: false, do: reply(:unexported)
    def my_unexported(server), do: GenServer.call(server, :unexported)

    defcall reply_leave_state, do: reply(3)
    defcast leave_state, do: (4; noreply)
    defcall full_reply, do: set_and_reply(6, 5)

    def callp_interface(server), do: private_call(server)
    defcallp private_call, do: reply(:private_call)

    def castp_interface(server), do: private_cast(server)
    defcastp private_cast, do: new_state(:private)

    defcall test_exc do
      try do
        throw(__ENV__.line)
      catch _,line ->
        reply({line, hd(System.stacktrace) |> elem(3)})
      end
    end

    defcall test_from, from: {from, _} do
      send(from, :from_ok)
      reply(:ok)
    end

    definfo {:msg1, from} do
      send(from, :reply_msg1)
      noreply
    end

    definfo {:msg_get, from}, state: state do
      send(from, state)
      noreply
    end

    definfo sender, when: is_pid(sender) do
      send(sender, :echo)
      noreply
    end
  end

  test "basic" do
    {:ok, actor} = TestActor.start(1)
    assert is_pid(actor)
    assert TestActor.get(actor) == 1

    TestActor.set(actor, 2)
    assert TestActor.get(actor) == 2

    TestActor.pm_set(actor)
    assert TestActor.get(actor) == :two

    assert TestActor.callp_interface(actor) == :private_call
    assert catch_error(TestActor.private_call(actor)) == :undef

    TestActor.castp_interface(actor)
    assert TestActor.get(actor) == :private
    assert catch_error(TestActor.private_cast(actor)) == :undef

    {:timeout, _} =  catch_exit(TestActor.timeout(actor))

    assert catch_error(TestActor.unexported(actor)) == :undef
    assert TestActor.my_unexported(actor) == :unexported

    TestActor.set(actor, 2)
    assert TestActor.reply_leave_state(actor) == 3
    assert TestActor.get(actor) == 2

    TestActor.leave_state(actor)
    assert TestActor.get(actor) == 2

    assert TestActor.full_reply(actor) == 5
    assert TestActor.get(actor) == 6

    {line, exception} = TestActor.test_exc(actor)
    assert (exception[:file] |> Path.basename |> to_string) == "exactor_test.exs"
    assert exception[:line] == line

    assert TestActor.test_from(actor) == :ok
    assert_receive :from_ok

    send(actor, {:msg1, self})
    assert_receive :reply_msg1

    TestActor.set(actor, 10)
    send(actor, {:msg_get, self})
    assert_receive 10

    send(actor, self)
    assert_receive :echo
  end

  test "start" do
    {:ok, actor} = TestActor.start
    assert TestActor.get(actor) == nil

    {:ok, actor} = TestActor.start(1)
    assert TestActor.get(actor) == 1

    {:ok, actor} = TestActor.start(1,2,3)
    assert TestActor.get(actor) == 6

    {:ok, actor} = TestActor.my_start(3,4)
    assert TestActor.get(actor) == 7

    Process.exit(actor, :kill)
    refute_receive {:EXIT, ^actor, :killed}
  end

  test "start_link" do
    {:ok, actor} = TestActor.start_link
    assert TestActor.get(actor) == nil

    Process.flag(:trap_exit, true)
    Process.exit(actor, :kill)
    assert_receive {:EXIT, ^actor, :killed}
    Process.flag(:trap_exit, false)
  end


  defmodule PrivateStarterActor do
    use ExActor.GenServer

    def my_start, do: start(5)
    defstartp start(x), do: initial_state(x)
    defcall get, state: state, do: reply(state)
  end

  test "private starter" do
    assert catch_error(PrivateStarterActor.start(5)) == :undef

    {:ok, actor} = PrivateStarterActor.my_start
    assert PrivateStarterActor.get(actor) == 5
  end


  defmodule RuntimeGenServerOptsActor do
    use ExActor.GenServer

    defstart start(x), gen_server_opts: :runtime, do: initial_state(x)
    defcall get, state: state, do: reply(state)
  end

  test "runtime gen_server_opts" do
    RuntimeGenServerOptsActor.start(5, name: :foo)
    assert RuntimeGenServerOptsActor.get(:foo) == 5

    RuntimeGenServerOptsActor.start(3, name: :bar)
    assert RuntimeGenServerOptsActor.get(:bar) == 3
  end


  defmodule SingletonActor do
    use ExActor.Tolerant, export: :singleton
    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "singleton" do
    {:ok, _} = SingletonActor.start(0)
    SingletonActor.set(5)
    assert SingletonActor.get == 5
  end


  defmodule GlobalSingletonActor do
    use ExActor.Tolerant, export: {:global, :global_singleton}
    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "global singleton" do
    {:ok, _} = GlobalSingletonActor.start(0)
    GlobalSingletonActor.set(3)
    assert GlobalSingletonActor.get == 3
  end


  defmodule ViaSingletonActor do
    use ExActor.Tolerant, export: {:via, :global, :global_singleton2}

    defstart start(x), do: initial_state(x)

    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end

  test "via singleton" do
    {:ok, _} = ViaSingletonActor.start(0)
    ViaSingletonActor.set(4)
    assert ViaSingletonActor.get == 4
  end


  defmodule InitialState2 do
    use ExActor.Tolerant

    defstart start(1), do: initial_state(:one)
    defstart start(x), when: x < 3, do: initial_state(:two)
    defstart start(_), do: initial_state(:rest)

    defcall get, state: state, do: reply(state)
  end

  test "initial state" do
    assert (InitialState2.start(1) |> elem(1) |> InitialState2.get) == :one
    assert (InitialState2.start(2) |> elem(1) |> InitialState2.get) == :two
    assert (InitialState2.start(3) |> elem(1) |> InitialState2.get) == :rest
  end


  defmodule PatternMatch do
    use ExActor.Tolerant

    defstart start, do: initial_state(nil)
    defstart start(state), do: initial_state(state)

    defcall test(1), do: reply(:one)
    defcall test(2), do: reply(:two)
    defcall test(x), when: x < 4, do: reply(:three)
    defcall test(_), state: 4, do: reply(:four)
    defcall test(_), state: state, when: state < 6, do: reply(:five)
    defcall test(_), do: reply(:rest)
  end

  test "pattern matching" do
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(1)) == :one
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(2)) == :two
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(3)) == :three
    assert (PatternMatch.start(4) |> elem(1) |> PatternMatch.test(4)) == :four
    assert (PatternMatch.start(5) |> elem(1) |> PatternMatch.test(4)) == :five
    assert (PatternMatch.start(6) |> elem(1) |> PatternMatch.test(4)) == :rest
  end


  defmodule DynActor do
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
    {:ok, pid} = DynActor.start
    DynActor.set(pid, 1)
    assert DynActor.get(pid) == 1
  end


  defmodule HashDictActor do
    use ExActor.Tolerant
    import ExActor.Delegator

    defstart start, do: initial_state(HashDict.new)

    delegate_to HashDict do
      init
      query get/2
      query size/1
      trans put/3
    end

    defcall normal_call, do: reply(2)
  end

  test "wrapper" do
    {:ok, actor} = HashDictActor.start

    assert HashDictActor.get(actor, :a) == nil
    assert HashDictActor.size(actor) == 0

    HashDictActor.put(actor, :a, 1)
    assert HashDictActor.get(actor, :a) == 1
    assert HashDictActor.size(actor) == 1
    assert HashDictActor.normal_call(actor) == 2
  end


  defmodule InterfaceReqsActor do
    use ExActor.GenServer

    defstart start_link, do: initial_state(nil)

    defcall foo
    defcast bar
    defcall baz(x, y)

    def handle_call({:baz, x, y}, _, state) do
      {:reply, x+y, state}
    end
  end

  test "just interfaces" do
    Logger.remove_backend(:console)
    Process.flag(:trap_exit, true)

    {:ok, actor} = InterfaceReqsActor.start_link
    assert InterfaceReqsActor.baz(actor, 1, 2) == 3

    try do InterfaceReqsActor.foo(actor) catch _,_ -> nil end
    assert_receive {:EXIT, ^actor, {:function_clause, _}}

    {:ok, actor} = InterfaceReqsActor.start_link
    try do InterfaceReqsActor.bar(actor) catch _,_ -> nil end
    assert_receive {:EXIT, ^actor, {:bad_cast, :bar}}

    Process.flag(:trap_exit, false)
    :timer.sleep(100)
    Logger.add_backend(:console)
  end


  defmodule ClusterActor do
    use ExActor.GenServer, export: :cluster_actor

    defstart start, do: initial_state(nil)
    defmulticall sum(x, y), do: reply(x + y)

    def diff(x, y), do: do_diff(x, y)
    defmulticallp do_diff(x, y), do: reply(x - y)

    defcall get, state: state, do: reply(state)
    defabcast set(x), do: new_state(x)

    def set2(x), do: do_set2(x)
    defabcastp do_set2(x), do: new_state(2 * x)
  end

  test "cluster actor" do
    ClusterActor.start
    assert ClusterActor.sum(3, 2) == {[{:"nonode@nohost", 5}], []}
    assert ClusterActor.sum([node()], 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert ClusterActor.sum([:unknown_node], 3, 2) == {[], [:unknown_node]}
    assert ClusterActor.sum([], 3, 2) == {[], []}
    assert catch_error(ClusterActor.do_diff(3, 2)) == :undef
    assert ClusterActor.diff(3, 2) == {[{:"nonode@nohost", 1}], []}

    assert ClusterActor.set(4) == :abcast
    assert ClusterActor.get == 4

    assert catch_error(ClusterActor.do_set2(3)) == :undef
    assert ClusterActor.set2(4) == :abcast
    assert ClusterActor.get == 8
  end



  defmodule NonRegisteredClusterActor do
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

  test "non registered cluster actor" do
    NonRegisteredClusterActor.start
    assert NonRegisteredClusterActor.sum(:nrca, 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert NonRegisteredClusterActor.sum([node()], :nrca, 3, 2) == {[{:"nonode@nohost", 5}], []}
    assert NonRegisteredClusterActor.sum([:unknown_node], :nrca, 3, 2) == {[], [:unknown_node]}
    assert NonRegisteredClusterActor.sum([], :nrca, 3, 2) == {[], []}
    assert catch_error(NonRegisteredClusterActor.do_diff(:nrca, 3, 2)) == :undef
    assert NonRegisteredClusterActor.diff(3, 2) == {[{:"nonode@nohost", 1}], []}

    assert NonRegisteredClusterActor.set(:nrca, 4) == :abcast
    assert NonRegisteredClusterActor.get(:nrca) == 4

    assert catch_error(NonRegisteredClusterActor.do_set2(:nrca, 3)) == :undef
    assert NonRegisteredClusterActor.set2(4) == :abcast
    assert NonRegisteredClusterActor.get(:nrca) == 8
  end

  defmodule TestDefaultsActor do
    use ExActor.GenServer

    defstart start(a, b \\ 0, c), do: initial_state(a + b + c)
    defcall get(x \\ nil), state: state, do: reply(x || state)
    defcast set(x \\ 0), do: new_state(x)
  end

  test "defaults" do
    {:ok, actor} = TestDefaultsActor.start(1, 2, 3)
    assert TestDefaultsActor.get(actor) == 6

    {:ok, actor} = TestDefaultsActor.start(1, 2)
    assert TestDefaultsActor.get(actor) == 3

    assert TestDefaultsActor.get(actor, 4) == 4

    TestDefaultsActor.set(actor)
    assert TestDefaultsActor.get(actor) == 0

    TestDefaultsActor.set(actor, 5)
    assert TestDefaultsActor.get(actor) == 5
  end
end