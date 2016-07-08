defmodule BasicTest do
  use ExUnit.Case

  defmodule TestServer do
    use ExActor.Tolerant

    defstart start, do: initial_state(nil)
    defstart start(x), do: initial_state(x)
    defstart start(x,y,z), do: initial_state(x+y+z)
    defstart my_start(x,y), link: false, do: initial_state(x+y)

    defstart start_link

    defcast set(x), do: new_state(x)
    defcall get, state: state, do: reply(state)

    defcast pm_set, state: nil, do: new_state(:two)
    defcast pm_set, state: 3, do: new_state(:three)

    defcast pm_set(1), do: new_state(:one)
    defcast pm_set(x), when: x == 2, do: new_state(:two)
    defcast pm_set(_), state: :two, do: new_state(:three)
    defcast pm_set(_), state: state, when: [handler: state == :three], do: new_state(:four)
    defcast pm_set(x), do: new_state(x)

    defcall timeout1, timeout: 10, do: (:timer.sleep(100); reply(:ok))
    defcall timeout2, timeout: foo, do: (:timer.sleep(100); reply(:ok))
    defcall timeout3, timeout: foo \\ 10, do: (:timer.sleep(100); reply(:ok))

    defhandlecall unexported, do: reply(:unexported)
    def my_unexported(server), do: GenServer.call(server, :unexported)

    defcall reply_leave_state, do: reply(3)
    defcast leave_state, do: noreply
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

    defcall test_composite_match(%{} = arg) do
      reply(arg)
    end

    defhandleinfo {:msg1, from} do
      send(from, :reply_msg1)
      noreply
    end

    defhandleinfo {:msg_get, from}, state: state do
      send(from, state)
      noreply
    end

    defhandleinfo sender, when: is_pid(sender) do
      send(sender, :echo)
      noreply
    end
  end

  test "basic" do
    {:ok, pid} = TestServer.start(1)
    assert is_pid(pid)
    assert TestServer.get(pid) == 1

    TestServer.set(pid, 2)
    assert TestServer.get(pid) == 2

    TestServer.set(pid, nil)
    TestServer.pm_set(pid)
    assert TestServer.get(pid) == :two

    TestServer.pm_set(pid, 1)
    assert TestServer.get(pid) == :one

    TestServer.pm_set(pid, 2)
    assert TestServer.get(pid) == :two

    TestServer.pm_set(pid, 3)
    assert TestServer.get(pid) == :three

    TestServer.pm_set(pid, 4)
    assert TestServer.get(pid) == :four

    TestServer.pm_set(pid, 5)
    assert TestServer.get(pid) == 5

    assert TestServer.callp_interface(pid) == :private_call
    assert catch_error(TestServer.private_call(pid)) == :undef

    TestServer.castp_interface(pid)
    assert TestServer.get(pid) == :private
    assert catch_error(TestServer.private_cast(pid)) == :undef

    {:timeout, _} =  catch_exit(TestServer.timeout1(pid))
    {:timeout, _} =  catch_exit(TestServer.timeout2(pid, 10))
    assert :ok == TestServer.timeout2(pid, 2000)
    {:timeout, _} =  catch_exit(TestServer.timeout3(pid))
    {:timeout, _} =  catch_exit(TestServer.timeout3(pid, 10))
    assert :ok == TestServer.timeout3(pid, 2000)

    assert catch_error(TestServer.unexported(pid)) == :undef
    assert TestServer.my_unexported(pid) == :unexported

    TestServer.set(pid, 2)
    assert TestServer.reply_leave_state(pid) == 3
    assert TestServer.get(pid) == 2

    TestServer.leave_state(pid)
    assert TestServer.get(pid) == 2

    assert TestServer.full_reply(pid) == 5
    assert TestServer.get(pid) == 6

    {line, exception} = TestServer.test_exc(pid)
    assert (exception[:file] |> Path.basename |> to_string) == "basic_test.exs"
    assert exception[:line] == line

    assert TestServer.test_from(pid) == :ok
    assert_receive :from_ok

    send(pid, {:msg1, self})
    assert_receive :reply_msg1

    TestServer.set(pid, 10)
    send(pid, {:msg_get, self})
    assert_receive 10

    send(pid, self)
    assert_receive :echo
  end

  test "start" do
    {:ok, pid} = TestServer.start
    assert TestServer.get(pid) == nil

    {:ok, pid} = TestServer.start(1)
    assert TestServer.get(pid) == 1

    {:ok, pid} = TestServer.start(1,2,3)
    assert TestServer.get(pid) == 6

    {:ok, pid} = TestServer.my_start(3,4)
    assert TestServer.get(pid) == 7

    Process.exit(pid, :kill)
    refute_receive {:EXIT, ^pid, :killed}
  end

  test "start_link" do
    {:ok, pid} = TestServer.start_link
    assert TestServer.get(pid) == nil

    Process.flag(:trap_exit, true)
    Process.exit(pid, :kill)
    assert_receive {:EXIT, ^pid, :killed}
    Process.flag(:trap_exit, false)
  end


  defmodule PrivateStarterServer do
    use ExActor.GenServer

    def my_start, do: start(5)
    defstartp start(x), do: initial_state(x)
    defcall get, state: state, do: reply(state)
  end

  test "private starter" do
    assert catch_error(PrivateStarterServer.start(5)) == :undef

    {:ok, pid} = PrivateStarterServer.my_start
    assert PrivateStarterServer.get(pid) == 5
  end


  defmodule RuntimeGenServerOptsServer do
    use ExActor.GenServer

    defstart start(x), gen_server_opts: :runtime, do: initial_state(x)
    defcall get, state: state, do: reply(state)
  end

  test "runtime gen_server_opts" do
    RuntimeGenServerOptsServer.start(5, name: :foo)
    assert RuntimeGenServerOptsServer.get(:foo) == 5

    RuntimeGenServerOptsServer.start(3, name: :bar)
    assert RuntimeGenServerOptsServer.get(:bar) == 3
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
    defcall test(_)
    defhandlecall test(_), state: 4, do: reply(:four)
    defhandlecall test(_), state: state, when: state < 6, do: reply(:five)
    defhandlecall test(_), do: reply(:rest)
  end

  test "pattern matching" do
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(1)) == :one
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(2)) == :two
    assert (PatternMatch.start |> elem(1) |> PatternMatch.test(3)) == :three
    assert (PatternMatch.start(4) |> elem(1) |> PatternMatch.test(4)) == :four
    assert (PatternMatch.start(5) |> elem(1) |> PatternMatch.test(4)) == :five
    assert (PatternMatch.start(6) |> elem(1) |> PatternMatch.test(4)) == :rest
  end

  defmodule TestDefaultsServer do
    use ExActor.GenServer

    defstart start(a, b \\ 0, c, _ = d, e = _, _ = _ = f), do: initial_state(a + b + c + d + e + f)
    defcall get(_ = x \\ nil), state: state, do: reply(x || state)
    defcast set(x = _ \\ 0), do: new_state(x)
  end

  test "defaults" do
    {:ok, pid} = TestDefaultsServer.start(1, 2, 3, 4, 5, 6)
    assert TestDefaultsServer.get(pid) == 21

    {:ok, pid} = TestDefaultsServer.start(1, 2, 3, 4, 5)
    assert TestDefaultsServer.get(pid) == 15

    assert TestDefaultsServer.get(pid, 4) == 4

    TestDefaultsServer.set(pid)
    assert TestDefaultsServer.get(pid) == 0

    TestDefaultsServer.set(pid, 5)
    assert TestDefaultsServer.get(pid) == 5
  end


  defmodule TestGlobalTimeout do
    use ExActor.GenServer, export: TestGlobalTimeout

    defstart start(hibernate? \\ false) do
      if hibernate? do
        hibernate
      else
        timeout_after(50)
      end
      initial_state(nil)
    end

    defcast noexpire, do: noreply(:infinity)
    defcast expire, do: noreply

    defhandleinfo :timeout, do: stop_server(:normal)
  end

  test "timeout" do
    TestGlobalTimeout.start
    TestGlobalTimeout.noexpire
    :timer.sleep(100)
    assert is_pid(Process.whereis(TestGlobalTimeout))

    TestGlobalTimeout.expire
    :timer.sleep(100)
    assert nil == Process.whereis(TestGlobalTimeout)

    TestGlobalTimeout.start(true)
    assert is_pid(Process.whereis(TestGlobalTimeout))
  end


  defmodule InterfaceReqsServer do
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

    {:ok, pid} = InterfaceReqsServer.start_link
    assert InterfaceReqsServer.baz(pid, 1, 2) == 3

    try do InterfaceReqsServer.foo(pid) catch _,_ -> nil end
    assert_receive {:EXIT, ^pid, {:function_clause, _}}

    {:ok, pid} = InterfaceReqsServer.start_link
    try do InterfaceReqsServer.bar(pid) catch _,_ -> nil end
    assert_receive {:EXIT, ^pid, {:bad_cast, :bar}}

    Process.flag(:trap_exit, false)
    :timer.sleep(100)
    Logger.add_backend(:console)
  end

  defmodule FragmentModule do
    defmacro __using__(_) do
      quote do
        defstart start_link, do: initial_state(nil)
        defcall operation, do: reply(:ok)
      end
    end
  end

  defmodule ComposedModule do
    use ExActor.Tolerant
    use FragmentModule
  end

  test "composed module" do
    {:ok, pid} = ComposedModule.start_link()
    assert :ok == ComposedModule.operation(pid)
  end
end
