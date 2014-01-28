defmodule ExActor.Test do
  use ExUnit.Case 
  
  defmodule TestActor do
    use ExActor
    
    defcast set(x), do: new_state(x)
    defcall get, state: state, do: reply(state)
    
    defcast pm_set, state: 2, do: new_state(:two)
    defcast pm_set, state: 3, do: new_state(:three)
    
    defcall timeout, timeout: 10, do: (:timer.sleep(100); noreply)
    
    defcall unexported, export: false, do: reply(:unexported)
    def my_unexported(server), do: :gen_server.call(server, :unexported)
    
    defcall reply_leave_state, do: reply(3)
    defcast leave_state, do: (4; noreply)
    defcall full_reply, do: set_and_reply(6, 5)
        
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
    assert (exception[:file] |> Path.basename) == 'exactor_test.exs'
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

  test "starting" do
    {:ok, actor} = TestActor.start
    assert TestActor.get(actor) == nil
    
    {:ok, actor} = TestActor.start(1)
    assert TestActor.get(actor) == 1
    
    {:ok, actor} = TestActor.start(1, [])
    assert TestActor.get(actor) == 1
    
    {:ok, actor} = TestActor.start_link
    assert TestActor.get(actor) == nil
    
    {:ok, actor} = TestActor.start_link(1)
    assert TestActor.get(actor) == 1
    
    {:ok, actor} = TestActor.start_link(1, [])
    assert TestActor.get(actor) == 1
  end
  
  
  defmodule SingletonActor do
    use ExActor, export: :singleton
    
    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end
  
  test "singleton" do
    {:ok, _} = SingletonActor.start(0)
    SingletonActor.set(5)
    assert SingletonActor.get == 5
  end
  
  
  defmodule GlobalSingletonActor do
    use ExActor, export: {:global, :global_singleton}
    
    defcall get, state: state, do: reply(state)
    defcast set(x), do: new_state(x)
  end
  
  test "global singleton" do
    {:ok, _} = GlobalSingletonActor.start(0)
    GlobalSingletonActor.set(3)
    assert GlobalSingletonActor.get == 3
  end


  defmodule InitialState1 do
    use ExActor, initial_state: HashDict.new
    defcall get, state: state, do: reply(state)
  end

  defmodule InitialState2 do
    use ExActor

    definit 1, do: :one
    definit x, when: x < 3, do: :two
    definit do
      :rest
    end

    defcall get, state: state, do: reply(state)
  end

  test "initial state" do
    assert (InitialState1.start |> elem(1) |> InitialState1.get) == HashDict.new
    assert (InitialState2.start(1) |> elem(1) |> InitialState2.get) == :one
    assert (InitialState2.start(2) |> elem(1) |> InitialState2.get) == :two
    assert (InitialState2.start(3) |> elem(1) |> InitialState2.get) == :rest
  end


  defmodule PatternMatch do
    use ExActor

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
    use ExActor

    lc op inlist [:get] do
      defcall unquote(op), state: state do
        reply(state)
      end
    end

    lc op inlist [:set] do
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
    use ExActor

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
end