defmodule ExActor.Test do
  use ExUnit.Case 
  
  defmodule TestActor do
    use ExActor
    
    defcast set(x), do: new_state(x)
    defcall get, state: state, do: state
    
    defcast pm_set, state: 2, do: new_state(:two)
    defcast pm_set, state: 3, do: new_state(:three)
    
    defcall timeout, timeout: 10, do: :timer.sleep(100)
    
    defcall unexported, export: false, do: :unexported
    def my_unexported(server), do: :gen_server.call(server, :unexported)
    
    defcall reply_leave_state, do: 3
    defcast leave_state, do: 4
    defcall full_reply, do: reply(5,6)
    
    defcall me, do: this
    
    defcall test_exc do
      try do
        throw(__ENV__.line) 
      catch _,line ->
        {line, hd(System.stacktrace) |> elem(3)}
      end
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
    
    tupmod = TestActor.actor(actor)
    assert tupmod.set(7).get == 7
    assert tupmod.me == tupmod
    
    {line, exception} = TestActor.test_exc(actor)
    assert (exception[:file] |> Path.basename) == 'exactor_test.exs'
    assert exception[:line] == line
  end

  test "actor start" do
    assert TestActor.actor_start(0).set(1).get == 1
    assert TestActor.actor_start_link(0).set(2).get == 2
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
    
    defcall get, state: state, do: state
    defcast set(x), do: new_state(x)
  end
  
  test "singleton" do
    {:ok, _} = SingletonActor.start(0)
    SingletonActor.set(5)
    assert SingletonActor.get == 5
  end
  
  
  defmodule GlobalSingletonActor do
    use ExActor, export: {:global, :global_singleton}
    
    defcall get, state: state, do: state
    defcast set(x), do: new_state(x)
  end
  
  test "global singleton" do
    {:ok, _} = GlobalSingletonActor.start(0)
    GlobalSingletonActor.set(3)
    assert GlobalSingletonActor.get == 3
  end


  defmodule InitialState1 do
    use ExActor, initial_state: HashDict.new
    defcall get, state: state, do: state
  end

  defmodule InitialState2 do
    use ExActor
    definit do: HashSet.new
    defcall get, state: state, do: state
  end

  defmodule InitialState3 do
    use ExActor
    definit input: x, do: x + 1
    defcall get, state: state, do: state
  end

  test "initial state" do
    assert (InitialState1.start |> elem(1) |> InitialState1.get) == HashDict.new
    assert (InitialState2.start |> elem(1) |> InitialState2.get) == HashSet.new
    assert (InitialState3.start(1) |> elem(1) |> InitialState3.get) == 2
  end


  defmodule PatternMatch do
    use ExActor

    defcall test(1), do: :one
    defcall test(2), do: :two
    defcall test(x), when: x < 4, do: :three
    defcall test(_), state: 4, do: :four
    defcall test(_), state: state, when: state < 6, do: :five
    defcall test(_), do: :rest
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
        state
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
end