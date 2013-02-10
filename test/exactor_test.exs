defmodule ExActor.Test do
  use ExUnit.Case 
  require ExActor
  
  defmodule FunActor do
    use ExActor
    
    def init({arg, register_name}) do 
      Process.register(self, register_name)
      initial_state(arg)
    end
    
    def init(arg) do
      super(arg)
    end
    
    defcast set(x), do: new_state(x)
    defcall get, state: state, do: state
    
    defcast pm_set, state: 2, do: new_state(:two)
    defcast pm_set, state: 3, do: new_state(:three)
    
    defcall timeout, timeout: 10, do: :timer.sleep(100)
    
    defcall unexported, export: false, do: :unexported
    def my_unexported(server), do: :gen_server.call(server, :unexported)
    
    defcall wellknown, export: :fun_actor, do: :wellknown
    
    defcall reply_leave_state, do: 3
    defcast leave_state, do: 4
    defcall full_reply, do: reply(5,6)
  end
  
  test "functional actor" do    
    {:ok, actor} = FunActor.start({1, :fun_actor})
    assert is_pid(actor)
    assert FunActor.get(actor) == 1
    
    FunActor.set(actor, 2)
    assert FunActor.get(actor) == 2
    
    FunActor.pm_set(actor)
    assert FunActor.get(actor) == :two
    
    {:timeout, _} =  catch_exit(FunActor.timeout(actor))
    assert catch_error(FunActor.unexported) == :undef
    assert FunActor.my_unexported(actor) == :unexported
    assert FunActor.wellknown == :wellknown
    
    FunActor.set(actor, 2)
    assert FunActor.reply_leave_state(actor) == 3
    assert FunActor.get(actor) == 2
    
    FunActor.leave_state(actor)
    assert FunActor.get(actor) == 2
    
    assert FunActor.full_reply(actor) == 5
    assert FunActor.get(actor) == 6
  end
  
  test "functional starting" do
    {:ok, actor} = FunActor.start
    assert FunActor.get(actor) == nil
    
    {:ok, actor} = FunActor.start(1)
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start(1, [])
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start_link
    assert FunActor.get(actor) == nil
    
    {:ok, actor} = FunActor.start_link(1)
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start_link(1, [])
    assert FunActor.get(actor) == 1
  end
  
  defmodule ObjActor do
    use ExActor, tupmod: true
    
    def init(nil), do: initial_state(0)
    def init(other), do: initial_state(other)
    
    defcast set(x), do: new_state(x)
    defcall get, state: value, do: value
    defcall me, do: this
  end
  
  test "objectified actor" do
    {:ok, actor} = ObjActor.start
    
    assert is_pid(actor.pid)
    assert actor === ObjActor.actor(actor.pid)
    
    assert actor.set(1).get == 1
    assert actor.me == actor
    
    ObjActor.set(actor.pid, 2)
    assert ObjActor.get(actor.pid) == 2
  end
  
  test "objectified starting" do
    assert elem(ObjActor.start, 1).get == 0
    assert elem(ObjActor.start(1), 1).get == 1
    assert elem(ObjActor.start(1, []), 1).get == 1
    
    assert elem(ObjActor.start_link, 1).get == 0
    assert elem(ObjActor.start_link(1), 1).get == 1
    assert elem(ObjActor.start_link(1, []), 1).get == 1
  end
end