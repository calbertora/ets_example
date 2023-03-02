defmodule ProcessManager do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__,[],name: __MODULE__)
  end

  def handle_cast({:create_process, session, message}, state) do
    spawn_link(fn -> create_process(session) end)

    :ets.insert(:processes, {session, message})

    {:noreply, state}
  end

  def handle_call(:num_processes, _from, state) do
    processes = :ets.info(:processes)
    {:reply, processes, state}
  end

  def create_process(session) do
    sleep_time = :rand.uniform(20_000..60_000)
    :timer.sleep(sleep_time)

    :ets.delete(:processes, session)

    exit(:normal)
  end

  def init(_) do
    :ets.new(:processes, [:set, :public])
    {:ok, []}
  end
end
