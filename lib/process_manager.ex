defmodule ProcessManager do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__,[],name: __MODULE__)
  end

  def handle_cast({:create_process, message}, state) do
    session = :rand.uniform(999_999)
    spawn_link(fn -> create_process(session, message) end)
    {:noreply, state}
  end

  def handle_call(:num_processes, _from, state) do
    processes = :ets.tab2list(:process_manager)
    {:reply, processes, state}
  end

  def create_process(session, message) do
    :ets.insert(:process_manager, {session, message})

    sleep_time = Enum.random(20_000..60_000)
    :timer.sleep(sleep_time)

    IO.inspect "going to delete session #{session}"
    :ets.delete(:process_manager, session)

    exit(:normal)
  end

  def init(_) do
    :ets.new(:process_manager, [:named_table, :set, :public])
    {:ok, []}
  end

  def generate_processes(pid, quantity) do
    1..quantity |> Enum.each(fn i ->
      message = "Hello, I'm the process ##{i} of this batch"
      GenServer.cast(pid, {:create_process, message})
    end)
  end
end
