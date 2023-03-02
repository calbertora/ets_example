defmodule ProcessManager do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__,[],name: __MODULE__)
  end

  def handle_cast({:create_process, message}, state) do
    spawn_link(fn -> create_process(message) end)
    {:noreply, state}
  end

  def handle_call(:num_processes, _from, state) do
    processes = :ets.info(:processes)
    {:reply, processes, state}
  end

  def create_process(message) do
    session = :rand.uniform(999_999)

    :ets.insert(:processes, {session, message})

    sleep_time = Enum.random(20_000..60_000)
    :timer.sleep(sleep_time)

    :ets.delete(:processes, {session, message})

    exit(:normal)
  end

  def init(_) do
    :ets.new(:processes, [:set, :public])
    {:ok, []}
  end

  def generate_processes(pid, quantity) do
    1..quantity |> Enum.each(fn i ->
      message = "Hello, I'm the process ##{i} of this batch"
      GenServer.cast(pid, {:create_process, message})
    end)
  end
end
