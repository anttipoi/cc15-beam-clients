defmodule ElixirClient do

  @name Application.get_env(:elixir_client, :name)
  @lobby_port Application.get_env(:elixir_client, :port)
  
  defp random_game_color(), do: "red"

  defp sample_players() do
    [%{:number => 1,
       :name => "Teemu"},
     %{:number => 2,
       :name => "Kaali"},
     %{:number => 3,
       :name => "Mighty Eagleflow"}]
  end

  def run_client() do
    initial_message = connect_message(@name, "bendersgame")
    transport = open_transport(:localhost, @lobby_port, initial_message)
    loop(%{:lobby_transport => transport})
  end

  defp loop(state) do
    debug("in run loop")
    receive do
      {:udp, _socket, _ip, _in_port_no, json_data} ->
        message = :jsx.decode(json_data, [:return_maps, {:labels, :atom}])
        debug("received JSON", message)
        new_state = on_server_message(message, state)
        loop(new_state)
      m ->
        debug("Don't know what to do with message", m)
        loop(State)
    end
  end

  defp on_server_message(%{:type => "connect-ok",
                           :"connection-id" => connection_id}, 
                         state) do
    Dict.put_new(state, :connection_id, connection_id)
  end

  defp on_server_message(%{:type => "ping"}, 
                         state = %{:connection_id => connection_id, 
                                   :lobby_transport => lobby_transport}) do
    send_message(pong_message(connection_id), lobby_transport)
    state
  end
  
  defp on_server_message(%{:type => "game-at",
                           :address => _Address,
                           :port => port,
                           :"game-id" => game_id},
                         state = %{:connection_id => connection_id}) do
    initial_message = join_message(game_id, @name, random_game_color(), sample_players(), connection_id)
    game_transport = open_transport(:localhost, port, initial_message)
    Dict.put_new(state, :gameTransport, game_transport)
  end
  
  defp on_server_message(%{:type => "join-ok"}, 
                         state) do
    debug("Join was successful, let the games begin")
    state
  end
  
  defp on_server_message(%{:type => "join-error",
                           :description => description}, 
                         state) do
    debug("Join failed", description)
    state
  end

  defp on_server_message(msg, state) do
    debug("Unknown msg from server", msg)
    state
  end

  defp open_transport(server_host, server_port, initial_message) do
    {:ok, socket} = :gen_udp.open(0, [{:active, :true}, :binary])
    transport = %{:server_host => server_host,
                  :server_port => server_port,
                  :socket => socket}
    send_message(initial_message, transport)
    transport
  end

  defp send_message(json_message, %{:server_host => server_host, :server_port => server_port, :socket => socket}) do
    message = :jsx.encode(json_message)
    debug("sending json", message)
    :gen_udp.send(socket, server_host, server_port, message)
  end
    
  defp connect_message(name, game_id) do
    %{:type => "connect", 
      :name => name, 
      :"game-id" => game_id}
  end

  defp pong_message(connection_id) do
    %{:type => "pong",
      :"connection-id" => connection_id}
  end

  defp join_message(game_id, name, color, players, connection_id) do
    %{:type => "join",
      :color => color, 
      :"connection-id" => connection_id,
      :"game-id" => game_id,
      :name => name, 
      :players => players}
  end

  defp debug(message) do
    :io.format(message <> "~n")
  end

  defp debug(message, arg) do
    format = message <> ": ~p~n"
    :io.format(format, [arg])
  end
end
