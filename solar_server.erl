-module(solar_server).
-export([start/0]).

-define(SSID, "wifiap").
-define(PSK, "wifipw").
-define(PLATFORM, "esp").

start() ->
	Self = self(),

	BatPin = 35,
	SolarPin = 34,
	{ok, BatADC} = adc:start(BatPin),
	{ok, SolarADC} = adc:start(SolarPin),
	io:format("adc started! solar pin ~p / bat pin ~p~n", [SolarPin, BatPin]),

	timer:sleep(250),

	{ok, StatePid} = start_state_server(),
	io:format("state server started ~p~n", [StatePid]),

	timer:sleep(250),

	%ok = start_network(Self),
	%io:format("network started~n"),
	%timer:sleep(250),

	ok = start_tcp_server(StatePid),
	io:format("TCP server started!~n"),

	adc_loop(StatePid, BatADC, SolarADC).

%
% STATE
%
start_state_server() ->
	Pid = spawn(fun() -> state_loop(0.0, 0.0) end),
	{ok, Pid}.

state_loop(BatV, SolarV) ->
	io:format("state_loop(~p,~p)~n", [BatV, SolarV]),
	receive 
		{get, Sender} ->
			Sender ! {solar_state, BatV, SolarV},
			state_loop(BatV, SolarV);
		{bat, NewVal} ->
			state_loop(NewVal, SolarV);
		{solar, NewVal} ->
			state_loop(BatV, NewVal)
		after 5000 ->
			io:format("state_loop(~p, ~p)~n", [BatV, SolarV]),
			state_loop(BatV, SolarV)
	end.

%
% SENSOR
% 
read_adc(Pin, StatePid, Type) ->
	case adc:read(Pin) of
		{ok, {Raw, MilliVolts}} ->
				io:format("read_adc(~p) raw: ~p, volts: ~pmV~n", [Type, Raw, MilliVolts]),
				StatePid ! {Type, MilliVolts};
		Error ->
				io:format("Error taking battery reading: ~p~n", [Error])
	end.

adc_loop(StatePid, BatADC, SolarADC) ->
	io:format("adc_loop~n"),
	read_adc(BatADC, StatePid, bat),
	read_adc(SolarADC, StatePid, solar),
	timer:sleep(1000),
	adc_loop(StatePid, BatADC, SolarADC).

%
% NETWORK PHY
%
start_network(_Self) ->
	io:format("network starting..~n"),
	Config = [
		{sta, [
			{ssid, esp:nvs_get_binary(atomvm, sta_ssid, ?SSID)}, % XXX config.erl
			{psk,  esp:nvs_get_binary(atomvm, sta_psk, ?PSK)}
			%{connected, fun() -> Self ! connected end},
			%{got_ip, fun(IpInfo) -> io:format("network got IP ~p", [IpInfo]) end},
			%{disconnected, fun() -> Self ! disconnected end}
		]}
	],
	io:format("network config: ~p~n", [Config]),
	case network_fsm:start(Config) of
		ok ->
			io:format("network started~n"),
			ok;
		Error ->
			erlang:display(Error)
	end.

%
% TCP API
% 
start_tcp_server(StatePid) ->
	erlang:display("starting tcp server~n"),
	timer:sleep(250),
	case gen_tcp:listen(4444, []) of
		{ok, ListenSocket} ->
				io:format("Listening on ~p.~n", [local_address(ListenSocket)]),
				spawn(fun() -> accept(StatePid, ListenSocket) end),
				ok;
		Error ->
				io:format("An error occurred listening: ~p~n", [Error])
	end.

accept(StatePid, ListenSocket) ->
	io:format("Waiting to accept connection...~n"),
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			io:format("Accepted connection.  local: ~p peer: ~p~n", [local_address(Socket), peer_address(Socket)]),
			spawn(fun() -> accept(StatePid, ListenSocket) end),
			send_tcp_response(StatePid, Socket);
		Error ->
			io:format("An error occurred accepting connection: ~p~n", [Error])
	end.

send_tcp_response(StatePid, Socket) ->
	io:format("Waiting to receive data...~n"),
	StatePid ! get,
	receive
		{solar_state, BatV, SolarV} ->
			Msg = io_lib:format("bat:~.2f; solar:~.2f", [BatV, SolarV]),
			gen_tcp:send(Socket, Msg),
			send_tcp_response(StatePid, Socket);
		{tcp_closed, _Socket} ->
			io:format("Connection closed.~n"),
			ok;
		{tcp, Socket, Packet} ->
			io:format("Received packet ~p from ~p.  Echoing back...~n", [Packet, peer_address(Socket)]),
			gen_tcp:send(Socket, Packet),
			send_tcp_response(StatePid, Socket)
	end.

local_address(Socket) ->
	{ok, SockName} = inet:sockname(Socket),
	to_string(SockName).

peer_address(Socket) ->
	{ok, Peername} = inet:peername(Socket),
	to_string(Peername).

to_string({{A,B,C,D}, Port}) ->
	io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]).

