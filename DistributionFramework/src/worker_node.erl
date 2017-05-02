%%%-------------------------------------------------------------------
%%% @author Chris
%%% @copyright (C) 2017, <MUNRO, CHRISTOPHER>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2017 13:06
%%%-------------------------------------------------------------------
-module(worker_node).
-author("Chris").

%% API
-compile(export_all).

-define(Port, 4850).

start()->
	
	NodePid = spawn(worker_node, message_router, []),

	TCPPid = spawn_link(fun() ->
			{ok, LSocket} = gen_tcp:listen(?Port, [binary, {active, false}, {reuseaddr, true}]),
			accept_connection(LSocket, dict:new(), NodePid)
			end),

	%io:format("ip ~p~n", inet:getif()),
	{ok, TCPPid}.
	
accept_connection(LSocket, ListOfWorkers, NodePid)->
	
	io:format("waiting to accept ~n"),
	{ok, ASocket} = gen_tcp:accept(LSocket),
	io:format("Client Accepted ~p~n", [ASocket]),

	WorkerID = dict:size(ListOfWorkers),
	NewDict = dict:store(WorkerID, ASocket, ListOfWorkers),

	io:format("Worker ID ~p~n", [WorkerID]),
	spawn(fun()-> handler(ASocket, NewDict) end),
	accept_connection(LSocket, NewDict, NodePid).

%Handles TCP messages
handler(ASocket, ListOfWorkers)->
	
	io:format("Handler ~p~n", [dict:size(ListOfWorkers)]).

%Handles Node Messages
message_router()->
	
	io:format("Message Router ~n"),
	receive

		{reg_accpeted}->
			io:format("Reg Accpted ~n");

		{reg_failed, Reason}->
			io:format("Reg Failed: ~p~n", [Reason]);

		_->
			io:format("Unkown Message ~n")

	end,
	message_router().

register_to_slave(SlaveNodeID, SlaveNodeName, RegID)->
	
	{SlaveNodeID, SlaveNodeName} ! {reg_worker, RegID, node(), self()}.
	


