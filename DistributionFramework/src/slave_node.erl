%%%-------------------------------------------------------------------
%%% @author Chris
%%% @copyright (C) 2017, <MUNRO, CHRISTOPHER>
%%% @doc
%%%
%%% @end
%%% Created : 04. Mar 2017 17:12
%%%-------------------------------------------------------------------
-module(slave_node).
-author("Chris").

%% API
-compile(export_all).

%Start the node by making a connection and registration request to the masternode
start(MasterNodeName, SlaveRegID)->
	
	SlaveNodeName = node(),
	Pid = spawn(slave_node, message_router, [dict:new(), MasterNodeName]),
	{masterNode, MasterNodeName} ! {reg_slave, SlaveRegID, SlaveNodeName, Pid}.

message_router(ListOfWorkerNodes, MasterNodeName)->
	  io:format("Started Message Router (slave) ~n"),
		io:format("SELF: ~p~n", [self()]),

		receive

			{reg_accepted, SlaveRegID}->
				io:format("Reg Accepted ~n"),
				register(SlaveRegID, self());

			{reg_failed, Reason}->
				io:format("Reg Failed ~n Reason: ~p~n", [Reason]);

			{reg_worker, WorkerRegID, WorkerNodeName, From}->
				
				io:format("Got Reg Worker ~n"),
				case dict:find(WorkerRegID, ListOfWorkerNodes) of
					
					{ok, _}->
						%Cannot Reg as WokerRegID is already in use
						Reason = atom_to_list(WorkerRegID) ++ " is Already registered",
						From ! {reg_failed, Reason};
				
					error->
						%Carry out registration
						%% Add the Node to the Dictionary
      					NewDict = dict:store(WorkerRegID, WorkerNodeName, ListOfWorkerNodes),
        				io:format("ListOfWorkerNodes: ~p ~n", [dict:fetch_keys(NewDict)]),

						%Notify Slave node
						From ! {reg_accepted},
        				message_router(NewDict, MasterNodeName)					


				end;

			_ ->
				io:format("Unkown Message~n")

		end,
		message_router(ListOfWorkerNodes, MasterNodeName).
