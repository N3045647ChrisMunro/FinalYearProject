%%%-------------------------------------------------------------------
%%% @author Chris
%%% @copyright (C) 2017, <MUNRO, CHRISTOPHER>
%%% @doc
%%%
%%% @end
%%% Created : 04. Mar 2017 16:46
%%%-------------------------------------------------------------------
-module(master_node).
-author("Chris").

%% API
-compile(export_all).

-define(Port, 8001).

start()->
	
	%% Register the node
  Pid = spawn(master_node, message_router, [dict:new()]),
  register(masterNode, Pid),

	TCPPid = spawn_link(fun()-> {ok, LSocket} = gen_tcp:listen(?Port, [binary, {active, false}, {reuseaddr, 			true}]), accept_connection(LSocket, Pid) end), 

	{ok, TCPPid}.

accept_connection(LSocket, NodePid)->
	
	io:format("Listening for MainApp ~n"),
	{ok, ASocket} = gen_tcp:accept(LSocket),

	io:format("MainApp Accepted ~p~n", [ASocket]),
	%NewList = dict:store('mainApp', ASocket, ListOfMasters),

	spawn(fun()-> handler(ASocket, NodePid) end),
	accept_connection(LSocket, NodePid).
	

message_router(ListOfSlaveNodes) ->
 	io:format("Listening for node Messages ~n"),
	receive
    	{reg_slave, SlaveRegID, SlaveNodeName, From}->
        
			%Check to see if theres a node registered with the same node/info
			case dict:find(SlaveRegID, ListOfSlaveNodes) of
				
				{ok, _}->
					%Cannot Reg as SlaveRegID is already in use
					Reason = atom_to_list(SlaveRegID) ++ " is Already registered",
					From ! {reg_failed, Reason};
				
				error->
					%Carry out registration
					%% Add the Node to the Dictionary
      				NewDict = dict:store(SlaveRegID, SlaveNodeName, ListOfSlaveNodes),
        			io:format("ListOfSlaveNodes: ~p ~n", [dict:fetch_keys(NewDict)]),

					%Notify Slave node
					From ! {reg_accepted, SlaveRegID},
        			message_router(NewDict)

			end;

		{render, Filepath, Grade}->
			io:format("Filepath: ~p~n", [Filepath]),
			io:format("Grade: ~p~n", [Grade]);

		_ ->
			io:format("Unkown Message~n")
			

  	end,
	message_router(ListOfSlaveNodes).

%Handle TCP Messages from the 'MainApp'
handler(ASocket, NodePid)->
	io:format("Waiting for TCP Message ~n"),
	
	case gen_tcp:recv(ASocket, 0) of
		
		{ok, MessageBody}->
			[H | T] = binary:split(MessageBody, <<":">>, [global]),
			case H of
				<<"Render">>->
					io:format("Got Render Excute Message: ~n"),

					[Filepath, Grade] = T,
					NodePid ! {render, Filepath, Grade}
			end

	end,
	handler(ASocket, NodePid).

	

send_file(FilePath, DestinationNode)->

  %% TODO: Move this connect to where the nodes will register
  net_kernel:connect(DestinationNode),
  %% Read the file
  {ok, Bin} = file:read_file(FilePath),
  %% Write the file to the remote destination node
  rpc:call(DestinationNode, file, write_file, ["SceneFile.xml", Bin]),

  io:format("File Sent ~n").
