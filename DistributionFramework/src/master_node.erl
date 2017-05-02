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

start()->
	
	%% Register the node
  Pid = spawn(master_node, message_router, [dict:new()]),
  register(masterNode, Pid).

message_router(ListOfSlaveNodes) ->
  io:format("Started Message Router ~n"),
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

		_ ->
			io:format("Unkown Message~n")
			

  end,
	message_router(ListOfSlaveNodes).


send_file(FilePath, DestinationNode)->

  %% TODO: Move this connect to where the nodes will register
  net_kernel:connect(DestinationNode),
  %% Read the file
  {ok, Bin} = file:read_file(FilePath),
  %% Write the file to the remote destination node
  rpc:call(DestinationNode, file, write_file, ["SceneFile.xml", Bin]),

  io:format("File Sent ~n").
