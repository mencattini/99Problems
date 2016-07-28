-module(interaction).
-export([start/2,noeud/1,ring/1,start_ring/3]).


%% part 1
%% Write a function which starts 2 processes, and sends a message M times forewards and backwards between them.
%% After the messages have been sent the processes should terminate gracefully.
start(M,Message) ->
	Pid1 = spawn(?MODULE,noeud,[M]),
	Pid2 = spawn(?MODULE,noeud,[M]),
	Pid1 ! {Pid2,Message}.

noeud(M) ->
	if
		M > 0 ->
			receive
				{From,Message} ->
					From ! {self(),Message},
					io:format("~s~n",[Message]),
					noeud(M-1)
			end;
		M =< 0 ->
			exit(self(),normal)		
	end.


%% part 2
%% Write a function which starts N processes in a ring, and sends a message M times around all the processes in the ring. 
%% After the messages have been sent the processes should terminate gracefully.
%%

start_ring(M,N,Message) ->
	Liste_proc = create_list_proc(M,N),
	Pid1 = lists:nth(1,Liste_proc),
	Pid1 ! {self(),[Message,1,Liste_proc]}.

create_list_proc(_,0) ->
	[];
create_list_proc(M,N) ->
	lists:append([spawn(?MODULE,ring,[M])],
	create_list_proc(M,N-1)).

ring(M) ->
	if
		M > 0 ->
			receive
				{_From,[Message,Pos,Liste]} ->
					io:format("I'm ~p : ~s~n",[self(),Message]),
					if
						Pos + 1 =< length(Liste) ->
							Next = lists:nth(Pos + 1,Liste),
							Next ! {self(),[Message,Pos+1,Liste]},
							ring(M-1);
						Pos + 1 > length(Liste) ->
							Next = lists:nth(1,Liste),
							Next ! {self(),[Message,1,Liste]},
							ring(M-1)
					end
			end;
		M =< 0 ->
			exit(self(),normal)
	end.
