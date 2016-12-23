-module(server_1).
-compile(export_all).

start(Name,Port) ->
	net_kernel:start([Name]),
	spawn(?MODULE,server,[Port]).

server(Port) ->
	{ok,LSock} = gen_tcp:listen(Port,  [{packet, 0},{active,false}]),

	PidPbalance = spawn(?MODULE, pbalance, [statistics(total_active_tasks),node()]),
	register(pbalance,PidPbalance),

	PidMatch = spawn(?MODULE, match_adm, [[],1]),
  register(matchadm,PidMatch),

	spawn(?MODULE, stat, [PidPbalance]),
	dispatcher(LSock,PidPbalance,1).

dispatcher(LSock,PidPbalance,Cont) ->
	{ok, CSock} = gen_tcp:accept(LSock),
	NameSocket = "socket"++integer_to_list(Cont),
	NameListener = "listener"++integer_to_list(Cont),
	PidListener = spawn(?MODULE, listener, [CSock]),
	register(list_to_atom(NameListener),PidListener),
  PidSocket = spawn(?MODULE, psocket, [CSock,PidPbalance,none,list_to_atom(NameSocket),true,none,none,list_to_atom(NameListener)]),
	register(list_to_atom(NameSocket),PidSocket),
	dispatcher(LSock,PidPbalance,Cont+1).

stat(PidPbalance) ->
	Total = statistics(total_active_tasks),
  PidPbalance!{stat,Total,node()},
	lists:map(fun(X) -> {pbalance,X}!{stat,Total,node()} end,nodes()),
  receive after 2000 -> ok end,
  stat(PidPbalance).

user(Name,Pid) ->
	case global:register_name(Name,self()) of
		yes -> Pid!register_ok,
		       receive
						 kill -> ok
					 end;
		no  -> Pid!register_wrong
	end.

pbalance(Total,Node) ->
	receive
		{stat,NewTotal,NewNode} -> if NewTotal =< Total -> pbalance(NewTotal,NewNode);
			                               true          -> pbalance(Total,Node)
															 end;
		{conect,Pid}  -> Pid!{spawn,Node},pbalance(Total,Node)
	end.

match_adm(Games,Cont) ->
	NameG = list_to_atom("match"++integer_to_list(Cont)),
	receive
		{new,UserName,NameSock,NodeSock,Listener,PidCom} -> Pid = spawn(?MODULE, match,[{UserName,Listener,NodeSock},{none,none,none},[]]),
	                                             register(NameG,Pid),
			  			 		    	                       PidCom!{created,NameG},
                                               lists:map(fun(X) -> {matchadm,X}!{refresh,[{UserName,NameG,node(),true}]} end,nodes()),
												                       match_adm(Games++[{UserName,NameG,node(),true}],Cont+1);
		{join,Game,UserName,NameSock,NodeSock,Listener,PidCom} -> {_,G,GameNode,_} = lists:nth(1,lists:filter(fun({N,_,_,_}) -> Game == N end,Games)),
	                                              {G,GameNode}!{join,Listener,NodeSock,UserName},
															  	              lists:map(fun(X) -> {matchadm,X}!{join_refresh,Game} end,nodes()),
																	              PidCom!{joined,G,GameNode},
												     		                match_adm(lists:map(fun(X) -> aux(X,Game) end,Games),Cont);
  	{ending,UserName}           ->  match_adm(lists:filter(fun({N,_,_,_}) -> UserName /= N end,Games),Cont);
		{spect,Game,UserName,NameSock,NodeSock,PidCom} -> {_,G,GameNode,_} = lists:nth(1,lists:filter(fun({N,_,_,_}) -> Game == N end,Games)),
		                                                  {G,GameNode}!{spect,UserName,NameSock,NodeSock},
																											PidCom!spect_ok;
		{refresh,NewGame}       -> match_adm(Games++NewGame,Cont);
		{join_refresh,Name}     -> match_adm(lists:map(fun(X) -> aux(X,Name) end,Games),Cont);
		{list,PidCom}           -> PidCom!{list,Games},match_adm(Games,Cont)
  end.

match(Player1,Player2,Spects) ->
	case Player2 of
		{none,none,none} -> receive
			                  {spect,SpectUser,SpectName,SpectNode}  -> {SpectName,SpectNode}!spect_ok, match(Player1,Player2,Spects++[{SpectUser,SpectName,SpectNode}]);
			                  {join,NameSock,NodeSock,UserName} -> game(Player1,{UserName,NameSock,NodeSock},true,"---------",Spects)
					              end
  end.

game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects) ->
	case Turn of
		true ->
		        receive
							{movement,X,Y,User} ->
                                 if
																 User == UserName1 ->
																 case lists:nth((3*(X-1)+Y),Board) of
								                 45 -> NewBoard = replace((3*(X-1)+Y),Board,"X"),
																       {NameSock1,NodeSock1}!{update,NewBoard},
																			 {NameSock2,NodeSock2}!{update,NewBoard},
																			 lists:map(fun({_,NameS,NodeS}) -> {NameS,NodeS}!{update,NewBoard} end,Spects),
																       game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},false,NewBoard,Spects);
																 _  -> {NameSock1,NodeSock1}!no_valid,game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects)
															  end;
																true -> {NameSock1,NodeSock1}!no_valid,game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects)
															  end;
							{spect,SpectUSer,SpectName,SpectNode} -> game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects++[{SpectUSer,SpectName,SpectNode}])
						end;
		false ->
		         receive
						 {movement,X,Y,User} ->
                                 if
																 User == UserName2 ->
																 case lists:nth((3*(X-1)+Y),Board) of
								                 45 -> NewBoard = replace((3*(X-1)+Y),Board,"O"),
																       {NameSock1,NodeSock1}!{update,NewBoard},
																			 {NameSock2,NodeSock2}!{update,NewBoard},
																			 lists:map(fun({_,NameS,NodeS}) -> {NameS,NodeS}!{update_view,NewBoard} end,Spects),
																       game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},true,NewBoard,Spects);
																 _  -> {NameSock2,NodeSock2}!no_valid,game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects)
															  end;
																true -> {NameSock2,NodeSock2}!no_valid,game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects)
															  end;
							{spect,SpectUSer,SpectName,SpectNode} -> game({UserName1,NameSock1,NodeSock1},{UserName2,NameSock2,NodeSock2},Turn,Board,Spects++[{SpectUSer,SpectName,SpectNode}])
						end
	end.

replace(N,List,Token) ->
	case N of
		1 -> Token++lists:nthtail(1,List);
		_ -> puto(lists:nth(1,List))++replace(N-1,lists:nthtail(1,List),Token)
	end.

puto(X) ->
	case X of
		45  -> "-";
		88  -> "X";
		79  -> "O"
	end.

aux(Game,Name) ->
	case Game of
    {N,Gi,Node,F} -> if Name == N -> {N,Gi,Node,false};
			                  true -> {N,Gi,Node,F}
										 end
  end.

psocket(CSock,PidPbalance,UserName,Name,Flag,MatchName,MatchNode,Listener) ->
	case gen_tcp:recv(CSock,0) of
		{ok, Binary} ->
      PidPbalance!{conect,self()},
			receive
			  {spawn,Node} -> spawn(Node,server_1,pcommand,[Binary,Name,UserName,node(),Flag,MatchName,MatchNode,Listener])
			end,
			receive
				{log, M,Usr} -> gen_tcp:send(CSock,M), psocket(CSock,PidPbalance,Usr,Name,false,MatchName,MatchNode,Listener);
				{log_out, _} -> exit(log_out);
				{match,MName,MNode} -> gen_tcp:send(CSock,"ANS creada la partida"),psocket(CSock,PidPbalance,UserName,Name,Flag,MName,MNode,Listener);
				{joined,MName,MNode} -> gen_tcp:send(CSock,"ANS Te uniste pah"),psocket(CSock,PidPbalance,UserName,Name,Flag,MName,MNode,Listener);
        well_delivered -> psocket(CSock,PidPbalance,UserName,Name,Flag,MatchName,MatchNode,Listener);
        {answer, M} -> gen_tcp:send(CSock,M)
			end,
			psocket(CSock,PidPbalance,UserName,Name,Flag,MatchName,MatchNode,Listener);
		{error,closed} ->
			error
	end.

listener(CSock) ->
	receive
		{update,Board} -> gen_tcp:send(CSock,"UPD "++Board);
		turn -> gen_tcp:send(CSock,"ANS Tu turno");
		no_valid -> gen_tcp:send(CSock,"ANS Te confudiste brother")
	end,
  listener(CSock).

match_print(G) ->
	case G of
		{N,_,Node,true} -> io_lib:format("La partida de ~p busca contricante en este nodo ~p ",[N,Node]);
		{N,_,Node,false} -> io_lib:format("La partida de ~p esta en curso ~p ",[N,Node])
	end.

pcommand(Binary,Name,UserName,Node,Flag,MatchName,MatchNode,Listener) ->
	Command = lists:map(fun(X) -> list_to_atom(X) end,string:tokens(Binary," \n")),
	case lists:nth(1,Command) of
		'CON' -> case Flag of
			       true -> spawn(?MODULE,user,[lists:nth(2,Command),self()]),
		                 receive
						           register_ok -> {Name,Node}!{log,"Lo lograste",lists:nth(2,Command)};
							         register_wrong -> {Name,Node}!{answer,"so pecher"}
						         end;
						 false -> {Name,Node}!{answer,"Cuantas veces te vas a conectar!? la concha de tu madre lucio"}
					   end;
		  M   -> case Flag of
				       true  -> {Name,Node}!{answer,"no te registraste"};
						   false -> case M of
								          'BYE' -> global:send(UserName,kill),{Name,Node}!{log_out,"ni no"};
													'NEW' -> case {MatchName,MatchNode} of
														       {none,none} -> whereis(matchadm)!{new,UserName,Name,Node,Listener,self()},
																	                io:format("~p~n",[node()]),
													                        receive
																		                {created,MName} -> {Name,Node}!{match,MName,node()}
																								  end;
																			 _       -> {Name,Node}!{answer,"Cuantas partidas vas a crear?"}
																	 end;
													'LSG' -> whereis(matchadm)!{list,self()},
													         receive
																		 {list,Games} -> {Name,Node}!{answer,lists:flatten(lists:map(fun(X) -> match_print(X) end,Games))}
																	 end;
													'ACC' -> case {MatchName,MatchNode} of
													         {none,none} -> whereis(matchadm)!{join,lists:nth(2,Command),UserName,Name,Node,Listener,self()},
													                        receive
																										{joined,MName,MNode} -> {Name,Node}!{joined,MName,MNode}
																									end;
																			 _       -> {Name,Node}!{answer,"Ya estas jugando pah"}
																	 end;
													'PLA' -> {MatchName,MatchNode}!{movement,list_to_integer(atom_to_list((lists:nth(2,Command)))),list_to_integer(atom_to_list((lists:nth(3,Command)))),UserName},
													         {Name,Node}!well_delivered;
													  _   -> io:format("~p~n",[M]),{Name,Node}!{answer,"para la moto pah"}
												end
					   end
	end.

% io:format("ERROR no implementado~n").
% como usar bien statistics
% spawnear un solo pcomand o varios
% como gestionar las partidas
% registro usuario
