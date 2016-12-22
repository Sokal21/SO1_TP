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
	Name = "socket"++integer_to_list(Cont),
  Pid = spawn(?MODULE, psocket, [CSock,PidPbalance,none,list_to_atom(Name),true,none,none]),
	register(list_to_atom(Name),Pid),
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
		{new,Name,PidCom} ->Pid = spawn(?MODULE, match,[{Name,none}]),
	                      register(NameG,Pid),
			  			 		    	PidCom!{created,NameG},
                        lists:map(fun(X) -> {matchadm,X}!{refresh,[{Name,NameG,node(),true}]}end,nodes()),
												match_adm(Games++[{Name,NameG,node(),true}],Cont+1);
		{join,Name,Player,Node} ->  {_,G,_,GameNode} = lists:nth(1,lists:filter(fun({N,_,_,_}) -> Name == N end,Games)),
	                              {G,GameNode}!{join,Player},
												     		match_adm(lists:map(fun(X) -> aux(X,Name) end,Games),Cont);
  	{ending,Name}           ->  match_adm(lists:filter(fun({N,_,_,_}) -> Name /= N end,Games),Cont);
		{refresh,NewGame}       -> match_adm(Games++NewGame,Cont);
		{list,PidCom}        -> PidCom!{list,Games},match_adm(Games,Cont)
  end.

aux(Game,Name) ->
	case Game of
    {N,Gi,Node,F} -> if Name == N -> {N,Gi,Node,false};
			                  true -> {N,Gi,Node,F}
										 end
  end.

match(Players) ->
	case Players of
		{P,none} -> receive
			               {join,Name} -> match({P,Name})
					      end;
		_ ->  match(Players)
  end.

psocket(CSock,PidPbalance,UserName,Name,Flag,MatchName,MatchNode) ->
	case gen_tcp:recv(CSock,0) of
		{ok, Binary} ->
      PidPbalance!{conect,self()},
			receive
			  {spawn,Node} -> spawn(Node,server_1,pcommand,[Binary,Name,UserName,node(),Flag,MatchName,MatchNode])
			end,
			receive
				{log, M,Usr} -> gen_tcp:send(CSock,M), psocket(CSock,PidPbalance,Usr,Name,false,MatchName,MatchNode);
				{log_out, _} -> exit(log_out);
				{match,MName,MNode} -> gen_tcp:send(CSock,"creada la partida"),psocket(CSock,PidPbalance,UserName,Name,Flag,MName,MNode);
        {answer, M} -> gen_tcp:send(CSock,M)
			end,
			psocket(CSock,PidPbalance,UserName,Name,Flag,MatchName,MatchNode);
		{error,closed} ->
			error
	end.

match_print(G) ->
	case G of
		{N,_,Node,true} -> io_lib:format("La partida de ~p busca contricante en este nodo ~p ",[N,Node]);
		{N,_,Node,false} -> io_lib:format("La partida de ~p esta en curso ~p ",[N,Node])
	end.

pcommand(Binary,Name,UserName,Node,Flag,MatchName,MatchNode) ->
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
														       {none,none} -> whereis(matchadm)!{new,UserName,self()},
																	                io:format("~p~n",[node()]),
													                        receive
																		                {created,MName} -> io:format("recibi respuesta"),{Name,Node}!{match,MName,node()}
																								  end;
																			 _       -> {Name,Node}!{answer,"Cuantas partidas vas a crear?"}
																	 end;
													'LSG' -> whereis(matchadm)!{list,self()},
													         receive
																		 {list,Games} -> {Name,Node}!{answer,lists:flatten(lists:map(fun(X) -> match_print(X) end,Games))}
																	 end;
													'ACC' -> whereis(matchadm)!{join,lists:nth(2,Command),UserName,node()},
													         {Name,Node}!{answer,"te uniste pah"};
													  _   -> io:format("~p~n",[M]),{Name,Node}!{answer,"para la moto pah"}
												end
					   end
	end.

% io:format("ERROR no implementado~n").
% como usar bien statistics
% spawnear un solo pcomand o varios
% como gestionar las partidas
% registro usuario
