-module(server_1).
-compile(export_all).

start(Name,Port) ->
	net_kernel:start([Name]),
	spawn(?MODULE,server,[Port]).

server(Port) ->
	{ok,LSock} = gen_tcp:listen(Port,  [{packet, 0},{active,false}]),

	PidPbalance = spawn(?MODULE, pbalance, [statistics(total_active_tasks),node()]),
	register(pbalance,PidPbalance),

%	PidMatch = spawn(?MODULE, match_adm, [[],1]),
%	register(match_adm,PidMatch),

	spawn(?MODULE, stat, [PidPbalance]),
	dispatcher(LSock,PidPbalance,1).

dispatcher(LSock,PidPbalance,Cont) ->
	{ok, CSock} = gen_tcp:accept(LSock),
	Name = "socket"++integer_to_list(Cont),
  Pid = spawn(?MODULE, psocket, [CSock,PidPbalance,none,list_to_atom(Name),true]),
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
		{stat,NewTotal,NewNode} -> io:format("llego el stat de ~p estoy ~p total: ~p newtotal: ~p ~n",[NewNode,Node,Total,NewTotal]),
		                           if NewTotal =< Total -> pbalance(NewTotal,NewNode);
			                               true          -> pbalance(Total,Node)
															 end;
		{conect,Pid}  -> Pid!{spawn,Node},pbalance(Total,Node)
	end.

psocket(CSock,PidPbalance,UserName,Name,Flag) ->
	case gen_tcp:recv(CSock,0) of
		{ok, Binary} ->
      PidPbalance!{conect,self()},
			receive
			  {spawn,Node} -> spawn(Node,server_1,pcommand,[Binary,Name,UserName,node(),Flag])
			end,
			receive
				{log, M,Usr} -> gen_tcp:send(CSock,M), psocket(CSock,PidPbalance,Usr,Name,false);
				{log_out, M} -> exit(log_out);
        {answer, M} -> gen_tcp:send(CSock,M)
			end,
			psocket(CSock,PidPbalance,UserName,Name,Flag);
		{error,closed} ->
			error
	end.

pcommand(Binary,Name,UserName,Node,Flag) ->
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
													  _   -> io:format("~p~n",[M]),{Name,Node}!{answer,"para la moto pah"}
												end
					   end
	end.

% io:format("ERROR no implementado~n").
% como usar bien statistics
% spawnear un solo pcomand o varios
% como gestionar las partidas
% registro usuario
