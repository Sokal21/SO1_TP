-module(server_1).
-compile(export_all).

start(Name,Port) ->
	net_kernel:start([Name]),
	spawn(?MODULE,server,[Port]).

server(Port) ->
	{ok,LSock} = gen_tcp:listen(Port, [binary, {packet, 0},{active,false}]),

	PidPbalance = 	spawn(?MODULE, pbalance, [statistics(total_active_tasks),node()]),
	register(pbalance,PidPbalance),

	spawn(?MODULE, stat, [PidPbalance]),
	dispatcher(LSock,PidPbalance,1).

dispatcher(LSock,PidPbalance,Cont) ->
	{ok, CSock} = gen_tcp:accept(LSock),
	Name = "socket"++integer_to_list(Cont),
  Pid = spawn(?MODULE, psocket, [CSock,PidPbalance,Name]),
	register(list_to_atom(Name),Pid),
	dispatcher(LSock,PidPbalance,Cont+1).

stat(PidPbalance) ->
	Total = statistics(total_active_tasks),
  PidPbalance!{stat,Total,node()},
	lists:map(fun(X) -> {pbalance,X}!{stat,Total,node()} end,nodes()),
  receive after 2000 -> ok end,
  stat(PidPbalance).

pbalance(Total,Node) ->
	receive
		{stat,NewTotal,NewNode} -> io:format("llego el stat de ~p estoy ~p total: ~p newtotal: ~p ~n",[NewNode,Node,Total,NewTotal]),
		                           if NewTotal =< Total -> pbalance(NewTotal,NewNode);
			                               true          -> pbalance(Total,Node)
															 end;
		{conect,Pid}  -> Pid!{spawn,Node},pbalance(Total,Node)
	end.

psocket(CSock,PidPbalance,Name) ->
	case gen_tcp:recv(CSock,0) of
		{ok, Binary} ->
      PidPbalance!{conect,self()},
			receive
			  {spawn,Node} -> spawn(Node,server_1,pcommand,[Binary,Name,node()])
			end,
			receive
        {answer, M} -> gen_tcp:send(CSock,M)
			end,
			psocket(CSock,PidPbalance,Name);
		{error,closed} ->
			error
	end.

pcommand(Binary,Name,Node) ->
  {list_to_atom(Name),Node}!{answer,Binary}.

	%io:format("ERROR no implementado~n").
