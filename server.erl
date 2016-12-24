-module(server).
-compile(export_all).

% Start facilita la creacion de los servers

start(Name,Port) ->
    net_kernel:start([Name]),
    spawn(?MODULE,server,[Port]).

% Server se encarga de iniciar los procesos necesarios para el correcto funcionamiento del servidor

server(Port) ->
  	{ok,LSock} = gen_tcp:listen(Port,  [{packet, 0},{active,false}]),

  	PidPbalance = spawn(?MODULE, pbalance, [statistics(total_active_tasks),node()]),
  	register(pbalance,PidPbalance),

  	PidMatch = spawn(?MODULE, match_adm, [[],1]),
    register(matchadm,PidMatch),

  	spawn(?MODULE, stat, [PidPbalance]),
  	dispatcher(LSock,PidPbalance,1).

% Dispacher se encarga de aceptar la conexiones que llegan
% y spawnear un proceso Socket para establcer la comunicacion con el cliente

dispatcher(LSock,PidPbalance,Cont) ->
	{ok, CSock} = gen_tcp:accept(LSock),

	NameSocket = list_to_atom("socket"++integer_to_list(Cont)),
	NameListener = list_to_atom("listener"++integer_to_list(Cont)),

	PidListener = spawn(?MODULE, listener, [CSock]),
	register(NameListener,PidListener),

    PidSocket = spawn(?MODULE, psocket, [CSock,PidPbalance,none,NameSocket,true,NameListener]),
	register(NameSocket,PidSocket),

    dispatcher(LSock,PidPbalance,Cont+1).

% Pbalance se encarga de indicar que nodo es el menos cargado

pbalance(Total,Node) ->
	receive
		{stat,NewTotal,NewNode} -> if NewTotal =< Total -> pbalance(NewTotal,NewNode);
                                                   true -> pbalance(Total,Node)
                                   end;
		{conect,Pid}  -> Pid!{spawn,Node},
                         pbalance(Total,Node)
	end.

% Stat se encarga de informar a todos los nodos conectados el estado del servidor

stat(PidPbalance) ->
    Total = statistics(total_active_tasks),

    PidPbalance!{stat,Total,node()},
    lists:map(fun(X) -> {pbalance,X}!{stat,Total,node()} end,nodes()),

    receive after 2000 -> ok end,

    stat(PidPbalance).

% Psocket se encarga de recibir los comandos enviados por el cliente
% y spawnea pocommand en el nodo menos cargado

psocket(CSock,PidPbalance,UserName,Name,Flag,Listener) ->
	case gen_tcp:recv(CSock,0) of
        {ok, Binary} ->
            PidPbalance!{conect,self()},

			receive
                {spawn,Node} -> spawn(Node,server,pcommand,[Binary,Name,UserName,node(),Flag,Listener])
			end,

			receive
				{log,Usr} -> gen_tcp:send(CSock,"LOG "++atom_to_list(Usr)),
                             psocket(CSock,PidPbalance,Usr,Name,false,Listener);
                log_fail  -> gen_tcp:send(CSock,"LOG_FAIL");
                already_log -> gen_tcp:send(CSock,"ALREADY_LOG "++atom_to_list(UserName));
                    not_log -> gen_tcp:send(CSock,"NOT_LOG");
				    log_out ->  gen_tcp:send(CSock,"EXIT"),
                                exit(log_out);
				match_created -> gen_tcp:send(CSock,"GAME");
                already_playing -> gen_tcp:send(CSock,"ALREADY_PLAYING");
				joined -> gen_tcp:send(CSock,"JOIN");
                well_delivered -> gen_tcp:send(CSock,"WELL_DELIVERED");
                not_start -> gen_tcp:send(CSock,"NOT_START");
                spect_ok  -> gen_tcp:send(CSock,"SPECT_OK");
                {games,ListGames} -> gen_tcp:send(CSock,"GAMES "++ListGames);
                no_valid_game -> gen_tcp:send(CSock,"NO_VALID_GAME");
                incorrect -> gen_tcp:send(CSock,"INCORRECT")
			end;
        {error,closed} ->   error
	end,
    psocket(CSock,PidPbalance,UserName,Name,Flag,Listener).

% Listener se encarga de redirigir mensajes al cliente cuyo origen no es la respuesta de un comando

listener(CSock) ->
	receive
		{update,Board} -> gen_tcp:send(CSock,"UPD "++Board);
		turn -> gen_tcp:send(CSock,"TURN");
		no_valid -> gen_tcp:send(CSock,"NO_VALID")
	end,
  listener(CSock).

% User se encarga de garantizar que los nombres de los usuarios sean unicos

user(Listener,ListenerNode) ->
    receive
        kill -> ok;
         M   -> {Listener,ListenerNode}!M
    end,
    user(Listener,ListenerNode).

% Pcommand se encarga de interpretar los comandos de los usuarios y realizar las acciones correspondientes

pcommand(Binary,Name,UserName,Node,Flag,Listener) ->
    Command = lists:map(fun(X) -> list_to_atom(X) end,string:tokens(Binary," \n")),
    case lists:nth(1,Command) of
        'CON' -> case Flag of
                    true -> NewUserName = lists:nth(2,Command),
                            PidUser = spawn(?MODULE,user,[Listener,Node]),
                            case global:register_name(NewUserName,PidUser) of
                                yes -> {Name,Node}!{log,NewUserName};
                                no  -> PidUser!kill,
                                       {Name,Node}!log_fail
                            end;
    				false -> {Name,Node}!already_log
    			end;
         M   -> case Flag of
                    true  -> {Name,Node}!not_log;
    				false -> case M of
				                'BYE' -> global:send(UserName,kill),{Name,Node}!log_out;
    							'NEW' -> whereis(matchadm)!{new,UserName,self()},
    									 receive
    									    created -> {Name,Node}!match_created;
                                            already_playing -> {Name,Node}!already_playing
    									 end;
    							'LSG' -> whereis(matchadm)!{list,self()},
    									 receive
    									    {list,Games} -> {Name,Node}!{games,lists:flatten(lists:map(fun(X) -> match_print(X) end,Games))}
    									 end;
    							'ACC' -> whereis(matchadm)!{join,lists:nth(2,Command),UserName,self()},
    									 receive
    									    joined -> {Name,Node}!joined;
                                            no_valid -> {Name,Node}!no_valid_game;
                                            already_playing -> {Name,Node}!already_playing
    									 end;
    							'PLA' -> whereis(matchadm)!{movement,lists:nth(2,Command),atom_to_integer(lists:nth(3,Command)),atom_to_integer(lists:nth(4,Command)),UserName,self()},
    							         receive
                                             well_delivered -> {Name,Node}!well_delivered;
                                             no_valid       -> {Name,Node}!no_valid_game;
                                             not_start      -> {Name,Node}!not_start
                                         end;
                                'OBS' -> whereis(matchadm)!{spect,lists:nth(2,Command),UserName,self()},
                                         receive
                                             no_valid       -> {Name,Node}!no_valid_game;
                                             spect_ok      -> {Name,Node}!spect_ok
                                         end;
    							  _   -> {Name,Node}!incorrect
    					    end
    			end
    	end.

% Match_adm se encarga de administrar las partidas
% crear las nuevas salas, unir a los jugadores y recibir las jugadas

match_adm(Games,Cont) ->
	NameG = list_to_atom("match"++integer_to_list(Cont)),
	receive
		{new,UserName,PidCom} -> case is_in(UserName,Games) of
                                     true -> PidCom!already_playing;
                                     false -> NewGame = [{UserName,none,NameG,node()}],
                                              PidGame = spawn(?MODULE, match,[UserName,none,[]]),
	                                          register(NameG,PidGame),
                 		    	              PidCom!created,
                                              lists:map(fun(X) -> {matchadm,X}!{refresh,NewGame} end,nodes()),
								              match_adm(Games++NewGame,Cont+1)
                                 end;
		{join,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({P1,_,_,_}) -> Game == P1 end,Games),
                                       case PosibleGame of
                                           [] -> PidCom!no_valid;
                                            _ -> {_,P2,GameName,GameNode} = lists:nth(1,PosibleGame),
                                                 case P2 of
                                                     none -> {GameName,GameNode}!{join,UserName},
													         lists:map(fun(X) -> {matchadm,X}!{join_refresh,UserName,Game} end,nodes()),
													         PidCom!joined,
											     		     match_adm(lists:map(fun(X) -> actualization(X,Game,UserName) end,Games),Cont);
                                                      _   -> PidCom!no_valid
                                                 end
                                        end;
  	    {ending,UserName} -> ok;
		{spect,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({P1,_,_,_}) -> Game == P1 end,Games),
		                                case PosibleGame of
                                            [] -> PidCom!no_valid;
                                            _  -> {_,_,GameName,GameNode} = lists:nth(1,PosibleGame),
                                                  {GameName,GameNode}!{spect,UserName},
									              PidCom!spect_ok
                                        end;
		{refresh,NewGame} -> match_adm(Games++NewGame,Cont);
		{join_refresh,Game,UserName} -> match_adm(lists:map(fun(X) -> actualization(X,Game,UserName) end,Games),Cont);
        {movement,Game,X,Y,UserName,PidCom} -> PosibleGame = lists:filter(fun({P1,_,_,_}) -> Game == P1 end,Games),
		                                       case PosibleGame of
                                                   [] -> PidCom!no_valid;
                                                   _  -> {_,P2,GameName,GameNode} = lists:nth(1,PosibleGame),
                                                         case P2 of
                                                            none -> PidCom!not_start;
                                                            _ -> {GameName,GameNode}!{movement,X,Y,UserName},
                                                                 PidCom!well_delivered
                                                         end
                                                end;
		{list,PidCom} -> PidCom!{list,Games},match_adm(Games,Cont)
  end,
  match_adm(Games,Cont).

match(Player1,Player2,Spects) ->
	case Player2 of
		none -> receive
		          {spect,SpectUser}  -> match(Player1,Player2,Spects++[SpectUser]);
	              {join,UserName} -> game(Player1,UserName,true,"---------",Spects)
			    end
  end.

game(Player1,Player2,Turn,Board,Spects) ->
	case Turn of
		true -> global:send(Player1,turn),
                receive
				{movement,X,Y,User} -> if
				                        User == Player1 -> case lists:nth((3*(X-1)+Y),Board) of
								                                45 -> NewBoard = replace((3*(X-1)+Y),Board,"X"),
																      global:send(Player1,{update,NewBoard}),
																	  global:send(Player2,{update,NewBoard}),
																	  lists:map(fun(S) -> global:send(S,{update,NewBoard}) end,Spects),
																      game(Player1,Player2,false,NewBoard,Spects);
																 _  -> global:send(Player1,no_valid),
                                                                       game(Player1,Player2,Turn,Board,Spects)
										                   end;
											true -> global:send(User,no_valid),
                                                    game(Player1,Player2,Turn,Board,Spects)
                                        end;
				{spect,SpectUSer} -> game(Player1,Player2,Turn,Board,Spects++[SpectUSer])
				end;
		false -> global:send(Player2,turn),
                receive
				{movement,X,Y,User} -> if
				                        User == Player2 -> case lists:nth((3*(X-1)+Y),Board) of
								                                45 -> NewBoard = replace((3*(X-1)+Y),Board,"O"),
																      global:send(Player1,{update,NewBoard}),
																	  global:send(Player2,{update,NewBoard}),
																	  lists:map(fun(S) -> global:send(S,{update,NewBoard}) end,Spects),
																      game(Player1,Player2,true,NewBoard,Spects);
																 _  -> global:send(Player1,no_valid),
                                                                       game(Player1,Player2,Turn,Board,Spects)
										                   end;
											true -> global:send(User,no_valid),
                                                    game(Player1,Player2,Turn,Board,Spects)
                                        end;
				{spect,SpectUSer} -> game(Player1,Player2,Turn,Board,Spects++[SpectUSer])
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

actualization(Game,GameName,UserName) ->
	case Game of
    {P1,P2,G,N} -> if P1 == GameName -> {P1,UserName,G,N};
		                 true -> {P1,P2,G,N}
		           end
  end.

% Atom_to_integer transforma un atomo en un entero

atom_to_integer(Num) ->
  list_to_integer(atom_to_list(Num)).

% Match_print se encarga de crear un string con todas las partidas en curso en todos los servidores

match_print(G) ->
	case G of
		{P1,none,_,_} -> io_lib:format("La partida de ~p busca contricante ~n",[P1]);
		{P1,P2,_,_} -> io_lib:format("La partida de ~p esta en curso. ~p VS ~p ~n",[P1,P1,P2])
	end.

% Is_in verifica si un usuario ya tiene una partida activa

is_in(UserName,Games) ->
    length(lists:filter(fun({P1,P2,_,_}) -> (P1 == UserName) or (P2 == UserName) end,Games)) == 1.
