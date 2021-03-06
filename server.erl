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

  	PidMatch = spawn(?MODULE, match_adm, [[]]),
    register(matchadm,PidMatch),

  	spawn(?MODULE, stat, [PidPbalance]),
  	dispatcher(LSock,PidPbalance,1).

% Dispatcher se encarga de aceptar la conexiones que llegan
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
% y spawnea pcommand en el nodo menos cargado

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
                log_fail  -> gen_tcp:send(CSock,"LOG_FAIL ");
                already_log -> gen_tcp:send(CSock,"ALREADY_LOG "++atom_to_list(UserName));
                    not_log -> gen_tcp:send(CSock,"NOT_LOG ");
				    log_out ->  gen_tcp:send(CSock,"EXIT "),
                                exit(log_out);
				match_created -> gen_tcp:send(CSock,"GAME ");
                already_playing -> gen_tcp:send(CSock,"ALREADY_PLAYING ");
				joined -> gen_tcp:send(CSock,"JOIN ");
                well_delivered -> gen_tcp:send(CSock,"WELL_DELIVERED ");
                not_start -> gen_tcp:send(CSock,"NOT_START ");
                spect_ok  -> gen_tcp:send(CSock,"SPECT_OK ");
                {games,ListGames} -> gen_tcp:send(CSock,"GAMES "++ListGames);
                no_valid_game -> gen_tcp:send(CSock,"NO_VALID_GAME ");
                incorrect -> gen_tcp:send(CSock,"INCORRECT ");
                end_spect_ok ->  gen_tcp:send(CSock,"END_SPECT_OK ");
                no_valid_user -> gen_tcp:send(CSock,"NO_VALID_USER")
			end;
        {error,closed} ->   error
	end,
    psocket(CSock,PidPbalance,UserName,Name,Flag,Listener).

% Listener se encarga de redirigir mensajes al cliente cuyo origen no es la respuesta de un comando

listener(CSock) ->
	receive
        {victory, Player, Game} -> gen_tcp:send(CSock, "WIN "++atom_to_list(Player)++" "++atom_to_list(Game));
		{update,Board,Game} -> gen_tcp:send(CSock,"UPD "++Board++" "++atom_to_list(Game));
		turn -> gen_tcp:send(CSock,"TURN ");
		no_valid -> gen_tcp:send(CSock,"NO_VALID ");
        no_rights_game -> gen_tcp:send(CSock,"NO_RIGHTS_GAME ");
        you_end_game -> gen_tcp:send(CSock,"YOU_END_GAME ");
        {opponent_end_game,UserName} -> gen_tcp:send(CSock,"OPPONENT_END_GAME "++atom_to_list(UserName));
        {say,UserName,Msg} -> gen_tcp:send(CSock,"SAY "++atom_to_list(UserName)++" "++Msg);
        {end_spect,UserName,Game} -> gen_tcp:send(CSock,"END_SPECT "++atom_to_list(UserName)++" "++atom_to_list(Game))
	end,
  receive after 1000 -> ok end,
  listener(CSock).

% User se encarga de garantizar que los nombres de los usuarios sean unicos

user(Listener,ListenerNode) ->
    receive
        kill -> exit(log_out);
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
    							'NEW' -> whereis(matchadm)!{new,lists:nth(2,Command),UserName,self()},
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
    							'PLA' -> case lists:nth(3,Command) of
                                            'END' -> whereis(matchadm)!{ending,lists:nth(2,Command),UserName,self()};
                                             _    -> whereis(matchadm)!{movement,lists:nth(2,Command),atom_to_integer(lists:nth(3,Command)),atom_to_integer(lists:nth(4,Command)),UserName,self()}
                                         end,
						                 receive
                                            well_delivered -> {Name,Node}!well_delivered;
                                            no_valid       -> {Name,Node}!no_valid_game;
                                            not_start      -> {Name,Node}!not_start
                                        end;
                                'LEA' -> whereis(matchadm)!{end_spect,lists:nth(2,Command),UserName,self()},
                                         {Name,Node}!end_spect_ok;
                                'OBS' -> whereis(matchadm)!{spect,lists:nth(2,Command),UserName,self()},
                                         receive
                                             no_valid       -> {Name,Node}!no_valid_game;
                                             spect_ok      -> {Name,Node}!spect_ok
                                         end;
                                'SAY' ->  User = lists:nth(2,Command),
                                          Users = global:registered_names(),
                                          case length(lists:filter(fun(X) -> X == User end,Users)) of
                                              0 -> {Name,Node}!no_valid_user;
                                              _ -> global:send(User,{say,UserName,lists:subtract(Binary,"SAY "++atom_to_list(User))}),
                                                   {Name,Node}!well_delivered
                                          end;
    							  _   -> {Name,Node}!incorrect
    					    end
    			end
    	end.

% Match_adm se encarga de administrar las partidas
% crear las nuevas salas, unir a los jugadores y recibir las jugadas

match_adm(Games) ->
	receive
		{new,MatchName,UserName,PidCom} -> PidGame = spawn(?MODULE, match,[UserName,none,[],MatchName]),
                                           NewGame = [{MatchName,UserName,none}],
                                           case global:register_name(MatchName, PidGame) of
                                              yes -> PidCom!created,
                                                     lists:map(fun(X) -> {matchadm,X}!{refresh,NewGame} end,nodes()),
                                                     match_adm(Games++NewGame);
                                              no -> PidCom!already_playing,
                                                    PidGame!kill
                                           end;
		{join,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({G,_,_}) -> Game == G end,Games),
                                       case PosibleGame of
                                           [] -> PidCom!no_valid;
                                            _ -> {_,_,P2} = lists:nth(1,PosibleGame),
                                                 case P2 of
                                                     none -> global:send(Game,{join,UserName}),
													         lists:map(fun(X) -> {matchadm,X}!{join_refresh,Game,UserName} end,nodes()),
													         PidCom!joined,
											     		     match_adm(lists:map(fun(X) -> actualization(X,Game,UserName) end,Games));
                                                      _   -> PidCom!no_valid
                                                 end
                                        end;
  	    {ending,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({G,_,_}) -> Game == G end,Games),
                             PidCom!well_delivered,
                             case PosibleGame of
                                [] -> PidCom!no_valid;
                                _ -> {G,P1,P2} = lists:nth(1,PosibleGame),
                                     case (P1 == UserName) or (P2 == UserName) of
                                         true -> global:send(G,{ending,UserName}),
                                                 lists:map(fun(X) -> {matchadm,X}!{end_refresh,Game} end,nodes()),
                                                 match_adm(lists:filter(fun ({X,_,_}) -> X /= Game end ,Games));
                                         false -> gloabl:send(UserName,no_rights_game)
                                     end
                              end;
		{spect,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({G,_,_}) -> Game == G end,Games),
		                                case PosibleGame of
                                            [] -> PidCom!no_valid;
                                            _  -> {G,_,_} = lists:nth(1,PosibleGame),
                                                  global:send(G,{spect,UserName}),
									              PidCom!spect_ok
                                        end;
        {end_refresh,Game} -> match_adm(lists:filter(fun ({G,_,_}) -> G /= Game end ,Games));
        {end_spect,Game,UserName,PidCom} -> PosibleGame = lists:filter(fun({G,_,_}) -> Game == G end,Games),
		                                    case PosibleGame of
                                                [] -> PidCom!no_valid;
                                                _  -> {G,_,_} = lists:nth(1,PosibleGame),
                                                      global:send(G,{end_spect,UserName}),
									                  PidCom!end_spect
                                            end;
		{refresh,NewGame} -> match_adm(Games++NewGame);
		{join_refresh,Game,UserName} -> match_adm(lists:map(fun(X) -> actualization(X,Game,UserName) end,Games));
        {movement,Game,X,Y,UserName,PidCom} -> PosibleGame = lists:filter(fun({G,_,_}) -> Game == G end, Games),
                                               case PosibleGame of
                                                   [] -> PidCom!no_valid;
                                                   _ -> PidCom!well_delivered,
                                                        global:send(Game,{movement,X,Y,UserName})
                                                end;
		{list,PidCom} -> PidCom!{list,Games},match_adm(Games)
  end,
  match_adm(Games).

match(Player1,Player2,Spects,Name) ->
	case Player2 of
		none -> receive
		          {spect,SpectUser}  -> match(Player1,Player2,Spects++[SpectUser],Name);
                  {movement,_,_,UserName} -> global:send(UserName,not_start);
	              {join,UserName} -> global:send(Player1,turn),game(Player1,UserName,true,"---------",Spects,Name);
                  {ending,UserName} -> global:send(UserName,you_end_game);
                  kill -> ok
			    end
  end.

game(Player1,Player2,Turn,Board,Spects,Name) ->
	case Turn of
		true ->
                receive
				{movement,X,Y,User} -> if
				                        User == Player1 -> case lists:nth((3*(X-1)+Y),Board) of
								                                45 -> NewBoard = replace((3*(X-1)+Y),Board,"X"),
																      global:send(Player1,{update,NewBoard,Name}),
																	  global:send(Player2,{update,NewBoard,Name}),
																	  lists:map(fun(S) -> global:send(S,{update,NewBoard,Name}) end,Spects),
                                                                      case someoneWon(NewBoard) of
																            true -> global:send(Player1,{victory, Player1, Name}),
																	                global:send(Player2,{victory, Player1, Name}),
																	                lists:map(fun(S) -> global:send(S,{victory, Player1, Name}) end,Spects);

                                                                            false -> global:send(Player2,turn),
																                     game(Player1,Player2,false,NewBoard,Spects,Name)
                                                                      end;
																 _  -> global:send(Player1,no_valid),
                                                                       game(Player1,Player2,Turn,Board,Spects,Name)
										                   end;
											true -> global:send(User,no_valid),
                                                    game(Player1,Player2,Turn,Board,Spects,Name)
                                        end;
				{spect,SpectUSer} -> game(Player1,Player2,Turn,Board,Spects++[SpectUSer],Name);
                {end_spect,SpectUSer} -> game(Player1,Player2,Turn,Board,lists:filter(fun(X) -> SpectUSer /= X end,Spects),Name);
                {ending,UserName} -> global:send(UserName,you_end_game),
                                     if
                                        UserName == Player1 -> global:send(Player2,{opponent_end_game,UserName});
                                        true -> global:send(Player1,{opponent_end_game,UserName})
                                    end,
                                    lists:map(fun(S) -> global:send(S,{end_spect,UserName,Name}) end,Spects)
				end;
		false ->
                receive
				{movement,X,Y,User} -> if
				                        User == Player2 -> case lists:nth((3*(X-1)+Y),Board) of
								                                45 -> NewBoard = replace((3*(X-1)+Y),Board,"O"),
																      global:send(Player1,{update,NewBoard,Name}),
																	  global:send(Player2,{update,NewBoard,Name}),
																	  lists:map(fun(S) -> global:send(S,{update,NewBoard,Name}) end,Spects),
                                                                      case someoneWon(NewBoard) of
																            true -> global:send(Player1,{victory, Player2, Name}),
																	                global:send(Player2,{victory, Player2, Name}),
																	                lists:map(fun(S) -> global:send(S,{victory, Player2, Name}) end,Spects);
                                                                            false -> global:send(Player1,turn),
																                     game(Player1,Player2,true,NewBoard,Spects,Name)
                                                                      end;
																 _  -> global:send(Player2,no_valid),
                                                                       game(Player1,Player2,Turn,Board,Spects,Name)
										                   end;
											true -> global:send(User,no_valid),
                                                    game(Player1,Player2,Turn,Board,Spects,Name)
                                        end;
                {ending,UserName} -> global:send(UserName,you_end_game),
                                     if
                                        UserName == Player1 -> global:send(Player2,{opponent_end_game,UserName});
                                        true -> global:send(Player1,{opponent_end_game,UserName})
                                    end,
                                    lists:map(fun(S) -> global:send(S,{end_spect,UserName,Player1}) end,Spects);
                {end_spect,SpectUSer} -> game(Player1,Player2,Turn,Board,lists:filter(fun(X) -> SpectUSer /= X end,Spects),Name);
				{spect,SpectUSer} -> game(Player1,Player2,Turn,Board,Spects++[SpectUSer],Name)
				end
	end.

replace(N,List,Token) ->
	case N of
		1 -> Token++lists:nthtail(1,List);
		_ -> change(lists:nth(1,List))++replace(N-1,lists:nthtail(1,List),Token)
	end.

change(X) ->
	case X of
		45  -> "-";
		88  -> "X";
		79  -> "O"
	end.

actualization(Game,GameName,UserName) ->
	case Game of
    {GN,P1,P2} -> if GN == GameName -> {GN,P1,UserName};
		                true -> {GN,P1,P2}
		          end
  end.

% Atom_to_integer transforma un atomo en un entero

atom_to_integer(Num) ->
  list_to_integer(atom_to_list(Num)).

% Match_print se encarga de crear un string con todas las partidas en curso en todos los servidores

match_print(G) ->
	case G of
		{GameName,P1,none} -> io_lib:format("La partida de ~p busca contricante, el nombre es ~p~n",[P1,GameName]);
		{GameName,P1,P2} -> io_lib:format("La partida de ~p esta en curso. ~p VS ~p, el nombre es ~p~n",[P1,P1,P2,GameName])
	end.

% someoneWon verifica si alguien gano al realizar su movimiento

someoneWon(Board) ->
    rowWin(Board) or columnWin(Board) or diagWin(Board).

rowWin(Board) ->
    ((lists:nth(1,Board) == lists:nth(2,Board)) and (lists:nth(2,Board) == lists:nth(3,Board)) and (lists:nth(1,Board) /= 45)) or
    ((lists:nth(4,Board) == lists:nth(5,Board)) and (lists:nth(5,Board) == lists:nth(6,Board)) and (lists:nth(4,Board) /= 45)) or
    ((lists:nth(7,Board) == lists:nth(8,Board)) and (lists:nth(8,Board) == lists:nth(9,Board)) and (lists:nth(7,Board) /= 45)).

columnWin(Board) ->
    ((lists:nth(1,Board) == lists:nth(4,Board)) and (lists:nth(4,Board) == lists:nth(7,Board)) and (lists:nth(1,Board) /= 45)) or
    ((lists:nth(2,Board) == lists:nth(5,Board)) and (lists:nth(5,Board) == lists:nth(8,Board)) and (lists:nth(2,Board) /= 45)) or
    ((lists:nth(3,Board) == lists:nth(6,Board)) and (lists:nth(6,Board) == lists:nth(9,Board)) and (lists:nth(3,Board) /= 45)).

diagWin(Board) ->
    ((lists:nth(1,Board) == lists:nth(5,Board)) and (lists:nth(5,Board) == lists:nth(9,Board)) and (lists:nth(1,Board) /= 45)) or
    ((lists:nth(7,Board) == lists:nth(5,Board)) and (lists:nth(5,Board) == lists:nth(3,Board)) and (lists:nth(7,Board) /= 45)).

% Is_in verifica si un usuario ya tiene una partida activa

is_in(UserName,Games) ->
    length(lists:filter(fun({P1,P2,_,_}) -> (P1 == UserName) or (P2 == UserName) end,Games)) == 1.
