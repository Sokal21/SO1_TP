-module(client).
-compile(export_all).

%% Starts the client, initializing the console.

start(Host, Port) ->
    {ok,Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    io:format("Bienvenido! ~n"),
    spawn(?MODULE,listener,[Socket,self()]),
    console(Socket, []).

%% A console. As all of them. Here goes the commands.
console (Socket,UserName) ->
    Command = io:get_line(UserName ++ "> "),
    gen_tcp:send(Socket,Command),
    receive 
        {newPrompt, NewUserName} -> console(Socket, NewUserName);
         continue                -> console(Socket, UserName);
         close                   -> exit(normal)
    end.

%% Constantly listening messages/responses from server.
listener (Socket, Pid) ->
    case gen_tcp:recv(Socket,0) of
        {ok, Data} -> TokenList = string:tokens(Data," \n"),
                      case lists:nth(1,TokenList) of
                        "UPD"               -> GameName = lists:nth(3, TokenList),
                                               print_table (lists:nth(2, TokenList),GameName);
                        "EXIT"              -> io:format("Te has retirado del servidor ~n"),
                                               Pid!close,
                                               exit(normal);
                        "GAME"              -> io:format("Tu sala ha sido creada exitosamente.~n");
                        "TURN"              -> io:format("Es tu turno. ~n");
                        "SPECT_OK"          -> io:format("Has logrado entrar como espectador a la partida. ~n");
                        "JOIN"              -> io:format("Te has unido a la partida correctamente. ~n");
                        "WELL_DELIVERED"    -> ok;
                        "NO_VALID"          -> io:format("No es tu turno, o la jugada no puede realizarse. ~n");
                        "LOG"               -> UserName = lists:nth(2, TokenList),
                                               io:format("Bienvenido " ++ UserName ++ ", te registraste correctamente. ~n"),
                                               Pid!{newPrompt,UserName},
                                               listener(Socket,Pid);
                        "NOT_START"         -> io:format("La partida no ha comenzado.~n");
                        "LOG_FAIL"          -> io:format("El nombre de usuario ya está en uso. Elija otro.~n");
                        "NOT_LOG"           -> io:format("Debes loguearte primero.~n");
                        "ALREADY_LOG"       -> UserName = lists:nth(2, TokenList),
                                               io:format("Ya estás conectado como" ++ UserName ++".~n");
                        "ALREADY_PLAYING"   -> io:format("Ya estás en una sala.~n");
                        "GAMES"             -> NewData = lists:subtract(Data, "GAMES "),
                                               io:format(NewData);
                        "INCORRECT"         -> io:format("Comando incorrecto.~n");
                        "NO_VALID_GAME"     -> io:format("Sala no encontrada.~n");
                        "NO_RIGHTS_GAME"    -> io:format("Usted no está jugando en esa sala.~n");
                        "OPPONENT_END_GAME" -> UserName = lists:nth(2, TokenList),
                                               io:format("El usuario " ++ UserName ++ " terminó la partida.~n");
                        "END_SPECT"         -> UserName = lists:nth(2,TokenList),
                                               NameGame = lists:nth(3,TokenList),
                                               io:format("El usuario " ++ UserName ++ " terminó la partida de " ++ NameGame ++ ".~n");
                        "END_SPECT_OK"      -> io:format("Has dejado de observar la partida.~n");
                        "YOU_END_GAME"      -> io:format("Te has retirado de la sala.~n");
                        "NO_VALID_USER"     -> io:format("El usuario no existe.~n");
                        "SAY"               -> UserName = lists:nth(2, TokenList),
                                               Message = lists:subtract(Data, "SAY "++ UserName),
                                               io:format("El usuario " ++ UserName ++ " dice:" ++Message );
                        "WIN"               -> UserName = lists:nth(2,TokenList),
                                               NameGame = lists:nth(3,TokenList),
                                               io:format("El usuario " ++ UserName ++ " ganó la partida de " ++ NameGame ++ ".~n");
                         M                  -> io:format("Error: {~p,~p}~n",[M,Data])
                      end;
        {error, closed} -> io:format("Error recibiendo respuesta del servidor (gen_tcp:recv)")
    end,
    Pid!continue,
    listener(Socket,Pid).


%% Pretty-printing for the table
print_table (Table, GameName) ->
         io:format(GameName ++ "~n~n"),
         io:format(aux(lists:nth(1,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(2,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(3,Table))),
         io:format("~n---------~n"),
         io:format(aux(lists:nth(4,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(5,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(6,Table))),
         io:format("~n---------~n"),
         io:format(aux(lists:nth(7,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(8,Table))),
         io:format(" | "),
         io:format(aux(lists:nth(9,Table))),
         io:format( "~n~n~n").

%% Blame Erlang for this
aux (Int) ->
     case Int of
         88 -> "X";
         79 -> "O";
         45 -> "-"
     end.
