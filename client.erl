-module(client).
-compile(export_all).

%% Starts the client, initializing the console.

start(Host, Port) -> 
    {ok,Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    io:format("Hola bigote, bienvenido al sv del doni. Registate, no seas Lucio.~n"),
    spawn(?MODULE,listener,[Socket]),
    console(Socket).

%% A console. As all of them. Here goes the commands.
console (Socket) -> 
    Command = io:get_line("> "),
    gen_tcp:send(Socket,Command),
    receive after 500 -> ok end,
    console(Socket).

%% Constantly listening messages/responses from server.
listener (Socket) ->
    case gen_tcp:recv(Socket,0) of
        {ok, Data} -> TokenList = string:tokens(Data," "),
                      case lists:nth(1,TokenList) of
                        "UPD"             -> print_table (lists:nth(2, TokenList));
                        "EXIT"            -> io:format("Sesión finalizada ~n"),
                                             exit(log_out);
                        "GAME"            -> io:format("Tu sala ha sido creada exitosamente.~n");
                        "TURN"            -> io:format("Es tu turno. ~n");
                        "NO_VALID"        -> io:format("No es tu turno, o la jugada no puede realizarse. ~n");
                        "LOG"             -> UserName = lists:nth(2, TokenList),
                                             io:format("Bienvenido " ++ UserName ++ ", te registraste correctamente. ~n");
                        "LOG_FAIL"        -> io:format("El nombre de usuario ya está en uso. Elija otro.~n")
                        "ALREADY_LOG"     -> UserName = lists:nth(2, TokeList),
                                             io:format("Ya estás conectado como" ++ UserName ++".~n");
                        "ALREADY_PLAYING" -> io:format("Ya estás en una sala.~n");
                        "GAMES"           -> NewData = lists:substract(Data, "GAMES "),
                                             io:format(NewData);
                        "INCORRECT"       -> io:format("Comando incorrecto.~n");
                        "NO_VALID_GAME"   -> io:format("Sala no encontrada.~n");
                         _                -> io:format("Revisar los mensajes que recibe el cliente, hay uno que no pattern-matchea ~n")
                      end;
        {error, closed} -> io:format("Error recibiendo respuesta del servidor (gen_tcp:recv)")
    end,
    listener(Socket).


%% Pretty-printing for the table
print_table (Table) -> 
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
         io:format( "~n~n=============~n~n").

%% Blame Erlang for this
aux (Int) -> 
     case Int of
         88 -> "X";
         79 -> "O";
         45 -> "-"
     end.
