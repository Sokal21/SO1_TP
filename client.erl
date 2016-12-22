-module(client).
-compile(export_all).

start(Host, Port) -> 
    {ok,Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    io:format("Hola bigote, bienvenido al sv del doni. Registate, no seas Lucio.~n"),
    spawn(?MODULE,listener,[Socket]),
    console(Socket).

console (Socket) -> 
    Command = io:get_line("> "),
    gen_tcp:send(Socket,Command),
    receive after 500 -> ok end,
    console(Socket).
   
listener (Socket) ->
    case gen_tcp:recv(Socket,0) of
        {ok, Data} -> TokenList = string:tokens(Data," "),
                      case lists:nth(1,TokenList) of
                        "UPD" -> print_table (lists:nth(2,TokenList));
                         _    -> io:format(Data),
                                 io:format("~n")
                      end;
        {error, closed} -> error
    end,
    listener(Socket).

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
         io:format( "~n").

aux (Int) -> 
     case Int of
         88 -> "X";
         79 -> "O";
         45 -> "-"
     end.

