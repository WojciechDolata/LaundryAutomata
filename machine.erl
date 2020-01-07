
-module(machine).
-compile([export_all]).

print({printstate,Msg}) ->
    io:format("\e[~p;~pH~s\n",[1, 20, "State:   " ++ Msg ++ "                                              "]);   
print({printstep,Msg}) ->
    io:format("\e[~p;~pH~s\n",[2, 20,"Step:    " ++ Msg ++ "                                               "]); 
print({finished}) ->
    io:format("\e[~p;~pH~s\n",[3, 20,"Status:  Finished" ++ "                                               "]),
    mySleep(1*pace());
print({in_progress}) ->
    io:format("\e[~p;~pH~s\n",[3, 20,"Status:  In progress" ++ "                                               "]);
print({clear}) ->
    io:format("\e[2J",[]).

% how fast does the simmulation go (x20)
pace() -> 20.

% in seconds
mySleep(T) -> 
    timer:sleep(round(1000*T/pace())).

lock(PPID) ->
    print({printstep, "Locking door"}),
    print({in_progress}),
    print({finished}),
    PPID!{"Lock_Success"}.

prewash(PPID) ->
    print({printstate, "PRE-WASHING"}),
    LoopPID = spawn(machine, prewash_receive, [self()]),
    LoopPID!{"Start"},
    receive
        {"Prewash_Success"} -> 
            % print({printstep, "koniec"}),
            % spawn(machine, wash, [PPID,30,100])
            PPID!{"Prewash_Success"}
    end.

prewash_receive(PPID) -> 
    receive
        {"Start"} -> 
            spawn(machine, water_fill, [self()]),
            prewash_receive(PPID);
        {"Filled"} -> 
            spawn(machine, heat, [self(), 30]),
            prewash_receive(PPID); %heat up to 30 C
        {"Heated"} -> 
            spawn(machine, rotate, [self(), 400, 15]),
            prewash_receive(PPID); %slowly spin at 400 RPM
        {"Rotated"} ->
            spawn(machine, pump, [self()]),
            prewash_receive(PPID);
        {"Pumped"} ->
            PPID!{"Prewash_Success"}
    end.

wash(PPID, T, RPM) ->
    print({printstate, "KKK"}).

pump(PPID) ->
    print({printstep, "Pumping water"}),
    print({in_progress}),
    mySleep(40),
    print({finished}),
    PPID!{"Pumped"}.

rotate(PPID, RPM, Time) ->
    print({printstep, "Rotating at " ++ integer_to_list(RPM) ++ " RPM"}),
    print({in_progress}),
    mySleep(Time),
    print({finished}),
    PPID!{"Rotated"}.

heat(PPID, T) ->
    print({printstep, "Heating water up to " ++ integer_to_list(T) ++ " C"}),
    print({in_progress}),
    mySleep(T*5),
    print({finished}),
    PPID!{"Heated"}.

water_fill(PPID) ->
    print({printstep, "Filling with water"}),
    print({in_progress}),
    mySleep(60),
    print({finished}),
    PPID!{"Filled"}.

main_receive(PPID, T, RPM, Should_Prewash) ->
    receive
        {"Lock_Success"} -> 
            if Should_Prewash ->
                spawn(machine, prewash, [self()]);
            true -> spawn(machine, wash, [self(), T, RPM])
            end,
            main_receive(PPID, T, RPM, Should_Prewash);
        {"Prewash_Success"} -> 
            spawn(machine, wash, [self(), T, RPM]),
            main_receive(PPID, T, RPM, Should_Prewash);
        {"Wash_Success"} -> 
            -1 % tu sie wykaz
    end.

start(T, RPM, Should_Prewash) ->
    print({clear}),
    ReceivePID = spawn(machine, main_receive, [self(), T, RPM, Should_Prewash]),
    spawn(machine, lock, [ReceivePID]),
    receive
        {"Finish"} -> 0 % na koniec przesyłamy sobie finisz
    end.
