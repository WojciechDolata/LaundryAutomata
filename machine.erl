
-module(machine).
-compile([export_all]).

% how fast does the simmulation go (x20)
pace() -> 20.

% in seconds
mySleep(T) -> 
    timer:sleep(round(1000*T/pace())).

print(N) ->
    io:format("~p ~n",[N]).

lock(PPID) ->
    print("Locking door"),
    PPID!{"Lock_Success"}.

prewash(PPID) ->
    print("Pre-washing begins"),
    LoopPID = spawn(machine, prewash_receive, [self()]),
    LoopPID!{"Start"},
    receive
        {"Prewash_Success"} -> 
            print("Finished prewashing"),
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
    print("KKK").

pump(PPID) ->
    print("Pumping water"),
    mySleep(40),
    print("Water pumped"),
    PPID!{"Pumped"}.

rotate(PPID, RPM, Time) ->
    print("Startint to rotate at"),
    mySleep(Time),
    print("Rotating ended"),
    PPID!{"Rotated"}.

heat(PPID, T) ->
    print("Heating water up"),
    mySleep(T*5),
    print("Water temperature at"),
    print(T),
    PPID!{"Heated"}.

water_fill(PPID) ->
    print("Filling with water"),
    mySleep(60),
    print("Filled"),
    PPID!{"Filled"}.

start(T, RPM, Should_Prewash) ->
    spawn(machine, lock, [self()]),
    receive
        {"Lock_Success"} -> if Should_Prewash ->
                                spawn(machine, prewash, [self()]);
                            true -> spawn(machine, wash, [self(), T, RPM])
                        end;
        {"Prewash_Success"} -> spawn(machine, prewash, [self(), T, RPM])
    end.

