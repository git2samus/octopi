-module(receiver).
-export([file_writer_service/1]).

file_writer_service(Filename) ->
    case file:open(Filename, [write]) of
        {ok, IoDevice} ->
            file_writer(IoDevice)
    end.

file_writer(IoDevice) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, IoDevice)
    after
        0 ->
            receive
                {ctl, Command} -> process_command(Command, IoDevice);
                {msg, Message} -> process_message(Message, IoDevice)
            end
    end.


process_command(Command, IoDevice) ->
    case Command of
        quit ->
            file:close(IoDevice)
    end.

process_message(Message, IoDevice) ->
    io:fwrite(IoDevice, "[~s] ~w~n", [utils:isoformat(utils:local_ts()), Message]),
    file_writer(IoDevice).

