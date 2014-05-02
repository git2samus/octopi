-module(utils).
-export([is_pid_or_registered/1,
         pipeline_messages/2,
         local_ts/0, local_ts/1,
         universal_ts/0, universal_ts/1,
         isoformat/1]).


% check if Term is a valid message destination
is_pid_or_registered(Term) ->
    is_pid(Term) orelse lists:member(Term, registered()).


% send every item from Messages as a separate message to Recipient
pipeline_messages(Recipient, Messages) ->
    SendMessageFun = fun(Message) -> Recipient ! {msg, Message} end,
    lists:foreach(SendMessageFun, Messages).


% date formatting
isoformat(Parts) ->
    lists:flatten(io_lib:format("~4..0B-~2..0B~2..0BT~2..0B:~2..0B:~2..0B.~6..0B", Parts)).


local_ts() ->
    local_ts(os:timestamp()).

local_ts(Timestamp) ->
    ts_to_parts(Timestamp, {calendar, now_to_local_time}).


universal_ts() ->
    universal_ts(os:timestamp()).

universal_ts(Timestamp) ->
    ts_to_parts(Timestamp, {calendar, now_to_universal_time}).


ts_to_parts(Timestamp = {_MacroSecond, _Second, MicroSecond}, {ConvertModule, ConvertFun}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = apply(ConvertModule, ConvertFun, [Timestamp]),
    [Year, Month, Day, Hour, Minute, Second, MicroSecond].

