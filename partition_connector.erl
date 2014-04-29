-module(partition_connector).
-export([partition_service/1]).

partition_service(FilterFun) ->
    partition_service({undefined, undefined}, FilterFun).

partition_service(Recipients, FilterFun) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, Recipients, FilterFun)
    after
        0 -> receive
            {ctl, Command} -> process_command(Command, Recipients, FilterFun);
            {msg, Message} -> process_message(Message, Recipients, FilterFun)
        end
    end.


process_command(Command, {PassRecipient, FailRecipient}, FilterFun) ->
    case Command of
        {set_pass_recipient, NewPassRecipient} ->
            partition_service({NewPassRecipient, FailRecipient}, FilterFun);
        {set_fail_recipient, NewFailRecipient} ->
            partition_service({PassRecipient, NewFailRecipient}, FilterFun);
        {unset_pass_recipient} ->
            partition_service({undefined, FailRecipient}, FilterFun);
        {unset_fail_recipient} ->
            partition_service({PassRecipient, undefined}, FilterFun);
        quit -> ok
    end.

process_message(Message, {PassRecipient, FailRecipient}, FilterFun) ->
    case FilterFun(Message) of
        true ->
            if
                PassRecipient /= undefined ->
                    PassRecipient ! {msg, Message};
                true -> true
            end;
        false ->
            if
                FailRecipient /= undefined ->
                    FailRecipient ! {msg, Message};
                true -> true
            end
    end,
    partition_service({PassRecipient, FailRecipient}, FilterFun).
