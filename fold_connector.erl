-module(fold_connector).
-export([fold_service/2]).

fold_service(FoldFun, Acc0) when is_function(FoldFun, 2) ->
    fold_service(undefined, FoldFun, Acc0).

fold_service(Recipient, FoldFun, Acc) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, Recipient, FoldFun, Acc)
    after
        0 ->
            receive
                {ctl, Command} -> process_command(Command, Recipient, FoldFun, Acc);
                {msg, Message} -> process_message(Message, Recipient, FoldFun, Acc)
            end
    end.


process_command(Command, _Recipient, FoldFun, Acc) ->
    case Command of
        {set_recipient, NewRecipient} ->
            case utils:is_pid_or_registered(NewRecipient) of
                true ->
                    fold_service(NewRecipient, FoldFun, Acc)
            end;
        {unset_recipient} ->
            fold_service(undefined, FoldFun, Acc);
        quit -> ok
    end.


process_message(Message, Recipient, FoldFun, Acc) ->
    {Results, NextAcc} = FoldFun(Message, Acc),
    if
        Recipient /= undefined ->
            utils:pipeline_messages(Recipient, Results);
        true -> true
    end,
    fold_service(Recipient, FoldFun, NextAcc).

