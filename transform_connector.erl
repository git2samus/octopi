-module(transform_connector).
-export([transform_service/1]).

transform_service(TransformFun) when is_function(TransformFun, 1) ->
    transform_service(undefined, TransformFun).

transform_service(Recipient, TransformFun) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, Recipient, TransformFun)
    after
        0 ->
            receive
                {ctl, Command} -> process_command(Command, Recipient, TransformFun);
                {msg, Message} -> process_message(Message, Recipient, TransformFun)
            end
    end.


process_command(Command, _Recipient, TransformFun) ->
    case Command of
        {set_recipient, NewRecipient} ->
            case utils:is_pid_or_registered(NewRecipient) of
                true ->
                    transform_service(NewRecipient, TransformFun)
            end;
        {unset_recipient} ->
            transform_service(undefined, TransformFun);
        quit -> ok
    end.


process_message(Message, Recipient, TransformFun) ->
    if
        Recipient /= undefined ->
            utils:pipeline_messages(Recipient, TransformFun(Message));
        true -> true
    end,
    transform_service(Recipient, TransformFun).

