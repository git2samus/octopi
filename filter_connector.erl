-module(filter_connector).
-export([filter_service/1]).

filter_service(FilterFun) when is_function(FilterFun, 1) ->
    filter_service(undefined, FilterFun).

filter_service(Recipient, FilterFun) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, Recipient, FilterFun)
    after
        0 ->
            receive
                {ctl, Command} -> process_command(Command, Recipient, FilterFun);
                {msg, Message} -> process_message(Message, Recipient, FilterFun)
            end
    end.


process_command(Command, Recipient, FilterFun) ->
    case Command of
        {set_recipient, NewRecipient} ->
            case utils:is_pid_or_registered(NewRecipient) of
                true ->
                    filter_service(NewRecipient, FilterFun)
            end;
        {unset_recipient} ->
            filter_service(undefined, FilterFun);
        {get_recipient, ReplyTo} ->
            ReplyTo ! {recipient, Recipient},
            filter_service(Recipient);
        quit -> ok
    end.

process_message(Message, Recipient, FilterFun) ->
    if
        Recipient /= undefined ->
            case FilterFun(Message) of
                true ->
                    Recipient ! {msg, Message};
                false ->
                    true
            end;
        true -> true
    end,
    filter_service(Recipient, FilterFun).

