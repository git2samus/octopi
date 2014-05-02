-module(broadcast_connector).
-export([broadcast_service/0]).

broadcast_service() ->
    broadcast_service(sets:new()).

broadcast_service(Recipients) ->
    receive
        % try to clear control messages first
        {ctl, Command} -> process_command(Command, Recipients)
    after
        0 ->
            receive
                {ctl, Command} -> process_command(Command, Recipients);
                {msg, Message} -> process_message(Message, Recipients)
            end
    end.


process_command(Command, Recipients) ->
    case Command of
        {add_recipient, Recipient} ->
            case utils:is_pid_or_registered(Recipient) of
                true ->
                    NewRecipients = sets:add_element(Recipient, Recipients),
                    broadcast_service(NewRecipients)
            end;
        {del_recipient, Recipient} ->
            NewRecipients = sets:del_element(Recipient, Recipients),
            broadcast_service(NewRecipients);
        {get_recipients, ReplyTo} ->
            ReplyTo ! {recipients, Recipients},
            broadcast_service(Recipients);
        quit -> ok
    end.

process_message(Message, Recipients) ->
    SendMessageFun = fun(Elem, Acc) -> Elem ! Acc end,
    sets:fold(SendMessageFun, {msg, Message}, Recipients),
    broadcast_service(Recipients).

