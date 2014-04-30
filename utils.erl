-module(utils).
-export([is_pid_or_registered/1,
         pipeline_messages/2]).

% check if Term is a valid message destination
is_pid_or_registered(Term) ->
    is_pid(Term) orelse lists:member(Term, registered()).


% send every item from Messages as a separate message to Recipient
pipeline_messages(Recipient, Messages) ->
    SendMessageFun = fun(Message) -> Recipient ! {msg, Message} end,
    lists:foreach(SendMessageFun, Messages).

