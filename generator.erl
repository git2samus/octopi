-module(generator).
-export([integer_sequence_service/1]).

integer_sequence_service(Recipient) ->
    integer_sequence(Recipient, 0).


integer_sequence(Recipient, N) ->
    Recipient ! {msg, N},
    timer:sleep(1000),
    integer_sequence(Recipient, N+1).

