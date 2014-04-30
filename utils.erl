-module(utils).
-export([is_pid_or_registered/1]).

is_pid_or_registered(Term) ->
    is_pid(Term) orelse lists:member(Term, registered()).

