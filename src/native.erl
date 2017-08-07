-module(native).

% Define native implemented functions here.
-export([clock/0]).


clock() -> get_timestamp_millis() / 1000.0.




% UTILS
get_timestamp_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).