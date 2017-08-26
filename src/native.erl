-module(native).

% Define native implemented functions here.
-export([clock/0, str/1]).

clock() -> get_timestamp_millis() / 1000.0.

str(X) when is_list(X) -> X;
str(X) when is_integer(X) -> integer_to_list(X);
str(X) when is_float(X) -> float_to_list(X);
str(true) -> "true";
str(false) -> "false".




% UTILS
get_timestamp_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).