
-module(example2).

-include_udg(for_tests_1_11, [ \\test_pattern/3, is_uep_test_var/1, \\test_var/5 ]).

-record(message, { a = 0, b = <<>>, c = <<>>}).

test_1() ->
    receive 
        A \\ test_pattern(B, C, D)
           when C /= 0, is_uep_test_var(B) orelse B =:= <<>> ->
               test_module:i(D)
    after 1000 -> ok
    end.

test_2(A0) ->
    case A0 of
        _ \\ test_var(B,C,D,E,F) ->
            test_module:i([B,C,D,E,F]);
        _ -> test_module:i([])
    end.
