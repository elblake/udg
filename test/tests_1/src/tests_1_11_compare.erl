
-module(example2).



-record(message, { a = 0, b = <<>>, c = <<>>}).

test_1() ->
    receive 
        Message = #message{ a = C, b = D, c = B = TMP_UDG_1 = TMP_UDG_2 = <<_:1/bytes,"-",_:2/bytes,"-",_:3/bytes,"-",_:4/bytes,"-",_:5/bytes>>} when is_binary(D), byte_size(D) =/= 0, C /= 0, (true) orelse B =:= <<>> -> test_module:i(D);
        Message = #message{ a = C, b = D, c = B} when is_binary(D), byte_size(D) =/= 0, C /= 0, false orelse B =:= <<>> -> test_module:i(D)
    after 1000 -> ok
    end.

test_2(A0) ->
    case A0 of
        _ = <<B:1/bytes,"-",C:2/bytes,"-",D:3/bytes,"-",E:4/bytes,"-",F:5/bytes>> ->
            test_module:i([B,C,D,E,F]);
        _ -> test_module:i([])
    end.
