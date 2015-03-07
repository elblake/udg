
-module(example1).
-author('Edward L. Blake at < edwardlblake at gmail.com >').

-export([ test/0 ]).

-auto_include_udg([ \\non_empty_message/3, is_uep_displayable_uuid/1 ]).

-record(message, { flags = 0, data = <<>>, uuid = <<>>}).

test() ->
    receive 
        Message \\ non_empty_message(UUID, MsgFlags, MsgLine)
          when MsgFlags /= 0, is_uep_displayable_uuid(UUID)
                                  orelse UUID =:= <<>> ->
            io:format("From UUID: ~s~n", [UUID]),
            io:format("Message: ~s", [MsgLine])
    after 1000 -> ok
    end.
