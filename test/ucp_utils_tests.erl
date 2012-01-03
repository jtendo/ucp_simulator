-module(ucp_utils_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_hex_test_() ->
  [{"int to hexstr",
    fun () ->
        ?assertEqual("0F", ucp_utils:to_hexstr(15))
    end},
   {"list to hexstr",
    ?_test(?assertEqual("081E", ucp_utils:to_hexstr([8,30])))},
   {"binary to hexstr",
    ?_test(?assertEqual("1E12", ucp_utils:to_hexstr(<<30,18>>)))},
   {"unicode to hexstr",
       ?_test(?assertEqual("001E01420105006B0061", ucp_utils:to_hexstr([30,322,261,107,97])))},
   {"str to hexstr",
    ?_test(?assertEqual("74657374", ucp_utils:to_hexstr("test")))}].

-endif.
