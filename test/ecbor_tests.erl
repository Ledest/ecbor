-module(ecbor_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("otpbp/include/otpbp_pt.hrl").

bench_test() ->
    erlang:garbage_collect(),
    N = 10000,
    lists:foreach(fun({Name, Hex}) ->
                      Bin = binary:decode_hex(Hex),
                      {Usec, ok} = timer:tc(fun() -> repeat_decode_n(N, Bin) end),
                      io:fwrite("~s: ~p #/s ~.4f us/# ~.2f MB/s~n",
                                [Name, N * 1000000 div Usec, Usec / N, byte_size(Bin) * N / Usec])
                  end,
                  [{"nd list", <<"9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff">>},
                   {"fixed list", <<"98190102030405060708090a0b0c0d0e0f101112131415161718181819">>},
                   {"fixed map", <<"a56161614161626142616361436164614461656145">>},
                   {"nested", <<"bf61610161629f0203ffff">>}]).

repeat_decode_n(0, _) -> ok;
repeat_decode_n(N, Bin) -> ecbor:decode(Bin), repeat_decode_n(N - 1, Bin).

encode_test_() ->
    lists:map(fun({V, E}) -> {timeout, 30, ?_assertEqual(ecbor:encode(V), binary:decode_hex(E))};
                 ({T, V, E}) -> {T, timeout, 30, ?_assertEqual(ecbor:encode(V), binary:decode_hex(E))}
              end,
              [{0, <<"00">>},
               {1, <<"01">>}, {23, <<"17">>}, {24, <<"1818">>}, {16#FF, <<"18FF">>},
               {16#100, <<"190100">>}, {16#FFFF, <<"19FFFF">>},
               {16#10000, <<"1A00010000">>}, {16#FFFFFFFF, <<"1AFFFFFFFF">>},
               {16#100000000, <<"1B0000000100000000">>}, {16#FFFFFFFFFFFFFFFF, <<"1BFFFFFFFFFFFFFFFF">>},
               {-1, <<"20">>}, {-24, <<"37">>}, {-25, <<"3818">>}, {-16#100, <<"38FF">>},
               {-16#101, <<"390100">>}, {-16#10000, <<"39FFFF">>},
               {-16#10001, <<"3A00010000">>}, {-16#100000000, <<"3AFFFFFFFF">>},
               {-16#100000001, <<"3B0000000100000000">>}, {-16#10000000000000000, <<"3BFFFFFFFFFFFFFFFF">>},
               {"Binary", <<"">>, <<"40">>},
               {"Binary", <<1, 2, 3, 4>>, <<"4401020304">>},
               {"Binary", <<"a">>, <<"4161">>},
               {"Binary", <<"IETF">>, <<"4449455446">>},
               {"Binary", <<"\"\\">>, <<"42225c">>},
               {"Tiny Tuple", {1, 2, 3, 4}, <<"8401020304">>},
               {"Small Tuple", list_to_tuple(lists:seq(1, 25)), <<"98190102030405060708090a0b0c0d0e0f101112131415161718181819">>},
               {"Small Tuple", erlang:make_tuple(16#FF, 0), <<"98FF", (binary:copy(<<"00">>, 16#FF))/binary>>},
               {"Short Tuple", erlang:make_tuple(16#100, 1), <<"990100", (binary:copy(<<"01">>, 16#100))/binary>>},
               {"Short Tuple", erlang:make_tuple(16#FFFF, 1), <<"99FFFF", (binary:copy(<<"01">>, 16#FFFF))/binary>>},
               {"Long Tuple", erlang:make_tuple(16#10000, 2), <<"9A00010000", (binary:copy(<<"02">>, 16#10000))/binary>>},
               {"Long Tuple", erlang:make_tuple(16#FFFFFF, 2), <<"9A00FFFFFF", (binary:copy(<<"02">>, 16#FFFFFF))/binary>>},
               {"List", [1, 2, 3, 4], <<"9F01020304FF">>},
               {"Boolean", false, <<"F4">>}, {"Boolean", true, <<"F5">>},
               {"NULL", null, <<"F6">>}, {"Undefined", undefined, <<"F7">>}]).

decode_test_() ->
    lists:map(fun({V, E}) -> {timeout, 30, ?_assertEqual(V, ecbor:decode(binary:decode_hex(E)))};
                 ({T, V, E}) -> {T, timeout, 30, ?_assertEqual(V, ecbor:decode(binary:decode_hex(E)))}
              end,
              [{0, <<"00">>},
               {1, <<"01">>}, {23, <<"17">>}, {24, <<"1818">>}, {16#FF, <<"18FF">>},
               {16#100, <<"190100">>}, {16#FFFF, <<"19FFFF">>},
               {16#10000, <<"1A00010000">>}, {16#FFFFFFFF, <<"1AFFFFFFFF">>},
               {16#100000000, <<"1B0000000100000000">>}, {16#FFFFFFFFFFFFFFFF, <<"1BFFFFFFFFFFFFFFFF">>},
               {-1, <<"20">>}, {-24, <<"37">>}, {-25, <<"3818">>}, {-16#100, <<"38FF">>},
               {-16#101, <<"390100">>}, {-16#10000, <<"39FFFF">>},
               {-16#10001, <<"3A00010000">>}, {-16#100000000, <<"3AFFFFFFFF">>},
               {-16#100000001, <<"3B0000000100000000">>}, {-16#10000000000000000, <<"3BFFFFFFFFFFFFFFFF">>},
               {"Float16 +Inf", positive_infinity, <<"F97C00">>},
               {"Float16 NaN", nan, <<"F97E00">>},
               {"Float16 -Inf", negative_infinity, <<"F9FC00">>},
               {"Float32 +Inf", positive_infinity, <<"FA7F800000">>},
               {"Float32 NaN", nan, <<"FA7FC00000">>},
               {"Float32 -Inf", negative_infinity, <<"FAFF800000">>},
               {"Float64 +Inf", positive_infinity, <<"FB7FF0000000000000">>},
               {"Float64 NaN", nan, <<"FB7FF8000000000000">>},
               {"Float64 -Inf", negative_infinity, <<"FBFFF0000000000000">>},
               {"Binary", <<"">>, <<"40">>},
               {"Binary", <<1, 2, 3, 4>>, <<"4401020304">>},
               {"String", <<"">>, <<"60">>},
               {"String", <<"a">>, <<"6161">>},
               {"String", <<"IETF">>, <<"6449455446">>},
               {"String", <<"\"\\">>, <<"62225c">>},
               {"Date/Time", <<"2013-03-21T20:04:00Z">>, <<"C074323031332D30332D32315432303A30343A30305A">>},
               {"Timestamp integer", 1363896240, <<"C11A514B67B0">>},
               {"Timestamp float", 1363896240.5, <<"C1FB41D452D9EC200000">>},
               {"Tag base16", <<1, 2, 3, 4>>, <<"D74401020304">>},
               {"Tag CBOR", <<"dIETF">>, <<"D818456449455446">>},
               {"Tag URI", <<"http://www.example.com">>, <<"D82076687474703A2F2F7777772E6578616D706C652E636F6D">>},
               {"Tiny Array", {1, 2, 3, 4}, <<"8401020304">>},
               {"Small Array", list_to_tuple(lists:seq(1, 25)), <<"98190102030405060708090a0b0c0d0e0f101112131415161718181819">>},
               {"Small Array", erlang:make_tuple(16#FF, 0), <<"98FF", (binary:copy(<<"00">>, 16#FF))/binary>>},
               {"Short Array", erlang:make_tuple(16#100, 1), <<"990100", (binary:copy(<<"01">>, 16#100))/binary>>},
               {"Short Array", erlang:make_tuple(16#FFFF, 1), <<"99FFFF", (binary:copy(<<"01">>, 16#FFFF))/binary>>},
               {"Long Array", erlang:make_tuple(16#10000, 2), <<"9A00010000", (binary:copy(<<"02">>, 16#10000))/binary>>},
               {"Long Array", erlang:make_tuple(16#FFFFFF, 2), <<"9A00FFFFFF", (binary:copy(<<"02">>, 16#FFFFFF))/binary>>},
               {"Large Array", lists:duplicate(16#1000000, 2), <<"9A01000000", (binary:copy(<<"02">>, 16#1000000))/binary>>},
               {"Indefinite Array", [1, 2, 3, 4], <<"9F01020304FF">>},
               {"Boolean", false, <<"F4">>}, {"Boolean", true, <<"F5">>},
               {"NULL", null, <<"F6">>}, {"Undefined", undefined, <<"F7">>}]).
