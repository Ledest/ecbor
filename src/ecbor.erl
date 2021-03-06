-module(ecbor).

-export([encode/1, decode/1, encode_seq/1, decode_seq/1]).
-export([enc/1, dec/1, enc_seq/1, dec_seq/1]).

-define(SIZE1, 24).
-define(SIZE2, 25).
-define(SIZE4, 26).
-define(SIZE8, 27).

-define(SIZE(S), (1 bsl (S band 2#11))).

-define(DATETIME, 0).
-define(SECONDS, 1).
-define(BIG_PINT, 2).
-define(BIG_NINT, 3).
-define(ETERM, 131).
-define(IS_ATOM(T), (T =:= 100 orelse T =:= 115)).
-define(IS_UATOM(T), (T =:= 118 orelse T =:= 119)).

-define(PINT, 0).
-define(NINT, 1).
-define(BSTR, 2).
-define(TSTR, 3).
-define(ARRAY, 4).
-define(MAP, 5).
-define(TAG, 6).
-define(SIMPLE, 7).
-define(FLOAT, 7).

-define(INDEFINITE, 16#1F).
-define(BREAK, 16#FF).

-define(NEG(I), (-1 - I)).

-define(TYPE(T, L, S), T:3, L:5, S:?SIZE(L)/unit:8).
-define(TYPE0(T, S), T:3, S:5).
-define(TYPE1(T, S), ?TYPE(T, ?SIZE1, S)).
-define(TYPE2(T, S), ?TYPE(T, ?SIZE2, S)).
-define(TYPE4(T, S), ?TYPE(T, ?SIZE4, S)).
-define(TYPE8(T, S), ?TYPE(T, ?SIZE8, S)).

-define(TAG0(S), ?TYPE0(?TAG, S)).
-define(TAG1(S), ?TYPE1(?TAG, S)).
-define(TAG2(S), ?TYPE2(?TAG, S)).
-define(TAG4(S), ?TYPE4(?TAG, S)).
-define(TAG8(S), ?TYPE8(?TAG, S)).

-define(SIMPLE(N), ?TYPE0(?SIMPLE, N)).
-define(FLOAT(N), ?TYPE0(?FLOAT, N)).

-define(PINT(S), ?TYPE0(?PINT, S)).
-define(PINT0(S), ?TYPE0(?PINT, S)).
-define(PINT1(S), ?TYPE1(?PINT, S)).
-define(PINT2(S), ?TYPE2(?PINT, S)).
-define(PINT4(S), ?TYPE4(?PINT, S)).
-define(PINT8(S), ?TYPE8(?PINT, S)).

-define(NINT(S), ?TYPE0(?NINT, S)).
-define(NINT0(S), ?TYPE0(?NINT, S)).
-define(NINT1(S), ?TYPE1(?NINT, S)).
-define(NINT2(S), ?TYPE2(?NINT, S)).
-define(NINT4(S), ?TYPE4(?NINT, S)).
-define(NINT8(S), ?TYPE8(?NINT, S)).

-define(BSTR(S), ?TYPE0(?BSTR, S)).
-define(BSTR0(S), ?TYPE0(?BSTR, S)).
-define(BSTR1(S), ?TYPE1(?BSTR, S)).
-define(BSTR2(S), ?TYPE2(?BSTR, S)).
-define(BSTR4(S), ?TYPE4(?BSTR, S)).
-define(BSTR8(S), ?TYPE8(?BSTR, S)).

-define(TSTR(S), ?TYPE0(?TSTR, S)).
-define(TSTR0(S), ?TYPE0(?TSTR, S)).
-define(TSTR1(S), ?TYPE1(?TSTR, S)).
-define(TSTR2(S), ?TYPE2(?TSTR, S)).
-define(TSTR4(S), ?TYPE4(?TSTR, S)).
-define(TSTR8(S), ?TYPE8(?TSTR, S)).

-define(ARRAY(S), ?TYPE0(?ARRAY, S)).
-define(ARRAY0(S), ?TYPE0(?ARRAY, S)).
-define(ARRAY1(S), ?TYPE1(?ARRAY, S)).
-define(ARRAY2(S), ?TYPE2(?ARRAY, S)).
-define(ARRAY4(S), ?TYPE4(?ARRAY, S)).
-define(ARRAY8(S), ?TYPE8(?ARRAY, S)).

-define(MAP(S), ?TYPE0(?MAP, S)).
-define(MAP0(S), ?TYPE0(?MAP, S)).
-define(MAP1(S), ?TYPE1(?MAP, S)).
-define(MAP2(S), ?TYPE2(?MAP, S)).
-define(MAP4(S), ?TYPE4(?MAP, S)).
-define(MAP8(S), ?TYPE8(?MAP, S)).

-define(FLOAT2, ?FLOAT(25)).
-define(FLOAT4, ?FLOAT(26)).
-define(FLOAT8, ?FLOAT(27)).

encode(T) -> iolist_to_binary(enc(T)).

encode_seq(L) -> list_to_binary(enc_seq(L)).


decode(B) ->
    {T, _} = dec(B),
    T.

decode_seq(B) -> dec_seq(B).

enc_seq(L) -> lists:map(fun enc/1, L).

enc(false) -> <<?SIMPLE(20)>>;
enc(true) -> <<?SIMPLE(21)>>;
enc(null) -> <<?SIMPLE(22)>>;
enc(undefined) -> <<?SIMPLE(23)>>;
enc(I) when is_integer(I) -> enc_integer(I);
enc(F) when is_float(F) -> enc_float(F);
enc(B) when is_binary(B) -> enc_binary(B);
enc(L) when is_list(L) -> enc_list(L);
enc(T) when is_tuple(T) -> enc_tuple(T);
enc(M) when is_map(M) -> enc_map(M);
enc(A) when is_atom(A) -> enc_atom(A);
enc(T) -> enc_eterm(T).

dec_seq(<<>>) -> [];
dec_seq(B) ->
    {T, R} = dec(B),
    [T|dec_seq(R)].

dec_int(0, <<I:1/unit:8, R/binary>>) -> {I, R};
dec_int(1, <<I:2/unit:8, R/binary>>) -> {I, R};
dec_int(2, <<I:4/unit:8, R/binary>>) -> {I, R};
dec_int(3, <<I:8/unit:8, R/binary>>) -> {I, R}.

-compile({inline, [neg/1, dec_float/2, dec_float16/1, dec_bin/2]}).

neg({I, R}) -> {?NEG(I), R}.

dec_float(<<16#7FF0:2/unit:8, 0:6/unit:8, R/binary>>, 3) -> {positive_infinity, R};
dec_float(<<16#7FF8:2/unit:8, 0:6/unit:8, R/binary>>, 3) -> {nan, R};
dec_float(<<16#FFF0:2/unit:8, 0:6/unit:8, R/binary>>, 3) -> {negative_infinity, R};
dec_float(<<F/float, R/binary>>, 3) -> {F, R};
dec_float(<<16#7F80:2/unit:8, 0:2/unit:8, R/binary>>, 2) -> {positive_infinity, R};
dec_float(<<16#7FC0:2/unit:8, 0:2/unit:8, R/binary>>, 2) -> {nan, R};
dec_float(<<16#FF80:2/unit:8, 0:2/unit:8, R/binary>>, 2) -> {negative_infinity, R};
dec_float(<<F:32/float, R/binary>>, 2) -> {F, R};
dec_float(<<16#7C00:2/unit:8, R/binary>>, 1) -> {positive_infinity, R};
dec_float(<<16#7E00:2/unit:8, R/binary>>, 1) -> {nan, R};
dec_float(<<16#FC00:2/unit:8, R/binary>>, 1) -> {negative_infinity, R};
dec_float(B, _) -> dec_float16(B).
-ifdef(HAVE_float16).
dec_float16(<<F:16/float, R/binary>>) -> {F, R}.
-else.
dec_float16(<<S:1, E:5, F:10, R/binary>>) ->
    <<T:32/float>> = <<S:1, (E + 16#70):8, F:10, 0:13>>,
    {T, R}.
-endif.

dec_bin(B, S) ->
    <<V:S/binary, R/binary>> = B,
    {V, R}.

dec_big_int(B, S) ->
    <<V:S/unit:8, R/binary>> = B,
    {V, R}.

dec_array(B, S) ->
    {L, R} = dec_array_(B, S),
    {try
         list_to_tuple(L)
     catch
         _:_ -> L
     end,
     R}.

dec_array_(R, 0) -> {[], R};
dec_array_(B, S) ->
    {E, R} = dec(B),
    {L, T} = dec_array_(R, S - 1),
    {[E|L], T}.

dec_map(B, S) ->
    {L, R} = dec_map_(B, S),
    {maps:from_list(L), R}.

dec_map_(R, 0) -> {[], R};
dec_map_(B, S) ->
    {K, VT} = dec(B),
    {V, T} = dec(VT),
    {L, R} = dec_map_(T, S - 1),
    {[{K, V}|L], R}.

dec_map(B) ->
    {L, R} = dec_map_(B),
    {maps:from_list(L), R}.

dec_map_(<<?BREAK, R/binary>>) -> {[], R};
dec_map_(B) ->
    {K, VT} = dec(B),
    {V, T} = dec(VT),
    {L, R} = dec_map_(T),
    {[{K, V}|L], R}.

dec(<<?PINT:3, I:5, B/binary>>) when I < ?SIZE1 -> {I, B};
dec(<<?PINT:3, 2#110:3, S:2, B/binary>>) -> dec_int(S, B);
dec(<<?NINT:3, I:5, B/binary>>) when I < ?SIZE1 -> {?NEG(I), B};
dec(<<?NINT:3, 2#110:3, S:2, B/binary>>) -> neg(dec_int(S, B));
dec(<<?ARRAY:3, ?INDEFINITE:5, B/binary>>) -> dec_array(B);
dec(<<?ARRAY:3, S:5, B/binary>>) when S < ?SIZE1 -> dec_array(B, S);
dec(<<?ARRAY:3, 2#110:3, S:2, B/binary>>) ->
    {I, R} = dec_int(S, B),
    dec_array(R, I);
dec(<<?MAP:3, ?INDEFINITE:5, B/binary>>) -> dec_map(B);
dec(<<?MAP:3, S:5, B/binary>>) when S < ?SIZE1 -> dec_map(B, S);
dec(<<?MAP:3, 2#110:3, S:2, B/binary>>) ->
    {I, R} = dec_int(S, B),
    dec_map(R, I);
dec(<<?TAG:3, ?BIG_PINT:5, 2:3, S:5, B/binary>>) when S < ?SIZE1 -> dec_big_int(B, S);
dec(<<?TAG:3, ?BIG_PINT:5, 2:3, 2#110:3, S:2, B/binary>>) ->
    {I, R} = dec_int(S, B),
    dec_big_int(R, I);
dec(<<?TAG:3, ?BIG_NINT:5, 2:3, S:5, B/binary>>) when S < ?SIZE1 -> neg(dec_big_int(B, S));
dec(<<?TAG:3, ?BIG_NINT:5, 2:3, 2#110:3, S:2, B/binary>>) ->
    {I, R} = dec_int(S, B),
    neg(dec_big_int(R, I));
dec(<<?TAG:3, S:5, B/binary>>) when S < ?SIZE1 -> dec(B);
dec(<<?TAG:3, 2#110:3, 0:2, ?ETERM, ?BSTR:3, ?SIZE1:5, S, B/binary>>) -> dec_eterm(B, S);
dec(<<?TAG:3, 2#110:3, 0:2, ?ETERM, ?BSTR:3, S:5, B/binary>>) when S < ?SIZE1 -> dec_eterm(B, S);
dec(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, ?SIZE1:5, S, B/binary>>) when ?IS_UATOM(T) -> dec_atom(B, utf8, S);
dec(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, S:5, B/binary>>) when ?IS_UATOM(T), S < ?SIZE1 -> dec_atom(B, utf8, S);
dec(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, ?SIZE1:5, S, B/binary>>) when ?IS_ATOM(T) -> dec_atom(B, latin1, S);
dec(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, S:5, B/binary>>) when ?IS_ATOM(T), S < ?SIZE1 -> dec_atom(B, latin1, S);
dec(<<?TAG:3, 2#110:3, S:2, B/binary>>) ->
    I = 1 bsl S,
    <<_:I/binary, R/binary>> = B,
    dec(R);
dec(<<?SIMPLE:3, 2#101:3, I:2, R/binary>>) -> {element(I + 1, {false, true, null, undefined}), R};
dec(<<?FLOAT:3, 2#110:3, S:2, B/binary>>) when S =/= 0 -> dec_float(B, S);
dec(<<2#01:2, _:1, ?INDEFINITE:5, B/binary>>) ->
    {L, R} = dec_array(B),
    {list_to_binary(L), R};
dec(<<2#01:2, _:1, S:5, B/binary>>) when S < ?SIZE1 -> dec_bin(B, S);
dec(<<2#01:2, _:1, 2#110:3, S:2, B/binary>>) ->
    {I, R} = dec_int(S, B),
    dec_bin(R, I);
dec(T) -> error(badarg, [T]).

enc_integer(I) when I >= 1 bsl 64 -> [<<?TAG0(?BIG_PINT)>>, enc_binary(binary:encode_unsigned(I))];
enc_integer(I) when I >= 0 -> enc_int(?PINT, I);
enc_integer(I) ->
    case ?NEG(I) of
        N when N >= 1 bsl 64 -> [<<?TAG0(?BIG_NINT)>>, enc_binary(binary:encode_unsigned(N))];
        N -> enc_int(?NINT, N)
    end.

enc_int(T, I) when I < ?SIZE1 -> <<T:3, I:5>>;
enc_int(T, I) when I < 1 bsl 8 -> <<T:3, ?SIZE1:5, I:1/unit:8>>;
enc_int(T, I) when I < 1 bsl 16 -> <<T:3, ?SIZE2:5, I:2/unit:8>>;
enc_int(T, I) when I < 1 bsl 32 -> <<T:3, ?SIZE4:5, I:4/unit:8>>;
enc_int(T, I) -> <<T:3, ?SIZE8:5, I:8/unit:8>>.

enc_binary(B) -> [enc_int(?BSTR, byte_size(B))|B].

enc_list(L) -> [<<?ARRAY:3, ?INDEFINITE:5>>|enc_list_(L)].

enc_list_([]) -> [?BREAK];
enc_list_([H|T]) -> [enc(H)|enc_list_(T)].

enc_tuple(T) ->
    S = tuple_size(T),
    [enc_int(?ARRAY, S)|enc_array(T, S)].

enc_array(T, S) -> enc_array(T, S, []).

enc_array(_, 0, L) -> L;
enc_array(T, S, L) -> enc_array(T, S - 1, [enc(element(S, T))|L]).

enc_map(M) -> [enc_int(?MAP, map_size(M))|lists:sort(maps:fold(fun(K, V, A) -> [[enc(K), enc(V)]|A] end, [], M))].

-ifdef(HAVE_float16).
enc_float(F) ->
    case <<F:16/float>> of
        <<F:16/float>> = F2 -> <<?FLOAT2, F2/binary>>;
        _ -> case <<F:32/float>> of
                 <<F:32/float>> = F4 -> <<?FLOAT4, F4/binary>>;
                 _ -> <<?FLOAT8, F/float>>
             end
    end.
-else.
enc_float(F) ->
    case <<F:32/float>> of
        <<F:32/float>> = F4 -> <<?FLOAT4, F4/binary>>;
        _ -> <<?FLOAT8, F/float>>
    end.
-endif.

enc_atom(A) ->
    B = atom_to_binary(A, utf8),
    [<<?TAG1(119)>>, enc_int(?TSTR, byte_size(B))|B].

enc_eterm(A) ->
    B = term_to_binary(A),
    [<<?TAG1(?ETERM)>>, enc_int(?BSTR, byte_size(B))|B].

dec_atom(B, E, S) ->
    <<V:S/binary, R/binary>> = B,
    {binary_to_atom(V, E), R}.

dec_eterm(B, S) ->
    <<V:S/binary, R/binary>> = B,
    {binary_to_term(V), R}.

dec_array(<<?BREAK, R/binary>>) -> {[], R};
dec_array(B) ->
    {V, T} = dec(B),
    {L, R} = dec_array(T),
    {[V|L], R}.

-ifdef(TEST).
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
repeat_decode_n(N, Bin) -> decode(Bin), repeat_decode_n(N - 1, Bin).

encode_test_() ->
    lists:map(fun({V, E}) -> {timeout, 30, ?_assertEqual(encode(V), binary:decode_hex(E))};
                 ({T, V, E}) -> {T, timeout, 30, ?_assertEqual(encode(V), binary:decode_hex(E))}
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
    lists:map(fun({V, E}) -> {timeout, 30, ?_assertEqual(V, decode(binary:decode_hex(E)))};
                 ({T, V, E}) -> {T, timeout, 30, ?_assertEqual(V, decode(binary:decode_hex(E)))}
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
-endif.
