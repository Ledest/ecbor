%%% Copyright 2018-2023 Oleksandr Chumachenko <ledest@gmail.com>
%%%
%%% This file is part of Ecbor.
%%%
%%% Ecbor is free software: you can redistribute it and/or modify it
%%% under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Ecbor is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with Ecbor. If not, see <http://www.gnu.org/licenses/>.
-module(ecbor).

-export([encode/1, encode/2, decode/1, decode/2, enc/1, enc/2, dec/1, dec/2]).
-export([encode_seq/1, encode_seq/2, decode_seq/1, decode_seq/2, enc_seq/1, enc_seq/2, dec_seq/1, dec_seq/2]).

-define(SIZE1, 24).
-define(SIZE2, 25).
-define(SIZE4, 26).
-define(SIZE8, 27).

-define(SIZE1(S), ?SIZE1:5, S:1/unit:8).
-define(SIZE2(S), ?SIZE2:5, S:2/unit:8).
-define(SIZE4(S), ?SIZE4:5, S:4/unit:8).

-define(SIZE(S), (1 bsl (S band 2#11))).

-define(DATETIME, 0).
-define(SECONDS, 1).
-define(BIG_PINT, 2).
-define(BIG_NINT, 3).
-define(LIST, 108).
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

-record(opt, {safe = false :: boolean()}).

-spec encode(T::term()) -> binary().
encode(T) -> iolist_to_binary(enc(T)).

-spec encode(T::term(), O::proplists:proplist()) -> binary().
encode(T, []) -> encode(T).

-spec decode(B::binary()) -> term().
decode(B) ->
    {T, _} = dec(B),
    T.

-spec decode(B::binary(), O::proplists:proplist()) -> term().
decode(B, O) ->
    {T, _} = dec(B, O),
    T.

-spec encode_seq(L::list()) -> binary().
encode_seq(L) -> list_to_binary(enc_seq(L)).

-spec encode_seq(L::list(), O::proplists:proplist()) -> binary().
encode_seq(L, []) -> encode_seq(L).

-spec decode_seq(B::binary()) -> list().
decode_seq(B) ->
    {T, _} = dec_seq(B),
    T.

-spec decode_seq(B::binary(), O::proplists:proplist()) -> list().
decode_seq(B, O) ->
    {T, _} = dec_seq(B, O),
    T.

-spec enc_seq(L::list()) -> binary().
enc_seq(L) -> lists:map(fun enc/1, L).

-spec enc_seq(L::list(), O::proplists:proplist()) -> binary().
enc_seq(L, []) -> enc_seq(L).

-spec dec_seq(B::binary()) -> {list(), binary}.
dec_seq(B) -> dec_seq(B, #opt{}).

-spec dec_seq(B::binary(), O::proplists:proplist()) -> {list(), binary()}.
dec_seq(<<>>, _) -> {[], <<>>};
dec_seq(B, O) when is_binary(B) ->
    try dec_(B, O) of
        {D, R} ->
            {L, T} = dec_seq(R, O),
            {[D|L], T}
    catch
        error:badarg -> {[], B};
        C:R -> erlang:C(R)
    end.

-spec dec(B::binary()) -> {term(), binary()}.
dec(B) -> dec_(B, #opt{}).

-spec dec(B::binary(), O::proplists:proplist()) -> {term(), binary()}.
dec(B, O) -> dec_(B, options(O)).

-spec enc(T::term, O::proplists:proplist()) -> iodata().
enc(T, []) -> enc(T).

-spec enc(T::term) -> iodata().
enc(false) -> <<?SIMPLE(20)>>;
enc(true) -> <<?SIMPLE(21)>>;
enc(null) -> <<?SIMPLE(22)>>;
enc(undefined) -> <<?SIMPLE(23)>>;
enc(I) when is_integer(I) -> enc_int(I);
enc(F) when is_float(F) -> enc_float(F);
enc(B) when is_binary(B) -> enc_binary(B);
enc(L) when is_list(L) -> enc_list(L);
enc(T) when is_tuple(T) -> enc_tuple(T);
enc(M) when is_map(M) -> enc_map(M);
enc(A) when is_atom(A) -> enc_atom(A);
enc(T) -> enc_term(T).

dec_(<<?PINT:3, I:5, B/binary>>, _) when I < ?SIZE1 -> {I, B};
dec_(<<?PINT:3, 2#110:3, S:2, B/binary>>, _) -> dec_int(S, B);
dec_(<<?NINT:3, I:5, B/binary>>, _) when I < ?SIZE1 -> {?NEG(I), B};
dec_(<<?NINT:3, 2#110:3, S:2, B/binary>>, _) -> neg(dec_int(S, B));
dec_(<<?ARRAY:3, ?INDEFINITE:5, B/binary>>, O) -> dec_array(B, O);
dec_(<<?ARRAY:3, S:5, B/binary>>, O) when S < ?SIZE1 -> dec_array(B, O, S);
dec_(<<?ARRAY:3, 2#110:3, S:2, B/binary>>, O) ->
    {I, R} = dec_int(S, B),
    dec_array(R, O, I);
dec_(<<?MAP:3, ?INDEFINITE:5, B/binary>>, O) -> dec_map(B, O);
dec_(<<?MAP:3, S:5, B/binary>>, O) when S < ?SIZE1 -> dec_map(B, O, S);
dec_(<<?MAP:3, 2#110:3, S:2, B/binary>>, O) ->
    {I, R} = dec_int(S, B),
    dec_map(R, O, I);
dec_(<<?TAG:3, ?BIG_PINT:5, 2:3, S:5, B/binary>>, _) when S < ?SIZE1 -> dec_big_int(B, S);
dec_(<<?TAG:3, ?BIG_PINT:5, 2:3, 2#110:3, S:2, B/binary>>, _) ->
    {I, R} = dec_int(S, B),
    dec_big_int(R, I);
dec_(<<?TAG:3, ?BIG_NINT:5, 2:3, S:5, B/binary>>, _) when S < ?SIZE1 -> neg(dec_big_int(B, S));
dec_(<<?TAG:3, ?BIG_NINT:5, 2:3, 2#110:3, S:2, B/binary>>, _) ->
    {I, R} = dec_int(S, B),
    neg(dec_big_int(R, I));
dec_(<<?TAG:3, S:5, B/binary>>, O) when S < ?SIZE1 -> dec_(B, O);
dec_(<<?TAG:3, 2#110:3, 0:2, ?LIST, ?ARRAY:3, ?INDEFINITE:5, B/binary>>, O) -> dec_improper_list(B, O);
dec_(<<?TAG:3, 2#110:3, 0:2, ?ETERM, ?BSTR:3, ?SIZE1(S), B/binary>>, O) -> dec_term(B, O, S);
dec_(<<?TAG:3, 2#110:3, 0:2, ?ETERM, ?BSTR:3, S:5, B/binary>>, O) when S < ?SIZE1 -> dec_term(B, O, S);
dec_(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, ?SIZE1(S), B/binary>>, O) when ?IS_UATOM(T) -> dec_atom(B, O, utf8, S);
dec_(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, S:5, B/binary>>, O) when ?IS_UATOM(T), S < ?SIZE1 -> dec_atom(B, O, utf8, S);
dec_(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, ?SIZE1(S), B/binary>>, O) when ?IS_ATOM(T) -> dec_atom(B, O, latin1, S);
dec_(<<?TAG:3, 2#110:3, 0:2, T, ?TSTR:3, S:5, B/binary>>, O) when ?IS_ATOM(T), S < ?SIZE1 -> dec_atom(B, O, latin1, S);
dec_(<<?TAG:3, 2#110:3, S:2, B/binary>>, O) ->
    I = 1 bsl S,
    <<_:I/binary, R/binary>> = B,
    dec_(R, O);
dec_(<<?SIMPLE:3, 2#101:3, 0:2, R/binary>>, _) -> {false, R};
dec_(<<?SIMPLE:3, 2#101:3, 1:2, R/binary>>, _) -> {true, R};
dec_(<<?SIMPLE:3, 2#101:3, 2:2, R/binary>>, _) -> {null, R};
dec_(<<?SIMPLE:3, 2#101:3, 3:2, R/binary>>, _) -> {undefined, R};
dec_(<<?FLOAT:3, 2#110:3, S:2, B/binary>>, _) when S =/= 0 -> dec_float(B, S);
dec_(<<2#01:2, _:1, ?INDEFINITE:5, B/binary>>, O) ->
    {L, R} = dec_array(B, O),
    {list_to_binary(L), R};
dec_(<<2#01:2, _:1, S:5, B/binary>>, _) when S < ?SIZE1 -> dec_bin(B, S);
dec_(<<2#01:2, _:1, 2#110:3, S:2, B/binary>>, _) ->
    {I, R} = dec_int(S, B),
    dec_bin(R, I);
dec_(T, _) -> error(badarg, [T]).

%% Internal

enc_int(I) when I >= 1 bsl 64 -> [<<?TAG0(?BIG_PINT)>>, enc_binary(binary:encode_unsigned(I))];
enc_int(I) when I >= 0 -> enc_int(?PINT, I);
enc_int(I) ->
    case ?NEG(I) of
        N when N >= 1 bsl 64 -> [<<?TAG0(?BIG_NINT)>>, enc_binary(binary:encode_unsigned(N))];
        N -> enc_int(?NINT, N)
    end.

dec_int(0, <<I:1/unit:8, R/binary>>) -> {I, R};
dec_int(1, <<I:2/unit:8, R/binary>>) -> {I, R};
dec_int(2, <<I:4/unit:8, R/binary>>) -> {I, R};
dec_int(3, <<I:8/unit:8, R/binary>>) -> {I, R}.

-compile({inline, neg/1}).
neg({I, R}) -> {?NEG(I), R}.

-compile({inline, dec_float/2}).
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

-compile({inline, dec_float16/1}).
-ifdef(HAVE_float16).
dec_float16(<<F:16/float, R/binary>>) -> {F, R}.
-else.
dec_float16(<<S:1, E:5, F:10, R/binary>>) ->
    <<T:32/float>> = <<S:1, (E + 16#70):8, F:10, 0:13>>,
    {T, R}.
-endif.

-compile({inline, dec_bin/2}).
dec_bin(B, S) ->
    <<V:S/binary, R/binary>> = B,
    {V, R}.

dec_big_int(B, S) ->
    <<V:S/unit:8, R/binary>> = B,
    {V, R}.

dec_array(B, O, S) ->
    {L, R} = dec_array_(B, O, S),
    {try
         list_to_tuple(L)
     catch
         _:_ -> L
     end,
     R}.

dec_array_(R, _, 0) -> {[], R};
dec_array_(B, O, S) ->
    {E, R} = dec_(B, O),
    {L, T} = dec_array_(R, O, S - 1),
    {[E|L], T}.

dec_map(B, O, S) ->
    {L, R} = dec_map_(B, O, S),
    {maps:from_list(L), R}.

dec_map_(R, _, 0) -> {[], R};
dec_map_(B, O, S) ->
    {K, VT} = dec_(B, O),
    {V, T} = dec_(VT, O),
    {L, R} = dec_map_(T, O, S - 1),
    {[{K, V}|L], R}.

dec_map(B, O) ->
    {L, R} = dec_map_(B, O),
    {maps:from_list(L), R}.

dec_map_(<<?BREAK, R/binary>>, _) -> {[], R};
dec_map_(B, O) ->
    {K, VT} = dec_(B, O),
    {V, T} = dec_(VT, O),
    {L, R} = dec_map_(T, O),
    {[{K, V}|L], R}.

enc_int(T, I) when I < ?SIZE1 -> <<T:3, I:5>>;
enc_int(T, I) when I < 1 bsl 8 -> <<T:3, ?SIZE1(I)>>;
enc_int(T, I) when I < 1 bsl 16 -> <<T:3, ?SIZE2(I)>>;
enc_int(T, I) when I < 1 bsl 32 -> <<T:3, ?SIZE4(I)>>;
enc_int(T, I) -> <<T:3, ?SIZE8:5, I:8/unit:8>>.

enc_binary(B) -> [enc_int(?BSTR, byte_size(B))|B].

enc_list(L) ->
    case enc_list_(L) of
        {true, R} -> [<<?ARRAY:3, ?INDEFINITE:5>>|R];
        {_false, R} -> [<<?TAG1(?LIST)>>, <<?ARRAY:3, ?INDEFINITE:5>>|R]
    end.

enc_list_([H|T]) ->
    {P, L} = enc_list_(T),
    {P, [enc(H)|L]};
enc_list_([]) -> {true, [?BREAK]};
enc_list_(T) -> {false, [enc(T), ?BREAK]}.

-compile({inline, enc_tuple/1}).
enc_tuple(T) ->
    S = tuple_size(T),
    [enc_int(?ARRAY, S)|enc_array(T, S)].

-compile({inline, enc_array/2}).
enc_array(T, S) -> enc_array(T, S, []).

enc_array(_, 0, L) -> L;
enc_array(T, S, L) -> enc_array(T, S - 1, [enc(element(S, T))|L]).

-compile({inline, enc_map/1}).
enc_map(M) -> [enc_int(?MAP, map_size(M))|lists:sort(maps:fold(fun(K, V, A) -> [[enc(K), enc(V)]|A] end, [], M))].

-compile({inline, enc_float/1}).
-ifdef(HAVE_float16).
enc_float(F) ->
    case <<F:16/float>> of
        <<F:16/float>> = F2 -> <<?FLOAT2, F2/binary>>;
        _ -> enc_float_(F)
    end.
-else.
enc_float(F) -> enc_float_(F).
-endif.

-compile({inline, enc_float_/1}).
enc_float_(F) ->
    case <<F:32/float>> of
        <<F:32/float>> = F4 -> <<?FLOAT4, F4/binary>>;
        _ -> <<?FLOAT8, F/float>>
    end.

-compile({inline, enc_atom/1}).
enc_atom(A) ->
    B = atom_to_binary(A, utf8),
    [<<?TAG1(119)>>, enc_int(?TSTR, byte_size(B))|B].

-compile({inline, enc_term/1}).
enc_term(A) ->
    B = term_to_binary(A),
    [<<?TAG1(?ETERM)>>, enc_int(?BSTR, byte_size(B))|B].

dec_atom(B, O, E, S) ->
    <<V:S/binary, R/binary>> = B,
    {case O of
         #opt{safe = true} ->
                 try
                     binary_to_existing_atom(V, E)
                 catch
                     _:_ -> V
                 end;
         _ -> binary_to_atom(V, E)
     end,
     R}.

dec_term(B, O, S) ->
    <<V:S/binary, R/binary>> = B,
    {dec_term(V, O), R}.

-compile({inline, dec_term/2}).
dec_term(<<?ETERM, _/binary>> = B, #opt{safe = true}) ->
    try
        binary_to_term(B, [safe])
    catch
        error:badarg -> B
    end;
dec_term(<<?ETERM, _/binary>> = B, _) -> binary_to_term(B);
dec_term(B, _) -> B.

dec_array(<<?BREAK, R/binary>>, _) -> {[], R};
dec_array(B, O) ->
    {V, T} = dec_(B, O),
    {L, R} = dec_array(T, O),
    {[V|L], R}.

dec_improper_list(B, O) ->
    case dec_(B, O) of
        {V, <<?BREAK, R/binary>>} -> {V, R};
        {V, T} ->
            {L, R} = dec_improper_list(T, O),
            {[V|L], R}
    end.

-spec options(L::proplists:proplist()) -> #opt{}.
options(L) ->
    lists:foldl(fun(safe, A) -> A#opt{safe = true};
                   ({safe, V}, A) when is_boolean(V) -> A#opt{safe = V};
                   (_, A) -> A
                end,
                #opt{}, L).
