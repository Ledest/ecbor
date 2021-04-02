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
-define(ATOM, 6).

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

-define(TAG_DATETIME, ?TAG0(?DATETIME)).
-define(TAG_SECONDS, ?TAG0(?SECONDS)).
-define(TAG_BIG_PINT, ?TAG0(?BIG_PINT)).
-define(TAG_BIG_NINT, ?TAG0(?BIG_NINT)).
-define(TAG_ATOM, ?TAG0(?ATOM)).

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
enc(T) -> error(badarg, [T]).

dec_seq(<<>>) -> [];
dec_seq(B) ->
    {T, R} = dec(B),
    [T|dec_seq(R)].

dec(<<?SIMPLE(20), R/binary>>) -> {false, R};
dec(<<?SIMPLE(21), R/binary>>) -> {true, R};
dec(<<?SIMPLE(22), R/binary>>) -> {null, R};
dec(<<?SIMPLE(23), R/binary>>) -> {undefined, R};
dec(<<?FLOAT8, F/float, R/binary>>) -> {F, R};
dec(<<?FLOAT4, F:32/float, R/binary>>) -> {F, R};
dec(<<?FLOAT2, F:2/binary, R/binary>>) -> {dec_float16(F), R};
dec(<<?PINT(S), B/binary>>) -> dec_pos_integer(B, S);
dec(<<?NINT(S), B/binary>>) -> dec_neg_integer(B, S);
dec(<<?BSTR(?INDEFINITE), B/binary>>) -> dec_binaries(B);
dec(<<?BSTR(S), B/binary>>) -> dec_binary(B, S);
dec(<<?TSTR(?INDEFINITE), B/binary>>) -> dec_binaries(B);
dec(<<?TSTR(S), B/binary>>) -> dec_binary(B, S);
dec(<<?ARRAY(?INDEFINITE), B/binary>>) -> dec_array(B);
dec(<<?ARRAY(S), B/binary>>) -> dec_tuple(B, S);
dec(<<?MAP(?INDEFINITE), B/binary>>) -> dec_map(B);
dec(<<?MAP(S), B/binary>>) -> dec_map(B, S);
dec(<<?TAG_BIG_PINT, ?BSTR(S), B/binary>>) -> dec_big_pos_integer(B, S);
dec(<<?TAG_BIG_NINT, ?BSTR(S), B/binary>>) -> dec_big_neg_integer(B, S);
dec(<<?TAG_ATOM, ?TSTR0(S), B/binary>>) when S < 24 -> dec_atom(B, S);
dec(<<?TAG_ATOM, ?TSTR1(S), B/binary>>) -> dec_atom(B, S);
dec(<<?TAG1(_), B/binary>>) -> dec(B);
dec(<<?TAG2(_), B/binary>>) -> dec(B);
dec(<<?TAG4(_), B/binary>>) -> dec(B);
dec(<<?TAG8(_), B/binary>>) -> dec(B);
dec(<<?TAG0(_), B/binary>>) -> dec(B);
dec(T) -> error(badarg, [T]).

-compile({inline, dec_float16/1}).
-ifdef(HAVE_float16).
dec_float16(<<F:16/float>>) -> F.
-else.
dec_float16(<<S:1, E:5, F:10>>) ->
    <<T:32/float>> = <<S:1, (E + (127 - 15)):8, F:10, 0:13>>,
    T.
-endif.

enc_integer(I) when I >= 0 -> enc_pos_integer(I);
enc_integer(I) -> enc_neg_integer(I).

enc_binary(B) ->
    [case byte_size(B) of
         S when S < 24 -> <<?BSTR0(S)>>;
         S when S < 16#100 -> <<?BSTR1(S)>>;
         S when S < 16#10000 -> <<?BSTR2(S)>>;
         S when S < 16#100000000 -> <<?BSTR4(S)>>;
         S -> <<?BSTR8(S)>>
     end,
     B].

enc_list(L) -> [<<?ARRAY(?INDEFINITE)>>|lists:foldr(fun(E, A) -> [enc(E)|A] end, [?BREAK], L)].

enc_tuple(T) ->
    S = tuple_size(T),
    [if
         S < 24 -> <<?ARRAY0(S)>>;
         S < 16#100 -> <<?ARRAY1(S)>>;
         S < 16#10000 -> <<?ARRAY2(S)>>;
         true -> <<?ARRAY4(S)>>
     end|enc_tuple(T, S, [])].

enc_tuple(_, 0, A) -> A;
enc_tuple(T, S, A) -> enc_tuple(T, S - 1, [enc(element(S, T))|A]).

enc_map(M) ->
    [case map_size(M) of
         S when S < 24 -> <<?MAP0(S)>>;
         S when S < 16#100 -> <<?MAP1(S)>>;
         S when S < 16#10000 -> <<?MAP2(S)>>;
         S when S < 16#100000000 -> <<?MAP4(S)>>;
         S -> <<?MAP8(S)>>
     end|lists:sort(maps:fold(fun(K, V, A) -> [[enc(K), enc(V)]|A] end, [], M))].

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
    [<<?TAG_ATOM>>,
     case byte_size(B) of
         S when S < 24 -> <<?TSTR0(S)>>;
         S -> <<?TSTR1(S)>>
     end,
     B].

enc_pos_integer(I) when I < 24 -> <<?PINT0(I)>>;
enc_pos_integer(I) when I < 16#100 -> <<?PINT1(I)>>;
enc_pos_integer(I) when I < 16#10000 -> <<?PINT2(I)>>;
enc_pos_integer(I) when I < 16#100000000 -> <<?PINT4(I)>>;
enc_pos_integer(I) when I < 16#10000000000000000 -> <<?PINT8(I)>>;
enc_pos_integer(I) -> [<<?TAG_BIG_PINT>>, enc_binary(binary:encode_unsigned(I))].

enc_neg_integer(I) when I >= -24 -> <<?NINT0(?NEG(I))>>;
enc_neg_integer(I) when I >= -16#100 -> <<?NINT1(?NEG(I))>>;
enc_neg_integer(I) when I >= -16#10000 -> <<?NINT2(?NEG(I))>>;
enc_neg_integer(I) when I >= -16#100000000 -> <<?NINT4(?NEG(I))>>;
enc_neg_integer(I) when I >= -16#10000000000000000 -> <<?NINT8(?NEG(I))>>;
enc_neg_integer(I) -> [<<?TAG_BIG_NINT>>, enc_binary(binary:encode_unsigned(?NEG(I)))].

dec_pos_integer(B, S) -> bs(B, S).

dec_neg_integer(B, S) ->
    {I, R} = bs(B, S),
    {?NEG(I), R}.

dec_big_pos_integer(B, S) ->
    {I, R} = dec_binary(B, S),
    {binary:decode_unsigned(I), R}.

dec_big_neg_integer(B, S) ->
    {I, R} = dec_big_pos_integer(B, S),
    {?NEG(I), R}.

dec_binary(B, S) ->
    {N, R} = bs(B, S),
    split_binary(R, N).

dec_atom(B, S) ->
    {A, R} = split_binary(B, S),
    {binary_to_atom(A), R}.

dec_binaries(B) ->
    {L, R} = dec_array(B),
    {list_to_binary(L), R}.

dec_array(<<?BREAK, R/binary>>) -> {[], R};
dec_array(B) ->
    {T, R} = dec(B),
    {A, E} = dec_array(R),
    {[T|A], E}.

dec_array(B, S) ->
    {N, R} = bs(B, S),
    decode_array(N, R).

decode_array(0, B) -> {[], B};
decode_array(S, B) ->
    {T, R} = dec(B),
    {A, E} = decode_array(S - 1, R),
    {[T|A], E}.

dec_tuple(B, S) ->
    {L, R} = dec_array(B, S),
    {list_to_tuple(L), R}.

dec_map(B, S) ->
    {N, R} = bs(B, S),
    decode_map(N, R).

decode_map(0, B) -> {#{}, B};
decode_map(S, B) ->
    {K, R1} = dec(B),
    {V, R2} = dec(R1),
    {M, R} = decode_map(S - 1, R2),
    {M#{K => V}, R}.

dec_map(<<?BREAK, R/binary>>) -> {#{}, R};
dec_map(B) ->
    {K, R1} = dec(B),
    {V, R2} = dec(R1),
    {M, R} = dec_map(R2),
    {M#{K => V}, R}.

bs(B, S) when S < 24 -> {S, B};
bs(B, S) when S =< 27 -> split_binary(B, ?SIZE(S)).
