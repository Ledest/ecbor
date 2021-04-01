-module(ecbor).

-export([encode/1, decode/1]).
-export([enc/1, dec/1]).

-define(SIZE1, 24).
-define(SIZE2, 25).
-define(SIZE4, 26).
-define(SIZE8, 27).

-define(TYPE(T), (T bsl 5)).

-define(INDEFINITE, 16#1F).
-define(BREAK, 16#FF).

-define(TYPE(T, L, S, B), T:3, L:5, S:B/unit:8).
-define(TYPE0(T, S), T:3, S:5).
-define(TYPE1(T, S), ?TYPE(T, ?SIZE1, S, 1)).
-define(TYPE2(T, S), ?TYPE(T, ?SIZE2, S, 2)).
-define(TYPE4(T, S), ?TYPE(T, ?SIZE4, S, 4)).
-define(TYPE8(T, S), ?TYPE(T, ?SIZE8, S, 8)).

-define(TAG0(S), ?TYPE0(?TAG, S)).
-define(TAG1(S), ?TYPE1(?TAG, S)).
-define(TAG2(S), ?TYPE2(?TAG, S)).
-define(TAG4(S), ?TYPE4(?TAG, S)).
-define(TAG8(S), ?TYPE8(?TAG, S)).

-define(PINT, 0).
-define(NINT, 1).
-define(BSTR, 2).
-define(TSTR, 3).
-define(ARRAY, 4).
-define(MAP, 5).
-define(TAG, 6).
-define(SIMPLE, 7).

-define(SIMPLE(N), ?TYPE0(?SIMPLE, N)).

-define(PINT(S), ?TYPE0(?PINT, S)).
-define(NINT(S), ?TYPE0(?NINT, S)).
-define(BSTR(S), ?TYPE0(?BSTR, S)).
-define(TSTR(S), ?TYPE0(?TSTR, S)).
-define(ARRAY(S), ?TYPE0(?ARRAY, S)).
-define(MAP(S), ?TYPE0(?MAP, S)).

-define(FLOAT2, ?SIMPLE(25)).
-define(FLOAT4, ?SIMPLE(26)).
-define(FLOAT8, ?SIMPLE(27)).

encode(T) -> iolist_to_binary(enc(T)).

decode(B) ->
    {T, _} = dec(B),
    T.

enc(false) -> <<?SIMPLE(20)>>;
enc(true) -> <<?SIMPLE(21)>>;
enc(null) -> <<?SIMPLE(22)>>;
enc(undefined) -> <<?SIMPLE(23)>>;
enc(I) when is_integer(I) -> enc_integer(I);
enc(F) when is_float(F) -> enc_float(F);
enc(B) when is_binary(B) -> enc_binary(B);
enc(L) when is_list(L) -> enc_list(L);
enc({{Y, Month, D} = Date, {H, M, S}} = DT) when is_integer(Y), is_integer(Month), is_integer(D),
                                                 is_integer(H), is_integer(M), is_integer(S),
                                                 Month =< 12, D =< 31, H < 24, M < 60, S < 60 ->
    case calendar:valid_date(Date) of
        true -> enc_datetime(DT);
        _false -> enc_tuple(DT)
    end;
enc(T) when is_tuple(T) -> enc_tuple(T);
enc(M) when is_map(M) -> enc_map(M);
enc(A) when is_atom(A) -> enc_atom(A);
enc(T) -> error(badarg, [T]).

dec(<<?SIMPLE(20), R/binary>>) -> {false, R};
dec(<<?SIMPLE(21), R/binary>>) -> {true, R};
dec(<<?SIMPLE(22), R/binary>>) -> {null, R};
dec(<<?SIMPLE(23), R/binary>>) -> {undefined, R};
dec(<<?FLOAT8, F/float, B/binary>>) -> {F, B};
dec(<<?FLOAT4, F:32/float, B/binary>>) -> {F, B};
dec(<<?FLOAT2, _/binary>> = B) -> dec_float16(B);
dec(<<?PINT(S), B/binary>>) -> dec_pos_integer(S, B);
dec(<<?NINT(S), B/binary>>) -> dec_neg_integer(S, B);
dec(<<?BSTR(?INDEFINITE), B/binary>>) -> dec_binaries(B);
dec(<<?BSTR(S), B/binary>>) -> dec_binary(S, B);
dec(<<?TSTR(?INDEFINITE), B/binary>>) -> dec_binaries(B);
dec(<<?TSTR(S), B/binary>>) -> dec_binary(S, B);
dec(<<?ARRAY(?INDEFINITE), B/binary>>) -> dec_array(B);
dec(<<?ARRAY(S), B/binary>>) -> dec_tuple(S, B);
dec(<<?MAP(?INDEFINITE), B/binary>>) -> dec_map(B);
dec(<<?MAP(S), B/binary>>) -> dec_map(S, B);
dec(<<?TAG0(0), B/binary>>) -> dec_datetime(B);
dec(<<?TAG0(1), B/binary>>) -> dec_seconds(B);
dec(<<?TAG0(2), ?BSTR(S), B/binary>>) -> dec_big_pos_integer(S, B);
dec(<<?TAG0(3), ?BSTR(S), B/binary>>) -> dec_big_neg_integer(S, B);
dec(<<?TAG0(6), ?TSTR(S), B/binary>>) -> dec_atom(S, B);
dec(<<?TAG1(_), B/binary>>) -> dec(B);
dec(<<?TAG2(_), B/binary>>) -> dec(B);
dec(<<?TAG4(_), B/binary>>) -> dec(B);
dec(<<?TAG8(_), B/binary>>) -> dec(B);
dec(<<?TAG0(_), B/binary>>) -> dec(B);
dec(T) -> error(badarg, [T]).

-compile({inline, dec_float16/1}).
-ifdef(HAVE_float16).
dec_float16(<<_, F:16/float, B/binary>>) -> {F, B}.
-else.
dec_float16(<<_, S:1, E:5, F:10, B/binary>>) ->
    <<T:32/float>> = <<S:1, (E + (127 - 15)):8, F:10, 0:13>>,
    {T, B}.
-endif.

enc_integer(I) when I >= 0 -> enc_pos_integer(I);
enc_integer(I) -> enc_neg_integer(I).

enc_binary(B) -> enc_binary(B, byte_size(B)).

enc_binary(B, S) when S < 24 -> <<?TYPE0(?BSTR, S), B/binary>>;
enc_binary(B, S) when S < 16#100 -> <<?TYPE1(?BSTR, S), B/binary>>;
enc_binary(B, S) when S < 16#10000 -> <<?TYPE2(?BSTR, S), B/binary>>;
enc_binary(B, S) when S < 16#100000000 -> <<?TYPE4(?BSTR, S), B/binary>>;
enc_binary(B, S) when S < 16#10000000000000000 -> <<?TYPE8(?BSTR, S), B/binary>>.

enc_string(B) -> enc_string(B, byte_size(B)).

enc_string(B, S) when S < 24 -> <<?TYPE0(?TSTR, S), B/binary>>;
enc_string(B, S) when S < 16#100 -> <<?TYPE1(?TSTR, S), B/binary>>;
enc_string(B, S) when S < 16#10000 -> <<?TYPE2(?TSTR, S), B/binary>>;
enc_string(B, S) when S < 16#100000000 -> <<?TYPE4(?TSTR, S), B/binary>>;
enc_string(B, S) when S < 16#10000000000000000 -> <<?TYPE8(?TSTR, S), B/binary>>.

enc_list(L) -> [<<?ARRAY(?INDEFINITE)>>|encode_list(L)].

encode_list([H|L]) -> [enc(H)|encode_list(L)];
encode_list([]) -> [?BREAK].

enc_tuple(T) ->
    S = tuple_size(T),
    [if
         S < 24 -> <<?TYPE0(?ARRAY, S)>>;
         S < 16#100 -> <<?TYPE1(?ARRAY, S)>>;
         S < 16#10000 -> <<?TYPE2(?ARRAY, S)>>;
         S < 16#100000000 -> <<?TYPE4(?ARRAY, S)>>;
         S < 16#10000000000000000 -> <<?TYPE8(?ARRAY, S)>>
     end|enc_tuple(T, S, [])].

enc_tuple(_, 0, A) -> A;
enc_tuple(T, S, A) -> enc_tuple(T, S - 1, [enc(element(S, T))|A]).

enc_map(M) ->
    [case map_size(M) of
         S when S < 24 -> <<?TYPE0(?MAP, S)>>;
         S when S < 16#100 -> <<?TYPE1(?MAP, S)>>;
         S when S < 16#10000 -> <<?TYPE2(?MAP, S)>>;
         S when S < 16#100000000 -> <<?TYPE4(?MAP, S)>>;
         S when S < 16#10000000000000000 -> <<?TYPE8(?MAP, S)>>
     end|lists:sort(maps:fold(fun(K, V, A) -> [[enc(K), enc(V)]|A] end, [], M))].

enc_float(F) -> <<?FLOAT8, F/float>>.

enc_atom(A) -> [?TYPE(?TAG) + 6, enc_string(atom_to_binary(A, utf8))].

enc_datetime(DT) ->
    I = calendar:datetime_to_gregorian_seconds(DT) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    [?TYPE(?TAG) + 1, enc_integer(I)].

enc_pos_integer(I) when I < 24 -> <<?TYPE0(?PINT, I)>>;
enc_pos_integer(I) when I < 16#100 -> <<?TYPE1(?PINT, I)>>;
enc_pos_integer(I) when I < 16#10000 -> <<?TYPE2(?PINT, I)>>;
enc_pos_integer(I) when I < 16#100000000 -> <<?TYPE4(?PINT, I)>>;
enc_pos_integer(I) when I < 16#10000000000000000 -> <<?TYPE8(?PINT, I)>>;
enc_pos_integer(I) -> [?TYPE(?TAG) + 2, enc_binary(binary:encode_unsigned(I))].

enc_neg_integer(I) when I >= -24 -> <<?TYPE0(?NINT, (-1 - I))>>;
enc_neg_integer(I) when I >= -16#100 -> <<?TYPE1(?NINT, (-1 - I))>>;
enc_neg_integer(I) when I >= -16#10000 -> <<?TYPE2(?NINT, (-1 - I))>>;
enc_neg_integer(I) when I >= -16#100000000 -> <<?TYPE4(?NINT, (-1 - I))>>;
enc_neg_integer(I) when I >= -16#10000000000000000 -> <<?TYPE8(?NINT, (-1 - I))>>;
enc_neg_integer(I) -> [?TYPE(?TAG) + 3, enc_binary(binary:encode_unsigned(-1 - I))].

dec_pos_integer(S, B) -> sb(S, B).

dec_neg_integer(S, B) ->
    {I, R} = sb(S, B),
    {-1 - I, R}.

dec_big_pos_integer(S, B) ->
    {I, R} = dec_binary(S, B),
    {binary:decode_unsigned(I), R}.

dec_big_neg_integer(S, B) ->
    {I, R} = dec_big_pos_integer(S, B),
    {-1 -I, R}.

dec_binary(S, B) ->
    {N, R} = sb(S, B),
    split_binary(R, N).

dec_atom(S, B) ->
    {A, R} = dec_binary(S, B),
    {binary_to_atom(A, utf8), R}.

dec_binaries(B) ->
    {L, R} = dec_array(B),
    {list_to_binary(L), R}.

dec_array(<<?BREAK, R/binary>>) -> {[], R};
dec_array(B) ->
    {T, R} = dec(B),
    {A, E} = dec_array(R),
    {[T|A], E}.

dec_array(S, B) ->
    {N, R} = sb(S, B),
    decode_array(N, R).

decode_array(0, B) -> {[], B};
decode_array(S, B) ->
    {T, R} = dec(B),
    {A, E} = decode_array(S - 1, R),
    {[T|A], E}.

dec_tuple(S, B) ->
    {L, R} = dec_array(S, B),
    {list_to_tuple(L), R}.

dec_map(S, B) ->
    {N, R} = sb(S, B),
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

dec_seconds(B) ->
    {S, R} = dec(B),
    {calendar:system_time_to_universal_time(round(S), seconds), R}.

dec_datetime(B) ->
    {S, R} = dec(B),
    {calendar:system_time_to_universal_time(calendar:rfc3339_to_system_time(binary_to_list(S)), seconds), R}.

sb(S, B) when S < 24 -> {S, B};
sb(S, B) ->
    N = 1 bsl (S band 2#11),
    <<I:N/unit:8, R/binary>> = B,
    {I, R}.
