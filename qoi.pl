:- module(qoi, [qoi_encode/2, qoi_encode_bytes/2, qoi_decode/2]).

:- use_module(library(dcg/basics)).

qoi_decode_uint32(N) -->
   [W1, W2, W3, W4],
   {N is W1 << 24 + W2 << 16 + W3 << 8 + W4}.

qoi_decode_header(header(W, H, C, Colorspace)) -->
   "qoif",
   qoi_decode_uint32(W),
   qoi_decode_uint32(H),
   [C, Colorspace].

qoi_decode(image(header(W, H, C, Co), Data)) -->
   qoi_decode_header(header(W, H, C, Co)),
   {  Arity is W*H,
      compound_name_arity(Data, data, Arity),
      length(Args, 64),
      maplist(=(color(0, 0, 0, 0)), Args),
      compound_name_arguments(Colors, colors, Args),
      Prev = color(0, 0, 0, 255)
   },
   qoi_decode_edgecase(Colors, Prev),
   qoi_decode_op(Data, 1, Arity, Colors, Prev),
   [0, 0, 0, 0, 0, 0, 0, 1].

qoi_decode_edgecase(Colors, Color), [Op] -->
   { Color = color(R, G, B, A) },
   [Op],
   { StartOp is Op >> 6 },
   (  { StartOp == 0b11 }
   -> {  Index is ((R * 3 + G * 5 + B * 7 + A * 11) mod 64) + 1,
         setarg(Index, Colors, Color)
      }
   ;  []
   ).

qoi_decode_op(Data, Counter, Arity, Colors, Prev) -->
   [Op],
   (  { Op == 0b11111110 }
   -> qoi_decode_op_rgb(Data, Counter, Arity, Colors, Prev)
   ;  { Op == 0b11111111 }
   -> qoi_decode_op_rgba(Data, Counter, Arity, Colors)
   ;  qoi_decode_op_short(Data, Counter, Arity, Colors, Prev, Op)
   ).
qoi_decode_op_short(Data, Counter, Arity, Colors, Prev, Op) -->
   { StartOp is Op >> 6 },
   (  { StartOp == 0b11 }
   -> { RunLength is Op /\ 0b00111111 },
      qoi_decode_op_runlength(Data, Counter, Arity, Colors, Prev, RunLength)
   ;  { StartOp == 0b00 }
   -> qoi_decode_op_index(Data, Counter, Arity, Colors, Op)
   ;  { StartOp == 0b01 }
   -> qoi_decode_op_diff(Data, Counter, Arity, Colors, Prev, Op)
   ;  qoi_decode_op_luma(Data, Counter, Arity, Colors, Prev, Op)
   ).
   
qoi_decode_op_index(Data, Counter, Arity, Colors, Op) -->
   {  Index is Op /\ 0b00111111 + 1,
      arg(Index, Colors, Color),
      arg(Counter, Data, Color)
   },
   qoi_decode_op_end(Data, Counter, Arity, Colors, Color).
qoi_decode_op_diff(Data, Counter, Arity, Colors, color(R, G, B, A), Op) -->
   {  R1 is (R + ((Op /\ 0b00110000) >> 4) - 2) mod 256,
      G1 is (G + ((Op /\ 0b00001100) >> 2) - 2) mod 256,
      B1 is (B + (Op /\ 0b00000011) - 2) mod 256
   },
   qoi_decode_new_color(Data, Counter, Arity, Colors, R1, G1, B1, A).
qoi_decode_op_luma(Data, Counter, Arity, Colors, color(R, G, B, A), Op) -->
   [DR_DG_DB_DG],
   {  DG is Op /\ 0b00111111 - 32,
      G1 is (DG + G) mod 256,
      R1 is (((DR_DG_DB_DG /\ 0b11110000) >> 4) - 8 + R + DG) mod 256,
      B1 is ((DR_DG_DB_DG /\ 0b00001111) - 8 + B + DG) mod 256
   },
   qoi_decode_new_color(Data, Counter, Arity, Colors, R1, G1, B1, A).

qoi_decode_op_rgb(Data, Counter, Arity, Colors, Prev) -->
   [R, G, B],
   { arg(4, Prev, A) },
   qoi_decode_new_color(Data, Counter, Arity, Colors, R, G, B, A).

qoi_decode_op_rgba(Data, Counter, Arity, Colors) -->
   [R, G, B, A],
   qoi_decode_new_color(Data, Counter, Arity, Colors, R, G, B, A).

qoi_decode_op_runlength(Data, Counter, Arity, Colors, Prev, RunLength) -->
   { arg(Counter, Data, Prev) },
   (  { RunLength == 0 }
   -> qoi_decode_op_end(Data, Counter, Arity, Colors, Prev)
   ;  {  Counter1 is Counter + 1,
         RunLength1 is RunLength - 1
      },
      qoi_decode_op_runlength(Data, Counter1, Arity, Colors, Prev, RunLength1)
   ).

qoi_decode_new_color(Data, Counter, Arity, Colors, R, G, B, A) -->
   {  Color = color(R, G, B, A),
      arg(Counter, Data, Color),
      Index is ((R * 3 + G * 5 + B * 7 + A * 11) mod 64) + 1,
      setarg(Index, Colors, Color)
   },
   qoi_decode_op_end(Data, Counter, Arity, Colors, Color).


qoi_decode_op_end(Data, Counter, Arity, Colors, Prev) -->
   (  { Counter >= Arity }
   -> []
   ;  { Counter1 is Counter + 1 },
      qoi_decode_op(Data, Counter1, Arity, Colors, Prev)
   ).

:- det(qoi_decode/2).

qoi_decode(Image, Filename) :-
   open(Filename, read, Stream, [encoding(octet)]),
   phrase_from_stream(qoi_decode(Image), Stream),
   close(Stream).

:- det(qoi_encode_bytes/2).

qoi_encode_bytes(Image, Bytes) :-
   phrase(qoi_encode(Image), Bytes).

:- det(qoi_encode/2).

qoi_encode(Image, Filename) :-
   qoi_encode_bytes(Image, Bytes),
   open(Filename, write, Stream, [encoding(octet)]),
   maplist(put_byte(Stream), Bytes),
   close(Stream).

qoi_encode_uint32(N) -->
   {  W1 is (N /\ 0xff000000) >> 24,
      W2 is (N /\ 0x00ff0000) >> 16,
      W3 is (N /\ 0x0000ff00) >> 8,
      W4 is (N /\ 0x000000ff)
   },
   [W1, W2, W3, W4].

qoi_encode_header(header(W, H, C, Colorspace)) -->
   "qoif",
   qoi_encode_uint32(W),
   qoi_encode_uint32(H),
   [C, Colorspace].

qoi_encode(image(header(W, H, C, Co), Data)) -->
   qoi_encode_header(header(W, H, C, Co)),
   {  Arity is W*H,
      length(Args, 64),
      maplist(=(color(0, 0, 0, 0)), Args),
      compound_name_arguments(Colors, colors, Args)
   },
   qoi_encode_op(Data, 1, Arity, Colors, color(0, 0, 0, 255)),
   [0, 0, 0, 0, 0, 0, 0, 1].

qoi_encode_op(Data, Counter, Arity, Colors, Prev) -->
   { arg(Counter, Data, Color) },
   (  { Prev == Color }
   -> { Counter1 is Counter + 1 },
      qoi_encode_op_run(Data, Counter1, Arity, Colors, Prev, 0)
   ;  { Color = color(R, G, B, A),
         Index is ((R * 3 + G * 5 + B * 7 + A * 11) mod 64),
         Index1 is Index + 1
      },
      (  { arg(Index1, Colors, Color) }
      -> [Index],
         qoi_encode_op_end(Data, Counter, Arity, Colors, Color)
      ;  { Prev = color(R1, G1, B1, A) }
      -> {  DR is (R - R1 + 128) mod 256 - 128,
            DG is (G - G1 + 128) mod 256 - 128,
            DB is (B - B1 + 128) mod 256 - 128
         },
         (  { DR >= -2, DR =< 1, DG >= -2, DG =< 1, DB >= -2, DB =< 1 }
         -> { Diff is 0b01000000 \/ (((DR + 2) << 4) \/ ((DG + 2) << 2) \/ (DB + 2)) },
            [Diff],
            { setarg(Index1, Colors, Color) },
            qoi_encode_op_end(Data, Counter, Arity, Colors, Color)
         ;  {  DRDG is DR - DG,
               DBDG is DB - DG,
               DRDG >= -8, DRDG =< 7, DG >= -32, DG =< 31, DBDG >= -8, DBDG =< 7
            }
         -> {  Op is 0b10000000 \/ (DG + 32),
               DRDB is ((DRDG + 8) << 4) \/ (DBDG + 8) },
            [Op, DRDB],
            { setarg(Index1, Colors, Color) },
            qoi_encode_op_end(Data, Counter, Arity, Colors, Color)
         ;  [0b11111110, R, G, B],
            { setarg(Index1, Colors, Color) },
            qoi_encode_op_end(Data, Counter, Arity, Colors, Color)
         )
      ;  [0b11111111, R, G, B, A],
         { setarg(Index1, Colors, Color) },
         qoi_encode_op_end(Data, Counter, Arity, Colors, Color)
      )
   ).

qoi_encode_op_run(Data, Counter, Arity, Colors, Prev, Run) -->
   (  { Run < 61, Counter =< Arity, arg(Counter, Data, Prev) }
   -> {  Counter1 is Counter + 1,
         Run1 is Run + 1
      },
      qoi_encode_op_run(Data, Counter1, Arity, Colors, Prev, Run1)
   ;  { Byte is 0b11000000 \/ Run },
      [Byte],
      (  { Counter > Arity }
      -> []
      ;  qoi_encode_op(Data, Counter, Arity, Colors, Prev)
      )
   ).

qoi_encode_op_end(Data, Counter, Arity, Colors, Prev) -->
   { Counter1 is Counter + 1 },
   (  { Counter1 > Arity }
   -> []
   ;  qoi_encode_op(Data, Counter1, Arity, Colors, Prev)
   ).

test_images("qoi_test_images/IMGP5493_seamless_2.qoi").
test_images("qoi_test_images/dice.qoi").
test_images("qoi_test_images/kodim23.qoi").
test_images("qoi_test_images/testcard_rgba.qoi").
test_images("qoi_test_images/edgecase.qoi").
test_images("qoi_test_images/qoi_logo.qoi").
test_images("qoi_test_images/wikipedia_008.qoi").
test_images("qoi_test_images/kodim10.qoi").
test_images("qoi_test_images/testcard.qoi").

benchmark :-
   format("decoding~n"),
   findall(Pixels-Time, (
      test_images(Name),
      format("~s~n", [Name]),
      call_time(qoi_decode(image(header(W, H, _, _), _), Name), Dict),
      Pixels is W*H,
      get_dict(wall, Dict, Time)
   ), DecodePairs),
   pairs_keys_values(DecodePairs, Pixels, DecodeTimes),
   sum_list(DecodeTimes, DecodeTime),
   sum_list(Pixels, NumPixels),
   DecodeSecPerPixel is NumPixels / (1e6 * DecodeTime),
   format("decoding speed: ~p megapixel/sec~n", [DecodeSecPerPixel]),
   format("encoding~n"),
   aggregate_all(sum(Time), (
      test_images(Name),
      format("~s~n", [Name]),
      file_name_extension(Base, _, Name),
      file_name_extension(Base, "pl", Pl),
      open(Pl, read, Stream, [encoding(octet)]),
      fast_read(Stream, Image),
      close(Stream),
      call_time(qoi_encode_bytes(Image, _), Dict),
      get_dict(wall, Dict, Time)
   ), EncodeTime),
   EncodeSecPerPixel is NumPixels / (1e6 * EncodeTime),
   format("encoding speed: ~p megapixel/sec~n", [EncodeSecPerPixel]).

:- begin_tests(qoi).

test(decode, [forall(test_images(Name))]) :-
   file_name_extension(Base, _, Name),
   file_name_extension(Base, "pl", Pl),
   open(Pl, read, Stream, [encoding(octet)]),
   fast_read(Stream, Image),
   close(Stream),
   qoi_decode(Image, Name).

test(encode, [forall(test_images(Name))]) :-
   file_name_extension(Base, _, Name),
   file_name_extension(Base, "pl", Pl),
   open(Pl, read, Stream, [encoding(octet)]),
   fast_read(Stream, Image),
   close(Stream),
   (  Name \== "qoi_test_images/edgecase.qoi"
   -> read_file_to_codes(Name, Bytes, [encoding(octet)])
   ;  true
   ),
   qoi_encode_bytes(Image, Bytes).

test(encode_diff) :-
   length(Args, 64),
   maplist(=(color(0, 0, 0, 0)), Args),
   compound_name_arguments(Colors, colors, Args),
   phrase(qoi_encode_op(
      data(color(0, 0, 0, 255)),
      1,
      1,
      Colors,
      color(255, 255, 255, 255)), [127], []).

:- end_tests(qoi).
