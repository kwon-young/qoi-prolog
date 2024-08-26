:- use_module(library(dcg/basics)).

qoi_decode_uint32(N) -->
   [W1, W2, W3, W4],
   {N is W1 << 24 + W2 << 16 + W3 << 8 + W4}.
qoi_encode_uint32(N) -->
   {  W1 is N /\ 0xf000 >> 24,
      W2 is N /\ 0x0f00 >> 16,
      W3 is N /\ 0x00f0 >> 8,
      W4 is N /\ 0x000f
   },
   [W1, W2, W3, W4].

qoi_decode_header(header(W, H, C, Colorspace)) -->
   "qoif",
   qoi_decode_uint32(W),
   qoi_decode_uint32(H),
   [C, Colorspace].
qoi_encode_header(header(W, H, C, Colorspace)) -->
   "qoif",
   qoi_encode_uint32(W),
   qoi_encode_uint32(H),
   [C, Colorspace].

qoi_decode(image(header(W, H, C, Co), Data)) -->
   qoi_decode_header(header(W, H, C, Co)),
   {  Arity is W*H,
      compound_name_arity(Data, data, Arity),
      length(Args, 64),
      maplist(=(color(0, 0, 0, 0)), Args),
      compound_name_arguments(Colors, colors, Args)
   },
   [Op],
   { StartOp is Op >> 6 },
   qoi_decode_op(StartOp, Op, Data, 1, Arity, Colors, color(0, 0, 0, 255)),
   [0, 0, 0, 0, 0, 0, 0, 1].
qoi_encode(image(header(W, H, C, Co), Data)) -->
   qoi_encode_header(header(W, H, C, Co)),
   {  Arity is W*H,
      length(Args, 64),
      maplist(=(color(0, 0, 0, 0)), Args),
      compound_name_arguments(Colors, colors, Args)
   },
   qoi_encode_op(Data, 1, Arity, Colors, color(0, 0, 0, 255)),
   [0, 0, 0, 0, 0, 0, 0, 1].

qoi_decode_op(0b11, Op, Data, Counter, Arity, Colors, Prev) -->
   (  { Op == 254 }
   -> qoi_decode_op_rgb(Data, Counter, Arity, Colors, Prev)
   ;  { Op == 255 }
   -> qoi_decode_op_rgba(Data, Counter, Arity, Colors, Prev)
   ;  qoi_decode_op_run(Op, Data, Counter, Arity, Colors, Prev)
   ).
qoi_decode_op(0b00, Op, Data, Counter, Arity, Colors, _Prev) -->
   {  Index is Op /\ 0b00111111 + 1,
      arg(Index, Colors, Color),
      arg(Counter, Data, Color) 
   },
   qoi_decode_op_end(Data, Counter, Arity, Colors, Color).
qoi_decode_op(0b01, Op, Data, Counter, Arity, Colors, color(R, G, B, A)) -->
   {  R1 is (R + ((Op /\ 0b00110000) >> 4) - 2) mod 256,
      G1 is (G + ((Op /\ 0b00001100) >> 2) - 2) mod 256,
      B1 is (B + (Op /\ 0b00000011) - 2) mod 256
   },
   qoi_decode_new_color(Data, Counter, Arity, Colors, R1, G1, B1, A).
qoi_decode_op(0b10, Op, Data, Counter, Arity, Colors, color(R, G, B, A)) -->
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

qoi_decode_op_rgba(Data, Counter, Arity, Colors, _Prev) -->
   [R, G, B, A],
   qoi_decode_new_color(Data, Counter, Arity, Colors, R, G, B, A).

qoi_decode_op_run(Op, Data, Counter, Arity, Colors, Prev) -->
   { RunLength is Op /\ 0b00111111 },
   qoi_decode_op_runlength(Data, Counter, Arity, Colors, Prev, RunLength).

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
      [Op],
      { StartOp is Op >> 6 },
      qoi_decode_op(StartOp, Op, Data, Counter1, Arity, Colors, Prev)
   ).

:- det(qoi_decode/2).

qoi_decode(Image, Filename) :-
   phrase_from_file(qoi_decode(Image), Filename, [encoding(octet)]).

:- begin_tests(qoi).


test_images("qoi_test_images/dice.qoi").
test_images("qoi_test_images/kodim23.qoi").
test_images("qoi_test_images/testcard_rgba.qoi").
test_images("qoi_test_images/edgecase.qoi").
test_images("qoi_test_images/qoi_logo.qoi").
test_images("qoi_test_images/wikipedia_008.qoi").
test_images("qoi_test_images/kodim10.qoi").
test_images("qoi_test_images/testcard.qoi").

test(decode, [forall(test_images(Name))]) :-
   qoi_decode(_Image, Name).

:- end_tests(qoi).
