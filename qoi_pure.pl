:- module(qoi_pure, [qoi/2, qoi_decode/2, qoi_encode/2]).

:- use_module(library(clpfd)).

=(X, Y, T) :-
   (  X == Y
   -> T = 1
   ;  X \= Y
   -> T = 0
   ;  T == 1
   -> X = Y
   ;  T == 0
   -> dif(X, Y)
   ;  T = 1, X = Y
   ;  T = 0,
      dif(X, Y)
   ).

uncompressed(image(Header, Data1), Filename) :-
   open(Filename, read, Stream, [encoding(octet)]),
   fast_read(Stream, image(Header, Data)),
   close(Stream),
   compound_name_arguments(Data, data, Colors),
   reverse(Colors, RColors),
   compound_name_arguments(Data1, data, RColors).

qoi(Image, Bytes) :-
   phrase(qoi(Image), Bytes).

qoi_decode(Image, Filename) :-
   phrase_from_file(qoi(Image), Filename, [encoding(octet)]).

qoi_encode(Image, Filename) :-
   phrase(qoi(Image), Bytes),
   open(Filename, write, Stream, [encoding(octet)]),
   maplist(put_byte(Stream), Bytes),
   close(Stream).

qoi(image(header(W, H, C, Co), Data)) -->
   qoi_header(header(W, H, C, Co)),
   {  Arity is W*H,
      compound_name_arity(Data, data, Arity),
      colors(Colors)
   },
   (  { Arity > 0 }
   -> qoi_op(1, Data, Arity, Colors, color(0, 0, 0, 255))
   ;  []
   ),
   [0, 0, 0, 0, 0, 0, 0, 1].

qoi_header(header(W, H, C, Colorspace)) -->
   "qoif",
   qoi_uint32(W),
   qoi_uint32(H),
   { C in 3..4, Colorspace in 0..1 },
   [C, Colorspace].

qoi_uint32(N) -->
   [W1, W2, W3, W4],
   {  [W1, W2, W3, W4] ins 0..255,
      N #= W1 << 24 + W2 << 16 + W3 << 8 + W4
   }.

colors(Colors) :-
   length(Zeros, 64),
   maplist(=(color(0, 0, 0, 0)), Zeros),
   compound_name_arguments(Colors, colors, Zeros).

qoi_op(1, Data, Counter, Colors, Prev) -->
   [Op],
   {  arg(Counter, Data, Color),
      Counter1 is Counter - 1,
      Short in 0..3,
      Rest in 0..63,
      Op #= Short << 6 + Rest,
      ((Short #= 0b11) #/\ (Rest #=< 61)) #<==> IsRun,
      =(Color, Prev, IsRun)
   },
   qoi_op_run(IsRun, Data, Counter1, Colors, Prev, Rest, Short, Color).

qoi_op_run(1, Data, Counter, Colors, Prev, Run, _Short, _Color) -->
   (  { Counter > 0 }
   -> {  arg(Counter, Data, Color),
         Run1 #= Run - 1
      },
      (  { Run1 #>= 0, Color = Prev }
      -> { Counter1 is Counter - 1 },
         qoi_op_run(1, Data, Counter1, Colors, Prev, Run1, _, _)
      ;  qoi_op(1, Data, Counter, Colors, Prev)
      )
   ;  { Run = 0 }
   ).
qoi_op_run(0, Data, Counter, Colors, Prev, Rest, Short, Color) -->
   {  [R, G, B, A] ins 0..255,
      Color = color(R, G, B, A),
      Index #= ((R * 3 + G * 5 + B * 7 + A * 11) mod 64),
      Index1 #= Index + 1,
      (Short #= 0b00) #<==> IsIndex,
      (Index #= Rest) #<== IsIndex,
      (  IsIndex == 0
      -> true
      ;  arg(Index1, Colors, IndexColor)
      ),
      =(Color, IndexColor, IsIndex)
   },
   qoi_op_index(IsIndex, Data, Counter, Colors, Prev, Rest, Short, Color, Index1, R, G, B, A).

qoi_op_index(1, Data, Counter, Colors, _Prev, _Rest, _Short, Color, _Index, _R, _G, _B, _A) -->
   (  { Counter > 0 }
   -> qoi_op(1, Data, Counter, Colors, Color)
   ;  []
   ).
qoi_op_index(0, Data, Counter, Colors, Prev, Rest, Short, Color, Index, R, G, B, A) -->
   {  arg(4, Prev, A1),
      #\ ((Short #= 0b11) #/\ (Rest #= 0b111111)) #<==> SameAlpha,
      (A1 #= A) #<==> SameAlpha
   },
   qoi_op_same_alpha(SameAlpha, Data, Counter, Colors, Prev, Rest, Short, Color, Index, R, G, B, A).

qoi_op_same_alpha(1, Data, Counter, Colors, Prev, Rest, Short, Color, Index, R, G, B, A) -->
   {  [R1, G1, B1] ins 0..255,
      Prev = color(R1, G1, B1, A),
      DR #= (R - R1 + 128) mod 256 - 128,
      DG #= (G - G1 + 128) mod 256 - 128,
      DB #= (B - B1 + 128) mod 256 - 128,
      (Short #= 0b01) #<==> IsDiff,
      ((DR #>= -2) #/\ (DR #=< 1)
         #/\ (DG #>= -2) #/\ (DG #=< 1)
         #/\ (DB #>= -2) #/\ (DB #=< 1)) #<==> IsDiff
   },
   qoi_op_diff(IsDiff, Data, Counter, Colors, Prev, Rest, Short, Color, Index, R, G, B, A, DR, DG, DB).
qoi_op_same_alpha(0, Data, Counter, Colors, _Prev, _Rest, _Short, Color, Index, R, G, B, A) -->
   [R, G, B, A],
   qoi_op_new_color(1, Data, Counter, Colors, Color, Index).

qoi_op_diff(1, Data, Counter, Colors, _Prev, Diff, _Short, Color, Index, _R, _G, _B, _A, DR, DG, DB) -->
   { Diff #= (DR + 2) << 4 + (DG + 2) << 2 + DB + 2 },
   qoi_op_new_color(1, Data, Counter, Colors, Color, Index).
qoi_op_diff(0, Data, Counter, Colors, _Prev, Rest, Short, Color, Index, R, G, B, A, DR, DG, DB) -->
   {  DRDG #= DR - DG,
      DBDG #= DB - DG,
      (Short #= 0b10) #<==> IsLuma,
      ((DRDG #>= -8) #/\ (DRDG #=< 7)
         #/\ (DG #>= -32) #/\ (DG #=< 31)
         #/\ (DBDG #>= -8) #/\ (DBDG #=< 7)) #<==> IsLuma
   },
   qoi_op_luma(IsLuma, Data, Counter, Colors, Rest, Short, Color, Index, R, G, B, A, DRDG, DG, DBDG).

qoi_op_luma(1, Data, Counter, Colors, Rest, _Short, Color, Index, _R, _G, _B, _A, DRDG, DG, DBDG) -->
   [Byte],
   {  Rest #= DG + 32,
      Byte #= (DRDG + 8) << 4 + DBDG + 8
   },
   qoi_op_new_color(1, Data, Counter, Colors, Color, Index).
qoi_op_luma(0, Data, Counter, Colors, 0b111110, 0b11, Color, Index, R, G, B, _A, _, _, _) -->
   [R, G, B],
   qoi_op_new_color(1, Data, Counter, Colors, Color, Index).

qoi_op_new_color(1, Data, Counter, Colors, Color, Index) -->
   (  { Counter > 0 }
   -> { setarg(Index, Colors, Color) },
      qoi_op(1, Data, Counter, Colors, Color)
   ;  []
   ).

test_images("qoi_test_images/testcard_rgba.qoi").
test_images("qoi_test_images/qoi_logo.qoi").
test_images("qoi_test_images/testcard.qoi").

benchmark :-
   format("decoding~n"),
   findall(Pixels-Time, (
      test_images(Name),
      format("~s~n", [Name]),
      call_time(qoi_decode(Image, Name), Dict),
      Image = image(header(W, H, _, _), _),
      Pixels is W*H,
      get_dict(wall, Dict, Time)
   ), DecodePairs),
   pairs_keys_values(DecodePairs, Pixels, DecodeTimes),
   sum_list(DecodeTimes, DecodeTime),
   sum_list(Pixels, NumPixels),
   DecodeSecPerPixel is NumPixels / (1e3 * DecodeTime),
   format("decoding speed: ~p kilopixel/sec~n", [DecodeSecPerPixel]),
   format("encoding~n"),
   aggregate_all(sum(Time), (
      test_images(Name),
      format("~s~n", [Name]),
      file_name_extension(Base, _, Name),
      file_name_extension(Base, "pl", Pl),
      uncompressed(Image, Pl),
      call_time(qoi(Image, _), Dict),
      get_dict(wall, Dict, Time)
   ), EncodeTime),
   EncodeSecPerPixel is NumPixels / (1e3 * EncodeTime),
   format("encoding speed: ~p kilopixel/sec~n", [EncodeSecPerPixel]).

:- begin_tests(qoi_pure).

test(decoding) :-
   uncompressed(Image, "qoi_test_images/testcard_rgba.pl"),
   qoi_decode(Image, "qoi_test_images/testcard_rgba.qoi").

test(encoding) :-
   uncompressed(Image, "qoi_test_images/testcard_rgba.pl"),
   read_file_to_codes("qoi_test_images/testcard_rgba.qoi", Bytes, [encoding(octet)]),
   qoi(Image, Bytes).

:- end_tests(qoi_pure).
