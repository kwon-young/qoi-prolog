# QOI format implementation in prolog

Quite Ok Image format implementation in prolog.
See the accompanying discussion for more context about this implementation: https://swi-prolog.discourse.group/t/quite-ok-image-qoi-format-challenge/7735/10

## How to use

```prolog
?- qoi_decode(Image, "myfile.qoi").
?- qoi_encode(Image, "myfile.qoi").
?- qoi_encode_bytes(Image, Bytes).
```

## Benchmark

```prolog
102 ?- benchmark.
decoding
qoi_test_images/IMGP5493_seamless_2.qoi
...
qoi_test_images/testcard.qoi
decoding speed: 1.3612986662328779 megapixel/sec
encoding
qoi_test_images/IMGP5493_seamless_2.qoi
...
qoi_test_images/testcard.qoi
encoding speed: 1.1895745549919616 megapixel/sec
true.
```
