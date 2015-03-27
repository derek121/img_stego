-module(img_bit).

-export([get_bit/2]).
-export([set_bit/3]).
-export([set_bits/2]).

%%%
get_bit(Byte, BitNum) ->
  (Byte bsr BitNum) band 1.

%%%
set_bit(Byte, BitNum, Bit) when Bit =:= 1 ->
  %?debugFmt("set_bit ~p ~p ~p", [Byte, BitNum, Bit]),
  Byte bor (1 bsl BitNum);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 0 ->
  %% These Bit =:= 0 functions could be collapsed into
  %%   Byte band (2#11111111 bxor (1 bsl BitNum))
  Byte band (2#11111110);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 1 ->
  Byte band (2#11111101);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 2 ->
  Byte band (2#11111011);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 3 ->
  Byte band (2#11110111);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 4 ->
  Byte band (2#11101111);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 5 ->
  Byte band (2#11011111);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 6 ->
  Byte band (2#10111111);
set_bit(Byte, BitNum, Bit) when Bit =:= 0, BitNum =:= 7 ->
  Byte band (2#01111111).

%%%
set_bits(Byte, Bits) ->
  set_bits(Byte, 0, Bits).

set_bits(Byte, _BitNum, []) ->
  Byte;
set_bits(Byte, BitNum, [Bit | T]) ->
  Byte2 = set_bit(Byte, BitNum, Bit),
  set_bits(Byte2, BitNum + 1, T).


