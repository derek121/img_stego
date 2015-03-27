-module(img_prep_spec).

-export([prep_spec/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

prep_spec(NumBits, Spacing, BitsPerByte, StartByteNum) ->
  prep_spec(NumBits, Spacing, BitsPerByte, StartByteNum, 0, []).

%%%
prep_spec(0, _Spacing, _BitsPerByte, _ByteNum, _BitNum, Acc) ->
  lists:reverse(Acc);

prep_spec(NumBits, Spacing, BitsPerByte, ByteNum, BitNum, Acc)
  when BitNum < BitsPerByte ->
  prep_spec(NumBits - 1, Spacing, BitsPerByte, ByteNum, BitNum + 1, [{ByteNum, BitNum} | Acc]);

prep_spec(NumBits, Spacing, BitsPerByte, ByteNum, BitNum, Acc)
  when BitNum =:= BitsPerByte ->
  prep_spec(NumBits, Spacing, BitsPerByte, ByteNum + Spacing, 0, Acc).



%%%
-ifdef(TEST).

prep_spec_test() ->
  %?debugMsg("prep_spec_test"),

  Expected = [
    {32, 0},
    {32, 1},
    {32, 2},
    {35, 0},
    {35, 1},
    {35, 2},
    {38, 0},
    {38, 1},
    {38, 2},
    {41, 0},
    {41, 1},
    {41, 2},
    {44, 0},
    {44, 1},
    {44, 2},
    {47, 0},
    {47, 1},
    {47, 2},
    {50, 0},
    {50, 1},
    {50, 2},
    {53, 0},
    {53, 1},
    {53, 2}
  ],

  Spacing = 3,
  BitsPerByte = 3,
  StartByteNum = 32,

  ToSet = prep_spec(3 * 8, Spacing, BitsPerByte, StartByteNum),

  %io:format(user, "Expected: ~P~n", [Expected, 1000]),
  %io:format(user, "Actual: ~P~n", [ToSet, 1000]),

  ?assertEqual(Expected, ToSet),
  ok.

-endif.

