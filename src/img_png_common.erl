-module(img_png_common).

-export([header_len/0]).

-export([start_idx_len/0]).
-export([start_idx_offset_pct/0]).
-export([start_idx_spacing/0]).
-export([start_idx_bits_per_byte/0]).

-export([len_len/0]).
-export([len_offset_pct/0]).
-export([len_spacing/0]).
-export([len_bits_per_byte/0]).

-export([compute_offset_from_pct/2]).
-export([num_bytes_in_rows/1]).


-ifdef(TEST).
-export([create_random_byte/0]).
-export([create_pixels_list/2]).
-export([test_set_bits/2]).
-include_lib("eunit/include/eunit.hrl").
-endif.


header_len() ->
  len_len() +
  len_offset_pct() +
  len_spacing() +
  len_bits_per_byte().

start_idx_len()           -> 0.
start_idx_offset_pct()    -> len_len().
start_idx_spacing()       -> len_len() + len_offset_pct().
start_idx_bits_per_byte() -> len_len() + len_offset_pct() + len_spacing().

len_len()           -> 16.
len_offset_pct()    -> 8.
len_spacing()       -> 8.
len_bits_per_byte() -> 8.

compute_offset_from_pct(TotalNumBytes, OffsetPct) ->
  NumBytesAvail = TotalNumBytes - header_len(),
  NumBytesAvail * OffsetPct div 100.

num_bytes_in_rows(Rows) ->
  lists:foldl(
    fun({_Idx, RowBytes}, Sum) -> Sum + erlang:byte_size(RowBytes) end,
    0,
    Rows).

-ifdef(TEST).

create_random_byte() ->
  random:uniform(256) - 1.

create_pixels_list(Bytes, RowLen) ->
  create_pixels_list(Bytes, RowLen, 0, []).

create_pixels_list([], _RowLen, _RowNum, Acc) ->
  lists:reverse(Acc);
create_pixels_list(Bytes, RowLen, RowNum, Acc) ->
  {RowBytes, RestBytes} = split(RowLen, Bytes),
  Pixels = << <<B>> || B <- RowBytes >>,
  Tup = {RowNum, Pixels},
  create_pixels_list(RestBytes, RowLen, RowNum + 1, [Tup | Acc]).

split(Len, L) when Len =< length(L) ->
  lists:split(Len, L);
split(_Len, L) ->
  {L, []}.

create_pixels_list_test() ->
  %?debugMsg("create_pixels_list_test"),
  B0 = 0,
  B1 = 1,
  B2 = 2,
  B3 = 3,
  B4 = 4,
  B5 = 5,
  B6 = 6,
  B7 = 7,
  B8 = 8,
  Bytes = [B0, B1, B2, B3, B4, B5, B6, B7, B8],
  Expected = [
    {0, <<B0, B1, B2>>},
    {1, <<B3, B4, B5>>},
    {2, <<B6, B7, B8>>}
  ],
  Actual = create_pixels_list(Bytes, 3),
  ?assertEqual(Expected, Actual).

%%% This is terribly inefficient, but it's only for testing purposes
test_set_bits(Bytes, []) ->
  Bytes;
test_set_bits(Bytes, [{ByteNum, BitNum, Bit} | SetterT]) ->
  Prefix = lists:sublist(Bytes, 1, ByteNum), % Start idx is 1-based, ByteNum is 0-based
  [Byte] = lists:sublist(Bytes, ByteNum + 1, 1),
  Postfix = lists:nthtail(ByteNum + 1, Bytes), % Start idx is 0-based
  Byte2 = img_bit:set_bit(Byte, BitNum, Bit),
  test_set_bits(Prefix ++ [Byte2 | Postfix], SetterT).



-endif.
