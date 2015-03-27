-module(img_png_write).

-export([init_state/1]).
-export([add_message/2]).
-export([add_message/3]).
-export([save/2]).

-export([check_message_fit/2]).
-export([check_message_fit/3]).

-include_lib("erl_img/include/erl_img.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
  curr_byte_num = 0,
  curr_row_byte_num = 0,
  curr_row,
  rest_rows,
  rows_acc = [],
  result,
  img
}).

init_state(ImgPath) ->
  {ok, Img} = erl_img:load(ImgPath),
  Pixmap = hd(Img#erl_image.pixmaps),
  Pixels = Pixmap#erl_pixmap.pixels,
  init_state(Pixels, Img).

init_state(Pixels, Img) ->
  [First | Rest] = Pixels,
  #state{
    curr_row = First,
    rest_rows = Rest,
    img = Img}.

%%%
check_message_fit(State, Msg) when is_binary(Msg) ->
  check_message_fit(State, erlang:binary_to_list(Msg));
check_message_fit(State, Msg) when is_list(Msg) ->
  check_message_fit(State, Msg, []).

check_message_fit(State, Msg, Opts) when is_binary(Msg) ->
  check_message_fit(State, erlang:binary_to_list(Msg), Opts);
check_message_fit(State, Msg, Opts) when is_list(Msg) ->
  Opts2 = process_opts(State, Opts),

  {Fits, NumBytesAvailable, ReqNumBytes} = message_will_fit(State, Msg, Opts2),
  Reply = create_check_message_fit_reply(NumBytesAvailable, ReqNumBytes, Msg),

  case Fits of
    true -> {fits,   Reply};
    _    -> {no_fit, Reply}
  end.

create_check_message_fit_reply(NumBytesAvailable, ReqNumBytes, Msg) ->
  [
    {image_bytes_for_use, NumBytesAvailable},
    {required_image_bytes_for_use, ReqNumBytes},
    {message_length, length(Msg)}
  ].

process_opts(State, Opts) ->
  Opts2 = add_default_opts(Opts),
  translate_offset_pct(State, Opts2).

add_message(State, Msg) when is_binary(Msg) ->
  add_message(State, erlang:binary_to_list(Msg));
add_message(State, Msg) when is_list(Msg) ->
  add_message(State, Msg, []).

add_message(State, Msg, Opts) when is_binary(Msg) ->
  add_message(State, erlang:binary_to_list(Msg), Opts);
add_message(State, Msg, Opts) when is_list(Msg) ->
  Opts2 = process_opts(State, Opts),

  validate_image_size(State, Msg, Opts2),

  OffsetPct    = proplists:get_value(offset_pct,    Opts2),
  Spacing      = proplists:get_value(spacing,       Opts2),
  BitsPerByte  = proplists:get_value(bits_per_byte, Opts2),

  ToSetLen         = prep_bits_header(length(Msg), img_png_common:start_idx_len(),           img_png_common:len_len()),
  ToSetOffsetPct   = prep_bits_header(OffsetPct,   img_png_common:start_idx_offset_pct(),    img_png_common:len_offset_pct()),
  ToSetSpacing     = prep_bits_header(Spacing,     img_png_common:start_idx_spacing(),       img_png_common:len_spacing()),
  ToSetBitsPerByte = prep_bits_header(BitsPerByte, img_png_common:start_idx_bits_per_byte(), img_png_common:len_bits_per_byte()),

  Offset = proplists:get_value(offset, Opts2),
  ToSet = create_setter(Msg, Spacing, BitsPerByte, Offset),

  State2 = set_bits(State,  ToSetLen),
  State3 = set_bits(State2, ToSetOffsetPct),
  State4 = set_bits(State3, ToSetSpacing),
  State5 = set_bits(State4, ToSetBitsPerByte),
  set_bits(State5, ToSet).

default_offset_pct()    -> 0.
default_spacing()       -> 1.
default_bits_per_byte() -> 1.

add_default_opts(Opts) ->
  %% List comprehensions without generators:
  %%   https://gist.github.com/wardbekker/6876d1ed13eb40a47b55#list-comprehension-without-generators
  Opts
    ++ [{offset_pct, default_offset_pct()}       || not lists:keymember(offset_pct, 1, Opts)]
    ++ [{spacing, default_spacing()}             || not lists:keymember(spacing, 1, Opts)]
    ++ [{bits_per_byte, default_bits_per_byte()} || not lists:keymember(bits_per_byte, 1, Opts)].

%%% Set offset, based on offset_pct, if offset isn't already set
translate_offset_pct(State, Opts) ->
  case proplists:get_value(offset, Opts) of
    undefined -> set_offset_from_pct(State, Opts);
    _         -> Opts
  end.

set_offset_from_pct(State, Opts) ->
  OffsetPct = proplists:get_value(offset_pct, Opts),
  TotalNumBytes = calc_num_bytes(State),
  Offset = img_png_common:compute_offset_from_pct(TotalNumBytes, OffsetPct),

  %% Set (or replace) the offset value
  lists:keystore(offset, 1, Opts, {offset, Offset}).

create_setter(Msg, Spacing, BitsPerByte, Offset) ->
  %% List of {ByteNum, BitNum}
  ToSetLocations = img_prep_spec:prep_spec(
    length(Msg) * 8, Spacing, BitsPerByte, img_png_common:header_len() + Offset),

  %% List of bits
  Bits = get_bits(Msg),

  %% List of {ByteNum, BitNum, Bit}
  lists:zipwith(
    fun({ByteNum, BitNum}, Bit) -> {ByteNum, BitNum, Bit} end,
    ToSetLocations,
    Bits).

get_bits(S) ->
  %% List of lists of bits, one per byte
  BytesBigEndian = lists:map(fun(B) -> [X || <<X:1>> <= <<B:8>>] end, S),

  %% Same, but each element list is reversed to be little-endian
  BytesLittleEndian = lists:map(fun(L) -> lists:reverse(L) end, BytesBigEndian),

  lists:flatten(BytesLittleEndian).

save(State, OutPath) ->
  #state{img = Img, result = Pixels} = State,

  PixmapIn = hd(Img#erl_image.pixmaps),
  PixmapOut = PixmapIn#erl_pixmap{pixels = Pixels},

  ImgOut = Img#erl_image{
    pixmaps = [PixmapOut],
    filename = OutPath},

  erl_img:save(ImgOut),
  ok.

%%%
validate_image_size(State, Msg, Opts) ->
  case message_will_fit(State, Msg, Opts) of
    {true, _, _}  -> ok;
    {false, _, _} -> erlang:error(insufficient_image_size)
  end.

message_will_fit(State, Msg, Opts) ->
  Offset = proplists:get_value(offset, Opts),
  NumBytesAvailForMsg = calc_num_bytes(State) - img_png_common:header_len() - Offset,
  NumBytesForMsg = num_bytes_for_msg(Msg, Opts),
  {NumBytesForMsg =< NumBytesAvailForMsg, NumBytesAvailForMsg, NumBytesForMsg}.

%%% Assumes State is as it's created upon init
%%% Since this is a relatively expensive operation, result could be saved if used again
calc_num_bytes(State) ->
  {_Idx, RowBytes} = State#state.curr_row,
  erlang:byte_size(RowBytes)
    + img_png_common:num_bytes_in_rows(State#state.rest_rows).

num_bytes_for_msg(Msg, Opts) ->
  Spacing      = proplists:get_value(spacing, Opts),
  BitsPerByte  = proplists:get_value(bits_per_byte, Opts),

  ReqForMsg = length(Msg) * 8,
  ReqWithBitsPerByte = (ReqForMsg div BitsPerByte) + calc_partial_byte(ReqForMsg, BitsPerByte),
  ReqWithSpacing = calc_with_spacing(ReqWithBitsPerByte, Spacing),
  ReqWithSpacing.

calc_partial_byte(Num, BitsPerByte) when Num rem BitsPerByte =:= 0 ->
  0;
calc_partial_byte(_Num, _BitsPerByte) ->
  1.

%%% Spacing of 1: xxx
%%% Spacing of 2: x-x-x
%%% Spacing of 3: x--x--x
%%% Spacing of 4: x---x---x
calc_with_spacing(Num, Spacing) ->
  (Num - 1) * Spacing + 1.

%%% All bits have been set
set_bits(State, []) ->
  RowsAcc2 = [State#state.curr_row | State#state.rows_acc],
  RowsAcc3 = lists:append(lists:reverse(RowsAcc2), State#state.rest_rows),
  State#state{result = RowsAcc3};

%%% Set on current byte
set_bits(
    #state{curr_byte_num = CurrByteNum} = State,
    [{ByteNumToSet, _, _} | _] = ToSet)
  when CurrByteNum =:= ByteNumToSet ->

  CurrRowByteNum = State#state.curr_row_byte_num,
  {Idx, CurrRowBytes} = State#state.curr_row,

  {Prefix, Byte, Postfix} = extract_parts_of_row(CurrRowBytes, CurrRowByteNum),

  [{ByteNumToSet, BitNumToSet, Bit} | ToSetT] = ToSet,
  Byte2 = img_bit:set_bit(Byte, BitNumToSet, Bit),

  CurrRowBytes2 = <<Prefix/binary, Byte2, Postfix/binary>>,

  State2 = State#state{curr_row = {Idx, CurrRowBytes2}},
  set_bits(State2, ToSetT);

%%% Advance to next byte
set_bits(
    #state{curr_byte_num = CurrByteNum} = State,
    [{ByteNumToSet, _, _} | _] = ToSet)
  when CurrByteNum < ByteNumToSet->

  State2 = advance_state(State),
  set_bits(State2, ToSet).

extract_parts_of_row(CurrRowBytes, ByteNumToSet) ->
  %% Could also do this with binary pattern matching
  Len = erlang:byte_size(CurrRowBytes),

  Prefix  = binary:part(CurrRowBytes, 0, ByteNumToSet),
  Byte    = binary:at(CurrRowBytes, ByteNumToSet),
  Postfix = binary:part(CurrRowBytes, ByteNumToSet + 1, Len - ByteNumToSet - 1),

  {Prefix, Byte, Postfix}.

%%% Not at last byte of row, so advance one byte
advance_state(
    #state{
      curr_row_byte_num = CurrRowByteNum,
      curr_row = {_Idx, CurrRowBytes}
    } = State)
  when CurrRowByteNum < erlang:byte_size(CurrRowBytes) - 1 ->

  CurrByteNum = State#state.curr_byte_num,
  State2 = State#state{
    curr_byte_num = CurrByteNum + 1,
    curr_row_byte_num = CurrRowByteNum + 1
  },
  State2;

%%% At last byte of row, so move to next. Crashes if no more rows.
advance_state(
    #state{
      curr_row_byte_num = CurrRowByteNum,
      curr_row = {_Idx, CurrRowBytes}
    } = State)
  when CurrRowByteNum =:= erlang:byte_size(CurrRowBytes) - 1 ->

  #state{curr_byte_num = CurrByteNum, curr_row = CurrRow, rest_rows = [RestH | RestT], rows_acc = RowsAcc} = State,
  State2 = State#state{
    curr_byte_num = CurrByteNum + 1,
    curr_row_byte_num = 0,
    curr_row = RestH,
    rest_rows = RestT,
    rows_acc = [CurrRow | RowsAcc]
  },
  State2.

prep_bits_header(Val, Base, NumBytes) ->
  [{Base + (N - 1), 0, img_bit:get_bit(Val, N - 1)} || N  <- lists:seq(1, NumBytes)].



-ifdef(TEST).

prep_bits_header_test() ->
  %?debugMsg("prep_bits_header_test"),

  Base = 10,

  Expected = [
    {Base + 0, 0, 1}, % 1 =:= bit 0 of 13
    {Base + 1, 0, 0}, % 0 =:= bit 1 of 13
    {Base + 2, 0, 1}, % 1 =:= bit 2 of 13
    {Base + 3, 0, 1}, % 1 =:= bit 3 of 13
    {Base + 4, 0, 0},
    {Base + 5, 0, 0},
    {Base + 6, 0, 0},
    {Base + 7, 0, 0}
  ],

  Actual = prep_bits_header(13, Base, 8),

  ?assertEqual(Expected, Actual),
  ok.


set_bits_3_by_3_incomplete_skip_bytes_multiple_bits_test() ->
  %?debugMsg("set_bits_3_by_3_incomplete_skip_bytes_multiple_bits_test"),

  random:seed(erlang:now()),

  Orig_0_0 = img_png_common:create_random_byte(),
  Orig_0_1 = img_png_common:create_random_byte(),
  Orig_0_2 = img_png_common:create_random_byte(),

  Orig_1_0 = img_png_common:create_random_byte(),
  Orig_1_1 = img_png_common:create_random_byte(),
  Orig_1_2 = img_png_common:create_random_byte(),

  Orig_2_0 = img_png_common:create_random_byte(),
  Orig_2_1 = img_png_common:create_random_byte(),

  SetVals_0_0 = [0, 1], %% For byte in row 0, col 0, set bit 0 to 0, and bit 1 to 1
  SetVals_0_1 = [1, 1], %% Etc...
  SetVals_1_0 = [0, 1],
  SetVals_1_2 = [1, 1],
  SetVals_2_1 = [1, 1],

  Mod_0_0 = img_bit:set_bits(Orig_0_0, SetVals_0_0),
  Mod_0_1 = img_bit:set_bits(Orig_0_1, SetVals_0_1),
  Mod_0_2 = Orig_0_2,

  Mod_1_0 = img_bit:set_bits(Orig_1_0, SetVals_1_0),
  Mod_1_1 = Orig_1_1,
  Mod_1_2 = img_bit:set_bits(Orig_1_2, SetVals_1_2),

  Mod_2_0 = Orig_2_0,
  Mod_2_1 = img_bit:set_bits(Orig_2_1, SetVals_2_1),

  Pixels = [
    %%    Byte 0    Byte 1    Byte 2
    {0, <<Orig_0_0, Orig_0_1, Orig_0_2>>},
    %%    Byte 3    Byte 4    Byte 5
    {1, <<Orig_1_0, Orig_1_1, Orig_1_2>>},
    %%    Byte 6    Byte 7
    {2, <<Orig_2_0, Orig_2_1>>}
  ],

  Expected = [
    {0, <<Mod_0_0, Mod_0_1, Mod_0_2>>},
    {1, <<Mod_1_0, Mod_1_1, Mod_1_2>>},
    {2, <<Mod_2_0, Mod_2_1>>}
  ],

  ToSet = [
    %% Row 0, Col 0
    {0, 0, lists:nth(1, SetVals_0_0)},
    {0, 1, lists:nth(2, SetVals_0_0)},

    %% Row 0, Col 1
    {1, 0, lists:nth(1, SetVals_0_1)},
    {1, 1, lists:nth(2, SetVals_0_1)},

    %% Row 1, Col 0
    {3, 0, lists:nth(1, SetVals_1_0)},
    {3, 1, lists:nth(2, SetVals_1_0)},

    %% Row 1, Col 2
    {5, 0, lists:nth(1, SetVals_1_2)},
    {5, 1, lists:nth(2, SetVals_1_2)},

    %% Row 2, Col 1
    {7, 0, lists:nth(1, SetVals_2_1)},
    {7, 1, lists:nth(2, SetVals_2_1)}
  ],

  State = init_state(Pixels, undefined),
  State2 = set_bits(State, ToSet),
  ?assertEqual(Expected, State2#state.result),
  ok.


add_message_binary_test() ->
  add_message_tester(<<"db">>, 2).

add_message_list_test() ->
  add_message_tester("db", 2).

add_message_tester(Msg, MsgByteLen) ->
  %?debugMsg("add_message_test"),

  random:seed(erlang:now()),

  %% Need img_png:header_len() bytes (40) for length, offset, spacing, and num bits/byte,
  %% plus 2 for offset padding.
  %% Message itself is 2 * 8. Times 2 for spacing of 2, and divided by 3 for 3 bits per byte,
  %% but if not evenly divided by 3, add 1 more for the leftover bits.
  %% (40 + 2 + ((16 * 2 / 3) + 1) == 53)
  %% So 6 rows of 9

  Orig_0_0 = img_png_common:create_random_byte(),
  Orig_0_1 = img_png_common:create_random_byte(),
  Orig_0_2 = img_png_common:create_random_byte(),
  Orig_0_3 = img_png_common:create_random_byte(),
  Orig_0_4 = img_png_common:create_random_byte(),
  Orig_0_5 = img_png_common:create_random_byte(),
  Orig_0_6 = img_png_common:create_random_byte(),
  Orig_0_7 = img_png_common:create_random_byte(),
  Orig_0_8 = img_png_common:create_random_byte(),

  Orig_1_0 = img_png_common:create_random_byte(),
  Orig_1_1 = img_png_common:create_random_byte(),
  Orig_1_2 = img_png_common:create_random_byte(),
  Orig_1_3 = img_png_common:create_random_byte(),
  Orig_1_4 = img_png_common:create_random_byte(),
  Orig_1_5 = img_png_common:create_random_byte(),
  Orig_1_6 = img_png_common:create_random_byte(),
  Orig_1_7 = img_png_common:create_random_byte(),
  Orig_1_8 = img_png_common:create_random_byte(),

  Orig_2_0 = img_png_common:create_random_byte(),
  Orig_2_1 = img_png_common:create_random_byte(),
  Orig_2_2 = img_png_common:create_random_byte(),
  Orig_2_3 = img_png_common:create_random_byte(),
  Orig_2_4 = img_png_common:create_random_byte(),
  Orig_2_5 = img_png_common:create_random_byte(),
  Orig_2_6 = img_png_common:create_random_byte(),
  Orig_2_7 = img_png_common:create_random_byte(),
  Orig_2_8 = img_png_common:create_random_byte(),

  Orig_3_0 = img_png_common:create_random_byte(),
  Orig_3_1 = img_png_common:create_random_byte(),
  Orig_3_2 = img_png_common:create_random_byte(),
  Orig_3_3 = img_png_common:create_random_byte(),
  Orig_3_4 = img_png_common:create_random_byte(),
  Orig_3_5 = img_png_common:create_random_byte(),
  Orig_3_6 = img_png_common:create_random_byte(),
  Orig_3_7 = img_png_common:create_random_byte(),
  Orig_3_8 = img_png_common:create_random_byte(),

  Orig_4_0 = img_png_common:create_random_byte(),
  Orig_4_1 = img_png_common:create_random_byte(),
  Orig_4_2 = img_png_common:create_random_byte(),
  Orig_4_3 = img_png_common:create_random_byte(),
  Orig_4_4 = img_png_common:create_random_byte(),
  Orig_4_5 = img_png_common:create_random_byte(),
  Orig_4_6 = img_png_common:create_random_byte(),
  Orig_4_7 = img_png_common:create_random_byte(),
  Orig_4_8 = img_png_common:create_random_byte(),

  Orig_5_0 = img_png_common:create_random_byte(),
  Orig_5_1 = img_png_common:create_random_byte(),
  Orig_5_2 = img_png_common:create_random_byte(),
  Orig_5_3 = img_png_common:create_random_byte(),
  Orig_5_4 = img_png_common:create_random_byte(),
  Orig_5_5 = img_png_common:create_random_byte(),
  Orig_5_6 = img_png_common:create_random_byte(),
  Orig_5_7 = img_png_common:create_random_byte(),
  Orig_5_8 = img_png_common:create_random_byte(),

  %% 15% of 16 (16 bytes to represent Msg) is 2.4, which is floored to 2,
  %%  which will be our offset to use in creating the expected result here
  ByteOffsetPct = 15,

  ByteSpacing = 2,
  ByteBitsPerByte = 3,
  ByteD = $d,
  ByteB = $b,

  %% Headers
  Mod_0_0 = img_bit:set_bit(Orig_0_0, 0, img_bit:get_bit(MsgByteLen, 0)),
  Mod_0_1 = img_bit:set_bit(Orig_0_1, 0, img_bit:get_bit(MsgByteLen, 1)),
  Mod_0_2 = img_bit:set_bit(Orig_0_2, 0, img_bit:get_bit(MsgByteLen, 2)),
  Mod_0_3 = img_bit:set_bit(Orig_0_3, 0, img_bit:get_bit(MsgByteLen, 3)),
  Mod_0_4 = img_bit:set_bit(Orig_0_4, 0, img_bit:get_bit(MsgByteLen, 4)),
  Mod_0_5 = img_bit:set_bit(Orig_0_5, 0, img_bit:get_bit(MsgByteLen, 5)),
  Mod_0_6 = img_bit:set_bit(Orig_0_6, 0, img_bit:get_bit(MsgByteLen, 6)),
  Mod_0_7 = img_bit:set_bit(Orig_0_7, 0, img_bit:get_bit(MsgByteLen, 7)),
  Mod_0_8 = img_bit:set_bit(Orig_0_8, 0, img_bit:get_bit(MsgByteLen, 8)),

  Mod_1_0 = img_bit:set_bit(Orig_1_0, 0, img_bit:get_bit(MsgByteLen, 9)),
  Mod_1_1 = img_bit:set_bit(Orig_1_1, 0, img_bit:get_bit(MsgByteLen, 10)),
  Mod_1_2 = img_bit:set_bit(Orig_1_2, 0, img_bit:get_bit(MsgByteLen, 11)),
  Mod_1_3 = img_bit:set_bit(Orig_1_3, 0, img_bit:get_bit(MsgByteLen, 12)),
  Mod_1_4 = img_bit:set_bit(Orig_1_4, 0, img_bit:get_bit(MsgByteLen, 13)),
  Mod_1_5 = img_bit:set_bit(Orig_1_5, 0, img_bit:get_bit(MsgByteLen, 14)),
  Mod_1_6 = img_bit:set_bit(Orig_1_6, 0, img_bit:get_bit(MsgByteLen, 15)),
  Mod_1_7 = img_bit:set_bit(Orig_1_7, 0, img_bit:get_bit(ByteOffsetPct, 0)),
  Mod_1_8 = img_bit:set_bit(Orig_1_8, 0, img_bit:get_bit(ByteOffsetPct, 1)),

  Mod_2_0 = img_bit:set_bit(Orig_2_0, 0, img_bit:get_bit(ByteOffsetPct, 2)),
  Mod_2_1 = img_bit:set_bit(Orig_2_1, 0, img_bit:get_bit(ByteOffsetPct, 3)),
  Mod_2_2 = img_bit:set_bit(Orig_2_2, 0, img_bit:get_bit(ByteOffsetPct, 4)),
  Mod_2_3 = img_bit:set_bit(Orig_2_3, 0, img_bit:get_bit(ByteOffsetPct, 5)),
  Mod_2_4 = img_bit:set_bit(Orig_2_4, 0, img_bit:get_bit(ByteOffsetPct, 6)),
  Mod_2_5 = img_bit:set_bit(Orig_2_5, 0, img_bit:get_bit(ByteOffsetPct, 7)),
  Mod_2_6 = img_bit:set_bit(Orig_2_6, 0, img_bit:get_bit(ByteSpacing, 0)),
  Mod_2_7 = img_bit:set_bit(Orig_2_7, 0, img_bit:get_bit(ByteSpacing, 1)),
  Mod_2_8 = img_bit:set_bit(Orig_2_8, 0, img_bit:get_bit(ByteSpacing, 2)),

  Mod_3_0 = img_bit:set_bit(Orig_3_0, 0, img_bit:get_bit(ByteSpacing, 3)),
  Mod_3_1 = img_bit:set_bit(Orig_3_1, 0, img_bit:get_bit(ByteSpacing, 4)),
  Mod_3_2 = img_bit:set_bit(Orig_3_2, 0, img_bit:get_bit(ByteSpacing, 5)),
  Mod_3_3 = img_bit:set_bit(Orig_3_3, 0, img_bit:get_bit(ByteSpacing, 6)),
  Mod_3_4 = img_bit:set_bit(Orig_3_4, 0, img_bit:get_bit(ByteSpacing, 7)),
  Mod_3_5 = img_bit:set_bit(Orig_3_5, 0, img_bit:get_bit(ByteBitsPerByte, 0)),
  Mod_3_6 = img_bit:set_bit(Orig_3_6, 0, img_bit:get_bit(ByteBitsPerByte, 1)),
  Mod_3_7 = img_bit:set_bit(Orig_3_7, 0, img_bit:get_bit(ByteBitsPerByte, 2)),
  Mod_3_8 = img_bit:set_bit(Orig_3_8, 0, img_bit:get_bit(ByteBitsPerByte, 3)),

  Mod_4_0 = img_bit:set_bit(Orig_4_0, 0, img_bit:get_bit(ByteBitsPerByte, 4)),
  Mod_4_1 = img_bit:set_bit(Orig_4_1, 0, img_bit:get_bit(ByteBitsPerByte, 5)),
  Mod_4_2 = img_bit:set_bit(Orig_4_2, 0, img_bit:get_bit(ByteBitsPerByte, 6)),
  Mod_4_3 = img_bit:set_bit(Orig_4_3, 0, img_bit:get_bit(ByteBitsPerByte, 7)),


  %% Spacing bytes
  Mod_4_4 = Orig_4_4,
  Mod_4_5 = Orig_4_5,
  %% Data bytes
  Mod_4_6 = img_bit:set_bits(Orig_4_6, [img_bit:get_bit(ByteD, 0), img_bit:get_bit(ByteD, 1), img_bit:get_bit(ByteD, 2)]),
  Mod_4_7 = Orig_4_7,

  Mod_4_8 = img_bit:set_bits(Orig_4_8, [img_bit:get_bit(ByteD, 3), img_bit:get_bit(ByteD, 4), img_bit:get_bit(ByteD, 5)]),
  Mod_5_0 = Orig_5_0,
  Mod_5_1 = img_bit:set_bits(Orig_5_1, [img_bit:get_bit(ByteD, 6), img_bit:get_bit(ByteD, 7), img_bit:get_bit(ByteB, 0)]),
  Mod_5_2 = Orig_5_2,
  Mod_5_3 = img_bit:set_bits(Orig_5_3, [img_bit:get_bit(ByteB, 1), img_bit:get_bit(ByteB, 2), img_bit:get_bit(ByteB, 3)]),
  Mod_5_4 = Orig_5_4,
  Mod_5_5 = img_bit:set_bits(Orig_5_5, [img_bit:get_bit(ByteB, 4), img_bit:get_bit(ByteB, 5), img_bit:get_bit(ByteB, 6)]),
  Mod_5_6 = Orig_5_6,
  Mod_5_7 = img_bit:set_bits(Orig_5_7, [img_bit:get_bit(ByteB, 7)]),
  Mod_5_8 = Orig_5_8,

  Orig = [Orig_0_0, Orig_0_1, Orig_0_2, Orig_0_3, Orig_0_4, Orig_0_5, Orig_0_6, Orig_0_7, Orig_0_8,
    Orig_1_0, Orig_1_1, Orig_1_2, Orig_1_3, Orig_1_4, Orig_1_5, Orig_1_6, Orig_1_7, Orig_1_8,
    Orig_2_0, Orig_2_1, Orig_2_2, Orig_2_3, Orig_2_4, Orig_2_5, Orig_2_6, Orig_2_7, Orig_2_8,
    Orig_3_0, Orig_3_1, Orig_3_2, Orig_3_3, Orig_3_4, Orig_3_5, Orig_3_6, Orig_3_7, Orig_3_8,
    Orig_4_0, Orig_4_1, Orig_4_2, Orig_4_3, Orig_4_4, Orig_4_5, Orig_4_6, Orig_4_7, Orig_4_8,
    Orig_5_0, Orig_5_1, Orig_5_2, Orig_5_3, Orig_5_4, Orig_5_5, Orig_5_6, Orig_5_7, Orig_5_8],

  Mod = [Mod_0_0, Mod_0_1, Mod_0_2, Mod_0_3, Mod_0_4, Mod_0_5, Mod_0_6, Mod_0_7, Mod_0_8,
    Mod_1_0, Mod_1_1, Mod_1_2, Mod_1_3, Mod_1_4, Mod_1_5, Mod_1_6, Mod_1_7, Mod_1_8,
    Mod_2_0, Mod_2_1, Mod_2_2, Mod_2_3, Mod_2_4, Mod_2_5, Mod_2_6, Mod_2_7, Mod_2_8,
    Mod_3_0, Mod_3_1, Mod_3_2, Mod_3_3, Mod_3_4, Mod_3_5, Mod_3_6, Mod_3_7, Mod_3_8,
    Mod_4_0, Mod_4_1, Mod_4_2, Mod_4_3, Mod_4_4, Mod_4_5, Mod_4_6, Mod_4_7, Mod_4_8,
    Mod_5_0, Mod_5_1, Mod_5_2, Mod_5_3, Mod_5_4, Mod_5_5, Mod_5_6, Mod_5_7, Mod_5_8],

  %% Of the form [{0, <<B0, B1, B2>>}, {1, <<B3, B4, B5>>}, ...]
  Pixels = img_png_common:create_pixels_list(Orig, 9),
  Expected = img_png_common:create_pixels_list(Mod, 9),

  State = init_state(Pixels, undefined),

  Opts = [
    {offset_pct, ByteOffsetPct},
    {spacing, ByteSpacing},
    {bits_per_byte, ByteBitsPerByte}],
  State2 = add_message(State, Msg, Opts),

  %io:format(user, "Expected: ~P~n", [Expected, 1000]),
  %io:format(user, "Actual: ~P~n", [State2#state.result, 1000]),

  ?assertEqual(Expected, State2#state.result),
  ok.

add_message_too_big_test() ->
  %?debugMsg("add_message_too_big_test"),

  random:seed(erlang:now()),

  %% Need 4 * 8 bytes for offset, spacing, num bits/byte, and length,
  %% plus 2 for offset padding.
  %% Message itself is 2 * 8. Times 2 for spacing of 2, and divided by 3 for 3 bits per byte,
  %% but if not evenly divided by 3, add 1 more for the leftover bits.
  %% (32 + 2 + ((16 * 2 / 3) + 1) == 45)
  %% So 5 rows of 9
  %% Subtract 1 to cause failure
  Orig = create_random_bytes(5 * 9 - 1),

  Msg = "db",
  ByteOffset = 2,
  ByteSpacing = 2,
  ByteBitsPerByte = 3,

  %% Of the form [{0, <<B0, B1, B2>>}, {1, <<B3, B4, B5>>}, ...]
  Pixels = img_png_common:create_pixels_list(Orig, 9),

  State = init_state(Pixels, undefined),

  Opts = [
    {offset, ByteOffset},
    {spacing, ByteSpacing},
    {bits_per_byte, ByteBitsPerByte}],

  ?assertError(insufficient_image_size, add_message(State, Msg, Opts)),
  ok.

create_setter_test() ->
  %?debugMsg("create_setter_test"),

  Msg = "abc",
  A = $a,
  B = $b,
  C = $c,

  Expected = [
    {50, 0, A band 1},
    {50, 1, (A bsr 1) band 1},
    {50, 2, (A bsr 2) band 1},
    {53, 0, (A bsr 3) band 1},
    {53, 1, (A bsr 4) band 1},
    {53, 2, (A bsr 5) band 1},
    {56, 0, (A bsr 6) band 1},
    {56, 1, (A bsr 7) band 1},
    {56, 2, B band 1},
    {59, 0, (B bsr 1) band 1},
    {59, 1, (B bsr 2) band 1},
    {59, 2, (B bsr 3) band 1},
    {62, 0, (B bsr 4) band 1},
    {62, 1, (B bsr 5) band 1},
    {62, 2, (B bsr 6) band 1},
    {65, 0, (B bsr 7) band 1},
    {65, 1, C band 1},
    {65, 2, (C bsr 1) band 1},
    {68, 0, (C bsr 2) band 1},
    {68, 1, (C bsr 3) band 1},
    {68, 2, (C bsr 4) band 1},
    {71, 0, (C bsr 5) band 1},
    {71, 1, (C bsr 6) band 1},
    {71, 2, (C bsr 7) band 1}
  ],

  Spacing = 3,
  BitsPerByte = 3,

  ToSet = create_setter(Msg, Spacing, BitsPerByte, 10),

  ?assertEqual(Expected, ToSet),
  ok.

calc_required_num_bytes_plain_test()->
  %?debugMsg("calc_required_num_bytes_plain_test"),

  Msg = "ab",
  Opts = [
    {offset, 0},
    {spacing, 1},
    {bits_per_byte, 1}],

  ?assertEqual(8 * 2, num_bytes_for_msg(Msg, Opts)),
  ok.

%calc_required_num_bytes_offset_test()->
%  ?debugMsg("calc_required_num_bytes_offset_test"),
%
%  Msg = "ab",
%  Opts = [
%    {offset, 2},
%    {spacing, 1},
%    {bits_per_byte, 1}],
%
%  ?assertEqual(8 * 2, num_bytes_for_msg(Msg, Opts)),
%  ok.

calc_required_num_bytes_spacing_test()->
  %?debugMsg("calc_required_num_bytes_spacing_test"),

  Msg = "ab",
  Opts = [
    {offset, 0},
    {spacing, 3},
    {bits_per_byte, 1}],

  %% 3 bytes for each byte except the last, and then one more for the last
  NumForMsg = 15 * 3 + 1,

  ?assertEqual(NumForMsg, num_bytes_for_msg(Msg, Opts)),
  ok.

calc_required_num_bytes_bits_per_byte_even_test()->
  %?debugMsg("calc_required_num_bytes_bits_per_byte_even_test"),

  Msg = "ab",
  Opts = [
    {offset, 0},
    {spacing, 1},
    {bits_per_byte, 2}],

  ?assertEqual(((8 * 2) div 2), num_bytes_for_msg(Msg, Opts)),
  ok.

calc_required_num_bytes_bits_per_byte_extra_test()->
  %?debugMsg("calc_required_num_bytes_bits_per_byte_extra_test"),

  Msg = "ab",
  Opts = [
    {offset, 0},
    {spacing, 1},
    {bits_per_byte, 3}],

  %% 16 bytes of msg. 5 output bytes for 15 of the bytes (3 * 5), plus 1 more
  NumForMsg = 6,
  ?assertEqual(NumForMsg, num_bytes_for_msg(Msg, Opts)),
  ok.

calc_required_num_bytes_all_test()->
  %?debugMsg("calc_required_num_bytes_all_test"),

  %% There are other combinations for Spacing and Bpb, which could be tested

  Offset = 2,
  Spacing = 3,
  Bpb = 2,

  Msg = "ab",
  Opts = [
    {offset, Offset},
    {spacing, Spacing},
    {bits_per_byte, Bpb}],

  NumSrc = 8 * 2,
  NumWithBpb = NumSrc div Bpb,

  %% 3 bytes for each byte except the last, and then one more for the last
  NumWithSpacing = (NumWithBpb - 1) * Spacing + 1,

  ?assertEqual(NumWithSpacing, num_bytes_for_msg(Msg, Opts)),
  ok.

create_random_bytes(N) ->
  create_random_bytes(N, []).

create_random_bytes(0, Acc) ->
  Acc;
create_random_bytes(N, Acc) ->
  create_random_bytes(N - 1, [img_png_common:create_random_byte() | Acc]).


-endif.

