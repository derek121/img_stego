-module(img_png_read).

-export([init_state/1]).
-export([read_message/2]).

-include_lib("erl_img/include/erl_img.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
  curr_byte_num = 0,
  curr_row_byte_num = 0,
  curr_row,
  rest_rows
}).

init_state(ImgPath) when is_list(ImgPath) ->
  {ok, Img} = erl_img:load(ImgPath),
  Pixmap = hd(Img#erl_image.pixmaps),
  Pixels = Pixmap#erl_pixmap.pixels,
  init_state2(Pixels).

init_state2(Pixels) when is_list(Pixels) ->
  [First | Rest] = Pixels,
  #state{
    curr_row = First,
    rest_rows = Rest
  }.

%%%
read_message(State, OutputFormat) when OutputFormat =:= as_list; OutputFormat =:= as_binary ->
  TotalNumBytes = calc_num_bytes(State),

  ToReadMsgLen      = prep_read_header(img_png_common:start_idx_len(),           img_png_common:len_len()),
  ToReadOffsetPct   = prep_read_header(img_png_common:start_idx_offset_pct(),    img_png_common:len_offset_pct()),
  ToReadSpacing     = prep_read_header(img_png_common:start_idx_spacing(),       img_png_common:len_spacing()),
  ToReadBitsPerByte = prep_read_header(img_png_common:start_idx_bits_per_byte(), img_png_common:len_bits_per_byte()),

  {State2, BitsMsgLen}      = read_bits(State,  ToReadMsgLen),
  {State3, BitsOffsetPct}   = read_bits(State2, ToReadOffsetPct),
  {State4, BitsSpacing}     = read_bits(State3, ToReadSpacing),
  {State5, BitsBitsPerByte} = read_bits(State4, ToReadBitsPerByte),

  [MsgLen]      = bits_to_nums(BitsMsgLen,      img_png_common:len_len()),
  [OffsetPct]   = bits_to_nums(BitsOffsetPct,   img_png_common:len_offset_pct()),
  [Spacing]     = bits_to_nums(BitsSpacing,     img_png_common:len_spacing()),
  [BitsPerByte] = bits_to_nums(BitsBitsPerByte, img_png_common:len_bits_per_byte()),

  Offset = img_png_common:compute_offset_from_pct(TotalNumBytes, OffsetPct),

  %% [[8, 9, 10, 11, 12, 13, 14, 15], [16, 17, 18, 19, 20, 21, 22, 23], ...]
  ToRead = img_prep_spec:prep_spec(
    MsgLen * 8,            % Number of bits
    Spacing,
    BitsPerByte,
    img_png_common:header_len() + Offset), % Starting byte number

  {_State6, Bits} = read_bits(State5, ToRead),
  Msg = bits_to_nums(Bits, 8),

  case OutputFormat of
    as_list   -> Msg;
    as_binary -> erlang:list_to_binary(Msg)
  end.

calc_num_bytes(State) ->
  {_Idx, RowBytes} = State#state.curr_row,
  erlang:byte_size(RowBytes)
    + img_png_common:num_bytes_in_rows(State#state.rest_rows).

prep_read_header(Base, NumBytes) ->
  [{Base + (N - 1), 0} || N  <- lists:seq(1, NumBytes)].

%%%
read_bits(State, ToRead) ->
  read_bits(State, ToRead, []).

%%%
read_bits(State, [], Acc) ->
  {State, lists:reverse(Acc)};

read_bits(
    #state{curr_byte_num = CurrByteNum} = State,
    [{ByteNumToRead, _} | _] = ToRead,
    Acc)
  when CurrByteNum =:= ByteNumToRead ->

  Byte         = get_current_byte(State),
  BitNumToRead = get_bit_num_to_read(ToRead),
  Bit          = img_bit:get_bit(Byte, BitNumToRead),

  read_bits(State, tl(ToRead), [Bit | Acc]);

read_bits(
    #state{curr_byte_num = CurrByteNum} = State,
    [{ByteNumToRead, _} | _] = ToRead,
    Acc)
  when CurrByteNum < ByteNumToRead -> % TODO

  State2 = advance_state(State),
  read_bits(State2, ToRead, Acc).

%%%
get_current_byte(State) ->
  CurrRowByteNum = State#state.curr_row_byte_num,
  {_Idx, CurrRowBytes} = State#state.curr_row,
  binary:at(CurrRowBytes, CurrRowByteNum).

%%%
get_bit_num_to_read([{_ByteNumToRead, BitNumToRead} | _Rest]) ->
  BitNumToRead.

bits_to_nums(Bits, BitsPerNum) ->
  bits_to_nums(Bits, BitsPerNum, []).

%%%
bits_to_nums([], _BitsPerNum, Acc) ->
  lists:reverse(Acc);

bits_to_nums(Bits, BitsPerNum, Acc) ->
  {CurrBits, Rest} = lists:split(BitsPerNum, Bits),

  %% CurrBits is a little-endian list, so reverse
  <<Num:BitsPerNum>> = << <<Bit:1>> || Bit <- lists:reverse(CurrBits) >>,

  bits_to_nums(Rest, BitsPerNum, [Num | Acc]).

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

  #state{curr_byte_num = CurrByteNum, rest_rows = [RestH | RestT]} = State,

  State2 = State#state{
    curr_byte_num = CurrByteNum + 1,
    curr_row_byte_num = 0,
    curr_row = RestH,
    rest_rows = RestT
  },
  State2.




-ifdef(TEST).

read_message_binary_test() ->
  read_message_tester(<<"db">>, 2, as_binary).

read_message_list_test() ->
  read_message_tester("db", 2, as_list).

read_message_tester(Msg, MsgByteLen, OutputFormat) ->
  %?debugMsg("read_message_test"),

  random:seed(erlang:now()),

  %% Need 4 * 8 bytes for offset, spacing, num bits/byte, and length,
  %% plus 2 for offset padding.
  %% Message itself is 2 * 8. Times 2 for spacing of 2, and divided by 3 for 3 bits per byte,
  %% but if not evenly divided by 3, add 1 more for the leftover bits.
  %% (32 + 2 + ((16 * 2 / 3) + 1) == 45)
  %% So 5 rows of 9

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

  Mod = [Mod_0_0, Mod_0_1, Mod_0_2, Mod_0_3, Mod_0_4, Mod_0_5, Mod_0_6, Mod_0_7, Mod_0_8,
    Mod_1_0, Mod_1_1, Mod_1_2, Mod_1_3, Mod_1_4, Mod_1_5, Mod_1_6, Mod_1_7, Mod_1_8,
    Mod_2_0, Mod_2_1, Mod_2_2, Mod_2_3, Mod_2_4, Mod_2_5, Mod_2_6, Mod_2_7, Mod_2_8,
    Mod_3_0, Mod_3_1, Mod_3_2, Mod_3_3, Mod_3_4, Mod_3_5, Mod_3_6, Mod_3_7, Mod_3_8,
    Mod_4_0, Mod_4_1, Mod_4_2, Mod_4_3, Mod_4_4, Mod_4_5, Mod_4_6, Mod_4_7, Mod_4_8,
    Mod_5_0, Mod_5_1, Mod_5_2, Mod_5_3, Mod_5_4, Mod_5_5, Mod_5_6, Mod_5_7, Mod_5_8],

  %% Of the form [{0, <<B0, B1, B2>>}, {1, <<B3, B4, B5>>}, ...]
  Pixels = img_png_common:create_pixels_list(Mod, 7),

  State = init_state2(Pixels),
  MsgRead = read_message(State, OutputFormat),
  ?assertEqual(Msg, MsgRead),

  ok.

bits_to_num_8_test() ->
  %?debugMsg("bits_to_num_8_test"),

  %% Little-endian: 42 and 137
  Bits = [0, 1, 0, 1, 0, 1, 0, 0] ++ [1, 0, 0, 1, 0, 0, 0, 1],
  Nums = bits_to_nums(Bits, 8),

  ?assertEqual([42, 137], Nums).

bits_to_num_16_test() ->
  %?debugMsg("bits_to_num_16_test"),

  %% Little-endian: 65000 and 298
  Bits = [0,0,0,1,0,1,1,1,1,0,1,1,1,1,1,1] ++ [0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0],
  Nums = bits_to_nums(Bits, 16),

  ?assertEqual([65000, 298], Nums).


prep_read_header_test() ->
  %?debugMsg("prep_read_header_test"),

  Base = 10,

  Expected = [
    {Base + 0, 0},
    {Base + 1, 0},
    {Base + 2, 0},
    {Base + 3, 0},
    {Base + 4, 0},
    {Base + 5, 0},
    {Base + 6, 0},
    {Base + 7, 0}
  ],

  Actual = prep_read_header(Base, 8),

  ?assertEqual(Expected, Actual),
  ok.

-endif.

