-module(img_png).

-export([add_message_from_file/3]).
-export([add_message_from_file/4]).
-export([add_message/3]).
-export([add_message/4]).

-export([read_message/2]).
-export([read_and_write_message/2]).

-export([check_message_fit/2]).
-export([check_message_fit/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%
add_message_from_file(MsgFile, InFile, OutFile) ->
  add_message_from_file(MsgFile, InFile, OutFile, []).

add_message_from_file(MsgFile, InFile, OutFile, Opts) ->
  {ok, Bin} = file:read_file(MsgFile),
  add_message(Bin, InFile, OutFile, Opts).

%%%
add_message(Msg, InFile, OutFile) ->
  add_message(Msg, InFile, OutFile, []).

add_message(Msg, InFile, OutFile, Opts) ->
  State = img_png_write:init_state(InFile),
  State2 = img_png_write:add_message(State, Msg, Opts),
  ok = img_png_write:save(State2, OutFile).

%%%
%%% OutputFormat: as_list, as_binary
read_message(InFile, OutputFormat) ->
  State = img_png_read:init_state(InFile),
  img_png_read:read_message(State, OutputFormat).

read_and_write_message(InFile, OutFile) ->
  %% file:write_file/2 takes iodata as its input, so can take either the list or
  %%   binary output from read_message/2. We'll specify as_list to avoid the
  %%   erlang:list_to_binary call before its return.
  Msg = read_message(InFile, as_list),
  ok = file:write_file(OutFile, Msg).

%%%
check_message_fit(Msg, InFile) ->
  check_message_fit(Msg, InFile, []).

check_message_fit(Msg, InFile, Opts) ->
  State = img_png_write:init_state(InFile),
  img_png_write:check_message_fit(State, Msg, Opts).



-ifdef(TEST).

add_and_read_list_test() ->
  Msg = "Erlang is a programming language used to build massively scalable soft "
    ++ "real-time systems with requirements on high availability.",
  add_and_read_tester(Msg, as_list).

add_and_read_binary_test() ->
  S = "Erlang is a programming language used to build massively scalable soft "
    ++ "real-time systems with requirements on high availability.",
  Msg = erlang:list_to_binary(S),
  add_and_read_tester(Msg, as_binary).

add_and_read_tester(Msg, OutputFormat) ->
  %?debugMsg("add_and_read_test"),

  InFile = "all_black.png",
  OutFile = "test_out.png",

  %% code:priv_dir/1 doesn't succeed under eunit
  %% TODO:
  %BaseDir = code:priv_dir(?MODULE),
  BaseDir = "../priv/",
  InPath = filename:join(BaseDir, InFile),
  OutPath = filename:join(BaseDir, OutFile),

  io:format("~p ~p~n", [InPath, OutPath]),

  add_message(Msg, InPath, OutPath),
  MsgRead = read_message(OutPath, OutputFormat),

  ?assertEqual(Msg, MsgRead),
  ok.

-endif.



