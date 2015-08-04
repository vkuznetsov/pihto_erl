-module(thumbs).
-behaviour(gen_server).

-include_lib("riakc/include/riakc.hrl").

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% API
-export([get/1, save_async/2, save_sync/2]).

save_async(ImageId, URL) ->
  gen_server:cast(?MODULE, {save, ImageId, URL}).

save_sync(ImageId, URL) ->
  gen_server:call(?MODULE, {save, ImageId, URL}, 30000).

get(ImageId) ->
  gen_server:call(?MODULE, {get, ImageId}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, _Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087).

terminate(_Reason, _State) ->
  ok.

handle_cast({save, ImageId, URL}, Riak) ->
  save(ImageId, URL, Riak),
  {noreply, Riak}.

handle_call({save, ImageId, URL}, _From, Riak) ->
  save(ImageId, URL, Riak),
  {reply, ok, Riak};

handle_call({get, ImageId}, _From, Riak) ->
  Fetched = case riakc_pb_socket:get(Riak, <<"thumbs">>, ImageId) of
    {ok, Fetched} ->
      Fetched;
    {error,notfound} ->
      {ok, Fetched} = riakc_pb_socket:get(Riak, <<"thumbs">>, <<"default">>),
      Fetched
  end,

  Thumb = riakc_obj:get_value(Fetched),
  {reply, Thumb, Riak}.

handle_info(_Message, State) ->
  {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

save(ImageId, URL, Riak) ->
  Port = open_port({spawn, "bin/thumber"}, [binary, exit_status]),
  true = port_command(Port, io_lib:format("~s\n", [binary_to_list(URL)])),
  Thumb = wait(Port, Riak, <<>>),
  Obj = riakc_obj:new(<<"thumbs">>, ImageId, Thumb),
  riakc_pb_socket:put(Riak, Obj),
  ok.

wait(Port, Riak, Data) ->
  receive
    {Port, {data, Thumb}} ->
      wait(Port, Riak, <<Data/binary, Thumb/binary>>);
    {Port, {exit_status, 0}} ->
      Data;
    {Port, {exit_status, Status}} ->
      {error, Status}
  after
    10000 ->
      {error, timeout}
  end.