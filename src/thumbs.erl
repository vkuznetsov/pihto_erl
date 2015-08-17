-module(thumbs).
-behaviour(gen_server).

-include_lib("riakc/include/riakc.hrl").

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% API
-export([get/2, save_async/3, save_sync/3]).

save_async(Type, ImageId, URL) ->
  gen_server:cast(?MODULE, {save, Type, ImageId, URL}).

save_sync(Type, ImageId, URL) ->
  gen_server:call(?MODULE, {save, Type, ImageId, URL}, 30000).

get(Type, ImageId) ->
  gen_server:call(?MODULE, {get, Type, ImageId}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, _Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087).

terminate(_Reason, _State) ->
  ok.

handle_cast({save, Type, ImageId, URL}, Riak) ->
  save(Type, ImageId, URL, Riak),
  {noreply, Riak}.

handle_call({save, Type, ImageId, URL}, _From, Riak) ->
  save(Type, ImageId, URL, Riak),
  {reply, ok, Riak};

handle_call({get, Type, ImageId}, _From, Riak) ->
  Bucket = case Type of
             thumb -> <<"thumbs">>;
             image -> <<"images">>
           end,

  Fetched = case riakc_pb_socket:get(Riak, Bucket, ImageId) of
    {ok, F} ->
      F;
    {error,notfound} ->
      {ok, F} = riakc_pb_socket:get(Riak, Bucket, <<"default">>),
      F
  end,

  Thumb = riakc_obj:get_value(Fetched),
  {reply, Thumb, Riak}.

handle_info(_Message, State) ->
  {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

save(Type, ImageId, URL, Riak) ->
  {Command, Bucket} = case Type of
    thumb -> {"bin/thumber", <<"thumbs">>};
    image -> {"bin/reductor", <<"images">>}
  end,

  Port = open_port({spawn, Command}, [binary, exit_status]),
  true = port_command(Port, io_lib:format("~s\n", [binary_to_list(URL)])),
  Thumb = wait(Port, Riak, <<>>),
  Obj = riakc_obj:new(Bucket, ImageId, Thumb),
  riakc_pb_socket:put(Riak, Obj),
  io:format("Save ~s for ~s~n", [Type, URL]),
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
