-module(thumb_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {riak :: pid()}).

-define(COMMAND, "bin/thumber").
-define(BUCKET, {<<"thumbs_type3">>, <<"thumbs">>}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    process_flag(trap_exit, true),
    {_, RiakHost} = lists:keyfind(riak_host, 1, Args),
    {_, RiakPort} = lists:keyfind(riak_port, 1, Args),

    {ok, Riak} = riakc_pb_socket:start_link(RiakHost, RiakPort),
    {ok, #state{riak=Riak}}.

handle_call({save, ImageId, URL}, _From, #state{riak=Riak}=State) ->
    {reply, save(Riak, ImageId, URL), State};
handle_call({get, ImageId}, _From, #state{riak=Riak}=State) ->
    {reply, get(Riak, ImageId), State}.

handle_cast({save, ImageId, URL}, #state{riak=Riak}=State) ->
    save(Riak, ImageId, URL),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get(Riak, ImageId) ->
    Fetched = case riakc_pb_socket:get(Riak, ?BUCKET, ImageId) of
                  {ok, F} ->
                      F;
                  {error,notfound} ->
                      {ok, F} = riakc_pb_socket:get(Riak, <<"thumbs">>, <<"default">>),
                      F
              end,

    Thumb = riakc_obj:get_value(Fetched),
    Thumb.

save(Riak, ImageId, URL) ->
    case riakc_pb_socket:get(Riak, ?BUCKET, ImageId) of
        {ok, _} -> io:format("Already exists thumbnail for ~s~n", [URL]);
        {error, _} ->
            Port = open_port({spawn, ?COMMAND}, [binary, exit_status]),
            true = port_command(Port, io_lib:format("~s\n", [binary_to_list(URL)])),
            Thumb = wait(Port, <<>>),

            Obj = riakc_obj:new(?BUCKET, ImageId, Thumb),
            ok = riakc_pb_socket:put(Riak, Obj),

            io:format("Saved thumbnail for ~s~n", [URL])
    end.

wait(Port, Data) ->
    receive
        {Port, {data, Thumb}} ->
            wait(Port, <<Data/binary, Thumb/binary>>);
        {Port, {exit_status, 0}} ->
            Data;
        {Port, {exit_status, Status}} ->
            {error, Status}
    after
        10000 ->
            {error, timeout}
    end.
