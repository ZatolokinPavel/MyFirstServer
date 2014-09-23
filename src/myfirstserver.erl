-module(myfirstserver).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(cache_item, {expire_date, user_record}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([insert/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    io:format("starting serv link~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(Data, Time) ->
    {Mega, Seconds, _} = now(),
    ExpDate = Mega*1000000+Seconds+Time,
    Record = #cache_item{expire_date=ExpDate, user_record=Data},
    gen_server:call(?MODULE, {insert, Record}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("starting serv init~n"),
    ets:new(for_cache, [named_table, public, set,
            {keypos, #cache_item.expire_date}]),
    self() ! revisionETS,   % Посылаем сообщение самому себе
    {ok, Args}.

handle_call({insert, Record}, _From, State) ->
    ets:insert(for_cache, Record),
    {reply, inserted, State};

handle_call(_Request, _From, State) ->      % синхронно
    {reply, ok, State}.

handle_cast(_Msg, State) ->                  % асинхронно
    {noreply, State}.

handle_info(revisionETS, State) ->
    FirstElement = ets:first(for_cache),
    revisionETS(FirstElement),
    erlang:send_after(5000, self(), revisionETS),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(for_cache),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

revisionETS(Key) when Key /= '$end_of_table' ->
    NextKey = ets:next(for_cache, Key),
    Pos = #cache_item.expire_date,
    ElementTime = ets:lookup_element(for_cache, Key, Pos),
    {Mega, Seconds, _} = now(),
    case ElementTime > Mega*1000000+Seconds of
        false -> ets:delete(for_cache, Key);
        true  -> true
    end,
    revisionETS(NextKey);
revisionETS('$end_of_table') ->
    ok.
