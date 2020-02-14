%%%-----------------------------------------------------------------------------
%%% @doc for pulling the stats in un-aggregated form to a http request @end
%%%-----------------------------------------------------------------------------
-module(stats_wm).
-include_lib("webmachine/include/webmachine.hrl").

%% wm resource exports
-export([
    init/1,
    encodings_provided/2,
    content_types_provided/2,
    service_available/2,
    produce_body/2,
    pretty_print/2]).

-record(ctx, {
    prefix,     %% prefix for resource uris
    app        %% local | {node(), atom()}
}).

-type props()       :: proplists:proplist().
-type context()     :: #ctx{}.
-type rd ()         :: webmachine:wrq().
-type encoding()    :: string().
-type encoder()     :: fun((iodata()) -> iodata()).
-type enc_prov()    :: [{encoding(), encoder()}].
-type media()       :: string().
-type handler()     :: atom().
-type ctype()       :: [{media(), handler()}].
-type halt()        :: {error, term()} | {halt, 200..599}.

-define(CONTENT_TYPE,               <<"content-type">>).
-define(PLAIN_TEXT_CONTENT_TYPE,          "text/plain").
-define(JSON_CONTENT_TYPE,          "application/json").
-define(PLAIN_TEXT_HEADER,         [{?CONTENT_TYPE, ?PLAIN_TEXT_CONTENT_TYPE}]).
-define(JSON_HEADER,                     [{?CONTENT_TYPE, ?JSON_CONTENT_TYPE}]).
-define(DEFAULT_ENCODINGS,         [{ "identity", fun(X) -> X end},
                                         {"gzip", fun(X) -> zlib:gzip(X) end}]).
%%%-----------------------------------------------------------------------------
%% @doc
%% receives a config property list from dispatcher as the Arg, for every request
%% to this module, if successful should return {ok, Context}, Initialise the
%% resource and pull out needed properties
%% @end
%%%-----------------------------------------------------------------------------
-spec(init(props()) -> {ok, context()}).
init(Props) ->
    {ok, #ctx{prefix = proplists:get_value(prefix, Props),
              app    = proplists:get_value(app,    Props)}}.

%%%-----------------------------------------------------------------------------
%% @doc
%% Get a list of encodings that this resource provides,
%% "identity" -> all methods
%% "gzip"     -> GET method
%% @end
%%%-----------------------------------------------------------------------------
-spec(encodings_provided(rd(), context()) -> {enc_prov(), rd(), context()}).
encodings_provided(Req, Ctx) ->
    case wrq:method(Req) of
        'GET' ->
            {?DEFAULT_ENCODINGS, Req, Ctx};
        _ ->
            {[{"identity", fun(X) -> X end}], Req, Ctx}
    end.


%%%-----------------------------------------------------------------------------
%% @doc
%% Return the list of pairs -> the media in ctype() being a string of
%% content-type format (such as jsonheader) and the handler being and atom of
%% the function that can provide a representation of the media
%% @end
%%%-----------------------------------------------------------------------------
-spec(content_types_provided(rd(), context()) ->
    {ctype(), rd(), context()}).
content_types_provided(Req, Ctx) ->
    {[{?JSON_CONTENT_TYPE, produce_body},
        {?PLAIN_TEXT_CONTENT_TYPE, pretty_print}],
        Req, Ctx}.

%%%-----------------------------------------------------------------------------
%% @doc Determine a connection to app @end
%%%-----------------------------------------------------------------------------
-spec(service_available(rd(), context()) ->
    {boolean() | halt(), rd(), context()}).
service_available(Req, Ctx=#ctx{}) ->
    {true, Req, Ctx}.

%%%-----------------------------------------------------------------------------
%% @doc Retrieve the stats and return an encoded json object to send back @end
%%%-----------------------------------------------------------------------------
-spec(produce_body(rd(), context()) -> {list(), rd(), context()}).
produce_body(ReqData, Ctx) ->
    Body  = stats_push_util:json_stats(['_']),
    {Body, ReqData, Ctx}.


%%%-----------------------------------------------------------------------------
%% @doc Format the response JSON object in a "pretty-printed" style. @end
%%%-----------------------------------------------------------------------------
-spec(pretty_print(rd(), context()) -> {string(), rd(), context()}).
pretty_print(RD1, C1=#ctx{}) ->
    {Json, RD2, C2} = produce_body(RD1, C1),
    String = json_pp:print(binary_to_list(list_to_binary(Json))),
    {String, RD2, C2}.
