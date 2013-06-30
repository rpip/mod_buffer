%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, mawuli
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2013 by Mawuli Adzaku<mawuli@mawuli.me>
%%% @todo : add_item, add_feed, get_all, is_rss, parse_feed,
%%% get_feed_data, share_buffer(now or schedule)
%%% @todo :  custom type specs and table initialization
%%%-------------------------------------------------------------------
-module(mod_buffer).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-behaviour(gen_server).

-mod_title("Social Buffer").
-mod_description("Automagically share articles, pictures, videos, and RSS Feed links through the day!. Inspired by http://bufferapp.com").
-mod_depends([admin,mod_cron]).
-mod_provides([buffer]).
-mod_prio(500).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([observe_admin_menu/3, manage_schema/2]).
 
%% API
-export([share_buffer/2, share/4, add_item/1, add_feed/1, get_all/1, is_rss/1, parse_feed/1,
get_feed_data/1]).

-define(BASE_URL(X), "http://www.twitter.com/" ++ X).
-define(SERVER, ?MODULE). 
-include_lib("zotonic.hrl").
-include("include/mod_buffer.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-record(state, {context, twitter_pid, buffer}).



%%%===================================================================
%%% API
%%%=================================================================== 
observe_admin_menu(admin_menu, Acc, Context) -> [
                   #menu_item{id=admin_buffer, parent=admin_modules,
                   label=?__("Social Buffer", Context), url={admin_buffer},
                   visiblecheck={acl, use, mod_buffer}} |Acc].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) when is_list(Args) ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
   {context, Context} = proplists:lookup(context, Args),

   manage_schema(install, Context),
   
   %% load all buffered posts from db
   Buffer = [],

   %% Start the twitter process
   case share_buffer(Buffer,Context) of
        Pid when is_pid(Pid) ->
            {ok, #state{context=z_context:new(Context),twitter_pid=Pid, buffer=Buffer}};
        undefined ->
            {ok, #state{context=z_context:new(Context)}};
        not_configured ->
            z_session_manager:broadcast(#broadcast{type="error", message="No configuration (mod_twitter.api_login / mod_twitter.api_password) found, not starting.", title="Social Buffer", stay=true}, z_acl:sudo(Context)),
            {ok, #state{context=z_context:new(Context)}}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    z_notifier:observe(restart_twitter, self(), State#state.context),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc Install tables used for storing buffers
manage_schema(install, Context) ->
    case z_db:table_exists(buffer, Context) of
        false ->
            z_db:create_table(buffer, [
                        #column_def{name=id, type="serial", is_nullable=false},
                        #column_def{name=user_id, type="integer", is_nullable=true},
                    #column_def{name=message, type="character varying", length=140, is_nullable=false},
                    #column_def{name=destination, type="character varying", length=32, is_nullable=false},
                        #column_def{name=schedule, type="character varying", length=70 ,is_nullable=true},
                        #column_def{name=status, type="character varying", length=32, is_nullable=false},
                        #column_def{name=created, type="timestamp", is_nullable=true},
                        #column_def{name=modified, type="timestamp", is_nullable=true}
                    ], Context);
        true ->
            ok
    end,
    z_datamodel:manage(
      mod_buffer,
      #datamodel{categories=
                 [
                  {buffer, undefined, [{title, "Buffer"}]}
                 ]}, Context),

    ok.


%% stolen from mod_twitter
share_buffer(Buffer, Context) when is_record(Buffer, buffer)->    
            %% spawn a share process and return the pid
            spawn_link(?MODULE, share, [Buffer#buffer.destination, 
                                        [{status, Buffer#buffer.message}],
                                        fun(X) -> io:format("mod_buffer response from Twitter : ~p", 
                                                            [X]) end, Context]).



share(<<"t">>, Args, Fun, Context) ->
    Login = case m_config:get_value(?MODULE, twitter_username, false, Context) of
                LB when is_binary(LB) ->
                    binary_to_list(LB);
                L -> L
            end,
    Pass  = case m_config:get_value(?MODULE, twitter_password, false, Context) of
                LP when is_binary(LP) ->
                    binary_to_list(LP);
                P -> P
            end,
    lager:info("Twitter: (~p) Username = ~p", [z_context:site(Context), Login]),
    case Login of
        false ->
            lager:warning(": (~p) No username/password configuration.", [z_context:site(Context)]),
            not_configured;
        _ ->
            z_session_manager:broadcast(#broadcast{type="notice", message="ready to share social buffer.. on Twitter.", title="Social Buffer", stay=false}, Context), 

    %% prepare request data and post to Twitter
    Url = build_url("statuses/updates.xml", Args),
    Body = compose_body(Args),
    case httpc:request(post, {?BASE_URL(Url), headers(Login, Pass), "application/x-www-form-urlencoded", Body} , [{timeout, 6000}], []) of
        {ok, {_, _, Body2}} -> Fun(Body2);
        Other -> {error, Other}
    end
end.


build_url(Url, []) -> Url;
build_url(Url, Args) ->
    Url ++ "?" ++ lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end, [],
            [K ++ "=" ++ z_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

headers(nil, nil) -> [{"User-Agent", "Zotonic mod_buffer/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    Basic = "Basic " ++ binary_to_list(base64:encode(User ++ ":" ++ Pass)),
    [{"User-Agent", "Zotonic mod_buffer/0.1"}, {"Authorization", Basic}, {"Host", "twitter.com"}].


compose_body(Args) ->
    lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [K ++ "=" ++ z_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

get_all(_Context)->
  ok.

add_item(_Item) ->
    ok.

is_rss(Arg) ->
    case z_string:ends_with(".rss", Arg) or z_string:ends_with(".xml", Arg) of
      true ->
            true;
      _  ->
            false                                                                     
      end.

add_feed(_URL) ->
    ok.

get_feed_data(_URL) ->
    %Result httpc:request(URL).
    ok.

parse_feed(_Feed)->
    ok.
