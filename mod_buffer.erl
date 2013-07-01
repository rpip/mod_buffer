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
-export([share_buffer/2, share/4, request/5, compose_body/1,oauth_test/0]).

-define(BASE_URL(X), "https://api.twitter.com/1.1/" ++ X).
-define(SERVER, ?MODULE). 
-define(DEPS_DIR,"./deps/ebin").
-include_lib("zotonic.hrl").
-include("include/mod_buffer.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-record(state, {context, twitter_pid, buffers}).



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
    
   %% add deps directory to code path
   code:add_path(?DEPS_DIR),

   %% setup buffer table
   manage_schema(install, Context),

    
   %% load all bufferes from db
   Buffers = m_buffer:list(Context),

   %% Start sharing the buffers
   %% use config's from mod_twitter and mod_facebook
   case share_buffer(Buffers,Context) of
        Pid when is_pid(Pid) ->
            {ok, #state{context=z_context:new(Context),twitter_pid=Pid, buffers=Buffers}};
        not_configured ->
            z_session_manager:broadcast(#broadcast{type="error", message="No configuration (mod_twitter.api_login / mod_twitter.api_password) found, not starting.", title="Social Buffer", stay=true}, z_acl:sudo(Context)),
           {ok, #state{context=z_context:new(Context)}};
         _ ->
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


%% @doc Share the buffered items
share_buffer(Buffers, Context) when is_list(Buffers)->    
            %% spawn a share process and return the pid
            [spawn_link(?MODULE, share, [proplists:get_value(destination, Buffer), 
                                        [{status, proplists:get_value(message, Buffer)}],
                                        fun(X) -> io:format("mod_buffer response from Twitter : ~p ~n", 
                                                            [X]) end, Context]) 
             || Buffer <- Buffers ].



%% @doc Share buffer on Twitter
share("t", Args, Fun, Context) ->
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
    lager:info("Twitter: (~p) Username = ~p and Password = ~p", [z_context:site(Context), Login, Pass]),
    case Login of
        false ->
            lager:warning(": (~p) No username/password configuration.", [z_context:site(Context)]),
            not_configured;
        _ ->
            z_session_manager:broadcast(#broadcast{type="notice", message="ready to share social buffer.. on Twitter.", title="Social Buffer", stay=false}, Context), 

    %% prepare request data and post to Twitter
    Url = build_url("statuses/update.json", Args),
    Body = compose_body(Args),
    request(post, Url, {Login,Pass},Body,Fun)
    end;


share(Destination,Args,_Fun,_Context) ->
    io:format("Mod_Buffer sharing : [~p , ~p]",[Destination,Args]).

build_url(Url, []) -> Url;
build_url(Url, Args) ->
    Url ++ "?" ++ lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end, [],
            [term_to_list(K) ++ "=" ++ z_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

headers(nil, nil) -> [{"User-Agent", "Zotonic ModBuffer/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    Basic = "Basic " ++ base64:encode_to_string(User ++ ":" ++ Pass),
    [{"User-Agent", "Zotonic Mod_Buffer/0.1"}, {"Authorization", Basic}, {"Host", "api.twitter.com"}].


compose_body(Args) ->
    lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [term_to_list(K) ++ "=" ++ z_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

%% @doc make HTTP POST request to twitter
request(post, Url,{Login,Pass},Body, Fun) ->
%    Taken from mod_twitter :
%    URL = "https://" ++ Login ++ ":" ++ Pass ++ "@api.twitter.com/1.1/statuses/update.json",
%    case httpc:request(post, 
%                       {URL, headers(Login,Pass), "application/x-www-form-urlencoded", Body},
%                       [{timeout, 6000}],[]) of
    case httpc:request(post, {?BASE_URL(Url), headers(Login, Pass), "application/x-www-form-urlencoded", Body}, [{timeout, 12000}], []) of
        {ok, {_, _, Body2}} -> Fun(Body2);
        Other -> {error, Other}  
    end.


term_to_list(Term) ->
    L = io_lib:format("~p",[Term]),
    lists:flatten(L).


    

URL = "https://api.twitter.com/1/statuses/update.json",
    {ok, Response} = oauth:post(URL, [{"status", "Hello World"}], Consumer, AccessToken, AccessTokenSecret),  
    oauth:params_decode(Response).
