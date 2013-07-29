%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, mawuli
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2013 by Mawuli Adzaku<mawuli@mawuli.me>
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
-export([knock/0, enter/2, twitter_auth/0]).

-define(BASE_URL(X), "https://api.twitter.com/" ++ X).
-define(SERVER, ?MODULE). 
-define(DEPS_DIR,"./deps/ebin").
-include_lib("zotonic.hrl").
-include("include/mod_buffer.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-record(state, {context, twitter_pid, buffers}).

%% Twitter Application keys
-define(Key,"5fjdd86uFbrun7rxtLQ").
-define(Secret,"CKs39sTmRTzeiXix9ZFAcimlVLTUxPcQ7IATvMXG3Q").
-define(Consumer,{?Key, ?Secret, hmac_sha1}).

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
    
   %% request for access tokens if missing    
   %% load buffers and start sharing process

   {ok, #state{context=z_context:new(Context)}}.

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

%%@todo: document and add docs
knock()->     
    RequestTokenURL = ?BASE_URL("oauth/request_token"),
    {ok, ResponseR} = oauth:get(RequestTokenURL, [], ?Consumer, "", ""),
    ParamsR = oauth_http:response_params(ResponseR),
    TokenR = oauth:token(ParamsR),
    TokenSecretR = oauth:token_secret(ParamsR),
    {TokenR, TokenSecretR}.
    

%%@todo: document and add specs
enter(Token, TokenSecret) ->
    SignedParams = oauth:signed_params("GET", ?BASE_URL("oauth/authorize"), [],
                               ?Consumer, Token, TokenSecret),
    oauth:uri(?BASE_URL("oauth/authorize"), SignedParams).

twitter_auth()->
    {RequestToken, RequestTokenSecret} = knock(),
    enter(RequestToken, RequestTokenSecret).


