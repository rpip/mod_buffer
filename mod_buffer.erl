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
-mod_depends([admin,twitter]).
-mod_provides([buffer]).
-mod_prio(500).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([observe_admin_menu/3]).
 

-define(SERVER, ?MODULE). 
-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-record(state, {context, buffer_queue}).

%% Request URLS
%define(Twitter, https://api.twitter.com/1/statuses/update.json
%params-> status, include_entities:true


%%%===================================================================
%%% API
%%%===================================================================
observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_buffer,
                parent=admin_modules,
                label=?__("Social Buffer", Context),
                url={admin_buffer},
                visiblecheck={acl, use, mod_buffer}}
     
     |Acc].

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
   %% load all buffers from db
   BufferQueue = [],
   {ok, #state{context=z_context:new(Context), buffer_queue=BufferQueue}}.

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
terminate(_Reason, _State) ->
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
