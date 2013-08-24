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
-mod_description("Automagically share articles, pictures, videos, and RSS Feed links through the day!.
 Inspired by http://bufferapp.com").
-mod_depends([admin]).
-mod_provides([buffer]).
-mod_prio(500).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([observe_admin_menu/3, manage_schema/2]).

%% API export
-export([share/4, join/1, post/4,
	 refresh_buffer_list/2, add_to_cron/5]).

%% include xmerl 
-include_lib("xmerl/include/xmerl.hrl").

-define(SERVER, ?MODULE). 
-define(DEPS_DIR,"./deps/ebin").
-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
% -include("include/mod_buffer.hrl").
-record(state, {context, buffers}).

observe_admin_menu(admin_menu, Acc, Context) ->
    [#menu_item{id=admin_buffer, parent=admin_modules,
		label=?__("Social Buffer", Context), url={admin_buffer},
		visiblecheck={acl, use, ?MODULE}} 
     | Acc].


%%%===================================================================
%%% API
%%%=================================================================== 

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
    
   %% load buffers and start sharing process
   Buffers = m_buffer:list(Context),
   {ok, #state{buffers=Buffers,context=z_context:new(Context)}}.

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
handle_cast({post, t, Id, Message}, #state{buffers=_Buffers, context=Z_Context} = Context) ->
    ConsumerKey = get_env(mb_twitter_ckey, false, Z_Context),
    ConsumerSecret = get_env(mb_twitter_csec, false, Z_Context),
    TwitterConsumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    AccessToken = get_env(mb_access_token, false, Z_Context),
    AccessTokenSecret = get_env(mb_access_token_secret, false, Z_Context),
    case oauth:post("https://api.twitter.com/1.1/statuses/update.json",
                    [{"status", Message}], TwitterConsumer, 
		    AccessToken, AccessTokenSecret) of
        {ok, {{_, 200, _}, _Header, _Body}} ->
            ?DEBUG(io:format("Mod_Buffer: Buffer ~p shared on Twitter.", [Id])),
	    m_buffer:mark_shared(Id, Z_Context),
	    GrowlMessage = io:format("Mod_Buffer: Buffer ~p shared on Twitter.", [Id]),
            z_render:growl(GrowlMessage, Z_Context),
	    {noreply, Context};
        R ->
            lager:warning("Mod_Buffer: Twitter error: ~p", [R]),
	    {noreply, Context}
    end;

%% @doc post to facebook
%% @todo GET access token and user id for buffered posts 
handle_cast({post, f, _Id, _Message}, #state{buffers=_Buffers, context=_Z_Context} = Context) ->
    {noreply, Context};

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
      #datamodel{categories=[
          {buffer, text, [{title, "Buffer"}]}
                 ]}, Context),

    ok.    

-spec get_env(term(), term(), #context{}) -> list() | false | true.
get_env(Key, Default, Context) ->
    case m_config:get_value(?MODULE, Key, Default, Context) of
	LP when is_binary(LP) ->
	    binary_to_list(LP);
	P -> P
    end.

%% @doc share post on Twiter and/or Facebook
-spec share(list(), integer(), string(), #context{}) -> ok | undefined.
share(Dest, Id, Message, Context) when is_list(Dest) -> 
    Destinations = z_string:split(Dest,","),
    F = fun(X)-> spawn(?MODULE, post, [list_to_atom(X), Id, Message, Context])end,
    _Reply = lists:foreach(F, Destinations),
    ok.
post(Destination, Id, Message, Context) when is_atom(Destination)-> 
   case is_xml_feed(Message) of 
       true ->
	   Entries = scan(Message),
	   Delay = get_env(mb_delay_time, 300, Context),
	   lists:foldl(
	     fun(Time)-> timter:apply_after(Time + Time, ?MODULE, share, 
					    {Destination, Id, Message, Context}) end, Delay, Entries);
       false ->
	   gen_server:cast(?SERVER, {post, Destination, Id, Message})
   end.



%% check if string is a link to an rss resource 
-spec is_xml_feed(string()) -> boolean().
is_xml_feed(Url) ->
   Tokens = string:tokens(Url, "."),
   Ext = lists:last(Tokens),
   Matches = ["xml", "atom", "rss"],
   case lists:member(Ext, Matches) of
       true ->
	    true;
       _Other ->
	    false
   end.

 
%% @doc Download RSS Feed
-spec scan(list())-> list() | undefined.
scan(Url)->
    case httpc:request(Url) of 
	{ok, {{_, 200, _}, _Header, Body}} ->
	    { Xml, _Rest } = xmerl_scan:string(Body),
	    format_entries(xmerl_xpath:string("//entry",Xml));
        _Other ->
	 undefined
    end.

%% @doc format the scanned XML data and return the posts, relevant links
-spec format_entries(string()) -> list().
format_entries(XmlNodes)-> 
    format_entries(XmlNodes, []).
format_entries([], Acc) -> Acc;
format_entries([Node|Rest], Acc) ->
  [ #xmlText{value=Title} ] = xmerl_xpath:string("title/text()", Node),
  [ #xmlAttribute{value=Link} ] = xmerl_xpath:string("link/@href", Node),
  Message = xmerl:export_simple_content([{a,[{href,Link}],[Title]}],xmerl_xml),
  %% Append html anchor tag to the post post title 
  %%Message2 = Title ++ " " ++ xmerl_ucs:to_utf8(Message),
  NewAcc = Acc ++ [Message],
  format_entries(Rest, NewAcc).
  


%% @doc add new cron job
-spec add_to_cron(integer(), string(), string(), tuple(), #context{}) -> term().
add_to_cron(BufferId, Message, Destination, Schedule, Context) ->
    %% parsing time definition into erlang term
    JobId = BufferId,
    When = cron_task:parse_when(Schedule),
    %Mfa  = cron_task:parse_mfa(?MODULE, share, {Destination, BufferId, Message}),
    Task = cron_task:new(When, ?MODULE, share, [Destination, BufferId, Message, Context]),
    case z_notifier:first({cron_job_insert, JobId, Task}, Context) of
	{ok,_}	-> RA = [{dialog_close, []}, {reload, []}],
		   z_render:wire(RA, Context);

	E	-> EText = io_lib:format("Error: ~p", [E]),
		   z_render:growl_error(EText, Context)
    end.

%% @doc split a list into a string.
%% Example: ["t","fb","g"] -> "t,fb,g"
join(L) ->
    join(L, "").
join([], Acc)->
    Acc;
join([H], Acc) ->
    join([], Acc ++ H);
join([H|T], Acc) ->
    join(T, Acc ++ H ++ ",").



refresh_buffer_list(TargetId, Context)->
    Buffers = m_buffer:list(Context),
    Html = z_template:render("buffer_list.tpl",[{buffers,Buffers}], Context),
    z_render:appear(TargetId, Html, Context).
