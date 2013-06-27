%%%-------------------------------------------------------------------
%%% @author Mawuli <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2013 by Mawuli Adzaku<mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(controller_admin_buffer).
-author("Mawuli Adzaku <mawuli@mawuli.me>").

%% API
-export([html/1, is_authorized/2, event/2]).

-include_lib("controller_html_helper.hrl").


%%%===================================================================
%%% API
%%%===================================================================
is_authorized(ReqData, Context) ->
   z_acl:wm_is_authorized(use, mod_buffer, ReqData, Context).

html(Context) ->
    Vars = [],
    Html = z_template:render("buffer.tpl", Vars, Context),
    z_context:output(Html, Context).

event({postback,{buffer_list, _Args}, _TriggerId, _TargetId}, Context) -> 
    z_render:growl(?__("Listing buffered items.", Context), Context);

event({postback,{buffer_logs, _Args}, _TriggerId, _TargetId}, Context) -> 
    z_render:growl(?__("Social Buffer Logs.", Context), Context);

event({postback,{buffer_help, _Args}, _TriggerId, _TargetId}, Context) -> 
    z_render:growl(?__("Social Buffer Help.", Context), Context);

event({postback,{buffer_new_form, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Displaying new buffer form.", Context), Context),
    Html = z_template:render("buffer_add_form.tpl", [],Context),
    z_render:appear(TargetId,Html,Context);
    
event({submit, create_buffer, _FormId, _TargetId}, Context) ->
    io:format("Mod_Buffer POST DATA :  ~p", [z_context:get_q_all_noz(Context)]),
    z_render:growl(?__("Adding new buffer", Context), Context);

event({postback,_Params, _TriggerId, _TargetId}, Context) -> 
    z_render:growl(?__("Social Buffer : Unmatched postback params.", Context), Context).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
