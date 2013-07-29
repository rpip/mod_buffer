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

event({postback,{buffer_new_form, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Displaying new buffer form.", Context), Context),
    Html = z_template:render("buffer_add_form.tpl", [],Context),
    z_render:appear(TargetId,Html,Context);
    
event({submit, create_buffer, _FormId, _TargetId}, Context) ->
    Message = z_context:get_q("message",Context),
    Schedule = z_context:get_q("schedule",Context),
    Destination = z_context:get_q("destination",Context),
    Status = z_context:get_q("status",Context),
    m_buffer:insert([Message, Schedule, Destination, Status], Context),
    z_render:growl(?__("Adding new buffer", Context), Context);

event({postback,{buffer_list, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Listing buffered items.", Context), Context),
    Buffers = m_buffer:list(Context),
    Html = z_template:render("buffer_list.tpl",[{buffers,Buffers}], Context),
    z_render:appear(TargetId,Html,Context);

event({postback,{buffer_help, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Social Buffer Help.", Context), Context),
    Html = z_template:render("buffer_help.tpl",[], Context),
    z_render:appear(TargetId,Html,Context);

event({postback,{buffer_delete, [{id, BufferId},{target_id,TargetId}]}, _TriggerId, _}, Context) -> 
    m_buffer:delete(BufferId, Context), 
    Buffers = m_buffer:list(Context),
    Html = z_template:render("buffer_list.tpl",[{buffers,Buffers}], Context),
    z_render:growl(?__("Buffer deleted", Context), Context),
    z_render:appear(TargetId,Html,Context); 


event({postback,{buffer_update, _Params}, _TriggerId, _}, Context) -> 
    BufferId = z_context:get_q("id",Context),
    Message = z_context:get_q("message",Context),
    Schedule = z_context:get_q("schedule",Context),
    Destination = z_context:get_q("destination",Context),
    Status = z_context:get_q("status",Context),
    m_buffer:update(BufferId,[Message, Schedule, Destination, Status], Context),
    z_render:growl(?__("Buffer updated", Context), Context);


event({postback,{buffer_share, [{id, BufferId}]}, _TriggerId, _}, Context) -> 
   z_render:growl(?__("Buffer updated", Context), Context),
   Buffer = m_buffer:get(BufferId,Context),
   Destination = proplists:get_value(destination,Buffer),
   mod_buffer:share(Destination, Buffer),
   m_buffer:update(BufferId,[{status,"shared"}], Context);

event({_Event, Params, _TriggerId, _TargetId}, Context) ->
    z_render:growl(?__("Social Buffer : Unmatched event.", Context),
    Context), io:format("Mod_Buffer Event params : ~p", [Params]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
