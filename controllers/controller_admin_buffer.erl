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
    FormData = z_context:get_q_all(Context),
    Destination = proplists:get_all_values("destination", FormData),
    Destination2 = mod_buffer:join(Destination),
    Message = proplists:get_value("message", FormData),
    Schedule = proplists:get_value("schedule", FormData),
    Status = proplists:get_value("status", FormData),
    m_buffer:insert([Message, Schedule, Destination2, Status], Context),
    z_render:growl(?__("Adding new buffer", Context), Context);

event({submit, update_buffer, _FormId, _TargetId}, Context) ->
    FormData = z_context:get_q_all(Context),
    Destination = proplists:get_all_values("destination", FormData),
    Destination2 = mod_buffer:join(Destination),
    BufferId = proplists:get_value("id", FormData),
    Message = proplists:get_value("message", FormData),
    Schedule = proplists:get_value("schedule", FormData),
    Props = [
        {user_id, z_acl:user(Context)},
        {message, z_html:escape(z_string:trim(Message))},
	{schedule, z_string:trim(Schedule)},
	{destination, Destination2},
	{modified, erlang:localtime()}
    ],
    case z_db:update(buffer, BufferId, Props, Context) of
	{ok, _} = _Result ->
            z_render:growl("Buffer updated", Context);
        _Error ->
            z_render:growl("Error updating buffer", Context)
    end,
    %% refresh buffers
    mod_buffer:refresh_buffer_list("content", Context);

event({postback,{buffer_list, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Listing buffered items.", Context), Context),
    mod_buffer:refresh_buffer_list(TargetId, Context);

event({postback,{buffer_help, _Args}, _TriggerId, TargetId}, Context) -> 
    z_render:growl(?__("Social Buffer Help.", Context), Context),
    Html = z_template:render("buffer_help.tpl",[], Context),
    z_render:appear(TargetId, Html, Context);

event(#postback_notify{message="delete-buffer"}, Context) ->
    BufferId = z_context:get_q("buffer_id", Context),
    m_buffer:delete(BufferId, Context), 
    z_render:growl(?__("Buffer deleted", Context), Context);

event({postback,{buffer_share, [{id, Id}]}, _TriggerId, _}, Context) -> 
   z_render:growl(?__("Buffer updated", Context), Context),
   Buffer = m_buffer:get(Id,Context),
   Message = binary_to_list(proplists:get_value(message,Buffer)),
   Destination = binary_to_list(proplists:get_value(destination,Buffer)),
   mod_buffer:share(Destination, Id, Message, Context),
   z_render:growl(?__(io:format("Sharing buffer ~i", [Id]), Context), Context);

event({_Event, Params, _TriggerId, _TargetId}, Context) ->
    z_render:growl(?__("Social Buffer : Unmatched event.", Context),Context),
    io:format("Mod_Buffer Event params : ~p", [Params]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
