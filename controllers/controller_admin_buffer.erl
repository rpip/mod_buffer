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
-export([init/1, html/1, is_authorized/2]).

-include_lib("controller_html_helper.hrl").


%%%===================================================================
%%% API
%%%===================================================================
init(DispatchArgs) ->
     {ok, DispatchArgs}.

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_buffer, ReqData, Context).

html(Context) ->
    Vars = [],
    Html = z_template:render("buffer.tpl", Vars, Context),
    z_context:output(Html, Context).
 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
