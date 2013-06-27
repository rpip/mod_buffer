%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% Date: 25-06-2013
%% @doc Simple buffer module. Implements datamodel logic for mod_buffer.

%% Copyright 2013 Mawuli Adzaku
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(m_buffer).
-author("Mawuli Adzaku <mawuli@mawuli.me>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    list/1,
    get/2,
    insert/5,
    update/6,
    delete/2
]).

-include_lib("zotonic.hrl").


m_find_value(_Key, #m{value=undefined}, _Context) ->
   undefined.

%% @doc return a buffer
m_value(BufferId, Context) ->
    get(BufferId, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> []
m_to_list(_, Context) ->
    list(Context).


%% @doc Fetch all buffers from the database
list(Context)->
    z_db:q("select * from buffer", Context).
    
%% @doc Fetch a specific buffer from the database.
%% @spec get(int(), Context) -> PropList
get(BufferId, Context) ->
    z_db:assoc_props_row("select * from buffer where id = $1", [BufferId], Context).


%% @doc Insert a new Buffer. Fetches the user information from the Context.
%% @spec insert(Content::string(), Schedule::string(), Destination::string(), Status::int(), Context) -> {ok, BufferId} | {error, Reason}
%% @todo Convert schedule date to postgresql compatible timestamp
insert(Content, Schedule, Destination, Status, Context) ->
    case z_auth:is_auth(Context) of
        true ->
            %Schedule = z_convert:to_integer(Schedule),
	    Props = [
                {user_id, z_acl:user(Context)},
                {content, z_html:escape(z_string:trim(Content))},
                {schedule, Schedule},
                {destination, z_string:trim(Destination)},
                {status, z_convert:to_integer(Status)},
                {created, z_utils:now_msec()},
                {modified, z_utils:now_msec()}
            ],
            case z_db:insert(buffer, Props, Context) of
                {ok, BufferId} = Result ->
                    z_depcache:flush({buffer, BufferId}, Context),
                    z_notifier:notify({buffer,BufferId}, Context),
                    Result;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.


%% @doc Update a Buffer. Fetches the user information from the Context.
%% @spec update(BufferId::integer(),Content::string(),Schedule::string(), Destination::string(), Status::int(), Context) -> {ok, BufferId} | {error, Reason}
%% @todo Convert schedule date to postgresql compatible timestamp
update(BufferId, Content, Schedule, Destination, Status, Context) ->
    case z_auth:is_auth(Context) of
        true ->
            %Schedule = z_convert:to_integer(Schedule),
	    Props = [
                {id, BufferId},
                {content, z_html:escape(z_string:trim(Content))},
                {schedule, Schedule},
                {destination, z_string:trim(Destination)},
                {status, z_convert:to_integer(Status)},
                {created, z_utils:now_msec()},
                {modified, z_utils:now_msec()}
            ],
            case z_db:update(buffer, BufferId, Props, Context) of
                {ok, BufferId} = Result ->
                    z_depcache:flush({buffer, BufferId}, Context),
                    z_notifier:notify({buffer,BufferId}, Context),
                    Result;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Delete a buffer.
delete(BufferId, Context) ->
    case check_editable(BufferId, Context) of
        {ok, UserId} ->
            z_db:q("delete from buffer where id = $1 and user_id = $2", [BufferId,UserId], Context),
            z_depcache:flush({buffer, BufferId}, Context),
            ok;
        {error, _} = Error ->
            Error
    end.

    

%% @doc Check if an user can edit or owns the buffer
check_editable(BufferId, Context) ->
    case z_db:q_row("select user_id from buffer where id = $1", [BufferId], Context) of
        {UserId} ->
            case (UserId /= undefined andalso z_acl:user(Context) == UserId)
            of
                true -> {ok, UserId};
                false -> {error, eacces}
            end;
        _ ->
            {error, enoent}
    end.
 
   
