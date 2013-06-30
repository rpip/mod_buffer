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
    insert/2,
    update/6,
    delete/2
]).

-include_lib("zotonic.hrl").
-include("../include/mod_buffer.hrl").

%% return a property of the buffer
m_find_value(value, #m{}=M, Context) ->
    m_value(M, Context);

m_find_value(Key, #m{value=V} = _M, _Context) ->
    case lists:member(Key,[id, user_id, message,schedule,destination,status, created, modifed]) of
        true ->  proplists:get_value(Key,V);
        false -> undefined
    end.     

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> []
m_to_list(#m{value=_V}, Context) ->
   list(Context).

%% @doc return a buffer
m_value(#m{value=V}, _Context) ->
    V.


%% @doc Fetch all buffers from the database
list(Context)->
    Buffers = z_db:q("select * from buffer", Context),
    BufferList = [[{id,Id},
                 {user_id,UserId},
                 {message,binary_to_list(Message)},
                 {destination,binary_to_list(Destination)},
                 {schedule,binary_to_list(Schedule)},
                 {status,binary_to_list(Status)},
                 {created,Created}, 
                 {modified,Modified}]
     || {Id,UserId,Message,Destination,Schedule,Status,Created,Modified} = _Buffer <- Buffers],
    BufferList.
    
%% @doc Fetch a specific buffer from the database.
%% @spec get(int(), Context) -> PropList
get(BufferId, Context) ->
   z_db:assoc_props_row("select * from buffer where id = $1", [BufferId], Context).
   

%% @doc Insert a new Buffer. Fetches the user information from the Context.
%% @spec insert(Message::string(), Schedule::string(), Destination::string(), Status::int(), Context) -> {ok, BufferId} | {error, Reason}
%% @todo Convert schedule date to postgresql compatible timestamp
insert([Message, Schedule, Destination, Status] = _PostData, Context) ->
    case z_auth:is_auth(Context) of
        true ->
           LocalTime = erlang:localtime(),
	    Props = [
                {user_id, z_acl:user(Context)},
                {message, z_html:escape(z_string:trim(Message))},
                {schedule, Schedule},
                {destination, z_string:trim(Destination)},
                {status, Status},
                {created, LocalTime},
                {modified, LocalTime}
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
%% @spec update(BufferId::integer(), Message::string(),Schedule::string(), Destination::string(), Status::int(), Context) -> {ok, BufferId} | {error, Reason}
%% @todo Convert schedule date to postgresql compatible timestamp
update(BufferId, Message, Schedule, Destination, Status, Context) ->
    case z_auth:is_auth(Context) of
        true ->
            %Schedule = z_convert:to_integer(Schedule),
	    Props = [
                {user_id, z_acl:user(Context)},
                {message, z_html:escape(z_string:trim(Message))},
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
 
   
