%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-01-15
%% @doc Simple comment module. Adds comments to any rsc.

%% Copyright 2010 Mawuli Adzaku
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

-mod_title("Buffers").
-mod_description("Buffer model for mod_buffer. Implements the logic for dealing with buffer datamodels.").
-mod_depends([admin, twitter]).
%-mod_provides([buffer]).

%% gen_server exports
-export([init/1]).

%% interface functions
-export([
    event/2,
    observe_search_query/2,
    observe_admin_menu/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
