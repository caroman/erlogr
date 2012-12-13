% Copyright (C) 2012 Carlos Roman. All rights reserved.
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(erlosr).

-export([
    import_from_epsg/1,
    export_to_wkt/1
    ]).

-ifdef(makecheck).
-compile(export_all).
-endif.

-on_load(init/0).


init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "erlosr"]);
        false ->
            filename:join(["priv", "erlosr"])
        end;
    Dir ->
        filename:join(Dir, "erlosr")
    end,
    (catch erlang:load_nif(SoName, 0)).

import_from_epsg(_EPSG) ->
    "NIF library not loaded".

export_to_wkt(_EPSG) ->
    "NIF library not loaded".


