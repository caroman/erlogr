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

-module(erlogr).

-export([
    ds_get_layer/2,
    ds_get_layer_count/1,
    dr_get_name/1,
    f_get_geometry_ref/1,
    g_export_to_wkb/1,
    g_export_to_wkt/1,
    l_get_feature/2,
    l_get_feature_count/1,
    get_driver/1,
    get_driver_by_name/1,
    open/1,
    open/2
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
            filename:join(["..", "priv", "erlogr"]);
        false ->
            filename:join(["priv", "erlogr"])
        end;
    Dir ->
        filename:join(Dir, "erlogr")
    end,
    (catch erlang:load_nif(SoName, 0)).

ds_get_layer(_Datasource, _Index) ->
    "NIF library not loaded".

ds_get_layer_count(_Datasource) ->
    "NIF library not loaded".

dr_get_name(_Driver) ->
    "NIF library not loaded".

g_export_to_wkb(_Geometry) ->
    "NIF library not loaded".

g_export_to_wkt(_Geometry) ->
    "NIF library not loaded".

f_get_geometry_ref(_Feature) ->
    "NIF library not loaded".

l_get_feature(_Layer, _Index) ->
    "NIF library not loaded".

l_get_feature_count(_Layer) ->
    "NIF library not loaded".

get_driver(_DriverIdx) ->
    "NIF library not loaded".

get_driver_by_name(_DriverName) ->
    "NIF library not loaded".

open(_FileName) ->
    "NIF library not loaded".

open(_FileName, _Update) ->
    "NIF library not loaded".

