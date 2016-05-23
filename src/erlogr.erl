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
    f_get_fields/1,
    f_get_geometry_ref/1,
    f_get_geometry/1,
    fd_get_feature_name/1,
    fd_get_field_count/1,
    fd_get_field_defn/2,
    fd_get_fields_name/1,
    fd_get_fields_type/1,
    fd_get_geom_type/1,
    fld_get_name_ref/1,
    fld_get_type/1,
    g_export_to_wkb/1,
    g_export_to_wkt/1,
    l_get_feature/2,
    l_get_feature_count/1,
    l_get_next_feature/1,
    l_get_layer_defn/1,
    l_reset_reading/1,
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
        case os:getenv("ESCRIPT") of
        "1" ->
            filename:join([filename:dirname(escript:script_name()),
                "..", "lib", "erlogr"]);
        _ ->
            filename:join(Dir, "erlogr")
        end
    end,
    (catch erlang:load_nif(SoName, 0)).

ds_get_layer(_Datasource, _Index) ->
    erlang:nif_error(nif_not_loaded).

ds_get_layer_count(_Datasource) ->
    erlang:nif_error(nif_not_loaded).

dr_get_name(_Driver) ->
    erlang:nif_error(nif_not_loaded).

g_export_to_wkb(_Geometry) ->
    erlang:nif_error(nif_not_loaded).

g_export_to_wkt(_Geometry) ->
    erlang:nif_error(nif_not_loaded).

f_get_fields(_Feature) ->
    erlang:nif_error(nif_not_loaded).

f_get_geometry(_Feature) ->
    erlang:nif_error(nif_not_loaded).

f_get_geometry_ref(_Feature) ->
    erlang:nif_error(nif_not_loaded).

fd_get_feature_name(_FeatureDefn) ->
    erlang:nif_error(nif_not_loaded).

fd_get_field_count(_FeatureDefn) ->
    erlang:nif_error(nif_not_loaded).

fd_get_field_defn(_FeatureDefn, _Index) ->
    erlang:nif_error(nif_not_loaded).

fd_get_fields_name(_FeatureDefn) ->
    erlang:nif_error(nif_not_loaded).

fd_get_fields_type(_FeatureDefn) ->
    erlang:nif_error(nif_not_loaded).

fd_get_geom_type(_FeatureDefn) ->
    erlang:nif_error(nif_not_loaded).

fld_get_name_ref(_FieldDefn) ->
    erlang:nif_error(nif_not_loaded).

fld_get_type(_FieldDefn) ->
    erlang:nif_error(nif_not_loaded).

l_get_feature(_Layer, _Index) ->
    erlang:nif_error(nif_not_loaded).

l_get_feature_count(_Layer) ->
    erlang:nif_error(nif_not_loaded).

l_get_next_feature(_Layer) ->
    erlang:nif_error(nif_not_loaded).

l_get_layer_defn(_Layer) ->
    erlang:nif_error(nif_not_loaded).

l_reset_reading(_Layer) ->
    erlang:nif_error(nif_not_loaded).

get_driver(_DriverIdx) ->
    erlang:nif_error(nif_not_loaded).

get_driver_by_name(_DriverName) ->
    erlang:nif_error(nif_not_loaded).

open(_FileName) ->
    erlang:nif_error(nif_not_loaded).

open(_FileName, _Update) ->
    erlang:nif_error(nif_not_loaded).

