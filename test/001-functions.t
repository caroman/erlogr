#!/usr/bin/env escript
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

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(4),
    test_get_driver(),
    test_get_driver_by_name(),
    test_g_export_to_wkt(),
    test_gref_export_to_wkt(),

    etap:end_tests().

% Feature Functions

test_g_export_to_wkt() ->
    {ok, DataSource} = erlogr:open("test/polygon.shp"),
    {ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
    {ok, Feature} = erlogr:l_get_feature(Layer, 0),
    {ok, Geometry} = erlogr:f_get_geometry(Feature),
    {ok, Wkt} = erlogr:g_export_to_wkt(Geometry),
    etap:is(Wkt, "POLYGON ((0.351988636363636 -0.969460227272728,2.058238636363636 0.086505681818182,2.690625 -1.524289772727273,0.0 -2.0015625,-0.304261363636364 -1.828551136363636,0.351988636363636 -0.969460227272728))",
        "Function g_export_to_wkt works").

test_gref_export_to_wkt() ->
    {ok, DataSource} = erlogr:open("test/polygon.shp"),
    {ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
    {ok, Feature} = erlogr:l_get_feature(Layer, 0),
    {ok, GeometryRef} = erlogr:f_get_geometry_ref(Feature),
    {ok, Wkt} = erlogr:g_export_to_wkt(GeometryRef),
    etap:is(Wkt, "POLYGON ((0.351988636363636 -0.969460227272728,2.058238636363636 0.086505681818182,2.690625 -1.524289772727273,0.0 -2.0015625,-0.304261363636364 -1.828551136363636,0.351988636363636 -0.969460227272728))",
        "Function g_export_to_wkt works").


% Driver Functions

test_get_driver() ->
    {ok, Driver} = erlogr:get_driver(0),
    {ok, DriverName} = erlogr:dr_get_name(Driver),
    etap:is(DriverName, "ESRI Shapefile",
        "Function get_driver works").

test_get_driver_by_name() ->
    {ok, Driver} = erlogr:get_driver_by_name("ESRI Shapefile"),
    {ok, DriverName} = erlogr:dr_get_name(Driver),
    etap:is(DriverName, "ESRI Shapefile",
        "Function get_driver_by_name works").











