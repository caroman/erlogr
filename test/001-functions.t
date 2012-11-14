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

    etap:plan(2),
    test_get_driver(),
    test_get_driver_by_name(),

    etap:end_tests().

% Driver Functions

test_get_driver() ->
    Driver = erlogr:get_driver(0),
    DriverName = erlogr:dr_name(Driver),
    etap:is(DriverName, "ESRI Shapefile",
        "Function get_driver works").

test_get_driver_by_name() ->
    Driver = erlogr:get_driver_by_name("ESRI Shapefile"),
    DriverName = erlogr:dr_name(Driver),
    etap:is(DriverName, "ESRI Shapefile",
        "Function get_driver_by_name works").











