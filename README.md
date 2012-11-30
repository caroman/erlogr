Install
-------

Build it with:

    make

Run tests with:

    make check
    
    or

    make check-verbose

On Windows
----------

You need to have GDAL installed, let's say it was installed to `C:\cygwin\opt\gdal`.

Open a shell which has all compilers and the MSVC environment set up (e.g. the Windows SDK 7.1 Command Prompt).

Now set it up so that GDAL and Erlang can be found:

    SET INCLUDE=%INCLUDE%;C:\cygwin\opt\gdal\include;C:\cygwin\opt\gdal\include\gdal
    SET LIB=%LIB%;C:\cygwin\opt\gdal\lib
    SET PATH=%PATH%;C:\cygwin\opt\gdal\bin;C:\erl5.9.1\bin

And finally compile the whole thing:

    rebar compile


Examples
--------

Here's an example session in the erlang shell. See the src/erlogr.erl file for
more examples.

    $ erl -pa ebin
    Erlang R14B04 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

    Eshell V5.8.4  (abort with ^G)

    1> Driver = erlogr:get_driver(0),
    1> erlogr:dr_get_name(Driver).
    "ESRI Shapefile"

    2> Driver = erlogr:get_driver_by_name("ESRI Shapefile"),
    2> erlogr:dr_get_name(Driver).
    "ESRI Shapefile"

    3> DataSource = erlogr:open("test/polygon.shp"),
    3> Layer = erlogr:ds_get_layer(DataSource, 0),
    3> Feature = erlogr:l_get_feature(Layer, 0),
    3> Geometry = erlogr:f_get_geometry_ref(Feature),
    3> Wkt = erlogr:g_export_to_wkt(Geometry).
    "POLYGON ((0.351988636363636 -0.969460227272728,2.058238636363636 0.086505681818182,2.690625 -1.524289772727273,0.0 -2.0015625,-0.304261363636364 -1.828551136363636,0.351988636363636 -0.969460227272728))" 

    4> Wkb = erlogr:g_export_to_wkb(Geometry).
    <<1,3,0,0,0,1,0,0,0,6,0,0,0,136,181,111,88,251,134,214,63,
      72,23,93,116,209,5,239,191,...>>

