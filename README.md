Erlang bindings for the GDAL/OGR  and GDAL/OSR APIs.Install
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
    1> erlogr:dr_name(Driver).
    "ESRI Shapefile"

    2> Driver = erlogr:get_driver_by_name("ESRI Shapefile"),
    2> erlogr:dr_name(Driver).
    "ESRI Shapefile"


