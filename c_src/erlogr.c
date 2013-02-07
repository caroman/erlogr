/*
 *   Copyright (C) 2012 Carlos Roman. All rights reserved.
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

// http://www.gdal.org/ogr/ogr__api_8h.html

#include <arpa/inet.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <ogr_api.h>

#include "erl_nif.h"

static ErlNifResourceType* OGR_DS_RESOURCE;
static ErlNifResourceType* OGR_F_RESOURCE;
static ErlNifResourceType* OGR_FD_RESOURCE;
static ErlNifResourceType* OGR_FLD_RESOURCE;
static ErlNifResourceType* OGR_G_RESOURCE;
static ErlNifResourceType* OGR_GREF_RESOURCE;
static ErlNifResourceType* OGR_D_RESOURCE;
static ErlNifResourceType* OGR_L_RESOURCE;
static ErlNifResourceType* OGR_LD_RESOURCE;

static void
datasource_destroy(ErlNifEnv *env, void *obj)
{
    OGRDataSourceH **datasource = (OGRDataSourceH**)obj;
    //OGR_DS_Destroy(*datasource);
    OGRReleaseDataSource(*datasource);
}

static void
feature_destroy(ErlNifEnv *env, void *obj)
{
    OGRFeatureH **feature = (OGRFeatureH**)obj;
    OGR_F_Destroy(*feature);
}

/*
static void
featuredefn_destroy(ErlNifEnv *env, void *obj)
{
    OGRFeatureDefnH **featuredefn = (OGRFeatureDefnH**)obj;
    OGR_FD_Destroy(*featuredefn);
}

static void
fielddefn_destroy(ErlNifEnv *env, void *obj)
{
    OGRFieldDefnH **fielddefn = (OGRFieldDefnH**)obj;
    OGR_Fld_Destroy(*fielddefn);
}
*/


static void
geometry_destroy(ErlNifEnv *env, void *obj)
{
    OGRGeometryH **geometry = (OGRGeometryH**)obj;
    OGR_G_DestroyGeometry(*geometry);
}


/* From https://github.com/iamaleksey/iconverl/blob/master/c_src/iconverl.c */
static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    OGRRegisterAll();

    OGR_DS_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_ds_resource", &datasource_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_F_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_f_resource", &feature_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_FD_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_fd_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_FLD_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_fld_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_G_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_g_resource", &geometry_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_GREF_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_gref_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_D_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_d_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_L_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_l_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_LD_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_ld_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);


    return 0;
}

void
unload(ErlNifEnv* env, void* priv_data)
{
    OGRCleanupAll();
}

/************************************************************************
 *
 *  OGRGeometry
 *
 ***********************************************************************/

/* OGR_G_ExportToWkb (OGRGeometryH, OGRwkbByteOrder, unsigned char *)

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_feature(Layer, 0),
Geometry = erlogr:f_get_geometry_ref(Feature),
Wkb = erlogr:g_export_to_wkb(Geometry).

*/
static ERL_NIF_TERM
g_export_to_wkb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRGeometryH *geom;
    ERL_NIF_TERM eterm;

    if(!(enif_get_resource(env, argv[0], OGR_GREF_RESOURCE, (void**)&geom))) {
        return 0; 
    }

    //if(!(enif_get_resource(env, argv[0], OGR_GREF_RESOURCE, (void**)&geom) ||
    //     enif_get_resource(env, argv[0], OGR_G_RESOURCE, (void**)&geom))) {
    //    return 0; 
    //}

    int size = OGR_G_WkbSize(*geom);
    unsigned char *wkb = malloc(sizeof(char)*(size));
    OGRErr eErr = OGR_G_ExportToWkb(*geom,
        (OGRwkbByteOrder)(( htonl( 1 ) == 1 ) ? 0 : 1),
        wkb);
    if (eErr != OGRERR_NONE) {
        return 0;
    }

    ErlNifBinary bin = {.size = size, .data = wkb};
    eterm = enif_make_binary(env, &bin);
    OGRFree(wkb);
    return eterm;
}

/* OGR_G_ExportToWkt (OGRGeometryH, char **)

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_feature(Layer, 0),
Geometry = erlogr:f_get_geometry_ref(Feature),
Wkt = erlogr:g_export_to_wkt(Geometry).

*/
static ERL_NIF_TERM
g_export_to_wkt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRGeometryH *geometry;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_GREF_RESOURCE, (void**)&geometry)) {
        return 0;
    }

    char *wkt = NULL;
    if (OGR_G_ExportToWkt(*geometry, &wkt) != OGRERR_NONE) {
        return 0;
    }

    eterm = enif_make_string(env, wkt, ERL_NIF_LATIN1);
    OGRFree(wkt);
    return eterm;
}
 
/************************************************************************
 *
 *  OGRFeature
 *
 ***********************************************************************/

/* OGRGeometryH OGR_F_GetGeometryRef(OGRFeatureH hFeat)    

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_feature(Layer, 0),
Geometry = erlogr:f_get_geometry_ref(Feature).

*/
static ERL_NIF_TERM
f_get_geometry_ref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureH *feature;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_F_RESOURCE, (void**)&feature)) {
        return 0;
    }

    OGRGeometryH geom = OGR_F_GetGeometryRef(*feature);
    if(geom == NULL) {
        return 0;
    }

    OGRGeometryH **geometry = \
        enif_alloc_resource(OGR_GREF_RESOURCE, sizeof(OGRGeometryH*));
    *geometry = geom;


    eterm = enif_make_resource(env, geometry);
    enif_release_resource(geometry);
    return eterm;
}
 
/************************************************************************
 *
 *  OGRLayer
 *
 ***********************************************************************/

/* OGRFeatureH OGR_L_GetFeature(OGRLayerH hLayer, long nFeatureId)   

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_feature(Layer, 0).

*/
static ERL_NIF_TERM
l_get_feature(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRLayerH *layer;
    int index;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return 0;
    }

    if (!enif_get_int(env, argv[1], &index)) {
        return 0;
    }

    OGRFeatureH feat = OGR_L_GetFeature(*layer, index);
    if(feat == NULL) {
        return 0;
    }

    OGRFeatureH **feature = \
        enif_alloc_resource(OGR_F_RESOURCE, sizeof(OGRFeatureH*));
    *feature = feat;

    eterm = enif_make_resource(env, feature);
    enif_release_resource(feature);
    return eterm;
}

/* OGRFeatureH OGR_L_GetNextFeature(OGRLayerH hLayer)   

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_next_feature(Layer).

*/
static ERL_NIF_TERM
l_get_next_feature(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRLayerH *layer;
    ERL_NIF_TERM eterm1, eterm2;

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return 0;
    }

    OGRFeatureH feat = OGR_L_GetNextFeature(*layer);
    if(feat == NULL) {
        eterm1 = enif_make_atom(env, "error");
        eterm2 = enif_make_string(env, "No more features", ERL_NIF_LATIN1);
        return enif_make_tuple2(env,  eterm1, eterm2); 
    }

    OGRFeatureH **feature = \
        enif_alloc_resource(OGR_F_RESOURCE, sizeof(OGRFeatureH*));
    *feature = feat;

    eterm1 = enif_make_atom(env, "ok");
    eterm2 = enif_make_resource(env, feature);
    enif_release_resource(feature);
    return enif_make_tuple2(env,  eterm1, eterm2); 
}
 
/* OGRFeatureH OGR_L_ResetReading(OGRLayerH hLayer)   

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_next_feature(Layer).
erlogr:l_get_next_feature(Layer).
erlogr:l_reset_reading(Layer).
erlogr:l_get_next_feature(Layer).

*/
static ERL_NIF_TERM
l_reset_reading(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRLayerH *layer;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return 0;
    }

    OGR_L_ResetReading(*layer);

    eterm = enif_make_atom(env, "ok");
    return eterm;
}
 
/* int OGR_L_GetFeatureCount(OGRLayerH hLayer, int bForce)

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
erlogr:l_get_feature_count(Layer).

*/
static ERL_NIF_TERM
l_get_feature_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRLayerH *layer;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return 0;
    }

    int count = OGR_L_GetFeatureCount(*layer, 1);
    eterm = enif_make_int(env, count);
    return eterm;
}
 
/* OGRFeatureDefnH OGR_L_GetLayerDefn(OGRLayerH hLayer)

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
FeatureDefn = erlogr:l_get_layer_defn(Layer).

*/
static ERL_NIF_TERM
l_get_layer_defn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRLayerH *layer;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return 0;
    }

    OGRFeatureDefnH feat_defn = OGR_L_GetLayerDefn(*layer);

    OGRFeatureDefnH **feature_defn = \
        enif_alloc_resource(OGR_FD_RESOURCE, sizeof(OGRFeatureDefnH*));
    *feature_defn = feat_defn;

    eterm = enif_make_resource(env, feature_defn);
    enif_release_resource(feature_defn);
    return eterm;
}

/************************************************************************
 *
 *  OGRFeatureDefn
 *
 ***********************************************************************/

/* OGRFieldDefnH OGR_FD_GetFieldDefn(OGRFeatureDefnH hDefn, int iField)   

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
FeatureDefn = erlogr:l_get_layer_defn(Layer),
FieldDefn = erlogr:fd_get_field_defn(FeatureDefn, 0).

*/
static ERL_NIF_TERM
fd_get_field_defn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    int index;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return 0;
    }

    if (!enif_get_int(env, argv[1], &index)) {
        return 0;
    }

    OGRFieldDefnH fd_defn = OGR_FD_GetFieldDefn(*feat_defn, index);

    if(fd_defn == NULL) {
        return 0;
    }

    OGRFeatureDefnH **field_defn = \
        enif_alloc_resource(OGR_FD_RESOURCE, sizeof(OGRFieldDefnH*));
    *field_defn = fd_defn;

    eterm = enif_make_resource(env, field_defn);
    enif_release_resource(field_defn);
    return eterm;
}
 
/* OGRwkbGeometryType OGR_FD_GetGeomType(OGRFeatureDefnH)

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
FeatureDefn = erlogr:l_get_layer_defn(Layer),
GeomType = erlogr:fd_get_geom_type(FeatureDefn).

*/
static ERL_NIF_TERM
fd_get_geom_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return 0;
    }

    OGRwkbGeometryType geom_type = OGR_FD_GetGeomType(*feat_defn);

    eterm = enif_make_int(env, geom_type);
    return eterm;
}
 

/************************************************************************
 *
 *  OGRDataSource
 *
 ***********************************************************************/

/* int    CPL_DLL OGR_DS_GetLayerCount( OGRDataSourceH );

DataSource = erlogr:open("test/polygon.shp"),
erlogr:ds_get_layer_count(DataSource).

*/
static ERL_NIF_TERM
ds_get_layer_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRDataSourceH *datasource;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_DS_RESOURCE, (void**)&datasource)) {
        return 0;
    }

    int count = OGR_DS_GetLayerCount(*datasource);
    eterm = enif_make_int(env, count);
    return eterm;
}
 
/* OGRLayerH CPL_DLL OGR_DS_GetLayer( OGRDataSourceH, int );

DataSource = erlogr:open("test/polygon.shp"),
erlogr:ds_get_layer(DataSource, 0).

*/
static ERL_NIF_TERM
ds_get_layer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRDataSourceH *datasource;
    int index;
    ERL_NIF_TERM eterm;

    if (!enif_get_int(env, argv[1], &index)) {
        return 0;
    }

    if(!enif_get_resource(env, argv[0], OGR_DS_RESOURCE, (void**)&datasource)) {
        return 0;
    }


    OGRLayerH lyr = OGR_DS_GetLayer(*datasource, index);
    if(lyr == NULL) {
        return 0;
    }

    OGRLayerH **layer = \
        enif_alloc_resource(OGR_L_RESOURCE, sizeof(OGRLayerH*));
    *layer = lyr;

    eterm = enif_make_resource(env, layer);
    enif_release_resource(layer);
    return eterm;
}
 
/************************************************************************
 *
 *  OGRSFDriverRegistrar
 *
 ***********************************************************************/

/* OGRDataSourceH CPL_DLL OGROpen(const char *, int, OGRSFDriverH *) 
    CPL_WARN_UNUSED_RESULT;

DataSource = erlogr:open("test/polygon.shp").
DataSource = erlogr:open("test/polygon.shp", 1).

*/
static ERL_NIF_TERM
open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char * filename;
    int update = 0; // read-only (default)
    OGRDataSourceH datasource;
    ERL_NIF_TERM eterm1;
    /*
    OGRSFDriverH  *pahDriver;
    ERL_NIF_TERM eterm1, eterm2;
    */

    unsigned len;
    if (!enif_get_list_length(env, argv[0], &len)) {
        return 0;
    }
    filename = malloc(sizeof(char)*(len+1));

    if(!enif_get_string(env, argv[0], filename, len+1, ERL_NIF_LATIN1)) {
        return 0;
    }

    if (argc>1 && !enif_get_int(env, argv[1], &update)) {
        return 0;
    }

    datasource = OGROpen(filename, update, NULL);
    //datasource = OGROpen(filename, upadate, pahDriver);

    if(datasource == NULL) {
        return 0;
    }

    OGRDataSourceH **hDS = \
        enif_alloc_resource(OGR_DS_RESOURCE, sizeof(OGRDataSourceH*));
    *hDS = datasource;

    /*
    OGRSFDriverH **hDriver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *hDriver = *pahDriver;
    */


    eterm1 = enif_make_resource(env, hDS);
    enif_release_resource(hDS);
    return eterm1;
    /*
    eterm2 = enif_make_resource(env, hDriver);
    enif_release_resource(hDriver);
    return enif_make_tuple2(env, eterm1, eterm1);
    */
}

/* OGRSFDriverH CPL_DLL OGRGetDriver( int );

Driver = erlogr:get_driver(0),
erlogr:dr_get_name(Driver).
"ESRI Shapefile"

*/
static ERL_NIF_TERM
get_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int driver_idx;
    ERL_NIF_TERM eterm;

    if (!enif_get_int(env, argv[0], &driver_idx)) {
        return 0;
    }

    OGRSFDriverH drv = OGRGetDriver(driver_idx); 

    if (!drv)
    {
        //printf("Driver not found!\n");
        return 0;
    } 

    OGRSFDriverH **driver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *driver = drv;

    eterm = enif_make_resource(env, driver);
    enif_release_resource(driver);
    return eterm;
}

/* OGRSFDriverH CPL_DLL OGRGetDriverByName( const char * );

Driver = erlogr:get_driver_by_name("ESRI Shapefile"),
erlogr:dr_get_name(Driver).
"ESRI Shapefile"

*/
static ERL_NIF_TERM
get_driver_by_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char *driver_name;
    ERL_NIF_TERM eterm;

    unsigned len;
    if (!enif_get_list_length(env, argv[0], &len)){
        return 0;
    }
    driver_name = malloc(sizeof(char)*(len+1));

    if(!enif_get_string(env, argv[0], driver_name, len+1, ERL_NIF_LATIN1)) {
        return 0;
    }

    OGRSFDriverH drv = OGRGetDriverByName(driver_name);

    if (!drv)
    {
        //printf("Driver not found!\n");
        return 0;
    } 

    OGRSFDriverH **driver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *driver = drv;

    eterm = enif_make_resource(env, driver);
    enif_release_resource(driver);
    free(driver_name);
    return eterm;
}

/************************************************************************
 *
 *  OGRSFDriver
 *
 ***********************************************************************/

/* const char CPL_DLL *OGR_Dr_GetName( OGRSFDriverH );

Driver = erlogr:get_driver(0),
erlogr:dr_get_name(Driver).
"ESRI Shapefile"

*/
static ERL_NIF_TERM
dr_get_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRSFDriverH **driver;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_D_RESOURCE, (void**)&driver)) {
        return 0;
    }
    const char *driver_name = OGR_Dr_GetName(*driver);
    eterm = enif_make_string(env, driver_name, ERL_NIF_LATIN1);
    return eterm;
}

/************************************************************************
 *
 *  Erlang Specific Functions
 *
 ***********************************************************************/

/*

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
Feature = erlogr:l_get_feature(Layer, 0),
erlogr:f_get_fields(Feature).

*/
static ERL_NIF_TERM
f_get_fields(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureH *feature;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_F_RESOURCE, (void**)&feature)) {
        return 0;
    }

    OGRFeatureDefnH feature_defn = OGR_F_GetDefnRef(*feature);
    int count = OGR_FD_GetFieldCount(feature_defn);
    ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM)*count);
    int index;
    for(index=0; index<count; index++)
    {
        OGRFieldDefnH field_defn = OGR_FD_GetFieldDefn(feature_defn, index);
        if(OGR_Fld_GetType(field_defn) == OFTInteger) {
            arr[index] = enif_make_int(env,
                OGR_F_GetFieldAsInteger(*feature, index));
        } else if(OGR_Fld_GetType(field_defn) == OFTReal) {
            arr[index] = enif_make_double(env,
                OGR_F_GetFieldAsDouble(*feature, index));
        } else if(OGR_Fld_GetType(field_defn) == OFTString) {
            arr[index] = enif_make_string(env,
                OGR_F_GetFieldAsString(*feature, index),
                ERL_NIF_LATIN1);
        } else {
            arr[index] = enif_make_string(env,
                OGR_F_GetFieldAsString(*feature, index),
                ERL_NIF_LATIN1);
        }
    }

    eterm = enif_make_tuple_from_array(env, arr, index);
    free(arr);
    return eterm;
}

/*

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
FeatureDefn = erlogr:l_get_layer_defn(Layer),
erlogr:fd_get_fields_name(FeatureDefn).

*/
static ERL_NIF_TERM
fd_get_fields_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return 0;
    }

    int count = OGR_FD_GetFieldCount(*feat_defn);
    ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM)*count);
    int index;
    for(index=0; index<count; index++)
    {
        OGRFieldDefnH field_defn = OGR_FD_GetFieldDefn(*feat_defn, index);
        arr[index] = enif_make_string(env,
            OGR_Fld_GetNameRef(field_defn),
            ERL_NIF_LATIN1);
    }

    eterm = enif_make_tuple_from_array(env, arr, index);
    free(arr);
    return eterm;
}

/*

DataSource = erlogr:open("test/polygon.shp"),
Layer = erlogr:ds_get_layer(DataSource, 0),
FeatureDefn = erlogr:l_get_layer_defn(Layer),
erlogr:fd_get_fields_type(FeatureDefn).

*/
static ERL_NIF_TERM
fd_get_fields_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return 0;
    }

    int count = OGR_FD_GetFieldCount(*feat_defn);
    ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM)*count);
    int index;
    for(index=0; index<count; index++)
    {
        OGRFieldDefnH field_defn = OGR_FD_GetFieldDefn(*feat_defn, index);
        arr[index] = enif_make_string(env,
            OGR_GetFieldTypeName(OGR_Fld_GetType(field_defn)),
            ERL_NIF_LATIN1);
    }

    eterm = enif_make_tuple_from_array(env, arr, index);
    free(arr);
    return eterm;
}



static ErlNifFunc nif_funcs[] =
{
    {"ds_get_layer", 2, ds_get_layer},
    {"ds_get_layer_count", 1, ds_get_layer_count},
    {"dr_get_name", 1, dr_get_name},
    {"f_get_fields", 1, f_get_fields},
    {"f_get_geometry_ref", 1, f_get_geometry_ref},
    {"fd_get_field_defn", 2, fd_get_field_defn},
    {"fd_get_fields_name", 1, fd_get_fields_name},
    {"fd_get_fields_type", 1, fd_get_fields_type},
    {"fd_get_geom_type", 1, fd_get_geom_type},
    {"g_export_to_wkb", 1, g_export_to_wkb},
    {"g_export_to_wkt", 1, g_export_to_wkt},
    {"l_get_feature", 2, l_get_feature},
    {"l_get_feature_count", 1, l_get_feature_count},
    {"l_get_next_feature", 1, l_get_next_feature},
    {"l_get_layer_defn", 1, l_get_layer_defn},
    {"l_reset_reading", 1, l_reset_reading},
    {"get_driver_by_name", 1, get_driver_by_name},
    {"get_driver", 1, get_driver},
    {"open", 1, open},
    {"open", 2, open}
};

ERL_NIF_INIT(erlogr, nif_funcs, &load, NULL, NULL, unload);
