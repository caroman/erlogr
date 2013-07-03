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
// http://www.gdal.org/ogr/ogr_apitut.html

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
static ErlNifResourceType* OGR_D_RESOURCE;
static ErlNifResourceType* OGR_L_RESOURCE;

typedef struct {
    ErlNifEnv *env;
    OGRLayerH *obj;
} EnvLayer_t;

typedef struct {
    ErlNifEnv *env;
    OGRFeatureH *obj;
} EnvFeature_t;

typedef struct {
    ErlNifEnv *env;
    OGRFeatureDefnH *obj;
} EnvFeatureDefn_t;

typedef struct {
    ErlNifEnv *env;
    OGRFieldDefnH *obj;
} EnvFieldDefn_t;

typedef struct {
    ErlNifEnv *env;
    OGRGeometryH *obj;
} EnvGeometry_t;

static void
datasource_destroy(ErlNifEnv *env, void *obj)
{
    // Release will destroy if reference count is 0
    OGRDataSourceH **datasource = (OGRDataSourceH**)obj;
    //OGR_DS_Destroy(*datasource);
    OGRReleaseDataSource(*datasource);
}

static void
layer_destroy(ErlNifEnv *env, void *obj)
{
    // Layer is owned by OGRDataSource, should not be deleted
    EnvLayer_t **layer = (EnvLayer_t**)obj;
    enif_free_env((**layer).env);
    enif_free(*layer);
}

static void
feature_destroy(ErlNifEnv *env, void *obj)
{
    // Feature is owned by the caller
    EnvFeature_t **feature = (EnvFeature_t**)obj;
    OGR_F_Destroy((**feature).obj);
    enif_free_env((**feature).env);
    enif_free(*feature);
}

static void
feature_defn_destroy(ErlNifEnv *env, void *obj)
{
    // OGRFeatureDefn is owned by the OGRLayer,
    // and should not be modified or freed by the application
    EnvFeatureDefn_t **feature_defn = (EnvFeatureDefn_t**)obj;
    //OGR_FD_Destroy((**feature_defn).obj);
    enif_free_env((**feature_defn).env);
    enif_free(*feature_defn);
}

static void
field_defn_destroy(ErlNifEnv *env, void *obj)
{
    // OGRFieldDefn should not be freed by the application
    EnvFieldDefn_t **field_defn = (EnvFieldDefn_t**)obj;
    //OGR_Fld_Destroy(*field_defn);
    enif_free_env((**field_defn).env);
    enif_free(*field_defn);
}

static void
geometry_destroy(ErlNifEnv *env, void *obj)
{
    // If env is NULL then is GeometryRef else is Geometry
    // GeometryRef should not be freed by the application
    EnvGeometry_t **geometry = (EnvGeometry_t**)obj;
    if ((**geometry).env == NULL) {
        OGR_G_DestroyGeometry((**geometry).obj);
    } else {
        enif_free_env((**geometry).env);
    }
    enif_free(*geometry);
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
        env, NULL, "ogr_fd_resource", &feature_defn_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_FLD_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_fld_resource", &field_defn_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_G_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_g_resource", &geometry_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_D_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_d_resource", NULL,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_L_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_l_resource", &layer_destroy,
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

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0),
{ok, Geometry} = erlogr:f_get_geometry_ref(Feature),
{ok, Wkb} = erlogr:g_export_to_wkb(Geometry).

*/
static ERL_NIF_TERM
g_export_to_wkb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvGeometry_t **geom;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], OGR_G_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env); 
    }

    int size = OGR_G_WkbSize((**geom).obj);
    unsigned char *wkb = malloc(sizeof(char)*(size));

    OGRErr eErr = OGR_G_ExportToWkb((**geom).obj,
        (OGRwkbByteOrder)(( htonl( 1 ) == 1 ) ? 0 : 1),
        wkb);

    if (eErr != OGRERR_NONE) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_int(env, eErr));
    }

    ErlNifBinary bin = {.size = size, .data = wkb};
    eterm = enif_make_binary(env, &bin);
    OGRFree(wkb);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}

/* OGR_G_ExportToWkt(OGRGeometryH, char **)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0),
{ok, Geometry} = erlogr:f_get_geometry_ref(Feature),
{ok, Wkt} = erlogr:g_export_to_wkt(Geometry).

*/
static ERL_NIF_TERM
g_export_to_wkt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvGeometry_t **geom;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], OGR_G_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env); 
    }

    char *wkt = NULL;
    OGRErr eErr = OGR_G_ExportToWkt((**geom).obj, &wkt);
    if (eErr != OGRERR_NONE) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_int(env, eErr));
    }

    eterm = enif_make_string(env, wkt, ERL_NIF_LATIN1);
    OGRFree(wkt);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}
 
/************************************************************************
 *
 *  OGRFeature
 *
 ***********************************************************************/

/* OGRGeometryH OGR_F_GetGeometryRef(OGRFeatureH hFeat)    

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0),
{ok, Geometry} = erlogr:f_get_geometry_ref(Feature).

*/
static ERL_NIF_TERM
f_get_geometry_ref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeature_t **feature;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_F_RESOURCE, (void**)&feature)) {
        return enif_make_badarg(env);
    }

    OGRGeometryH geom = OGR_F_GetGeometryRef((**feature).obj);
    if(geom == NULL) {
        return enif_make_atom(env, "undefined");
    }

    EnvGeometry_t **geometry = \
        enif_alloc_resource(OGR_G_RESOURCE, sizeof(EnvGeometry_t*));

    ErlNifEnv *geometry_env = enif_alloc_env();

    *geometry = (EnvGeometry_t*) enif_alloc(sizeof(EnvGeometry_t));
    (**geometry).env = geometry_env;
    (**geometry).obj = geom;

    // Save copy of feature so is not garbage collected
    enif_make_copy(geometry_env, argv[0]);

    eterm = enif_make_resource(env, geometry);
    enif_release_resource(geometry);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}

/* 
{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0),
{ok, Geometry} = erlogr:f_get_geometry(Feature).

*/
static ERL_NIF_TERM
f_get_geometry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeature_t **feature;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_F_RESOURCE, (void**)&feature)) {
        return enif_make_badarg(env);
    }

    OGRGeometryH geom = OGR_F_GetGeometryRef((**feature).obj);
    if(geom == NULL) {
        return enif_make_atom(env, "undefined");
    }

    OGRGeometryH geom_clone = OGR_G_Clone(geom);

    EnvGeometry_t **geometry = \
        enif_alloc_resource(OGR_G_RESOURCE, sizeof(EnvGeometry_t*));

    *geometry = (EnvGeometry_t*) enif_alloc(sizeof(EnvGeometry_t));
    (**geometry).env = NULL;
    (**geometry).obj = geom_clone;

    eterm = enif_make_resource(env, geometry);
    enif_release_resource(geometry);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}


/************************************************************************
 *
 *  OGRFeatureDefn
 *
 ***********************************************************************/

/* int  OGR_FD_GetFieldCount(OGRFeatureDefnH)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer),
{ok, FieldCount} = erlogr:fd_get_field_count(FeatureDefn).

*/
static ERL_NIF_TERM
fd_get_field_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeatureDefn_t **feat_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return enif_make_badarg(env);
    }

    int field_count = OGR_FD_GetFieldCount(*feat_defn);

    eterm = enif_make_int(env, field_count);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}
 
/* OGRFieldDefnH OGR_FD_GetFieldDefn(OGRFeatureDefnH hDefn, int iField)   

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer),
{ok, FieldDefn} = erlogr:fd_get_field_defn(FeatureDefn, 0).

*/
static ERL_NIF_TERM
fd_get_field_defn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeatureDefn_t **feat_defn;
    int index;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    OGRFieldDefnH fd_defn = OGR_FD_GetFieldDefn(*feat_defn, index);

    if(fd_defn == NULL) {
        return enif_make_atom(env, "undefined");
    }

    EnvFieldDefn_t **field_defn = \
        enif_alloc_resource(OGR_FLD_RESOURCE, sizeof(EnvFieldDefn_t*));

    ErlNifEnv *field_defn_env = enif_alloc_env();

    *field_defn = (EnvFieldDefn_t*) enif_alloc(sizeof(EnvFieldDefn_t));
    (**field_defn).env = field_defn_env;
    (**field_defn).obj = fd_defn;

    // Save copy of layer so is not garbage collected
    enif_make_copy(field_defn_env, argv[0]);

    eterm = enif_make_resource(env, field_defn);
    enif_release_resource(field_defn);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}
 
/* OGRwkbGeometryType OGR_FD_GetGeomType(OGRFeatureDefnH)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer),
{ok, GeomType} = erlogr:fd_get_geom_type(FeatureDefn).

*/
static ERL_NIF_TERM
fd_get_geom_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeatureDefn_t **feat_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return enif_make_badarg(env);
    }

    OGRwkbGeometryType geom_type = OGR_FD_GetGeomType((**feat_defn).obj);

    eterm = enif_make_int(env, geom_type);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}

/************************************************************************
 *
 *  OGRField
 *
 ***********************************************************************/
/* OGRFieldType OGR_Fld_GetNameRef(OGRFieldDefnH)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer).
{ok, FieldDefn} = erlogr:fd_get_field_defn(FeatureDefn, 0),
{ok, FieldName} = erlogr:fld_get_name_ref(FieldDefn).

*/
static ERL_NIF_TERM
fld_get_name_ref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFieldDefn_t **field_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FLD_RESOURCE, (void**)&field_defn)){
        return enif_make_badarg(env);
    }

    const char * field_name = OGR_Fld_GetNameRef((**field_defn).obj);

    eterm = enif_make_string(env, field_name, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}


/* OGRFieldType OGR_Fld_GetType(OGRFieldDefnH)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer).
{ok, FieldDefn} = erlogr:fd_get_field_defn(FeatureDefn, 0),
{ok, FieldType} = erlogr:fld_get_type(FieldDefn).

*/
static ERL_NIF_TERM
fld_get_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFieldDefn_t **field_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FLD_RESOURCE, (void**)&field_defn)){
        return enif_make_badarg(env);
    }

    OGRFieldType field_type = OGR_Fld_GetType((**field_defn).obj);

    eterm = enif_make_int(env, field_type);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}

 
/************************************************************************
 *
 *  OGRLayer
 *
 ***********************************************************************/

/* OGRFeatureH OGR_L_GetFeature(OGRLayerH hLayer, long nFeatureId)   

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0).

*/
static ERL_NIF_TERM
l_get_feature(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvLayer_t **layer;
    int index;
    ERL_NIF_TERM eterm;

    if(argc != 2) { 
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    OGRFeatureH feat = OGR_L_GetFeature((**layer).obj, index);
    if(feat == NULL) {
        return enif_make_atom(env, "undefined");
    }

    EnvFeature_t **feature = \
        enif_alloc_resource(OGR_F_RESOURCE, sizeof(EnvFeature_t*));

    ErlNifEnv *feature_env = enif_alloc_env();

    *feature = (EnvFeature_t*) enif_alloc(sizeof(EnvFeature_t));
    (**feature).env = feature_env;
    (**feature).obj = feat;

    // Save copy of layer so is not garbage collected
    enif_make_copy(feature_env, argv[0]);

    eterm = enif_make_resource(env, feature);
    enif_release_resource(feature);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
}

/* OGRFeatureH OGR_L_GetNextFeature(OGRLayerH hLayer)   

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_next_feature(Layer).

*/
static ERL_NIF_TERM
l_get_next_feature(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvLayer_t **layer;
    ERL_NIF_TERM eterm;

    if(argc != 1) { 
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return enif_make_badarg(env);
    }

    OGRFeatureH feat = OGR_L_GetNextFeature((**layer).obj);
    if(feat == NULL) {
        eterm = enif_make_string(env, "No more features", ERL_NIF_LATIN1);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), eterm); 
    }

    EnvFeature_t **feature = \
        enif_alloc_resource(OGR_F_RESOURCE, sizeof(EnvFeature_t*));

    ErlNifEnv *feature_env = enif_alloc_env();

    *feature = (EnvFeature_t*) enif_alloc(sizeof(EnvFeature_t));
    (**feature).env = feature_env;
    (**feature).obj = feat;

    // Save copy of layer so is not garbage collected
    enif_make_copy(feature_env, argv[0]);

    eterm = enif_make_resource(env, feature);
    enif_release_resource(feature);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}
 
/* OGRFeatureH OGR_L_ResetReading(OGRLayerH hLayer)   

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
erlogr:l_get_next_feature(Layer).
erlogr:l_get_next_feature(Layer).
erlogr:l_reset_reading(Layer).
erlogr:l_get_next_feature(Layer).

*/
static ERL_NIF_TERM
l_reset_reading(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvLayer_t **layer;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return enif_make_badarg(env);
    }

    OGR_L_ResetReading((**layer).obj);

    eterm = enif_make_atom(env, "ok");
    return eterm;
}
 
/* int OGR_L_GetFeatureCount(OGRLayerH hLayer, int bForce)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, COunt} = erlogr:l_get_feature_count(Layer).

*/
static ERL_NIF_TERM
l_get_feature_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvLayer_t **layer;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return enif_make_badarg(env);
    }

    int count = OGR_L_GetFeatureCount((**layer).obj, 1);
    eterm = enif_make_int(env, count);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}
 
/* OGRFeatureDefnH OGR_L_GetLayerDefn(OGRLayerH hLayer)

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer).

*/
static ERL_NIF_TERM
l_get_layer_defn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvLayer_t **layer;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_L_RESOURCE, (void**)&layer)) {
        return enif_make_badarg(env);
    }

    OGRFeatureDefnH feat_defn = OGR_L_GetLayerDefn((**layer).obj);

    EnvFeatureDefn_t **feature_defn = \
        enif_alloc_resource(OGR_FD_RESOURCE, sizeof(EnvFeatureDefn_t*));

    ErlNifEnv *feature_defn_env = enif_alloc_env();

    *feature_defn = (EnvFeatureDefn_t*) enif_alloc(sizeof(EnvFeatureDefn_t));
    (**feature_defn).env = feature_defn_env;
    (**feature_defn).obj = feat_defn;

    // Save copy of layer so is not garbage collected
    enif_make_copy(feature_defn_env, argv[0]);

    eterm = enif_make_resource(env, feature_defn);
    enif_release_resource(feature_defn);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}

/************************************************************************
 *
 *  OGRDataSource
 *
 ***********************************************************************/

/* int    CPL_DLL OGR_DS_GetLayerCount( OGRDataSourceH );

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Count} = erlogr:ds_get_layer_count(DataSource).

*/
static ERL_NIF_TERM
ds_get_layer_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRDataSourceH *datasource;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_DS_RESOURCE, (void**)&datasource)) {
        return enif_make_badarg(env);
    }

    int count = OGR_DS_GetLayerCount(*datasource);
    eterm = enif_make_int(env, count);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}
 
/* OGRLayerH CPL_DLL OGR_DS_GetLayer( OGRDataSourceH, int );

{ok, DataSource} = erlogr:open("test/polygon.shp"),
erlogr:ds_get_layer(DataSource, 0).

*/
static ERL_NIF_TERM
ds_get_layer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRDataSourceH *datasource;
    int index;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_DS_RESOURCE, (void**)&datasource)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    OGRLayerH lyr = OGR_DS_GetLayer(*datasource, index);
    if(lyr == NULL) {
        return enif_make_atom(env, "undefined");
    }

    EnvLayer_t **layer = \
        enif_alloc_resource(OGR_L_RESOURCE, sizeof(EnvLayer_t*));

    ErlNifEnv *layer_env = enif_alloc_env();

    *layer = (EnvLayer_t*) enif_alloc(sizeof(EnvLayer_t));
    (**layer).env = layer_env;
    (**layer).obj = lyr;

    // Save copy of datasource so is not garbage collected
    enif_make_copy(layer_env, argv[0]);

    eterm = enif_make_resource(env, layer);
    enif_release_resource(layer);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
 
}

/************************************************************************
 *
 *  OGRSFDriverRegistrar
 *
 ***********************************************************************/

/* OGRDataSourceH CPL_DLL OGROpen(const char *, int, OGRSFDriverH *) 
    CPL_WARN_UNUSED_RESULT;

{ok, DataSource} = erlogr:open("test/polygon.shp").
{ok, DataSource} = erlogr:open("test/polygon.shp", 1).

*/
static ERL_NIF_TERM
open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int update = 0; // read-only (default)
    OGRDataSourceH datasource;
    ERL_NIF_TERM eterm;
    /*
    OGRSFDriverH  *pahDriver;
    ERL_NIF_TERM eterm1, eterm2;
    */
 
    unsigned len;
    if (argc > 0 && !enif_get_list_length(env, argv[0], &len)) {
        return enif_make_badarg(env);
    }
    char * filename = enif_alloc(sizeof(char)*(len+1));

    if(!enif_get_string(env, argv[0], filename, len+1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if (argc == 2 && !enif_get_int(env, argv[1], &update)) {
        return enif_make_badarg(env);
    }

    datasource = OGROpen(filename, update, NULL);
    //datasource = OGROpen(filename, upadate, pahDriver);

    enif_free(filename);
    if(datasource == NULL) {
        return enif_make_atom(env, "undefined");
    }

    OGRDataSourceH **hDS = \
        enif_alloc_resource(OGR_DS_RESOURCE, sizeof(OGRDataSourceH*));
    *hDS = datasource;

    /*
    OGRSFDriverH **hDriver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *hDriver = *pahDriver;
    */

    eterm = enif_make_resource(env, hDS);
    enif_release_resource(hDS);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
    /*
    eterm2 = enif_make_resource(env, hDriver);
    enif_release_resource(hDriver);
    return enif_make_tuple2(env, eterm1, eterm1);
    */
}

/* OGRSFDriverH CPL_DLL OGRGetDriver( int );

{ok, Driver} = erlogr:get_driver(0),

*/
static ERL_NIF_TERM
get_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int driver_idx;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[0], &driver_idx)) {
        return enif_make_badarg(env);
    }

    OGRSFDriverH drv = OGRGetDriver(driver_idx); 

    if (!drv) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, "Driver not found", ERL_NIF_LATIN1));
    } 

    OGRSFDriverH **driver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *driver = drv;

    eterm = enif_make_resource(env, driver);
    enif_release_resource(driver);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm);
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

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    unsigned len;
    if (!enif_get_list_length(env, argv[0], &len)){
        return enif_make_badarg(env);
    }
    driver_name = malloc(sizeof(char)*(len+1));

    if(!enif_get_string(env, argv[0], driver_name, len+1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    OGRSFDriverH drv = OGRGetDriverByName(driver_name);

    if (!drv) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, "Driver not found", ERL_NIF_LATIN1));
    } 

    OGRSFDriverH **driver = \
        enif_alloc_resource(OGR_D_RESOURCE, sizeof(OGRSFDriverH*));
    *driver = drv;

    eterm = enif_make_resource(env, driver);
    enif_release_resource(driver);
    free(driver_name);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}

/************************************************************************
 *
 *  OGRSFDriver
 *
 ***********************************************************************/

/* const char CPL_DLL *OGR_Dr_GetName( OGRSFDriverH );

{ok, Driver} = erlogr:get_driver(0),
{ok, Name} = erlogr:dr_get_name(Driver).
{ok, "ESRI Shapefile"}

*/
static ERL_NIF_TERM
dr_get_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRSFDriverH **driver;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_D_RESOURCE, (void**)&driver)) {
        return enif_make_badarg(env);
    }
    const char *driver_name = OGR_Dr_GetName(*driver);
    eterm = enif_make_string(env, driver_name, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}

/************************************************************************
 *
 *  Erlang Specific Functions
 *
 ***********************************************************************/

/*

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, Feature} = erlogr:l_get_feature(Layer, 0),
{ok, Fields} = erlogr:f_get_fields(Feature).
{1,"first"}
*/
static ERL_NIF_TERM
f_get_fields(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EnvFeature_t **feature;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_F_RESOURCE, (void**)&feature)) {
        return enif_make_badarg(env);
    }

    OGRFeatureDefnH feature_defn = OGR_F_GetDefnRef((**feature).obj);
    int count = OGR_FD_GetFieldCount(feature_defn);
    ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM)*count);
    int index;
    for(index=0; index<count; index++)
    {
        OGRFieldDefnH field_defn = OGR_FD_GetFieldDefn(feature_defn, index);
        if(OGR_Fld_GetType(field_defn) == OFTInteger) {
            arr[index] = enif_make_int(env,
                OGR_F_GetFieldAsInteger((**feature).obj, index));
        } else if(OGR_Fld_GetType(field_defn) == OFTReal) {
            arr[index] = enif_make_double(env,
                OGR_F_GetFieldAsDouble((**feature).obj, index));
        } else if(OGR_Fld_GetType(field_defn) == OFTString) {
            arr[index] = enif_make_string(env,
                OGR_F_GetFieldAsString((**feature).obj, index),
                ERL_NIF_LATIN1);
        } else {
            arr[index] = enif_make_string(env,
                OGR_F_GetFieldAsString((**feature).obj, index),
                ERL_NIF_LATIN1);
        }
    }

    eterm = enif_make_tuple_from_array(env, arr, index);
    free(arr);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}

/*

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer),
{ok, Names} = erlogr:fd_get_fields_name(FeatureDefn).
{"id","name"}
*/
static ERL_NIF_TERM
fd_get_fields_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return enif_make_badarg(env);
    }

    int count = OGR_FD_GetFieldCount(*feat_defn);
    ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM)*count);
    int index;
    for(index=0; index<count; index++) {
        OGRFieldDefnH field_defn = OGR_FD_GetFieldDefn(*feat_defn, index);
        arr[index] = enif_make_string(env,
            OGR_Fld_GetNameRef(field_defn),
            ERL_NIF_LATIN1);
    }

    eterm = enif_make_tuple_from_array(env, arr, index);
    free(arr);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}

/*

{ok, DataSource} = erlogr:open("test/polygon.shp"),
{ok, Layer} = erlogr:ds_get_layer(DataSource, 0),
{ok, FeatureDefn} = erlogr:l_get_layer_defn(Layer),
{ok, Types} = erlogr:fd_get_fields_type(FeatureDefn).
{"Integer","String"}
*/
static ERL_NIF_TERM
fd_get_fields_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OGRFeatureDefnH *feat_defn;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], OGR_FD_RESOURCE, (void**)&feat_defn)) {
        return enif_make_badarg(env);
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
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), eterm); 
}



static ErlNifFunc nif_funcs[] =
{
    {"ds_get_layer", 2, ds_get_layer},
    {"ds_get_layer_count", 1, ds_get_layer_count},
    {"dr_get_name", 1, dr_get_name},
    {"f_get_fields", 1, f_get_fields},
    {"f_get_geometry_ref", 1, f_get_geometry_ref},
    {"f_get_geometry", 1, f_get_geometry},
    {"fd_get_field_count", 1, fd_get_field_count},
    {"fd_get_field_defn", 2, fd_get_field_defn},
    {"fd_get_fields_name", 1, fd_get_fields_name},
    {"fd_get_fields_type", 1, fd_get_fields_type},
    {"fd_get_geom_type", 1, fd_get_geom_type},
    {"fld_get_name_ref", 1, fld_get_name_ref},
    {"fld_get_type", 1, fld_get_type},
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
