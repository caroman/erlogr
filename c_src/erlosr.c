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
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <ogr_srs_api.h>

#include "erl_nif.h"

static ErlNifResourceType* OSR_CT_RESOURCE;
static ErlNifResourceType* OSR_SR_RESOURCE;

static void
coordinate_transformation_destroy(ErlNifEnv *env, void *obj)
{
    OGRCoordinateTransformationH **ct = (OGRCoordinateTransformationH**)obj;
    OCTDestroyCoordinateTransformation(*ct);
}

static void
spatial_reference_destroy(ErlNifEnv *env, void *obj)
{
    OGRSpatialReferenceH **sr = (OGRSpatialReferenceH**)obj;
    OSRDestroySpatialReference(*sr);
}


/* From https://github.com/iamaleksey/iconverl/blob/master/c_src/iconverl.c */
static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    OSR_CT_RESOURCE = enif_open_resource_type(
        env, NULL, "osr_ct_resource", &coordinate_transformation_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OSR_SR_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_sr_resource", &spatial_reference_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return 0;
}

void
unload(ErlNifEnv* env, void* priv_data)
{
    OSRCleanup();
}



/************************************************************************
 *
 *  Functions
 *
 ***********************************************************************/

/*
SRS = erlosr:import_from_epsg(4326).
"ESRI Shapefile"
*/
static ERL_NIF_TERM
import_from_epsg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int epsg;
    ERL_NIF_TERM eterm;

    if (!enif_get_int(env, argv[0], &epsg)) {
        return 0;
    }

    OGRSpatialReferenceH hSR1;
    if (hSR1 == NULL){printf("NULL\n");} else {printf("NOTNULL\n");}

    OGRSpatialReferenceH hSR = OSRNewSpatialReference(NULL);
    printf("hola\n");
    if (hSR == NULL){printf("NULL\n");} else {printf("NOTNULL\n");}
    if (!OSRImportFromEPSG(hSR, epsg)) {
        return 0;
    }

    printf("hola\n");
    OGRSpatialReferenceH **spatial_reference = \
        enif_alloc_resource(OSR_SR_RESOURCE, sizeof(OGRSpatialReferenceH*));
    printf("hola\n");
    *spatial_reference = hSR;

    printf("hola\n");
    eterm = enif_make_resource(env, spatial_reference);
    enif_release_resource(spatial_reference);
    return eterm;
}


static ErlNifFunc nif_funcs[] =
{
    {"import_from_epsg", 1, import_from_epsg},
};

ERL_NIF_INIT(erlosr, nif_funcs, &load, NULL, NULL, unload);
