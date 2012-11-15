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

#include <ogr_api.h>

#include "erl_nif.h"

static ErlNifResourceType* OGR_DS_RESOURCE;
static ErlNifResourceType* OGR_F_RESOURCE;
static ErlNifResourceType* OGR_FD_RESOURCE;
static ErlNifResourceType* OGR_FLD_RESOURCE;
static ErlNifResourceType* OGR_G_RESOURCE;
static ErlNifResourceType* OGR_D_RESOURCE;

static void
datasource_destroy(ErlNifEnv *env, void *obj)
{
    OGRDataSourceH **datasource = (OGRDataSourceH**)obj;
    OGR_DS_Destroy(*datasource);
}

static void
feature_destroy(ErlNifEnv *env, void *obj)
{
    OGRFeatureH **feature = (OGRFeatureH**)obj;
    OGR_F_Destroy(*feature);
}

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
        env, NULL, "ogr_fd_resource", &featuredefn_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_FLD_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_fld_resource", &fielddefn_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_G_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_g_resource", &geometry_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    OGR_D_RESOURCE = enif_open_resource_type(
        env, NULL, "ogr_d_resource", NULL,
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
 *  OGRSFDriverRegistrar
 *
 ***********************************************************************/

/*
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

/*
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

/*
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


static ErlNifFunc nif_funcs[] =
{
    {"get_driver_by_name", 1, get_driver_by_name},
    {"get_driver", 1, get_driver},
    {"dr_get_name", 1, dr_get_name} 
};

ERL_NIF_INIT(erlogr, nif_funcs, &load, NULL, NULL, unload);
