/// This file is part of RefactorErl.
///
/// RefactorErl is free software: you can redistribute it and/or modify
/// it under the terms of the GNU Lesser General Public License as published
/// by the Free Software Foundation, either version 3 of the License, or
/// (at your option) any later version.
///
/// RefactorErl is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU Lesser General Public License for more details.
///
/// You should have received a copy of the GNU Lesser General Public License
/// along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
///
/// The Original Code is RefactorErl.
///
/// The Initial Developer of the Original Code is Eötvös Loránd University.
/// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
/// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
/// and Ericsson Hungary. All Rights Reserved.
///
/// @author Andras Nemeth <andras.b.nemeth@ericsson.com>

// erlang-nif
#include <erl_nif.h>
// kyoto cabinet
#include <kclangc.h>

#include <stdlib.h>

#define STRING_MAXLEN 1024*10

// binary data or strings
#define BINARY_DATA 1

static KCDB* db;
static char* data_dir = 0;

static ERL_NIF_TERM kc_error(ErlNifEnv* env);

#ifdef BINARY_DATA

static ERL_NIF_TERM kyoto_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  ErlNifBinary key;
  ErlNifBinary value;

  if (!enif_inspect_binary(env, argv[0], &key) ||
      !enif_inspect_binary(env, argv[1], &value)) {
    return enif_make_badarg(env);
  }

  if (!kcdbset(db, (char*)key.data, key.size, (char*)value.data, value.size)) {
    ret = kc_error(env);
  } else {
    ret = enif_make_atom(env, "ok"); 
  }
  
  return ret;
}

#else

static ERL_NIF_TERM kyoto_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* key = (char*) malloc(STRING_MAXLEN);
  int key_len;
  char* val = (char *) malloc(STRING_MAXLEN);
  int val_len;
  ERL_NIF_TERM ret;
  
  key_len = enif_get_string(env, argv[0], key, STRING_MAXLEN, ERL_NIF_LATIN1);
  val_len = enif_get_string(env, argv[1], val, STRING_MAXLEN, ERL_NIF_LATIN1);
  
  if (!kcdbset(db, key, key_len, val, val_len)) {
    ret = kc_error(env);
  } else {
    ret = enif_make_atom(env, "ok"); 
  }
  
  free(key);
  free(val);

  return ret;
}
#endif

#ifdef BINARY_DATA

static ERL_NIF_TERM kyoto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  ErlNifBinary key;
  ErlNifBinary value;

  enif_inspect_binary(env, argv[0], &key);
  enif_inspect_binary(env, argv[1], &value);

  if (!kcdbreplace(db, (char*)key.data, key.size, (char*)value.data, value.size)) {
    ret = kc_error(env);
  } else {
    ret = enif_make_atom(env, "ok");
  }
  return ret;
}

#else

ERL_NIF_TERM kyoto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  // TODO: check if node is protected
  char* key = (char *) malloc(STRING_MAXLEN);
  int key_len;
  char* val = (char *) malloc(STRING_MAXLEN);
  int val_len;
  ERL_NIF_TERM ret;
  //char* checkb;
  //int checkb_len;
  
  key_len = enif_get_string(env, argv[0], key, STRING_MAXLEN, ERL_NIF_LATIN1);
  val_len = enif_get_string(env, argv[1], val, STRING_MAXLEN, ERL_NIF_LATIN1);

  /* checkb = kcdbget(db, key, key_len, &checkb_len); */
  /* if (checkb) { */
  /*   kcdbset(key, key_len, val, val_len); */
  /* } */
  /* kcfree(checkb); */
  if (!kcdbreplace(db, key, key_len, val, val_len)) {
    ret = kc_error(env);
  } else {
    ret = enif_make_atom(env, "ok");
  }
  
  free(key);
  free(val);

  return ret;
}
#endif

#ifdef BINARY_DATA
static ERL_NIF_TERM kyoto_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  ErlNifBinary key;
  
  enif_inspect_binary(env, argv[0], &key);
  
  if (!kcdbremove(db, (char*)key.data, key.size)) {
    ret = enif_make_atom(env, "no_exists");
  } else {
    ret = enif_make_atom(env, "ok");
  }
  return ret;
}

#else

ERL_NIF_TERM kyoto_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* key = (char *) malloc(STRING_MAXLEN);
  int key_len;
  ERL_NIF_TERM ret;
  
  key_len = enif_get_string(env, argv[0], key, STRING_MAXLEN, ERL_NIF_LATIN1);
  
  if (!kcdbremove(db, key, key_len)) {
    //ret = kc_error(env);
    ret = enif_make_atom(env, "no_exists");
  } else {
    ret = enif_make_atom(env, "ok");
  }

  free(key);

  return ret;
}
#endif
/*
ERL_NIF_TERM kyoto_mklink(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  char* fromtag;
  int fromtag_len;
  char* to;
  int to_len;
  
  fromtag_len = enif_get_string(env, argv[0], fromtag, STRING_MAXLEN, ERL_NIF_LATIN1);
  to_len = enif_get_string(env, argv[1], to, STRING_MAXLEN, ERL_NIF_LATIN1);

  kcdbset(db, fromtag, fromtag_len, to, to_len);
  
  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM kyoto_mklink_prot(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  // TODO
}

ERL_NIF_TERM kyoto_rmlink(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  char* fromtag;
  int fromtag_len;

  fromtag_len = enif_get_string(env, argv[0], fromtag, STRING_MAXLEN, ERL_NIF_LATIN1);
  
  kcdbremove(db, fromtag, fromtag_len);
  
  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM kyoto_root(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  return enif_make_atom(env, "root");
}
*/
#ifdef BINARY_DATA

static ERL_NIF_TERM kyoto_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  ErlNifBinary key;
  ErlNifBinary data;
  char* value = 0;//(char*) malloc(STRING_MAXLEN);
  static size_t size;
  //char * val = (char*) malloc(STRING_MAXLEN);
  //int val_len;
  
  if (!enif_inspect_binary(env, argv[0], &key)) {
    //return enif_make_atom(env, "bad_binary");
    return enif_make_badarg(env);
  }
  
  //enif_alloc_binary(STRING_MAXLEN, &data);
  //val_len = kcdbgetbuf(db, key.data, key.size, val, STRING_MAXLEN);
  //size = kcdbgetbuf(db, (char*)key.data, key.size, value, STRING_MAXLEN);
  //data.data = (unsigned char*) kcdbget(db, (char*)key.data, key.size, &data.size);
  value = kcdbget(db, (char*)key.data, key.size, &size);

  if (value == NULL) {
    //free(key.data);
    //enif_release_binary(&data);
    ret = enif_make_atom(env, "bad_node");
  } else if (size == 0) {
    //enif_release_binary(&data);
    free(value);
    ret = enif_make_atom(env, "bad_data_tag");
  } else {
    enif_alloc_binary(size, &data);
    //data.size = size;
    memcpy(data.data, (void*)value, size);
    free(value);
    ret = enif_make_binary(env, &data);
  }

  //free(value);

  return ret;
}

#else

ERL_NIF_TERM kyoto_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* key = (char *) malloc(STRING_MAXLEN);
  int key_len;
  char* val = (char *) malloc(STRING_MAXLEN);
  int val_len;
  ERL_NIF_TERM result;

  key_len = enif_get_string(env, argv[0], key, STRING_MAXLEN, ERL_NIF_LATIN1);

  //return enif_make_atom(env, "bad_node"); 
  
  val_len = kcdbgetbuf(db, key, key_len, val, STRING_MAXLEN);
  if (val_len == -1) {
    //result = kc_error(env);
    result = enif_make_atom(env, "bad_node"); 
  } else {
    result = enif_make_string_len(env, val, val_len, ERL_NIF_LATIN1);
    //kcfree(val);
  }

  free(key);
  free(val);
  
  return result;
}
#endif

#ifdef BINARY_DATA
static ERL_NIF_TERM kyoto_exists(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key;
  //ErlNifBinary data;
  //char* value = 0;//(char*)malloc(STRING_MAXLEN);
  //size_t size = 0;
  ERL_NIF_TERM ret;
  
  enif_inspect_binary(env, argv[0], &key);
/*  
  value = kcdbget(db, (char*)key.data, key.size, &size);
  
  if (value == NULL) {
    ret = enif_make_atom(env, "no_exists");
  } else {
    //enif_alloc_binary(size, &data);
    //data.size = size;
    //memcpy(data.data, (void*) value, size);
    //ret = enif_make_binary(env, &data);
    ret = enif_make_atom(env, "exists");
  }
*/

  if (kcdbcheck(db, (char*)key.data, key.size) == -1)
  {
     ret = enif_make_atom(env, "no_exists");
  }
  else
  {
     ret = enif_make_atom(env, "exists");
  }

  //free(value);
  
  return ret;
}

#else

ERL_NIF_TERM kyoto_exists(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* key = (char *) malloc(STRING_MAXLEN);
  int key_len;
  char* val;
  size_t val_len;
  ERL_NIF_TERM ret;

  key_len = enif_get_string(env, argv[0], key, STRING_MAXLEN, ERL_NIF_LATIN1);

  val = kcdbget(db, key, key_len, &val_len);

  if (val == NULL) {
    ret = enif_make_atom(env, "no_exists");
  } else {
    kcfree(val);
    ret = enif_make_atom(env, "exists");
  }

  free(key);

  return ret;
}
#endif

/*
ERL_NIF_TERM kyoto_index(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  
}

ERL_NIF_TERM kyoto_fwd_path(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  
}

ERL_NIF_TERM kyoto_back_path(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  
}
*/

static ERL_NIF_TERM kyoto_create_db(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  // data_dir is global, and filename contains the whole path to
  // kyoto cabinet file (or if memory only it is omitted)
  // plus all the db tuning parameters.
  char* filename = (char*)malloc(STRING_MAXLEN);
  ERL_NIF_TERM ret;
  
  if (!enif_get_string(env, argv[0], data_dir, STRING_MAXLEN, ERL_NIF_LATIN1) ||
      strlen(data_dir) == 0 ||
      !enif_get_string(env, argv[1], filename, STRING_MAXLEN, ERL_NIF_LATIN1)) {
    free(filename);
    return enif_make_badarg(env);
  }

  if (data_dir[strlen(data_dir) -1] != '/')
  {
    data_dir = strcat(data_dir, "/");
  }
  
  //data_dir = dirname(filename);
  
  if (db != 0) {
     kcdbclose(db);
     kcdbdel(db);
  }
  db = kcdbnew();
  
  /*
  The tuning parameter "log" is for the original "tune_logger" and the value specifies the path of the log file, or "-" for the standard output, or "+" for the standard error. 
  "logkinds" specifies kinds of logged messages and the value can be "debug", "info", "warn", or "error". 
  "logpx" specifies the prefix of each log message. 
  "opts" is for "tune_options" and the value can contain "s" for the small option, "l" for the linear option, and "c" for the compress option. 
  "bnum" corresponds to "tune_bucket". 
  "zcomp" is for "tune_compressor" and the value can be "zlib" for the ZLIB raw compressor, "def" for the ZLIB deflate compressor, "gz" for the ZLIB gzip compressor, "lzo" for the LZO compressor, "lzma" for the LZMA compressor, or "arc" for the Arcfour cipher. 
  "zkey" specifies the cipher key of the compressor. 
  
  "capcount" is for "cap_count". 
  "capsize" is for "cap_size". 
  
  "psiz" is for "tune_page". 
  "rcomp" is for "tune_comparator" and the value can be "lex" for the lexical comparator or "dec" for the decimal comparator. 
  "pccap" is for "tune_page_cache". 
  "apow" is for "tune_alignment". 
  "fpow" is for "tune_fbp". 
  "msiz" is for "tune_map". 
  "dfunit" is for "tune_defrag". 
  
  Every opened database must be closed by the kcdbclose method when it is no longer in use. It is not allowed for two or more database objects in the same process to keep their connections to the same database file at the same time.
  */
  
  
//  if (!kcdbopen(db, strcat(filename, ".kct#apow=3#msiz=4096#dfunit=64#psiz=8192#pccap=128000000#bnum=1000000"), 
  if (!kcdbopen(db, filename , KCOWRITER | KCOCREATE | KCOTRYLOCK)) {
    ret = kc_error(env);
    kcdbclose(db);
    kcdbdel(db);
    db = 0; // reset to null
  } else {
    ret = enif_make_atom(env, "ok");
  }
  
  free(filename);
  
  return ret;
}

void close_db()
{
   kcdbclose(db);
   kcdbdel(db);
   db = 0;
}

static ERL_NIF_TERM kyoto_close_db(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   close_db();
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM kyoto_erase_nodes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  kcdbclear(db);
  
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM kyoto_sync_db(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   //if (kcdbsync(db, false, 0, 0))
   if (kcdbsync(db, 0, 0, 0))
   {
     return enif_make_atom(env, "ok");
   }
   return enif_make_atom(env, "nok");
}

static ERL_NIF_TERM kyoto_make_backup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* filename = (char*)malloc(STRING_MAXLEN);
  char* filepath = (char*)malloc(STRING_MAXLEN);
  ERL_NIF_TERM ret;
  
  if (!enif_get_string(env, argv[0], filename, STRING_MAXLEN, ERL_NIF_LATIN1))
  {
    //free(filename); // it dies with "double free or corruption" error
    free(filepath);
    enif_make_badarg(env);
  }

  strcpy(filepath, data_dir);
 
  if (kcdbdumpsnap(db, strcat(filepath, filename)))
  {
    ret = enif_make_atom(env, "ok");
  }
  else
  {
    //ret = enif_make_atom(env, "nok");
    ret = kc_error(env);
    //ret = enif_make_string(env, data_dir, ERL_NIF_LATIN1);
    //ret = enif_make_string(env, filename, ERL_NIF_LATIN1);
  }

  free(filename);
  free(filepath);
  return ret;
}

static ERL_NIF_TERM kyoto_load_backup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* filename = (char*)malloc(STRING_MAXLEN);
  char* filepath = (char*)malloc(STRING_MAXLEN);
  ERL_NIF_TERM ret;

  if (!enif_get_string(env, argv[0], filename, STRING_MAXLEN, ERL_NIF_LATIN1))
  {
    //free(filename); // it dies with "double free or corruption" error
    free(filepath);
    enif_make_badarg(env);
  }
  
  strcpy(filepath, data_dir);

  if (kcdbloadsnap(db, strcat(filepath, filename)))
  {
    ret = enif_make_atom(env, "ok");
  }
  else
  {
    //ret = enif_make_atom(env, "nok");
    ret = kc_error(env);
  }

  free(filename);
  free(filepath);

  return ret;
}

static ERL_NIF_TERM kyoto_remove_backup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char* filename = (char*)malloc(STRING_MAXLEN);
  char* filepath = (char*)malloc(STRING_MAXLEN);
  ERL_NIF_TERM ret;

  if (!enif_get_string(env, argv[0], filename, STRING_MAXLEN, ERL_NIF_LATIN1))
  {
    //free(filename); // it dies with "double free or corruption" error
    free(filepath);
    enif_make_badarg(env);
  }

  strcpy(filepath, data_dir);
  if (remove(strcat(filepath, filename)) != 0)
  {
    ret = enif_make_atom(env, "nok");
  }
  else
  {
    ret = enif_make_atom(env, "ok");
  }

  free(filename);
  free(filepath);

  return ret;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	data_dir = (char*)malloc(1024);
  /*db = kcdbnew();
  
  if (!kcdbopen(db, "kyoto_minimal.kch", KCOWRITER | KCOCREATE))
  {
    //enif_make_badarg(env);
    return 1;
    }*/
  return 0;
}

static int reload(ErlNifEnv*env, void** priv_data, ERL_NIF_TERM load_info)
{
   return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
   close_db();
}

static ERL_NIF_TERM kc_error(ErlNifEnv* env)
{
  return enif_make_string(env, kcecodename(kcdbecode(db)), ERL_NIF_LATIN1);
}

static ErlNifFunc kyoto_funcs[] = {
  {"kyoto_create",        2, kyoto_create},
  {"kyoto_update",        2, kyoto_update},
  {"kyoto_delete",        1, kyoto_delete},
  /* 
  {"kyoto_mklink"  ,      3, kyoto_mklink},
  {"kyoto_mklink_prot",   3, kyoto_mklink},
  {"kyoto_rmlink",        3, kyoto_rmlink},
  */
  {"kyoto_data",          1, kyoto_data},
  /*
  {"kyoto_index",         3, kyoto_index},
  {"kyoto_fwd_path",      2, kyoto_fwd_path},
  {"kyoto_back_path",     2, kyoto_back_path},*/
  {"kyoto_exists",        1, kyoto_exists},
  {"kyoto_erase_nodes",   0, kyoto_erase_nodes},
  {"kyoto_create_db",     2, kyoto_create_db},
  {"kyoto_close_db",      0, kyoto_close_db},
  {"kyoto_sync_db",       0, kyoto_sync_db},
  {"kyoto_make_backup",   1, kyoto_make_backup},
  {"kyoto_load_backup",   1, kyoto_load_backup},
  {"kyoto_remove_backup", 1, kyoto_remove_backup}
};

// we don't care about -->               load reload upgrade unload
ERL_NIF_INIT(refdb_kyotomini,kyoto_funcs,load,reload,      0,unload);
