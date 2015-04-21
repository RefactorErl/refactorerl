#ifndef __igraph_nif_h_
#define __igraph_nif_h_

extern "C" {
    #include "erl_nif.h"
    
    int load(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int reload(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM load_info);
    void unload(ErlNifEnv*, void*);

    ERL_NIF_TERM search_maximal_cliques_to_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM search_maximal_cliques(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

#endif
