#ifndef __dep_graph_h_
#define __dep_graph_h_

extern "C" {
    #include "erl_nif.h"
    
    int load(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int reload(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM load_info);
    void unload(ErlNifEnv*, void*);
    
    static ERL_NIF_TERM add_vertex_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM add_edge_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM new_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM delete_mbgraph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM write_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM read_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM filter_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM graph_to_dot_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM graph_to_terms_name_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM graph_to_terms_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

#endif
