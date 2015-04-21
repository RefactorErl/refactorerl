/*
IGraph library.
Copyright (C) 2010-2012  Gabor Csardi <csardi.gabor@gmail.com&gt;
334 Harvard st, Cambridge MA, 02139 USA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA



*/
#include "igraph_nif.h"
#include <stdio.h>
#include <vector>
#include <igraph.h>
#include <iostream>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <algorithm>
#define INT(a) (igraph_rng_get_integer(igraph_rng_default(), 0, (a)))


igraph_vector_ptr_t search_cliques(std::vector<igraph_real_t> vector) {
    igraph_t g;
    igraph_vector_ptr_t cliques;
    
    igraph_rng_seed(igraph_rng_default(), 42);
    
    igraph_vector_t v;
    igraph_vector_t result;
    
    igraph_vector_view(&v, vector.data(), vector.size());
    igraph_create(&g, &v, 0, IGRAPH_UNDIRECTED);
    
    igraph_vector_init(&result, 0);
    
    igraph_simplify(&g, /*multiple=*/ 1, /*loop=*/ 0, /*edge_comb=*/ 0);
    
    /* Find the maximal cliques */
    
    igraph_vector_ptr_init(&cliques, 0);
    igraph_maximal_cliques(&g, &cliques, /*min_size=*/ 2,
    /*max_size=*/ 0 /*no limit*/);
    igraph_destroy(&g);
    return cliques;
}

// Prints a clique to the file to be read by erlang parser.
void print_vector(igraph_vector_t *v, FILE* f) {
    int i;
    fprintf(f,"[");
    for (i=0; i<igraph_vector_size(v)-1; i++) {
        fprintf(f,"%d,", (int) VECTOR(*v)[i]);
    }
    fprintf(f,"%d", (int) VECTOR(*v)[igraph_vector_size(v)-1]);
    fprintf(f,"]");
}

// Prints all cliques to the file.
void print_and_destroy_cliques(igraph_vector_ptr_t *cliques, FILE* f) {
    int i;
    for (i=0; i<igraph_vector_ptr_size(cliques)-1; i++) {
        igraph_vector_t *v=static_cast<igraph_vector_t*>(VECTOR(*cliques)[i]);
        print_vector(v,f);
        fprintf(f,".\n");
        igraph_vector_destroy(v);
        igraph_free(v);
    }
    igraph_vector_t *v=static_cast<igraph_vector_t*>(VECTOR(*cliques)[igraph_vector_ptr_size(cliques)-1]);
    print_vector(v,f);
    igraph_vector_destroy(v);
    igraph_free(v);
    fprintf(f,".\n");
}

// Reads the edges from the file then searches all maximal cliques,
// and writes them back to the file.
int ligraph_maximal_cliques_to_file(char* pathToFile) {
    FILE *myFile;
    myFile = fopen(pathToFile, "r");
    std::vector<igraph_real_t> edges;
    int j = 0;
    int d;

    while(fscanf(myFile, "%d", &d) != EOF)
    {
        j++;
        edges.push_back(d);
    }

    fclose(myFile);
    igraph_vector_ptr_t cliques;
    if(edges.size() != 0) {
        cliques = search_cliques(edges);
        
        /* Print and destroy them */
        myFile = fopen(pathToFile, "w");
        print_and_destroy_cliques(&cliques,myFile);
        fclose(myFile);
        /* Clean up */
        igraph_vector_ptr_destroy(&cliques);
    }else {
        myFile = fopen(pathToFile, "w");
        //fprintf(myFile,"");
        fclose(myFile);
    }
    return 0;
}

extern "C" {

    int load(ErlNifEnv* env, void**, ERL_NIF_TERM load_info) {
        return 0;
    };
    int reload(ErlNifEnv* env, void**, ERL_NIF_TERM load_info) {
        return 0;
    };
    int upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM load_info) {
        return 0;
    };
    void unload(ErlNifEnv*, void*) {
        return;
    };

    std::string erl_str2str(ErlNifEnv* env, const ERL_NIF_TERM& erl_str) {
        unsigned str_size;
        char* str_buff;
        std::string str;

        enif_get_list_length(env, erl_str, &str_size);
        ++str_size;
        
        str_buff = new char[str_size];
        enif_get_string(env, erl_str, str_buff, str_size, ERL_NIF_LATIN1);
        str = std::string(str_buff);

        delete[] str_buff;
        return str;
    };

    ERL_NIF_TERM search_maximal_cliques_to_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        std::string url = erl_str2str(env, argv[0]);
        char* filepath = new char[url.size()+1];
        copy(url.begin(), url.end(), filepath);
        filepath[url.size()] = '\0';
        int res = ligraph_maximal_cliques_to_file(filepath);
        delete[] filepath;
        return enif_make_int(env, res);
    };

    // Given the edges as parameter searches maximal cliques then returns them to erlang.
    ERL_NIF_TERM search_maximal_cliques(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
        ERL_NIF_TERM list = argv[0];
        ERL_NIF_TERM head, tail; 
        unsigned int length;
        int element,i,j;
        enif_get_list_length(env, list, &length);
        std::vector<igraph_real_t> cvector;
        std::vector<ERL_NIF_TERM> rvector;
        cvector.reserve(length);

        //create a vector from the erlang list
        while(enif_get_list_cell(env, list, &head, &tail)) {
            enif_get_int(env, head, &element);
            cvector.push_back(element);
            list = tail;
        }

        if(cvector.size() != 0) {
            //search for cliques
            igraph_vector_ptr_t cliques;
            cliques = search_cliques(cvector);
  
            //convert back the cliques to erlang lists
            for (i=0; i<igraph_vector_ptr_size(&cliques); i++) {
                igraph_vector_t *v=reinterpret_cast<igraph_vector_t*>(VECTOR(cliques)[i]);
                std::vector<ERL_NIF_TERM> nemtom;
                for (j=0; j<igraph_vector_size(v); j++) {
                     nemtom.push_back(enif_make_int(env,(int) VECTOR(*v)[j]));
                }
                rvector.push_back(enif_make_list_from_array(env,nemtom.data(),nemtom.size()));
                igraph_vector_destroy(v);
                igraph_free(v);
            }
            igraph_vector_ptr_destroy(&cliques);
        }
        return enif_make_list_from_array(env, rvector.data(), rvector.size());
    };


 static ErlNifFunc nif_funcs[] = {
            {"search_maximal_cliques_to_file", 1, search_maximal_cliques_to_file},
            {"search_maximal_cliques", 1, search_maximal_cliques}
    };
    
    ERL_NIF_INIT(refusr_igraph,nif_funcs,load,reload,upgrade,unload)
}