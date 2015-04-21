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
/// @author Peter Felker <felker.peter88@gmail.com>

#ifndef __types_h_
#define __types_h_

extern "C" {
    #include <erl_nif.h>
}

#include <iostream>
#include <vector>
#include <map>
#include <list>
#include <set>
#include <queue>

#ifdef __OLD_SOLARIS__
#include <inttypes.h>
#else
#include <stdint.h>
#endif

#include "macros.h"

#include <dirent.h>
typedef struct dirent*                            dir_elem_t;

class backup;
class gnode;

#ifndef SIZEOF_VOID_P
#if __LP64__ || defined(__x86_64__) || defined(__amd64__)
#define SIZEOF_VOID_P 8
#else
#define SIZEOF_VOID_P 4
#endif // __LP64__ || defined(__x86_64__) || defined(__amd64__)
#endif // SIZEOF_VOID_P

#if SIZEOF_LONG == SIZEOF_VOID_P
typedef unsigned long                             erl_ptr_t;
#define enif_get_ptr enif_get_ulong
#define enif_make_ptr enif_make_ulong
#elif SIZEOF_VOID_P == 8
typedef ErlNifUInt64                              erl_ptr_t;
#define enif_get_ptr enif_get_uint64
#define enif_make_ptr enif_make_uint64
#else
#error Not supported size of pointer
#endif
typedef int8_t                                    op_code_t;
typedef uint8_t                                   atom_t;
typedef uint8_t                                   data_t;
typedef uint32_t                                  data_size_t;
typedef uint32_t                                  gnode_id_t;
#ifndef __OLD_SOLARIS__
typedef int32_t                                   index_t;
#else
#define index_t int32_t
#endif
typedef uint32_t                                  backup_num_t;
typedef uint32_t                                  size_type; // size_t already reserved

typedef std::vector<backup>                       backups_t;
typedef std::vector<std::string>                  strings_t;
typedef std::pair<std::string, gnode_id_t>        simple_link_t;
typedef std::vector<gnode_id_t>                   gnode_ids_t;
typedef std::vector<simple_link_t>                simple_links_t;
typedef std::pair<atom_t, gnode_ids_t>            gnode_link_t;
typedef std::map<atom_t, gnode_ids_t>             gnode_links_t;
typedef std::map<std::string, atom_t>             erl_atom2atom_t;
typedef strings_t                                 atom2erl_atom_t;
typedef std::vector<gnode*>                       gnodes_t;
typedef std::priority_queue<gnode_id_t,
                    std::vector<gnode_id_t>,
                    std::greater<gnode_id_t> >    free_indexes_t;
typedef gnode_ids_t                               prot_nodes_t;
typedef std::pair<gnode_id_t, gnode_links_t>      prot_link_t;
typedef std::map<gnode_id_t, gnode_links_t>       prot_links_t;
typedef std::list<gnode_id_t>                     garbage_container_t;

#endif
