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

extern "C" {
#include "erl_nif.h"
}

#include <iostream>
#include <map>
#include <string.h>
#include <errno.h>

#include "directory.h"
#include "gnode.h"
#include "algorithms.h"
#include "conversions.h"
#include "persistence.h"
#include "nif_load.h"
#include "graph.h"
#include "globals.h"

using namespace std;

extern graph *globals::ngraph;


// -----------------------------------------------------------------------------
// NIF interface

extern "C" {
    // this is an abstraction of how the pointers to the graph (gnode*)
    // are passed to Erlang.


    //-------------------------------------------------------------------------
    // Node and link operations

    /** Creates a graph node, with a given data. */
    static ERL_NIF_TERM nif_create(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        ErlNifBinary insp_data = bin_term2nif_bin(env, argv[0]);
        bool is_protected = term2bool(env, argv[1]);

        gnode* new_node = new gnode(insp_data.data, insp_data.size);
        gnode_id_t gnode_pos = globals::ngraph->add_node(new_node, is_protected);

        return gnode_id2term(env, gnode_pos);
    }

    /** Updates the node's data, that belongs to the given ID. */
    static ERL_NIF_TERM nif_update(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);
        ErlNifBinary insp_data = bin_term2nif_bin(env, argv[1]);

        try {
            globals::ngraph->update_node(node_id, insp_data.data, insp_data.size);
        } catch (graph::invalid_node) {
            return BAD_NODE(env, node_id);
        } catch(graph::protected_node) {
            return PROTECTED_NODE(env, node_id);
        }

        return OK(env);
    }

    /** Deletes the node, that belongs to the given ID. */
    static ERL_NIF_TERM nif_delete(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);

        try {
            globals::ngraph->delete_node(node_id);
        } catch (graph::invalid_node) {
            return BAD_NODE(env, node_id);
        } catch(graph::protected_node) {
            return PROTECTED_NODE(env, node_id);
        }

        return OK(env);
    }

    /** Make a link between 2 nodes. */
    static ERL_NIF_TERM nif_mklink(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id1 = term2gnode_id(env, argv[0]);
        string tag_string   = term2str(env, argv[1]);
        index_t idx         = term2index(env, argv[2]);
        gnode_id_t node_id2 = term2gnode_id(env, argv[3]);
        bool is_protected   = term2bool(env, argv[4]);

        gnode* node1;
        try {
            node1 = globals::ngraph->get_node(node_id1);
        } catch(...) {
            return BAD_NODE(env, node_id1);
        }

        gnode* node2;
        try {
            node2 = globals::ngraph->get_node(node_id2);
        } catch(...) {
            return BAD_NODE(env, node_id2);
        }

        globals::ngraph->mk_link(node1, node_id1, tag_string, idx,
                                 node2, node_id2, is_protected);

        return OK(env);
    }

    /** Removes a link between 2 nodes. */
    static ERL_NIF_TERM nif_rmlink(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id1 = term2gnode_id(env, argv[0]);
        string tag_string   = term2str(env, argv[1]);
        gnode_id_t node_id2 = term2gnode_id(env, argv[2]);

        try {
            globals::ngraph->rm_link(node_id1, tag_string, node_id2);
        } catch(graph::invalid_link) {
            return NOT_EXISTS(env, node_id1, tag_string, node_id2);
        } catch(graph::invalid_node) {
            return NOT_EXISTS(env, node_id1, tag_string, node_id2);
        } catch(graph::protected_link) {
            return PROTECTED_LINK(env, node_id1, tag_string, node_id2);
        }

        return OK(env);
    }

    /** Removes isolated nodes from the graph. */
    static ERL_NIF_TERM nif_remove_garbage(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        globals::ngraph->remove_garbage();

        return OK(env);
    }


    //-------------------------------------------------------------------------
    // Query operations

    /** Gets the ID of the graph's root. */
    static ERL_NIF_TERM nif_root(ErlNifEnv* env, int, const ERL_NIF_TERM[]) {
        return gnode_id2term(env, globals::ngraph->get_root_id());
    }

    /** Gets data of a node, that belongs to the given ID. */
    static ERL_NIF_TERM nif_data(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);
        ErlNifBinary bin;

        if(node_id == globals::ngraph->get_root_id()) {
            return enif_make_tuple1(env, str2atom(env, "root"));
        }

        gnode* node;
        try {
            node = globals::ngraph->get_node(node_id);
        } catch (...) {
            return BAD_NODE(env, node_id);
        }

        enif_alloc_binary(node->get_data_size(), &bin);
        memcpy(bin.data, node->get_data(), node->get_data_size());

        return enif_make_binary(env, &bin);
    }

    /** Gets the index of the given link's tag,
     *  that is between the 2 given nodes.
     */
    static ERL_NIF_TERM nif_index(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id1 = term2gnode_id(env, argv[0]);
        string tag_string   = term2str(env, argv[1]);
        gnode_id_t node_id2 = term2gnode_id(env, argv[2]);

        try {
            index_t idx =
                     globals::ngraph->get_index(node_id1, tag_string, node_id2);
            if (idx > 0) {
                return index2term(env, idx);
            }
        } catch (...) { }

        return NONE(env);
    }

    /** Traverses the link with tag argv[1] from node argv[0].
     *  Returns the resulting node ids in order. */
    static ERL_NIF_TERM nif_fwd_path(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);
        string tag_string  = term2str(env, argv[1]);

        ERL_NIF_TERM fwd_path_list = enif_make_list(env, 0);
        try {
            gnode_ids_t& fwd_links =
                             globals::ngraph->get_fwd_path(node_id, tag_string);

            BACK_LOOP(fwd_links, i) {
                fwd_path_list =
                    enif_make_list_cell(env, gnode_id2term(env, fwd_links[i]),
                                        fwd_path_list);
            }

            return fwd_path_list;
        } catch (...) {
            return BADARG(env);
        }
    }

    /** Evaluates a path expression starting from Node, and returns the
      * resulting nodes. */
    static ERL_NIF_TERM nif_back_path(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);
        string tag_string  = term2str(env, argv[1]);

        ERL_NIF_TERM back_path_list = enif_make_list(env, 0);
        try {
            gnode_ids_t& back_links=
                            globals::ngraph->get_back_path(node_id, tag_string);

            BACK_LOOP(back_links, i) {
                back_path_list =
                        enif_make_list_cell(env, gnode_id2term(env, back_links[i]),
                                            back_path_list);
            }

            return back_path_list;
        } catch (...) {
            return BADARG(env);
        }
    }

    /** Returns the links starting from a node. */
    static ERL_NIF_TERM nif_links(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);

        ERL_NIF_TERM link_list = enif_make_list(env, 0);
        try {
            simple_links_t fwd_links =
                        globals::ngraph->get_fwd_links(node_id);

            LOOP(fwd_links, i) {
                simple_link_t& link = fwd_links[i];

                ERL_NIF_TERM tag   = str2atom(env, link.first);
                ERL_NIF_TERM to    = gnode_id2term(env, link.second);
                ERL_NIF_TERM tuple = enif_make_tuple(env, 2, tag, to);

                link_list = enif_make_list_cell(env, tuple, link_list);
            }

            return link_list;
        } catch (...) {
            return BAD_NODE(env, node_id);
        }
    }

    /** Returns the links starting from a node. */
    static ERL_NIF_TERM nif_back_links(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);

        ERL_NIF_TERM link_list = enif_make_list(env, 0);
        try {
            simple_links_t back_links =
                        globals::ngraph->get_back_links(node_id);

            LOOP(back_links, i) {
                simple_link_t& link = back_links[i];

                ERL_NIF_TERM tag   = str2atom(env, link.first);
                ERL_NIF_TERM to    = gnode_id2term(env, link.second);
                ERL_NIF_TERM tuple = enif_make_tuple(env, 2, tag, to);

                link_list = enif_make_list_cell(env, tuple, link_list);
            }

            return link_list;
        } catch (...) {
            return BAD_NODE(env, node_id);
        }
    }

    static ERL_NIF_TERM nif_is_protected_node(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id = term2gnode_id(env, argv[0]);
        bool is_protected  = globals::ngraph->is_protected_node(node_id);

        return (is_protected ? TRUE(env) : FALSE(env));
    }

    /*static ERL_NIF_TERM nif_is_protected_link(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        gnode_id_t node_id1 = term2gnode_id(env, argv[0]);
        string tag          = term2str(env, argv[1]);
        gnode_id_t node_id2 = term2gnode_id(env, argv[2]);
        bool is_protected   =
                    globals::ngraph->is_protected_link(node_id1, tag, node_id2);

        return (is_protected ? TRUE(env) : FALSE(env));
    }*/


    //-------------------------------------------------------------------------
    // Graph operations

    /** Creates a graph, with the given name. */
    static ERL_NIF_TERM nif_create_graph(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string name = atom2str(env, argv[0]);
        directory dir(globals::graphs_dir);

        CHECK_NAME(env, dir, name)
        dir.make_dir(name);
        if(!dir.exist(name)) {
            return INVALID_NAME(env, name);
        }

        return OK(env);
    }

    /** Renames a graph. */
    static ERL_NIF_TERM nif_rename_graph(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string old_name = atom2str(env, argv[0]);
        string new_name = atom2str(env, argv[1]);
        directory dir(globals::graphs_dir);

        if (!dir.exist(old_name)) {
            return GRAPH_NOT_EXIST(env, old_name);
        }
        CHECK_NAME(env, dir, new_name)

        dir.rename(old_name, new_name);

        if(!dir.exist(new_name)) {
            return INVALID_NAME(env, new_name);
        }
        if (old_name == globals::ngraph->get_name()) {
            globals::ngraph->set_name(new_name);
        }

        return OK(env);
    }

    /** Lists the created graphs. */
    static ERL_NIF_TERM nif_ls_graphs(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        ERL_NIF_TERM graph_list = enif_make_list(env, 0);
        ERL_NIF_TERM graph_name;

        directory d(globals::graphs_dir);

        for (d.first(); !d.end_of_dir(); d.next()) {
            if (d.is_directory(d.current())) {
                graph_name = str2atom(env, d.current());

                graph_list = enif_make_list_cell(env, graph_name, graph_list);
            }
        }

        return graph_list;
    }

    /** Gets the name of the actual graph. */
    static ERL_NIF_TERM nif_actual_graph(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        return str2atom(env, globals::ngraph->get_name());
    }

    /** Loads the given, previously created graph. */
    static ERL_NIF_TERM nif_load_graph(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string name = atom2str(env, argv[0]);
        directory dir(globals::graphs_dir);

        if (!dir.exist(name)) {
            return GRAPH_NOT_EXIST(env, name);
        }

        try {
            globals::backup_system->swap_graph(name);
        } catch(persistence::graph_load_fail) {
            return GRAPH_LOAD_FAIL(env, name);
        }

        return OK(env);
    }

    /** Deletes the given graph. */
    static ERL_NIF_TERM nif_delete_graph(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string name = atom2str(env, argv[0]);
        directory dir(globals::graphs_dir);

        if (globals::ngraph->get_name() == name) {
            return GRAPH_IS_IN_USE(env, name);
        }

        if (dir.exist(name)) {
            dir.remove(name);
        } else {
            return GRAPH_NOT_EXIST(env, name);
        }

        return OK(env);
    }

    /** Deletes all graphs. */
    static ERL_NIF_TERM nif_delete_all_graphs(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        directory dir(globals::graphs_dir);

        if (dir.is_open()) {
            globals::ngraph->unload();
            dir.remove_all();
            globals::backup_system->swap_graph(graph::create_default_graph());
        }

        return OK(env);
    }


    // ------------------------------------------------------------------------
    // Persistence functions

    static ERL_NIF_TERM nif_reset_schema(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        directory d(globals::ngraph->get_path());

        d.remove(BEFORE_TRANS_FILE);
        d.close();
        globals::ngraph->clear();

        return OK(env);
    }

    /** Creates a checkpoint from the actual graph.
     *  If the graph has been changed largely, then saves the whole graph,
     *  otherwise saves only the tiny changes.
     */
    static ERL_NIF_TERM nif_backup(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string commit_log = erl_str2str(env, argv[0]);
        string crated_backup_name =
                              globals::backup_system->create_backup(commit_log);

        return str2atom(env, crated_backup_name);
    }

    static ERL_NIF_TERM nif_ls_backups(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        ERL_NIF_TERM backup_list = enif_make_list(env, 0);
        backups_t backups =
                      backup::get_backups_from_dir(globals::ngraph->get_path());

        LOOP(backups, i) {
            backup_list = enif_make_list_cell(env, str2atom(env, backups[i].get_name()),
                                              backup_list);
        }

        return backup_list;
    }

    static ERL_NIF_TERM nif_backup_info(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        const ERL_NIF_TERM& term = argv[0];

        backup _backup;
        string last_mod;
        string commit_log;

        if(enif_is_atom(env, term)) {
            _backup = backup(atom2str(env, term));
        } else {
            _backup = backup(term2backup_num(env, term));
        }

        if(!_backup.exist()) {
            return INVALID_BACKUP(env, _backup.get_name());
        }

        last_mod = bin_file::last_modification(_backup.get_path());
        commit_log = _backup.get_commit_log();

        return enif_make_tuple3(env, str2atom(env, _backup.get_name()),
                                str2erl_str(env, last_mod),
                                str2erl_str(env, commit_log));
    }

    static ERL_NIF_TERM nif_undo(ErlNifEnv* env, int, const ERL_NIF_TERM[]) {
        directory d(globals::ngraph->get_path());

        try {
            bin_file f(d.get_path() + BEFORE_TRANS_FILE, ios::in);

            f.validate();
            f.close();

	    // windows doesn't allow rename to an existing file, hence the wo
	    d.remove(DB_FILE_BAK);
	    d.rename(DB_FILE, DB_FILE_BAK);
	    int ren = d.rename(BEFORE_TRANS_FILE, DB_FILE);
            if (ren == 0) {
	      d.remove(DB_FILE_BAK);
	      globals::backup_system->restore(state(DB_FILE));
	      return OK(env);
	    } else {
	      d.rename(DB_FILE_BAK, DB_FILE);
	      std::string err = strerror(errno);
	      return str2erl_str(env, "UNDO: rename failed: " + err);
	    }
        } catch(...) {
	  if (!d.exist(DB_FILE)) {
	    d.rename(DB_FILE_BAK, DB_FILE);
	  }
	  d.remove(BEFORE_TRANS_FILE);
	  return NO_BACKUP(env);
        }

        return OK(env);
    }

    static ERL_NIF_TERM nif_restore(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        const ERL_NIF_TERM& term = argv[0];

        backup backup_to_restore;
        if(enif_is_atom(env, term)) {
            string backup_name = atom2str(env, term);
            backup_to_restore = backup(backup_name);
        } else {
            backup_num_t cp_num = term2backup_num(env, term);
            backup_to_restore = backup(cp_num);
        }

        try {
            globals::backup_system->restore(backup_to_restore);
        } catch(bin_file::file_open_error) {
            return INVALID_BACKUP(env, backup_to_restore.get_name());
        } catch(bin_file::corrupted_file) {
            directory d(globals::ngraph->get_path());
            string backup_name = backup_to_restore.get_name();

            d.rename(backup_name, "corrupted_" + backup_name);

            return CORRUPTED_BACKUP(env, backup_name);
        }

        return OK(env);
    }

    static ERL_NIF_TERM nif_save(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
    {
        string name = atom2str(env, argv[0]);

        globals::backup_system->save(state(name));

        return OK(env);
    }

    /** Deletes all backups. */
    static ERL_NIF_TERM nif_delete_all_backups(ErlNifEnv* env, int, const ERL_NIF_TERM[])
    {
        globals::backup_system->delete_all_backups();

        return OK(env);
    }


    // ------------------------------------------------------------------------
    // NIF connection

    static ErlNifFunc nif_funcs[] = {
            {"nif_create",               2, nif_create },
            {"nif_update",               2, nif_update },
            {"nif_delete",               1, nif_delete },
            {"nif_mklink",               5, nif_mklink },
            {"nif_rmlink",               3, nif_rmlink },
            {"nif_remove_garbage",       0, nif_remove_garbage },
            {"nif_root",                 0, nif_root },
            {"nif_data",                 1, nif_data },
            {"nif_index",                3, nif_index },
            {"nif_fwd_path",             2, nif_fwd_path },
            {"nif_back_path",            2, nif_back_path },
            {"nif_links",                1, nif_links },
            {"nif_back_links",           1, nif_back_links },
            {"nif_is_protected_node",    1, nif_is_protected_node},
            //{"nif_is_protected_link",    3, nif_is_protected_link},

            {"nif_create_graph",         1, nif_create_graph },
            {"nif_rename_graph",         2, nif_rename_graph },
            {"nif_ls_graphs",            0, nif_ls_graphs },
            {"nif_actual_graph",         0, nif_actual_graph },
            {"nif_load_graph",           1, nif_load_graph },
            {"nif_delete_graph",         1, nif_delete_graph },
            {"nif_delete_all_graphs",    0, nif_delete_all_graphs },
            {"nif_reset_schema",         0, nif_reset_schema},
            {"nif_backup",               1, nif_backup },
            {"nif_save",                 1, nif_save },
            {"nif_undo",                 0, nif_undo },
            {"nif_ls_backups",           0, nif_ls_backups },
            {"nif_backup_info",          1, nif_backup_info },
            {"nif_restore",              1, nif_restore },
            {"nif_backup",               1, nif_backup },
            {"nif_delete_all_backups",   0, nif_delete_all_backups },

            {"nif_get_datastore",        0, nif_get_datastore }
    };

    ERL_NIF_INIT(refdb_nif, nif_funcs, load, reload, upgrade, unload)

}
