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

#include <iostream>
#include <string>
#include <stack>
#include <vector>
#include <fstream>
#include <limits.h>

#include "directory.h"
#include "algorithms.h"
#include "conversions.h"
#include "graph.h"
#include "globals.h"
#include "types.h"
#include "backup.h"

using namespace std;
extern graph* globals::ngraph;


persistence::persistence() {
    load_last_used_graph();
}


//-----------------------------------------------------------------------------
// Backup operations

string persistence::create_backup() {
    return create_backup("");
}

string persistence::create_backup(string commit_log) {
    backup new_backup = backup(++max_checkpoint_num);

    save(new_backup);
    new_backup.set_commit_log(commit_log);

    return new_backup.get_name();
}

void persistence::save(const state& _state) {
    globals::ngraph->save(_state.get_path());
    finishing_up(_state);
}

void persistence::restore(const state& _state) {
    globals::ngraph->load(_state.get_path());
    finishing_up(_state);
}

void persistence::delete_all_backups() {
    directory d(globals::ngraph->get_path());

    init_vars();
    d.remove_all();

    d.close();
}


//-----------------------------------------------------------------------------
// Other operations

void persistence::swap_graph(const string& graph_name) {
    init_vars();

    if(globals::ngraph != NULL) {
        delete globals::ngraph;
    }

    globals::ngraph = new graph(graph_name);
    directory d(globals::ngraph->get_path());
    if (d.exist(LAST_USED_BACKUP_FILE())) {
        state _state;
        try {
            read_last_backup_features(_state);
            if(_state.get_name() == BEFORE_TRANS_FILE) {
                d.rename(BEFORE_TRANS_FILE, DB_FILE);
                _state.set_name(DB_FILE);
            } else {
                d.remove(BEFORE_TRANS_FILE);
            }
            max_checkpoint_num = determine_max_checkpoint_num();
            restore(_state);
        } catch(...) {
            globals::ngraph->clear();
            d.remove(LAST_USED_BACKUP_FILE());
            d.rename(_state.get_name(), "corrupted_" + _state.get_name());

            d.close();
            throw graph_load_fail("Error occured while loading the graph: " +
                                   _state.get_name());
        }
    }

    d.close();
    globals::ngraph->write_name(globals::graphs_dir);
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Constants

string persistence::LAST_USED_BACKUP_FILE() {
    return "last_used";
}


//-----------------------------------------------------------------------------
// Read/Write operations

void persistence::write_last_backup_features(const state& _state) {
    if(_state.get_name() != "") {
        fstream f((globals::ngraph->get_path() + LAST_USED_BACKUP_FILE()).c_str(),
                   ios::out);

        if(f.is_open()) {
            _state.write_name(f);
        }

        f.close();
    }
}

void persistence::read_last_backup_features(state& _state) {
    fstream f((globals::ngraph->get_path() + LAST_USED_BACKUP_FILE()).c_str(),
               ios::in);

    if(f.is_open()) {
        _state.read_name(f);
    }

    f.close();
}

backup_num_t persistence::determine_max_checkpoint_num() {
    backups_t backups =
                      backup::get_backups_from_dir(globals::ngraph->get_path());

    backup_num_t max_cp_num = 0;
    LOOP(backups, i) {
        backup_num_t act_checkpoint_num = backups[i].get_checkpoint_num();

        if(act_checkpoint_num > max_cp_num) {
            max_cp_num = act_checkpoint_num;
        }
    }

    return max_cp_num;
}


//-----------------------------------------------------------------------------
// Other private operations

inline void persistence::init_vars() {
    max_checkpoint_num = 0;
}

inline void persistence::finishing_up(const state& _state) {
    write_last_backup_features(_state);
}

void persistence::load_last_used_graph() {
    directory d(globals::graphs_dir);
    string graph_to_load;

    try {
        graph_to_load = graph::read_name(globals::graphs_dir);
    } catch(...) { }

    if(graph_to_load == "" || !d.exist(graph_to_load)) {
        graph_to_load = graph::create_default_graph();
    }

    swap_graph(graph_to_load);
}
