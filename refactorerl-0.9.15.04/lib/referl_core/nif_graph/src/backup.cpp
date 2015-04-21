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
#include <fstream>
#include <cctype>
#include <exception>

#include "algorithms.h"
#include "conversions.h"
#include "backup.h"
#include "directory.h"
#include "globals.h"

using namespace std;


backup::backup(const string& name) : state(name) { }

backup::backup(const backup_num_t& cp_num) : state(gen_name(cp_num)) { }


//-----------------------------------------------------------------------------
// Static functions

bool backup::exist(const backup_num_t& checkpoint_num) {
    return exist(gen_name(checkpoint_num));
}

bool backup::exist(const string& name) {
    directory d(globals::ngraph->get_path());
    
    return d.get_names(name).size() > 0;
}

string backup::gen_name(const backup_num_t& checkpoint_num) {
    return  BACKUP_PREFIX() + SEPARATOR() + to_string(checkpoint_num);
}

backups_t backup::get_backups_from_dir(const std::string& path) {
    directory d(path);
    backups_t backups;
    strings_t backup_names;

    backup_names = d.get_names_by_prefix(BACKUP_PREFIX() + SEPARATOR());

    LOOP(backup_names, i) {
        backups.push_back(backup(backup_names[i]));
    }

    return backups;
}


//-----------------------------------------------------------------------------
// File read/write methods

void backup::set_commit_log(const string& commit_log) const {
    if(commit_log != "") {
        bin_file f(commit_log_path(), ios::out);
        f.write(commit_log);
        f.close();
    }
}

string backup::get_commit_log() const {
    string commit_log;
    bin_file f(commit_log_path(), ios::in);

    if(f.is_open()) {
        f.read(commit_log);
    }
    f.close();

    return commit_log;
}


//-----------------------------------------------------------------------------
// Other functions

bool backup::exist() const {
    return exist(name);
}

bool backup::is_valid_name() const {
    return str_starts_with(name, BACKUP_PREFIX() + SEPARATOR()) &&
           str_split(name, SEPARATOR()).size() == 2;
}

backup_num_t backup::get_checkpoint_num() const {
    if(is_valid_name()) {
        return str2backup_num(str_split(name, SEPARATOR())[1]);
    }

    throw invalid_name("The name of the backup is invalid: " + name);
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Constants

string backup::BACKUP_PREFIX() {
    return "backup";
}

string backup::COMMIT_LOG_PREFIX() {
    return "commit_log";
}

string backup::SEPARATOR() {
    return ".";
}


//-----------------------------------------------------------------------------
// Other functions

string backup::commit_log_path() const {
    return globals::ngraph->get_path() + commit_log_file();
}

string backup::commit_log_file() const {
    return COMMIT_LOG_PREFIX() + SEPARATOR() + to_string(get_checkpoint_num());
}
