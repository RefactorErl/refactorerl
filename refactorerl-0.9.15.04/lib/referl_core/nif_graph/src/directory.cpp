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
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <unistd.h> // getcwd build error on linux

#include "algorithms.h"
#include "directory.h"
#include "bin_file.h"
#include "types.h"

using namespace std;


//-----------------------------------------------------------------------------
// Constructors and open operations

directory::directory() {
    path = "";
    d = NULL;
    dir = NULL;
}

directory::directory(const char* dir_path) {
    open(dir_path);
}

directory::directory(const string& dir_path) {
    open(dir_path.c_str());
}

bool directory::open(const string& dir_path) {
    return open(dir_path.c_str());
}

bool directory::open(const char* dir_path) {
    d = opendir(dir_path);
    if(!is_open()) {
        return false;
    }

    path = string(dir_path);
    fix_path(path);

    return true;
}


//-----------------------------------------------------------------------------
// Static functions

string directory::get_working_dir() {
    const size_type buff_size = 2048;
    char buffer[buff_size];

    string cur_dir(getcwd(buffer, buff_size));
    
    return cur_dir;
}


string directory::get_file_name(string path) {
    size_type i;

    for(i = path.size() - 1; path[i] != directory::separator(); --i) { }

    return path.substr(i+1, path.size());
}

char directory::separator() {
    return
        #if IS_WINDOWS
            '\\';
        #else
            '/';
        #endif
}


//-----------------------------------------------------------------------------
// Iterator operations

std::string directory::first() {
    reinit();
    return next();
}

string directory::next() {
    string d_name;

    do {
        dir = readdir(d);
        if (dir != NULL) {
            d_name = string(dir->d_name);
        }
    }
    while( dir != NULL && (d_name == "." || d_name == "..") );

    return d_name;
}

string directory::current() {
    return string(dir->d_name);
}

bool directory::end_of_dir() const {
    return dir == NULL;
}


//-----------------------------------------------------------------------------
// Directory modification methods

#if IS_WINDOWS
void directory::make_dir(const string& dir_name) {
    mkdir(get_full_path(dir_name).c_str());
}
#else
void directory::make_dir(const string& dir_name, const mode_t& mode) {
    mkdir(get_full_path(dir_name).c_str(), mode);
}
#endif

bool directory::rename(const string& old_name, const string& new_name) {
    return std::rename(get_full_path(old_name).c_str(),
                       get_full_path(new_name).c_str());
}

bool directory::remove(const string& name) {
    string full_path = get_full_path(name);

    if ( is_directory(name) ) {
        directory sub_dir(full_path.c_str());

        if(sub_dir.is_open()) {
            sub_dir.remove_all();
        }
        sub_dir.close();

        return rmdir(full_path.c_str()) == 0;
    } else {
        return std::remove(full_path.c_str()) == 0;
    }
}

void directory::remove_all() {
    for(first(); !end_of_dir(); next()) {
        this->remove(current());
    }
}


//-----------------------------------------------------------------------------
// Query functions

string directory::get_path() const {
    return path;
}

bool directory::is_open() const {
    return d != NULL;
}

bool directory::is_directory(const string& name) const {
    struct stat st_buff;

    if(is_open()) {
        stat(get_full_path(name).c_str(), &st_buff);
        if (S_ISDIR(st_buff.st_mode)) {
            return true;
        }
    }

    return false;
}

bool directory::is_directory(const char* name) const {
    return is_directory(string(name));
}

bool directory::is_file(const string& name) const {
    return !is_directory(name);
}

bool directory::is_file(const char* name) const {
    return is_file(string(name));
}

bool directory::exist(const char* name) const {
    return exist(string(name));
}

bool directory::exist(const string& name) const {
    strings_t names = get_names(name);
    
    return name != "" && names.size() > 0;
}

strings_t directory::get_names(const string& name) const {
    strings_t found_names;
    directory _d(path);

    if(_d.is_open()) {
        for(_d.first(); !_d.end_of_dir(); _d.next()) {
            string act_name(_d.current());

            if(act_name == name) {
                found_names.push_back(act_name);
            }
        }
    }

    _d.close();

    return found_names;
}

strings_t directory::get_names_by_prefix(const string& prefix) const {
    strings_t found_names;
    directory _d(path);

    if(_d.is_open()) {
        for(_d.first(); !_d.end_of_dir(); _d.next()) {
            string act_name(_d.current());

            if(str_starts_with(act_name, prefix)) {
                found_names.push_back(act_name);
            }
        }
    }

    _d.close();

    return found_names;
}

strings_t directory::get_names_by_suffix(const string& suffix) const {
    strings_t found_names;
    directory _d(path);

    if(_d.is_open()) {
        for(_d.first(); !_d.end_of_dir(); _d.next()) {
            string act_name(_d.current());

            if(str_ends_with(act_name, suffix)) {
                found_names.push_back(act_name);
            }
        }
    }

    _d.close();

    return found_names;
}

size_type directory::count() const {
    size_type count = 0;
    directory _d(path);

    for(_d.first(); !_d.end_of_dir(); _d.next()) {
        ++count;
    }

    _d.close();

    return count;
}


//-----------------------------------------------------------------------------
// Other functions

bool directory::close() {
  bool ret = true;
  // segfault in Solaris 8 in libc.so, if d is NULL
  if (d != NULL) {
    ret = (closedir(d) == 0);
  }
    path = "";
    d = NULL;
    dir = NULL;

    return ret;
}

directory::~directory() {
    close();
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------

void directory::reinit() {
    closedir(d);
    open(path);
}

string directory::get_full_path(const string& name) const {
    return path + name;
}

void directory::fix_path(string& path_to_fix) {
    char dir_separator = directory::separator();
    if(path_to_fix.at(path_to_fix.size()-1) != dir_separator) {
            path_to_fix += dir_separator;
    }
}
