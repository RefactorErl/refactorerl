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
#include <map>
#include <string>

// it is already included in types.h
//#ifdef __OLD_SOLARIS__
//#include <inttypes.h>
//#else
//#include <stdint.h>
//#endif

#include "types.h"
#include "conversions.h"
#include "globals.h"

using namespace std;
extern graph* globals::ngraph;


//-------------------------------------------------------------------------
// Binary conversions

ErlNifBinary bin_term2nif_bin(ErlNifEnv* env, const ERL_NIF_TERM& bin_term) {
    ErlNifBinary insp_data;

    enif_inspect_binary(env, bin_term, &insp_data);

    return insp_data;
}


//-------------------------------------------------------------------------
// Number conversions

gnode_id_t term2gnode_id(ErlNifEnv* env, const ERL_NIF_TERM& gnode_id_term) {
    return term2uint(env, gnode_id_term);
}
ERL_NIF_TERM gnode_id2term(ErlNifEnv* env, const gnode_id_t& gnode_id) {
    return uint2term(env, gnode_id);
}

index_t term2index(ErlNifEnv* env, const ERL_NIF_TERM& index_term) {
    return term2int(env, index_term);
}
ERL_NIF_TERM index2term(ErlNifEnv* env, const index_t& index) {
    return int2term(env, index);
}

backup_num_t term2backup_num(ErlNifEnv* env, const ERL_NIF_TERM& backup_num_term)
{
    return term2uint(env, backup_num_term);
}
ERL_NIF_TERM backup_num2term(ErlNifEnv* env, const backup_num_t& backup_num) {
    return uint2term(env, backup_num);
}

int term2int(ErlNifEnv* env, const ERL_NIF_TERM& i_term) {
    int i;

    enif_get_int(env, i_term, &i);

    return i;
}
ERL_NIF_TERM int2term(ErlNifEnv* env, const int& i) {
    return enif_make_int(env, i);
}

unsigned int term2uint(ErlNifEnv* env, const ERL_NIF_TERM& ui_term) {
    unsigned int ui;

    enif_get_uint(env, ui_term, &ui);

    return ui;
}
ERL_NIF_TERM uint2term(ErlNifEnv* env, const unsigned int& ui) {
    return enif_make_uint(env, ui);
}


//-------------------------------------------------------------------------
// String conversions

string term2str(ErlNifEnv* env, const ERL_NIF_TERM& term) {
    ErlNifBinary term_name;
    
    enif_inspect_binary(env, term, &term_name);
    string str((char*)term_name.data, term_name.size);

    return str;
}

ERL_NIF_TERM str2erl_str(ErlNifEnv* env, const std::string str) {
    return enif_make_string(env, str.c_str(), ERL_NIF_LATIN1);
}

string erl_str2str(ErlNifEnv* env, const ERL_NIF_TERM& erl_string) {
    size_type str_size;
    char* str_buff;
    string ret_string;

    enif_get_list_length(env, erl_string, &str_size);
	// enif_get_atom_length returns the number of bytes,
	// excluding terminating null character.
	++str_size;

    str_buff = new char[str_size];
    enif_get_string(env, erl_string, str_buff, str_size, ERL_NIF_LATIN1);
	// do not copy the terminating null character.
    ret_string = string(str_buff);

    delete[] str_buff;
    return ret_string;
}

ERL_NIF_TERM str2atom(ErlNifEnv* env, const string& str) {
    return enif_make_atom(env, str.c_str());
}

string atom2str(ErlNifEnv* env, const ERL_NIF_TERM& atom) {
    size_type str_size;
    char* str_buff;
    string ret_string;

    enif_get_atom_length(env, atom, &str_size, ERL_NIF_LATIN1);
	// enif_get_atom_length returns the number of bytes,
	// excluding terminating null character.
	++str_size;

    str_buff = new char[str_size];
    if( enif_get_atom(env, atom, str_buff, str_size,
                        ERL_NIF_LATIN1) == 0 ) {
        return "";
	}
	// do not copy the terminating null character.
    ret_string = string(str_buff);

    delete[] str_buff;
    return ret_string;
}


//-------------------------------------------------------------------------
// Pointer conversions

ERL_NIF_TERM globalptr2term(ErlNifEnv* env, const void* gptr) {
    return enif_make_ptr(env, reinterpret_cast<erl_ptr_t>(gptr));
}

erl_ptr_t term2globalptr(ErlNifEnv* env, const ERL_NIF_TERM& ptr_term) {
    erl_ptr_t ptr;

    enif_get_ptr(env, ptr_term, &ptr);

    return ptr;
}


//-----------------------------------------------------------------------------
// Atom conversions

atom_t atom_as_fwd_link(const atom_t& atom)  {
    return 2 * atom;
}

atom_t atom_as_back_link(const atom_t& atom) {
    return 2 * atom + 1;
}

atom_t fwd_link_as_atom(const atom_t& atom) {
    return atom >> 1;
}

atom_t back_link_as_atom(const atom_t& atom) {
    return atom >> 1;
}


//-----------------------------------------------------------------------------
// Other conversions

bool term2bool(ErlNifEnv* env, const ERL_NIF_TERM& term) {
    return term2str(env, term) == "true";
}

bool str2bool(const string& str) {
    return
     (str_starts_with(str, "1") || str_starts_with(str, "true")) ? true : false;
}

op_code_t str2op_code(const string& str) {
    return str2uint(str);
}

gnode_id_t str2gnode_id(const string& str) {
    return str2uint(str);
}

data_size_t str2data_size(const string& str) {
    return str2uint(str);
}

index_t str2index(const string& str) {
    return str2int(str);
}

backup_num_t str2backup_num(const string& str) {
    return str2uint(str);
}

uint32_t str2uint(const string& str) {
    return (uint32_t)strtoul(str.c_str(), NULL, 0);
}

int str2int(const string& str) {
    return atoi(str.c_str());
}
