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

#ifndef __convert_h_
#define __convert_h_

extern "C"
{
    #include "erl_nif.h"
}

#include <iostream>

#include "types.h"
#include "gnode.h"
#include "graph.h"

class graph;

//-------------------------------------------------------------------------
// Erlang type conversions to C/C++ type, and vica versa.
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
// Binary conversions

ErlNifBinary bin_term2nif_bin(ErlNifEnv* env, const ERL_NIF_TERM& bin_term);


//-------------------------------------------------------------------------
// Number conversions


gnode_id_t term2gnode_id(ErlNifEnv* env, const ERL_NIF_TERM& gnode_id_term);
ERL_NIF_TERM gnode_id2term(ErlNifEnv* env, const gnode_id_t& gnode_id);

index_t term2index(ErlNifEnv* env, const ERL_NIF_TERM& index_term);
ERL_NIF_TERM index2term(ErlNifEnv* env, const index_t& index);

backup_num_t term2backup_num(ErlNifEnv* env, const ERL_NIF_TERM& backup_num_term);
ERL_NIF_TERM backup_num2term(ErlNifEnv* env, const backup_num_t& backup_num);

gnode_id_t term2uint(ErlNifEnv* env, const ERL_NIF_TERM& ui_term);
ERL_NIF_TERM uint2term(ErlNifEnv* env, const gnode_id_t& ui);

int term2int(ErlNifEnv* env, const ERL_NIF_TERM& i_term);
ERL_NIF_TERM int2term(ErlNifEnv* env, const int& i);


//-------------------------------------------------------------------------
// String conversions

std::string term2str(ErlNifEnv* env, const ERL_NIF_TERM& term);

ERL_NIF_TERM str2erl_str(ErlNifEnv* env, const std::string str);
std::string erl_str2str(ErlNifEnv* env, const ERL_NIF_TERM& erl_string);

ERL_NIF_TERM str2atom(ErlNifEnv* env, const std::string& tag_string);
std::string atom2str(ErlNifEnv* env, const ERL_NIF_TERM& atom);


//-------------------------------------------------------------------------
// Pointer conversions

ERL_NIF_TERM globalptr2term(ErlNifEnv* env, const void* gptr);
erl_ptr_t term2globalptr(ErlNifEnv* env, const ERL_NIF_TERM& ptr_term);


//-----------------------------------------------------------------------------
// Atom conversions

atom_t atom_as_fwd_link(const atom_t& atom);
atom_t atom_as_back_link(const atom_t& atom);

atom_t fwd_link_as_atom(const atom_t& atom);
atom_t back_link_as_atom(const atom_t& atom);


//-----------------------------------------------------------------------------
// Other conversions

bool term2bool(ErlNifEnv* env, const ERL_NIF_TERM& term);

bool str2bool(const std::string& str);
gnode_id_t str2gnode_id(const std::string& str);
data_size_t str2data_size(const std::string& str);
index_t str2index(const std::string& str);
backup_num_t str2backup_num(const std::string& str);

uint32_t str2uint(const std::string& str);
int str2int(const std::string& str);

#endif
