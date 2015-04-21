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

#ifdef __OLD_SOLARIS__
#include <inttypes.h>
#else
#include <stdint.h>
#endif

#include <string.h> // memcpy

#include "gnode.h"
#include "types.h"
#include "graph.h"
#include "algorithms.h"
#include "globals.h"
#include "conversions.h"

using namespace std;
extern graph* globals::ngraph;


//-----------------------------------------------------------------------------
// Constructors

gnode::gnode() {
    set_default_values();
}

gnode::gnode(const data_t* node_data, const data_size_t& data_size) {
    init(node_data, data_size);
}

gnode::gnode(const gnode* node) {
    init(node->get_data(), node->get_data_size());
}


//-----------------------------------------------------------------------------
// Node modification methods

void gnode::update(const data_t* new_data, const data_size_t& data_size) {
    fill_node(new_data, data_size);
}


//-----------------------------------------------------------------------------
// Destroy methods

void gnode::delete_node(const gnode_id_t& node_id) {
    delete_all_links(node_id);
	delete this;
}


//-----------------------------------------------------------------------------
// Link methods

void gnode::add_fwd_link(const atom_t& atom_tag, const index_t& idx,
                         const gnode_id_t& node_id)
{
    gnode_ids_t& fwds = links[atom_as_fwd_link(atom_tag)];

    if (idx == -1) {
        // inserting at end
        fwds.push_back(node_id);
    } else {
        // Only idx - 1 places are skipped,
        // as indices start from 1 in RefactorErl.
		fwds.insert(fwds.begin() + (idx - 1), node_id);
    }
}

void gnode::add_back_link(const atom_t& atom_tag, const gnode_id_t& node_id) {
    links[atom_as_back_link(atom_tag)].push_back(node_id);
}

void gnode::rm_fwd_link(const gnode_id_t& node_id1, const atom_t& atom_tag,
                        const gnode_id_t& node_id2) {
    rm_link(node_id1, atom_as_fwd_link(atom_tag), node_id2);
}

void gnode::rm_back_link(const gnode_id_t& node_id1, const atom_t& atom_tag,
                         const gnode_id_t& node_id2) {
    rm_link(node_id1, atom_as_back_link(atom_tag), node_id2);
}

void gnode::rm_link(const gnode_id_t& node_id1, const atom_t atom_tag,
                    const gnode_id_t& node_id2) {
    remove(links[atom_tag], node_id2);

    if(is_isolated()) {
        globals::ngraph->add_garbage(node_id1);
    }
}

gnode_links_t gnode::get_fwd_links() {
    gnode_links_t fwd_links;
    COND_ITERATE(gnode_links_t, fwd_link_it, links, is_fwd_link) {
        gnode_link_t fwd_link = *fwd_link_it;
        fwd_links[fwd_link.first] = fwd_link.second;
    }

    return fwd_links;
}

gnode_links_t gnode::get_back_links() {
    gnode_links_t back_links;
    COND_ITERATE(gnode_links_t, back_link_it, links, is_back_link) {
        gnode_link_t back_link = *back_link_it;
        back_links[back_link.first] = back_link.second;
    }

    return back_links;
}

gnode_links_t& gnode::get_links() {
    return links;
}

bool gnode::has_fwd_link(const atom_t& tag, const gnode_id_t& node_id) {
    gnode_ids_t& fwds = links[atom_as_fwd_link(tag)];

    return contains(fwds, node_id);
}

bool gnode::has_back_link(const atom_t& tag, const gnode_id_t& node_id) {
    gnode_ids_t& backs = links[atom_as_back_link(tag)];

    return contains(backs, node_id);
}

bool gnode::is_fwd_link(const atom_t& atom) {
    // Even numbers indicate forward links.
    return (atom & 1) == 0;
}

bool gnode::is_back_link(const atom_t& atom) {
    // Odd numbers indicate backward links.
    return (atom & 1) == 1;
}


//-----------------------------------------------------------------------------
// Node write out functions

void gnode::write_data_size(bin_file& f) const {
    f.write(get_data_size());
}

void gnode::write_data(bin_file& f) const {
    f.write(get_data(), get_data_size());
}

void gnode::write_fwd_links(bin_file& f) const {
    f.write(fwd_links_size());
    COND_CONST_ITERATE(gnode_links_t, fwd_link_it, links, is_fwd_link) {
        gnode_link_t fwd_link = *fwd_link_it;

        f.write(fwd_link_as_atom(fwd_link.first));
        f.write( (size_type)fwd_link.second.size() );
        LOOP(fwd_link.second, i) {
            f.write(fwd_link.second[i]);
        }
    }
}


//-----------------------------------------------------------------------------
// Node read functions

void gnode::read_data_size(bin_file& f) {
    f.read(size);
}

void gnode::read_data(bin_file& f) {
    delete_data();

    data = new data_t[get_data_size()];
    f.read(data, get_data_size());
}

void gnode::read_fwd_links(bin_file& f) {
    size_type node_fwd_links_size;

    f.read(node_fwd_links_size);

    for(size_type j=0; j < node_fwd_links_size; ++j) {
        atom_t fwd_link_first;
        size_type fwd_link_list_size;
        gnode_ids_t fwd_links_list;

        f.read(fwd_link_first);
        f.read(fwd_link_list_size);

        for(size_type k=0; k < fwd_link_list_size; ++k) {
            gnode_id_t fwd_links_list_el;

            f.read(fwd_links_list_el);
            fwd_links_list.push_back(fwd_links_list_el);
        }

        links.insert(pair<atom_t, gnode_ids_t>(atom_as_fwd_link(fwd_link_first),
                                               fwd_links_list));
    }
}


//-----------------------------------------------------------------------------
// Query functions

data_size_t gnode::get_data_size() const {
    return size;
}

data_t* gnode::get_data() const {
    return data;
}

gnode_ids_t& gnode::get_fwd_path(const atom_t& atom) {
    return links[atom_as_fwd_link(atom)];
}

gnode_ids_t& gnode::get_back_path(const atom_t& atom) {
    return links[atom_as_back_link(atom)];
}

bool gnode::is_isolated() {
    if(is_lex()) {
        ITERATE(gnode_links_t, node_link, links) {
            if(is_back_link(node_link->first) && node_link->second.size() > 0) {
                return false;
            }
        }
    } else {
        ITERATE(gnode_links_t, node_link, links) {
            if(node_link->second.size() > 0) {
                return false;
            }
        }
    }

    return true;
}


//-----------------------------------------------------------------------------
// Other functions

gnode::~gnode() {
    delete_data();
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Delete functions

void gnode::delete_all_links(const gnode_id_t& node_id) {
    delete_fwd_links(node_id);
    delete_back_links(node_id);
}

void gnode::delete_fwd_links(const gnode_id_t& node_id) {
    COND_ITERATE(gnode_links_t, fwd_link_it, links, is_fwd_link) {
        gnode_link_t fwd_link  = *fwd_link_it;
        atom_t fwd_atom        = fwd_link_as_atom(fwd_link.first);
        LOOP(fwd_link.second, i) {
            gnode_id_t other_node_id = fwd_link.second[i];
            
            globals::ngraph->get_node(other_node_id)->rm_back_link(other_node_id,
                                                                   fwd_atom,
                                                                   node_id);
        }
    }
}

void gnode::delete_back_links(const gnode_id_t& node_id) {
    COND_ITERATE(gnode_links_t, back_link_it, links, is_back_link) {
        gnode_link_t back_link = *back_link_it;
        atom_t back_atom        = back_link_as_atom(back_link.first);
        LOOP(back_link.second, i) {
            gnode_id_t other_node_id = back_link.second[i];

            globals::ngraph->get_node(other_node_id)->rm_fwd_link(other_node_id,
                                                                  back_atom,
                                                                  node_id);
        }
    }
}

void gnode::delete_data() {
    if(data != NULL) {
        delete[] data;
    }
}


//-----------------------------------------------------------------------------
// Other private functions

void gnode::init(const data_t* node_data, const data_size_t& data_size) {
    data = NULL;
    fill_node(node_data, data_size);    
}

size_type gnode::fwd_links_size() const {
    size_type size = 0;

    COND_CONST_ITERATE(gnode_links_t, it, links, is_fwd_link) {
        ++size;
    }

    return size;
}

size_type gnode::back_links_size() const {
    size_type size = 0;
    
    COND_CONST_ITERATE(gnode_links_t, it, links, is_back_link) {
        ++size;
    }

    return size;
}

void gnode::fill_node(const data_t* new_data,
                      const data_size_t& new_data_size) {
    delete_data();

    size = new_data_size;
    data = new data_t[new_data_size];
    memcpy(data, new_data, new_data_size);
}

bool gnode::is_lex() {
    string data_str = string((char*)data, size);
    return str_contains(data_str, "lex");
}

void gnode::set_default_values() {
    size = 0;
    data = NULL;
}
