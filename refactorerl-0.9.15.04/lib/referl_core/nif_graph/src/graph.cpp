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
#include <vector>
#include <list>
#include <string>
#include <queue>

#include "graph.h"
#include "gnode.h"
#include "globals.h"
#include "bin_file.h"
#include "graph.h"
#include "directory.h"
#include "persistence.h"
#include "algorithms.h"
#include "types.h"
#include "conversions.h"

using namespace std;

//-----------------------------------------------------------------------------
// Constructors

graph::graph(const string& graph_name) {
    name = graph_name;
    gnodes.push_back(new gnode);
}


//-----------------------------------------------------------------------------
// Static functions

string graph::create_default_graph() {
    directory d(globals::graphs_dir);
    string graph_name = "";

    for(size_type i = 0; graph_name == ""; ++i) {
        string act_name = "refactorerl_" + to_string(i);
        if(!d.exist(act_name)) {
            graph_name = act_name;
        }
    }

    d.make_dir(graph_name);

    return graph_name;
}

bool graph::is_valid_node(const gnode* node) {
    return node != NULL;
}

string graph::read_name(const string& dir_path) {
    string full_path = dir_path + LAST_USED_GRAPH_FILE();
    string graph_name = "";
    fstream f(full_path.c_str(), ios::in);

    if(f.is_open()) {
        f >> graph_name;
    }

    f.close();
    return graph_name;
}


//-----------------------------------------------------------------------------
// Persistence operations

void graph::save(const string& path) {
    bin_file f(path, ios::out | ios::trunc);
    save(f);
}

void graph::save(bin_file& f) {
    write_atom2eatom(f);
    write_gnodes(f);
    write_protected_nodes(f);
    write_protected_links(f);
    f.write_magic_number();

    f.close();
}

void graph::load(const string& path) {
    bin_file f(path, ios::in);
    f.validate();
    load(f);
}

void graph::load(bin_file& f) {
    clear();

    read_atom2eatom__fill_eatom2atom(f);
    read_gnodes(f);
    read_protected_nodes(f);
    read_protected_links(f);

    // Construct the back links, from the forward links.
    construct_back_links();

    f.close();
}

void graph::write_name(const string& dir_path) const {
    string full_path = dir_path + LAST_USED_GRAPH_FILE();
    fstream f(full_path.c_str(), ios::out);

    if(f.is_open()) {
        f << name;
    }

    f.close();
}

void graph::unload() {
    name = "";
    clear();
}


//-----------------------------------------------------------------------------
// Graph modifying methods

gnode_id_t graph::add_node(gnode* node, const bool& is_protected) {
    gnode_id_t new_gnode_id;

    if (free_indexes.empty()) {
        new_gnode_id = gnodes.size();
        gnodes.push_back(node);
    } else {
        new_gnode_id = free_indexes.top();
        free_indexes.pop();
        gnodes[new_gnode_id] = node;
    }

    add_prot_node(new_gnode_id, is_protected);
    
    return new_gnode_id;
}

void graph::update_node(const gnode_id_t& node_id,
                        const data_t* data, const data_size_t& size) {
    check_protection(node_id);

    get_node(node_id)->update(data, size);
}

void graph::delete_node(const gnode_id_t& node_id) {
    delete_node(node_id, get_node(node_id));
}

void graph::delete_node(const gnode_id_t& node_id, gnode* node) {
    check_protection(node_id);

    node->delete_node(node_id);
    gnodes[node_id] = NULL;

    delete_prot_links(node_id);
    free_indexes.push(node_id);
}

void graph::mk_link(const gnode_id_t& node_id1, const string& tag_string,
                    const index_t& idx, const gnode_id_t& node_id2,
                    const bool& is_protected)
{
    mk_link(get_node(node_id1), node_id1, tag_string, idx,
            get_node(node_id2), node_id2, is_protected);
}

void graph::mk_link(gnode* node1, const gnode_id_t& node_id1,
                    const string& tag_string, const index_t& idx,
                    gnode* node2, const gnode_id_t& node_id2,
                    const bool& is_protected)
{
    atom_t atom = get_atom(tag_string);

    // if the nodes were linked by the same tag,
    // do not create duplicate link
    if (node1->has_fwd_link(atom, node_id2)) {
        return;
    }

    node1->add_fwd_link(atom, idx, node_id2);
    node2->add_back_link(atom, node_id1);

    add_prot_link(node_id1, atom, node_id2, is_protected);
}

void graph::rm_link(const gnode_id_t& node_id1, const string& tag_string,
                    const gnode_id_t& node_id2)
{
    rm_link(get_node(node_id1), node_id1, tag_string,
            get_node(node_id2), node_id2);
}

void graph::rm_link(gnode* node1, const gnode_id_t& node_id1,
                    const string& tag_string,
                    gnode* node2, const gnode_id_t& node_id2)
{
    atom_t atom = get_atom(tag_string);

    check_protection(node_id1, atom, node_id2);

    //if(node1->has_fwd_link(atom, node_id2)) {
        node1->rm_fwd_link(node_id1, atom, node_id2);
        node2->rm_back_link(node_id2, atom, node_id1);
    //} else {
    //   throw invalid_link("Invalid link: "  + to_string(node_id1) + ", " +
    //                       tag_string + ", " + to_string(node_id2));
    //}
}


//-------------------------------------------------------------------------
// Garbage functions

void graph::add_garbage(const gnode_id_t& node_id) {
    garbage.push_back(node_id);
}

void graph::remove_garbage() {
    while(garbage.size() > 0) {
        gnode_id_t node_id = garbage.back();
        garbage.pop_back();

        try {
            gnode* node = get_node(node_id);

            if(node->is_isolated()) {
                delete_node(node_id, node);
            }
        } catch(...) { }
    }
}


//-----------------------------------------------------------------------------
// Query functions

gnode_ids_t& graph::get_fwd_path(const gnode_id_t node_id,
                                const string& tag_string) {
    gnode* node = get_node(node_id);
    atom_t atom = get_atom(tag_string);

    return node->get_fwd_path(atom);
}

gnode_ids_t& graph::get_back_path(const gnode_id_t node_id,
                                  const string& tag_string) {
    gnode* node = get_node(node_id);
    atom_t atom = get_atom(tag_string);

    return node->get_back_path(atom);
}

simple_links_t graph::get_fwd_links(const gnode_id_t& node_id) {
    gnode* node = get_node(node_id);
    simple_links_t fwd_links;

    gnode_links_t& links = node->get_links();
    COND_CONST_ITERATE(gnode_links_t, fwds_it, links, gnode::is_fwd_link) {
        gnode_link_t fwds = *fwds_it;
        string link_name = atom2eatom[fwd_link_as_atom(fwds.first)];

        LOOP(fwds.second, i) {
            fwd_links.push_back(simple_link_t(link_name, fwds.second[i]));
        }
    }

    return fwd_links;
}

simple_links_t graph::get_back_links(const gnode_id_t& node_id) {
    gnode* node = get_node(node_id);
    simple_links_t back_links;

    gnode_links_t& links = node->get_links();
    COND_CONST_ITERATE(gnode_links_t, back_it, links, gnode::is_back_link) {
        gnode_link_t back = *back_it;
        string link_name = atom2eatom[back_link_as_atom(back.first)];

        LOOP(back.second, i) {
            back_links.push_back(simple_link_t(link_name, back.second[i]));
        }
    }

    return back_links;
}

gnode* graph::get_node(const gnode_id_t& node_id) const {
    if(node_id < gnodes.size()) {
        gnode* node = gnodes[node_id];

        if(is_valid_node(node)) {
            return node;
        }
    }

    throw invalid_node("Invalid node: " + to_string(node_id));
}


index_t graph::get_index(const gnode_id_t& node_id1, const string& tag_string,
                         const gnode_id_t& node_id2)
{
    return get_index(get_node(node_id1), tag_string, node_id2);
}

index_t graph::get_index(gnode* node1, const string& tag_string,
                         const gnode_id_t& node_id2)
{
    atom_t atom = get_atom(tag_string);

    gnode_ids_t& fwd_nodes = node1->get_fwd_path(atom);
    LOOP(fwd_nodes, i) {
        if(node_id2 == fwd_nodes[i]) {
            return i + 1;
        }
    }

    return 0;
}

string graph::get_name() const {
    return name;
}

string graph::get_path() const {
	return globals::graphs_dir + get_name() + directory::separator();
}

gnode_id_t graph::get_root_id() const {
    return 0;
}

bool graph::is_protected_node(const gnode_id_t& node_id) const {
    return contains(prot_nodes, node_id);
}

bool graph::is_protected_link(const gnode_id_t& node_id1, const string& tag,
                              const gnode_id_t& node_id2)
{
    return is_protected_link(node_id1, eatom2atom[tag], node_id2);
}

bool graph::is_protected_link(const gnode_id_t& node_id1, const atom_t& atom,
                              const gnode_id_t& node_id2)
{
    CONST_ITERATE(prot_links_t, prot_link_it, prot_links) {
        prot_link_t prot_link = *prot_link_it;
        if(prot_link.first == node_id1) {
            CONST_ITERATE(gnode_links_t, fwd_nodes_it, prot_link.second) {
                gnode_link_t fwd_nodes = *fwd_nodes_it;
                if(fwd_nodes.first == atom && contains(fwd_nodes.second, node_id2))
                {
                    return true;
                }
            }
        }
    }
    
    return false;
}


//-----------------------------------------------------------------------------
// Other functions

void graph::set_name(const string& name) {
    this->name = name;
    write_name(globals::graphs_dir);
}

void graph::clear() {
    eatom2atom.clear();
    atom2eatom.clear();
    erase_gnodes();
    prot_nodes.clear();
    prot_links.clear();
    garbage.clear();
    // priority_queue does not have clear method.
    free_indexes = free_indexes_t();

    gnodes.push_back(new gnode); // root
}

graph::~graph() {
    erase_gnodes();
}


//-----------------------------------------------------------------------------
// PRIVATE OPERATIONS
//-----------------------------------------------------------------------------


//------------------------------------------------------------------------
// Constants

string graph::LAST_USED_GRAPH_FILE() {
    return "last_used_graph";
}


//-----------------------------------------------------------------------------
// Private functions used in the save method

void graph::write_atom2eatom(bin_file& f) const {
    f.write((size_type)atom2eatom.size());

    LOOP(atom2eatom, i) {
        f.write(atom2eatom[i]);
    }
}

void graph::write_gnodes(bin_file& f) const {
    bool is_root = true; // write the root node first

    f.write((size_type)gnodes.size());

    LOOP(gnodes, i) {
        gnode* node = gnodes[i];
        if (is_valid_node(node)) {
            if(!is_root) {
                node->write_data_size(f);
                node->write_data(f);
            } else {
                is_root = false;
            }

            node->write_fwd_links(f);
            // We do not write out the back links, because these
            // can be constructed by the forward links.
        } else {
            f.write((data_size_t)0);
        }
    }
}

void graph::write_protected_nodes(bin_file& f) const {
    f.write((size_type)prot_nodes.size());

    LOOP(prot_nodes, i) {
        f.write(prot_nodes[i]);
    }
}
void graph::write_protected_links(bin_file& f) const {
    f.write((size_type)prot_links.size());

    CONST_ITERATE(prot_links_t, prot_link_it, prot_links) {
        prot_link_t prot_link = *prot_link_it;

        f.write(prot_link.first);
        f.write((size_type)prot_link.second.size());
        CONST_ITERATE(gnode_links_t, gnode_link_it, prot_link.second) {
            gnode_link_t gnode_link = *gnode_link_it;
            
            f.write(gnode_link.first);
            f.write((size_type)gnode_link.second.size());
            LOOP(gnode_link.second, i) {
                f.write(gnode_link.second[i]);
            }
        }
    }
}


//-----------------------------------------------------------------------------
// Private functions used in the load method

void graph::read_atom2eatom__fill_eatom2atom(bin_file& f) {
    size_type atom2eatom_size;

    f.read(atom2eatom_size);

    for(size_type i=0; i < atom2eatom_size; ++i) {
        string eatom;

        f.read(eatom);

        eatom2atom[eatom] = atom2eatom.size();
        atom2eatom.push_back(eatom);
    }
}

void graph::read_gnodes(bin_file& f) {
    size_type gnodes_size;
    gnode* node;
    data_size_t data_size;
    data_t* data;

    f.read(gnodes_size);

    gnodes[get_root_id()]->read_fwd_links(f); // root

    for(size_type i = 1; i < gnodes_size; ++i) {
        f.read(data_size);

        if(data_size > 0) {
            data = new data_t[data_size];
            f.read(data, data_size);
            node = new gnode(data, data_size);
            node->read_fwd_links(f);
            delete data;

            gnodes.push_back(node);
        } else {
            free_indexes.push(i);
            gnodes.push_back(NULL);
        }
    }
}

void graph::read_protected_nodes(bin_file& f) {
    size_type prot_nodes_size;
    gnode_id_t gnode_id;

    f.read(prot_nodes_size);

    for(size_type i = 0; i < prot_nodes_size; ++i) {
        f.read(gnode_id);
        
        add_prot_node(gnode_id, true);
    }
}

void graph::read_protected_links(bin_file& f) {
    size_type prot_links_size;
    size_type simple_links_size;
    size_type gnode_ids_size;
    gnode_id_t gnode_id1;
    gnode_id_t gnode_id2;
    atom_t atom;

    f.read(prot_links_size);

    for(size_type i = 0; i < prot_links_size; ++i) {
        f.read(gnode_id1);

        f.read(simple_links_size);
        for(size_type j = 0; j < simple_links_size; ++j) {
            f.read(atom);

            f.read(gnode_ids_size);
            for(size_type k = 0; k < gnode_ids_size; ++k) {
                f.read(gnode_id2);
                add_prot_link(gnode_id1, atom, gnode_id2, true);
            }
        }
    }
}

void graph::construct_back_links() {
    LOOP(gnodes, act_node_id) {
        gnode* act_node = gnodes[act_node_id];
        if(is_valid_node(act_node)) {
            gnode_links_t fwd_links = act_node->get_fwd_links();
            ITERATE(gnode_links_t, gnode_link_it, fwd_links) {
                gnode_link_t gnode_link = *gnode_link_it;
                LOOP(gnode_link.second, j) {
                    gnodes[gnode_link.second[j]]->add_back_link(fwd_link_as_atom(gnode_link.first),
                                                                act_node_id);
                }
            }
        }
    }
}


//-----------------------------------------------------------------------------
// Private protection methods

void graph::add_prot_node(const gnode_id_t& node_id, const bool& is_prot) {
    if(is_prot) {
        prot_nodes.push_back(node_id);
    }
}

void graph::add_prot_link(const gnode_id_t& node_id1, const atom_t& atom,
                          const gnode_id_t& node_id2, const bool& is_prot) {
    if(is_prot) {
        prot_links[node_id1][atom].push_back(node_id2);
    }
}

void graph::delete_prot_links(const gnode_id_t& node_id) {
    ITERATE(prot_links_t, prot_link_it, prot_links) {
        if(prot_link_it->first == node_id) {
            prot_links.erase(prot_link_it);
        } else {
            ITERATE(gnode_links_t, gnode_link_it, prot_link_it->second) {
                remove(gnode_link_it->second, node_id);
            }
        }
    }
}

void graph::check_protection(const gnode_id_t& node_id) {
    if(is_protected_node(node_id)) {
        throw protected_node("Protected node: " + to_string(node_id));
    }
}

void graph::check_protection(const gnode_id_t& node_id1, const atom_t& atom,
                             const gnode_id_t& node_id2) {
    if(is_protected_link(node_id1, atom, node_id2)) {
        throw protected_link("Protected link: " + to_string(node_id1) + ", " +
                             atom2eatom[atom] + ", " + to_string(node_id2));
    }
}


//-------------------------------------------------------------------------
// Other private functions

atom_t graph::get_atom(const string& atom_string) {
    erl_atom2atom_t::iterator elem = eatom2atom.find(atom_string);

    if(elem != eatom2atom.end()) {
        return elem->second;
    }

    atom_t atom_pos = atom2eatom.size();

    eatom2atom[atom_string] = atom_pos;
    atom2eatom.push_back(atom_string);

    return atom_pos;
}

void graph::erase_gnodes() {
    LOOP(gnodes, i) {
        gnode* node = gnodes[i];

        if(is_valid_node(node)) {
            delete node;
        }
    }
    gnodes.clear();
}
