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

#ifndef __gnode_h_
#define __gnode_h_

#include <list>
#include <set>
#include <map>
#include <string>
#include <fstream>

#ifdef __OLD_SOLARIS__
#include <inttypes.h>
#else
#include <stdint.h>
#endif

#include "bin_file.h"
#include "types.h"


/** Represents a graph node. */
class gnode
{
public:

    //-------------------------------------------------------------------------
    // Constructors

    gnode();

    /** The gnode constructor with initial parameters.
     *
     *  @param node_data Data of the node.
     *  @param data_size Size of the data, in bytes.
     */
    gnode(const data_t* node_data, const data_size_t& data_size);

    /** Copy constructor. */
    gnode(const gnode* node);


    //-------------------------------------------------------------------------
    // Node modifying methods

    /** Updates the node's data.
     *
     *  @param new_data The new data.
     *  @param new_data_size The new size of data, in bytes.
     */
    void update(const data_t* new_data, const data_size_t& new_data_size);


    //-------------------------------------------------------------------------
    // Destroy methods

    /** Destroys the node.
     *  This function provides a faster destructor.
     *
     *  @param node_id The ID of the node.
     */
    void delete_node(const gnode_id_t& node_id);


    //-------------------------------------------------------------------------
    // Link methods

    /** Adds a forward link.
     *
     *  @param atom_tag The tag of the link.
     *  @param idx The index of the link.
     *  @param node_id The id of the other node.
     */
    void add_fwd_link(const atom_t& atom_tag, const index_t& idx,
                      const gnode_id_t& node_id);

    /** Adds a backward link.
     *
     *  @param atom_tag The tag of the link.
     *  @param idx The index of the link.
     *  @param node_id The id of the other node.
     */
    void add_back_link(const atom_t& atom_tag, const gnode_id_t& node_id);

    /** Removes a forward link.
     *
     *  @param node_id1 The ID of this node.
     *  @param atom_tag The tag of the link.
     *  @param node_id2 The ID of the other node.
     */
    void rm_fwd_link(const gnode_id_t& node_id1, const atom_t& atom_tag,
                     const gnode_id_t& node_id2);

    /** Removes a backward link.
     *
     *  @param node_id1 The ID of this node.
     *  @param atom_tag The tag of the link.
     *  @param node_id The ID of the other node.
     */
    void rm_back_link(const gnode_id_t& node_id1,
                      const atom_t& atom_tag, const gnode_id_t& node_id2);

    /** Returns the node's forward links.
     *  gnode_links type can be found in types.h .
     */
    gnode_links_t get_fwd_links();

    /** Returns the node's backward links.
     *  gnode_links type can be found in types.h .
     */
    gnode_links_t get_back_links();

    /** Returns a reference to an object, that contains all the
     *  forward and back links of the node.
     */
    gnode_links_t& get_links();

    /** Checks that if this is forward linked with the given node, with
     *  the given tag.
     *
     *  @param tag The link's tag.
     *  @param node_id The ID of the node.
     *  @return If this is forward linked with the given node, then returns true,
     *          otherwise false.
     */
    bool has_fwd_link(const atom_t& tag, const gnode_id_t& node_id);

    /** Checks that if this is back linked with the given node, with
     *  the given tag.
     *
     *  @param tag The link's tag.
     *  @param node_id The ID of the node.
     *  @return If this is back linked with the given node, then returns true,
     *          otherwise false.
     */
    bool has_back_link(const atom_t& tag, const gnode_id_t& node_id);

    /** Checks that the given tag is a forward link from the node.
     *
     *  @param tag The tag of the link to examine.
     */
    static bool is_fwd_link(const atom_t& tag);

    /** Checks that the given tag is a back link from the node.
     *
     *  @param tag The tag of the link to examine.
     */
    static bool is_back_link(const atom_t& tag);


    //-------------------------------------------------------------------------
    // Node write out functions

    /** Writes out the node's data size into the given file.
     *
     *  @param f The file into which the function writes.
     */
    void write_data_size(bin_file& f) const;

    /** Writes out the node's data into the given file.
     *
     *  @param f The file into which the function writes.
     */
    void write_data(bin_file& f) const;

    /** Writes out the node's forward links into the given file.
     *
     *  @param f The file into which the function writes.
     */
    void write_fwd_links(bin_file& f) const;


    //-------------------------------------------------------------------------
    // Node read functions

    /** Reads the node's data size from the given file.
     *
     *  @param f The file from which the function reads.
     */
    void read_data_size(bin_file& f);

    /** Reads the node's data from the given file.
     *
     *  @param f The file from which the function reads.
     */
    void read_data(bin_file& f);

    /** Reads the node's forward links from the given file.
     *
     *  @param f The file from which the function reads.
     */
    void read_fwd_links(bin_file& f);


    //-------------------------------------------------------------------------
    // Query functions

    /** Returns the size of the node's data, in bytes. */
    data_size_t get_data_size() const;

    /** Returns the node's data.
     *
     *  @return An unsigned char array of the node's data.
     */
    data_t* get_data() const;

    /** Gets a reference to the list of node ids,
     *  that's in the given (forward) direction.
     *
     *  @param atom The tag that we are querying in the forward direction.
     *  @return A reference to the list of node ids
     *          that are in the given (forward) direction.
     */
    gnode_ids_t& get_fwd_path(const atom_t& atom);

    /** Gets a reference to the set of node ids,
     *  that's in the given (backward) direction.
     *
     *  @param atom The tag that we are querying backwards.
     *  @return A reference to the list of node ids,
     *          that are in the given (backward) direction.
     */
    gnode_ids_t& get_back_path(const atom_t& atom);

    /** Checks that the node is isolated.
     *
     *  @return Returns true if the node is isolated.
     */
    bool is_isolated();


    //-------------------------------------------------------------------------
    // Other functions

    ~gnode();

private:

    //-------------------------------------------------------------------------
    // Properties

    /** The node's data. */
    data_t* data;
    /** The size of data. */
    data_size_t size;

    /** Contains the back and forward links. */
    gnode_links_t links;


    //-------------------------------------------------------------------------
    // Delete functions

    /** Deletes the node's (that belongs to node_id) all links.
     *
     *  @param node_id The ID of the node.
     */
    void delete_all_links(const gnode_id_t& node_id);

    /** Deletes the node's (that belongs to node_id) forward links.
     *
     *  @param node_id The ID of the node.
     */
    void delete_fwd_links(const gnode_id_t& node_id);

    /** Deletes the node's (that belongs to node_id) backward links.
     *
     *  @param node_id The ID of the node.
     */
    void delete_back_links(const gnode_id_t& node_id);

    /** Deletes the node's data. */
    void delete_data();

    /** Used in the rm_link methods.
     *
     *  @param node_id1 The ID of this node.
     *  @param atom_tag The link's atom.
     *  @param node_id2 The ID of the other node.
     */
    void rm_link(const gnode_id_t& node_id1, const atom_t atom_tag,
                 const gnode_id_t& node_id2);

    //-------------------------------------------------------------------------
    // Other private functions

    /** Used in constructors.
     *
     *  @param node_data The node's data.
     *  @param data_size The size of data.
     */
    void init(const data_t* node_data, const data_size_t& data_size);

    /** Returns the number of forward links. */
    size_type fwd_links_size() const;

    /** returns the number of back links. */
    size_type back_links_size() const;

    /** Fills the node with data.
     *
     *  @param new_data New data of the node.
     *  @param new_data_size Size of the new data, in bytes.
     */
    void fill_node(const data_t* new_data, const data_size_t& new_data_size);

    /** Checks that this node is a lexical node.
     *
     *  @return Returns true, if the node is a lexical node,
     *          otherwise false.
     */
    bool is_lex();

    /** Sets the default values for constructors. */
    void set_default_values();

};

#endif
