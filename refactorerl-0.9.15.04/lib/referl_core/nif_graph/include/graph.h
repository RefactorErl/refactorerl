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

#ifndef __graph_h_
#define __graph_h_

#include <iostream>
#include <vector>
#include <string>

#include "macros.h"
#include "gnode.h"
#include "bin_file.h"
#include "types.h"


/** Represents the graph */
class graph
{
public:

    //-------------------------------------------------------------------------
    // Constructors

    /** Constructor:
     *  Sets the name of the graph.
     *
     *  @param graph_name The name of the graph.
     */
    graph(const std::string& graph_name);


    //-------------------------------------------------------------------------
    // Static functions

    /** Creates the default "refactorerl" graph.
     *
     *  @return If the default graph has been created, then
     *          returns its name ("refactorerl"),
     *          otherwise returns "".
     */
     static std::string create_default_graph();

    /** Checks that the given node object is valid.
     *
     *  @param node Pointer to the node that needs to be checked.
     *  @return If the node object is valid, then returns true,
     *          otherwise false.
     */
    static bool is_valid_node(const gnode* node);

    /** Reads the graph's name from the specified directory's path,
     *  from a text file, which name is 'last_used_graph'.
     *
     *  @param path The path of the directory where the text file is.
     *  @return The name, read from the file.
     */
    static std::string read_name(const std::string& path);

    
    //-------------------------------------------------------------------------
    // Persistence operations

    /** Saves the graph to the given absolute path.
     *
     *  @param path The absolute path, where to save the graph.
     */
    void save(const std::string& path);

    /** Saves the graph to the given binary file.
     *
     * @param f The binary file where to save the graph.
     */
    void save(bin_file& f);

    /** Loads the graph from the given absolute path.
     *
     *  @param path The absolute path from where the graph is loaded.
     */
    void load(const std::string& path);

    /** Loads the graph from the given binary file.
     *
     *  @param f The binary file from where the graph is loaded.
     */
    void load(bin_file& f);

    /** Writes the graph's name to the specified directory's path,
     *  to a text file, which name is 'last_used_graph'.
     *
     *  @param path The path of the directory where to put the text file.
     */
    void write_name(const std::string& path) const;

    /** Unloads (actually clears) the graph. */
    void unload();


    //-------------------------------------------------------------------------
    // Graph modifying methods

    /** Inserts the node at the first free index.
     *  Returns the ID that the node receives.
     *
     *  @param node The node object to be added.
     *  @param is_protected Protected node or not. Default: false.
     *  @return The node's ID in the graph.
     */
    gnode_id_t add_node(gnode* node, const bool& is_protected = false);

    /** Updates the given node with the given data.
     *
     *  @param node_id The node's ID to be modified.
     *  @param data The new data.
     *  @param size The new data's size.
     *
     *  @exception invalid_node
     *  @exception protected_node
     */
    void update_node(const gnode_id_t& node_id,
                     const data_t* data, const data_size_t& size);

    /** Deletes a node.
     *
     *  @param node_id The node's ID, that we want to delete.
     *
     *  @exception invalid_node
     *  @exception protected_node
     */
    void delete_node(const gnode_id_t& node_id);

    /** Deletes a node.
     *  This function is faster, than the other delete_node function.
     *
     *  @param node_id The node's ID, that we want to delete.
     *  @param node The node's object pointer.
     *
     *  @exception invalid_node
     *  @exception protected_node
     */
    void delete_node(const gnode_id_t& node_id, gnode* node);

    /** Make a link between two nodes.
     *
     *  @param node1 Pointer to the first node.
     *  @param node_id1 The first node's ID.
     *  @param tag_string Tag of the link.
     *  @param idx Index of the link.
     *  @param node2 Pointer to the other node.
     *  @param node_id2 The other node's ID.
     *  @param prot Protected link or not. Default: false.
     */
    void mk_link(gnode* node1, const gnode_id_t& node_id1,
                 const std::string& tag_string, const index_t& idx,
                 gnode* node2, const gnode_id_t& node_id2,
                 const bool& prot = false);

    /** Make a link between two nodes.
     *
     *  @param node_id1 The first node's ID.
     *  @param tag_string Tag of the link.
     *  @param idx Index of the link.
     *  @param node_id2 The other node's ID.
     *  @param prot Protected link or not. Default: false.
     *
     *  @exception invalid_node
     */
    void mk_link(const gnode_id_t& node_id1, const std::string& tag_string,
                 const index_t& idx, const gnode_id_t& node_id2,
                 const bool& prot = false);

    /** Removes a link between two nodes.
     *
     *  @param node1 Pointer to the first node.
     *  @param node_id1 The first node's ID.
     *  @param tag_string Tag of the link.
     *  @param node2 Pointer to the other node.
     *  @param node_id2 The other node's ID.
     *
     *  @exception protected_link
     */
    void rm_link(gnode* node1, const gnode_id_t& node_id1,
                 const std::string& tag_string,
                 gnode* node2, const gnode_id_t& node_id2);

    /** Removes a link between two nodes.
     *
     *  @param node_id1 The first node's ID.
     *  @param tag_string Tag of the link.
     *  @param node_id2 The other node's ID.
     *
     *  @exception invalid_node
     *  @exception protected_link
     */
    void rm_link(const gnode_id_t& node_id1, const std::string& tag_string,
                 const gnode_id_t& node_id2);


    //-------------------------------------------------------------------------
    // Garbage functions

    /** Adds a node to the garbage container.
     *  That container contains isolated nodes, which will be deleted
     *  after calling the refcore_esg:finalize function, if that nodes
     *  are isolated at that time.
     *
     *  @param node_id The ID of the garbage node.
     */
    void add_garbage(const gnode_id_t& node_id);

    /** Removes all garbage nodes.
     *  A node is considered as garbage if it's isolated.
     *  Note that function can delete nodes that were not added by add_garbage
     *  function (for instance: isolated subgraph).
     */
    void remove_garbage();


    //-------------------------------------------------------------------------
    // Query functions

    /** Returns a reference to the list of gnode IDs, which are connected
     *  with the given node and with the given tag.
     *
     *  @param node_id The ID of the node.
     *  @param The tag of the forward link.
     *  @return Reference to a list of gnode's IDs.
     *
     *  @exception invalid_node
     */
    gnode_ids_t& get_fwd_path(const gnode_id_t node_id,
                              const std::string& tag_string);

    /** Returns a reference to the list of gnode's IDs, which are connected
     *  with the given node and with the given tag.
     *
     *  @param node_id The ID of the node.
     *  @param tag_string The tag of the back link.
     *  @return Reference to a list of gnode's IDs.
     *
     *  @exception invalid_node
     */
    gnode_ids_t& get_back_path(const gnode_id_t node_id,
                               const std::string& tag_string);

    /** Returns all forward links of the given node.
     *
     *  @param node_id The ID of the node.
     *  @return List of simple_links.
     *
     *  @exception invalid_node
     */
    simple_links_t get_fwd_links(const gnode_id_t& node_id);
    
    /** Returns all back links of the given node.
     *
     *  @param node_id The ID of the node.
     *  @return List of simple_links.
     *
     *  @exception invalid_node
     */
    simple_links_t get_back_links(const gnode_id_t& node_id);

    /** Returns the node object's pointer which belongs to the given ID.
     *
     *  @param gnode_id ID of the node.
     *  @return The node object pointer.
     *
     *  @exception invalid_node
     */
    gnode* get_node(const gnode_id_t& node_id) const;

    /** Returns the index of a link.
     *
     *  @param node_id1 The first node's ID.
     *  @param tag_string The link's tag.
     *  @param node_id2 The other node's ID.
     *  @return The index of the given link.
     *
     *  @exception invalid_node
     */
    index_t get_index(const gnode_id_t& node_id1, const std::string& tag_string,
                      const gnode_id_t& node_id2);

    /** Returns the index of a link.
     *  It's faster then the other get_index function.
     *
     *  @param node1 Pointer to the first node.
     *  @param tag_string The link's tag.
     *  @param node2 Pointer to the other node.
     *  @return The index of the given link.
     */
    index_t get_index(gnode* node1, const std::string& tag_string,
                      const gnode_id_t& node_id2);

    /** Returns the name of the graph. */
    std::string get_name() const;

    /** Gets the absolute path of the graph, where the backups are stored.
     *
     *  @return The absolute path of the graph.
     */
    std::string get_path() const;

    /** Returns the ID of the root. */
    gnode_id_t get_root_id() const;

    /** Checks that the given node is protected.
     *
     * @param node_id The ID of the node.
     * @return Retruns true, if the node is protected, otherwise false.
     *
     * @exception invalid_node
     */
    bool is_protected_node(const gnode_id_t& node_id) const;

    /** Checks that the given link is protected.
     *
     *  @param node_id1 The ID of the first node.
     *  @param tag The link's tag.
     *  @param node_id2 The ID of the other node.
     *  @return Returns true, if the link is protected, otherwise false.
     */
    bool is_protected_link(const gnode_id_t& node_id1, const std::string& tag,
                           const gnode_id_t& node_id2);
    bool is_protected_link(const gnode_id_t& node_id1, const atom_t& atom,
                           const gnode_id_t& node_id2);


    //-------------------------------------------------------------------------
    // Other functions

    /** Sets the name of the graph. */
    void set_name(const std::string& name);

    /** Erases all nodes and links. */
    void clear();

    ~graph();


    //-------------------------------------------------------------------------
    // Exceptions

    /** Thrown when somebody wants to operate with a node,
     *  that does not exist.
     */
    DEFINE_EXCEPTION(invalid_node, "Invalid node")

    /** Thrown when somebody wants to remove a link,
     *  that does not exist.
     */
    DEFINE_EXCEPTION(invalid_link, "Invalid link")

    /** Thrown when somebody wants to delete or modify a protected node. */
    DEFINE_EXCEPTION(protected_node, "Protected node")

    /** Thrown when somebody wants to remove a protected link. */
    DEFINE_EXCEPTION(protected_link, "Protected link")

private:

    //-------------------------------------------------------------------------
    // Properties

    /** The name of the graph. */
    std::string name;

    /** Conversion tables between
     *  known atom names (strings) and atom ids (ints).
     */
    erl_atom2atom_t eatom2atom;
    atom2erl_atom_t atom2eatom;

    /** The graph consists of gnode objects. */
    gnodes_t gnodes;

    /** Stores protected nodes. */
    prot_nodes_t prot_nodes;
    /** Stores protected links. */
    prot_links_t prot_links;

    /** The IDs of nodes that are not in use in the gnodes vector. */
    free_indexes_t free_indexes;

    /** Stores the ID of isolated gnodes, which will be deleted later,
     *  if then the gnode is still isolated. */
    garbage_container_t garbage;


    //------------------------------------------------------------------------
    // Constants

    static std::string LAST_USED_GRAPH_FILE();


    //-------------------------------------------------------------------------
    // Private functions used in the save method

    void write_atom2eatom(bin_file& f) const;
    void write_gnodes(bin_file& f) const;
    void write_protected_nodes(bin_file& f) const;
    void write_protected_links(bin_file& f) const;

    
    //-------------------------------------------------------------------------
    // Private functions used in the load method

    /** Reads the values to the atom2eatom container,
     *  and meanwhile fills the eatom2atom container also.
     */
    void read_atom2eatom__fill_eatom2atom(bin_file& f);
    void read_gnodes(bin_file& f);
    void read_protected_nodes(bin_file& f);
    void read_protected_links(bin_file& f);

    /** Constructs the back links, from the loaded forward links. */
    void construct_back_links();


    //-------------------------------------------------------------------------
    // Private protection methods

    /** Adds a new protected node to prot_nodes container.
     *
     *  @param node_id The ID of the node.
     *  @param is_prot The node will be protected or not.
     */
    void add_prot_node(const gnode_id_t& node_id, const bool& is_prot);

    /** Adds a new protected link to prot_links container.
     *
     *  @param node_id1 The ID of the first node.
     *  @param atom The link's atom.
     *  @param node_id2 The ID of the other node.
     *  @param is_prot The link will be protected or not.
     */
    void add_prot_link(const gnode_id_t& node_id1, const atom_t& atom,
                       const gnode_id_t& node_id2, const bool& is_prot);

    /** Deletes the node's (that belongs to node_id) protected links.
     *
     *  @param node_id The ID of the node.
     */
    void delete_prot_links(const gnode_id_t& node_id);

    /** Checks that the given node is protected, and if it's not
     *  then throws a protected_node exception.
     *
     *  @param node_id The ID of the node.
     *
     *  @exception protected_node
     */
    void check_protection(const gnode_id_t& node_id);

    /** Checks that the given link is protected, and if it's not
     *  then throws a protected_link exception.
     *
     *  @param node_id1 The ID of the first node.
     *  @param atom The link's atom.
     *  @param node_id2 The ID of the other node.
     *  @exception protected_link
     */
    void check_protection(const gnode_id_t& node_id1, const atom_t& atom,
                          const gnode_id_t& node_id2);

    
    //-------------------------------------------------------------------------
    // Other private functions

    /** Adds the given atom to the atom_idx map
     *  (if the atom is not in the map), and returns it's number.
     *
     *  @param atom_string The string of the atom.
     *  @return The number of the atom.
     */
    atom_t get_atom(const std::string& atom_string);
    
    /** Erases all nodes from gnodes container. */
    void erase_gnodes();

};

#endif
