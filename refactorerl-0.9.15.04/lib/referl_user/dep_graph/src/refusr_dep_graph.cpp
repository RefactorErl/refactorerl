/// -*- coding: latin-1 -*-

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

/// ============================================================================

/// @doc Dependency examination
///
/// @author Barbara Oláh <olahb@caesar.elte.hu>
/// @author Dávid Mikó <dav3333333@caesar.elte.hu>

#include <lemon/connectivity.h>
#include <lemon/list_graph.h>
#include <lemon/core.h>
#include <lemon/lgf_writer.h>
#include <lemon/lgf_reader.h>
#include <lemon/adaptors.h>
 
#include <iostream>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <vector>
#include <map>
#include <stack>
#include <thread>
#include <mutex>

#include "dep_graph.h"

using namespace lemon;
using namespace std;

// Types
typedef ListDigraph::Node Node;
typedef ListDigraph::Arc Arc;
typedef ListDigraph::NodeMap<bool> FilterType;

// Mutex
mutex mtx, mtx2, mtx3;

// The graph
ListDigraph g;

// Node and arc labels
ListDigraph::NodeMap<int> NodesId(g);
ListDigraph::NodeMap<string> NodesType(g); // Node's Type: module, func
ListDigraph::NodeMap<string> NodesName(g); // Node's Name fe: lists, erlang
ListDigraph::NodeMap<string> NodesConType(g); // Connection Node Type
ListDigraph::ArcMap<string> ArcType(g); // Arc's Type: funcall, ambcall, 
                                        // dyncall, contain, modcall
                                        // icallarc, pcallarc
ListDigraph::ArcMap<int> ArcMultipl(g); // Arc's Multiplicity
ListDigraph::NodeMap< vector <int> > ModulBlockId(g);
ListDigraph::ArcMap<string> ArcLabel(g);

ListDigraph::ArcMap<string> ConnectionProps(g);
ListDigraph::NodeMap<bool> IsArcBetCon(g);
ListDigraph::NodeMap<bool> IsVisitedCon(g);
ListDigraph::NodeMap<int> ParentNodeCon(g);

ListDigraph::NodeMap<map<Node, vector<vector<int> > > > NodePath(g);
vector<Node> CNodes;

// The moduleblock graph and its labels
ListDigraph mbGraph;
ListDigraph::NodeMap<int> GroupId(mbGraph);
ListDigraph::NodeMap<string> GroupLabel(mbGraph);
ListDigraph::ArcMap<int> ArcMp(mbGraph);
ListDigraph::ArcMap<bool> AcyclicArc(mbGraph);
ListDigraph::NodeMap< vector <int> > GroupElements(mbGraph);
ListDigraph::NodeMap<string> GroupElementsStr(mbGraph);

// Cyclic filters
ListDigraph::ArcMap<bool>  typeArcFilter(g, false);
ListDigraph::ArcMap<bool>  acyclicArcs(g, false);

// Filter variables for graph filters
FilterType nodeAllFilter(g, false);
FilterType stNodeFilter(g, false);
FilterType cyclicNodeFilter(g, false);
FilterType excludeFilter(g);
FilterType connectionFilter(g);
FilterType connectionBeforeFilter(g, false);
FilterType cyclicNodeFilterMB(mbGraph);

// Helper functions
Node find_node(const string type, const int id);
Node find_node(const int id);
Node find_node_by_name(const string type, const string name);
Node find_parent_node(const Node child);
string number_to_string(int number);
bool is_match_regexp(char regexp[1024], char expr[1024]);

// Helper functions for connection filter
void set_conmaps_values();
int select_cnodes(const ERL_NIF_TERM from,ErlNifEnv* env, const string level);
void path_traversal(Node s, vector<int> &Path, const int ctype);
void start_path_traversal(int ctype, int maxdepth);
void connect_path(Node s, Node n, map<Arc, string>* labels, vector<int> &Path,
					ListDigraph::NodeMap<bool> &IsDone, int depth, const int maxdepth, const int ctype);
void add_conarcs(Node* node, map<Arc, string>* labels, const int maxdepth, const int ctype);
void thread_man(stack<Node>* s, const int maxdepth, const int ctype);

// Functions for read Erlang paramaters
int read_param_id(vector<string> &nodeTypes, vector<int> &nodeIds, 
                    const ERL_NIF_TERM from, ErlNifEnv* env);
int read_param_name(vector<string> &names, const ERL_NIF_TERM from, 
                    ErlNifEnv* env);
string read_graph_level(const ERL_NIF_TERM from, ErlNifEnv* env);

// Helper functions for filter the graph
void include_nodes(Node n, FilterType &nodeFilter, const string level);
void exclude_nodes(Node n, FilterType &nodeFilter, const string level);
void include_parent(Node n, FilterType &nodeFilter);
void call_arc_filter(string type);
void erase_selected_nodes(const FilterType &nodeFilter);
void erase_isolated_modules();

// The filtering functions
void filter_level(const string graphLevel);
int filter_exclude_children(const ERL_NIF_TERM from, ErlNifEnv* env, const string level);
int filter_exclude_libraries(const ERL_NIF_TERM from, ErlNifEnv* env, const string level);
int filter_exclude_nodes(const ERL_NIF_TERM from, ErlNifEnv* env, const string level);
int filter_exclude_otp(const ERL_NIF_TERM from, ErlNifEnv* env);
int filter_starting_nodes(const ERL_NIF_TERM from, ErlNifEnv* env, const string level);
int filter_cyclic(const ERL_NIF_TERM from, ErlNifEnv* env, 
                    const string graphLevel);
int filter_cyclic_mb(const ERL_NIF_TERM from, ErlNifEnv* env);
int filter_connection(const int ctype, const int maxdepth);

// Functions for modulblock graph
void add_modulblock_nodes(const ERL_NIF_TERM from, ErlNifEnv* env);
void add_modulblock_arcs();
void convert_group_elements_to_str();
void convert_group_elements_from_str();

// Functions for write graph to dot
string module_node(const string label, const string tooltip);
string func_node(const string label, const string tooltip);
string path_node(const string label, const string tooltip);
string con_node(const string label, const string tooltip);
string simple_call_arc(const string type, const string label);
string cyclic_call_arc(const string type, const string label);
string simple_dyncall_arc(const string type, const string label);
string contain_arc(const string label);
string simple_pcall_arc(const string label);
string cyclic_pcall_arc(const string label);
string simple_icall_arc(const string label);
string cyclic_icall_arc(const string label);
string make_tooltip(const string type, const int id);

// Graph output functions
void graph_to_terms_name_func(ofstream &OutFile);
void graph_to_terms_name_module(ofstream &OutFile);
void graph_to_terms_name_mb(ofstream &OutFile, ofstream &OutMBFile);
void graph_to_terms_fun_or_mod(ofstream &OutFile, const string graphLevel);
void graph_to_terms_mb(ofstream &OutFile, ofstream &OutMBFile);
void graph_to_dot_func(ofstream &OutDotFile);
void graph_to_dot_comp(const string graphLevel, ofstream &OutDotFile);
void graph_to_dot_comp_mb(ofstream &OutDotFile);
void graph_to_dot_mb(ofstream &OutDotFile);
void graph_to_dot_simple(const string graphLevel, ofstream &OutDotFile);
void graph_to_terms_two_func(ofstream &OutFile);
void graph_to_terms_two_module(ofstream &OutFile);
void graph_to_terms_two_mb(ofstream &OutFile);

// NIF functions
extern int add_vertex(char* type, int id, char* name);
extern int add_edge(char* sourceType, int sourceId, char* targetType, 
                    int targetId, char* type);
extern int new_graph();
extern int new_mb_graph();
extern int delete_mbgraph();
extern int write_graph(char* filename);
extern int read_graph(char* filename);
extern int write_mb_graph(char* filename);
extern int read_mb_graph(char* filename);
extern int filter_graph(char* type, char* cyclic, int excO, int excN, 
                        int excL, int excLi, int stN, int groups, 
                        int conn, int conntype, int maxdepth);
extern int graph_to_dot(char* type,char* filename, char* dotpar, char* dottype);
extern int graph_to_terms(char* type, char* filename);
extern int graph_to_terms_name(char* type, char* filename);
extern int graph_to_terms_two(char* type, char* filename);

//  ============================================================================
//  Build graph
//  ============================================================================

// Add a node to the graph
static ERL_NIF_TERM add_vertex_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameters
    int id;             // node's id
    char type[256];     // node's type
    char name[256];     //nodes's name
    
    // read node's type
    if (enif_get_atom(env, argv[0], (char*)type, 
        sizeof(type), ERL_NIF_LATIN1) == 0 
            || ((string)type != "func" && (string)type != "module")) 
            return enif_make_badarg(env);
    
    // read node's id
    if (!enif_get_int(env, argv[1], &id) || id < 0) 
            return enif_make_badarg(env);
            
    // read node's name
    if (enif_get_atom(env, argv[2], (char*)name, 
        sizeof(name), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // add the node
    Node n = g.addNode();
    
    // add value to the labels
    NodesId[n] = id;
    NodesType[n] = type;
    NodesName[n] = name;
    
    return enif_make_int(env, 0);
}

// Add an arc to the graph
static ERL_NIF_TERM add_edge_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameters
    char sourceType[256];   // source node's type
    int sourceId;           // source node's id
    char targetType[256];   // target node's type
    int targetId;           // target node's id
    char type[256];         // arc's type
    
    // read source node's type
    if (enif_get_atom(env, argv[0], (char*)sourceType, 
        sizeof(sourceType), ERL_NIF_LATIN1) == 0
            || ((string)sourceType != "func" && (string)sourceType != "module")) 
            return enif_make_badarg(env);
    
    // read source node's id
    if (!enif_get_int(env, argv[1], &sourceId) || sourceId < 0) 
            return enif_make_badarg(env);

    // read target node's type            
    if (enif_get_atom(env, argv[2], (char*)targetType, 
        sizeof(targetType), ERL_NIF_LATIN1) == 0
            || ((string)targetType != "func" && (string)targetType != "module")) 
            return enif_make_badarg(env);
    // read target node's id
    if (!enif_get_int(env, argv[3], &targetId) || targetId < 0) 
            return enif_make_badarg(env);
            
    // read arc's type
    if (enif_get_atom(env, argv[4], (char*)type, 
        sizeof(type), ERL_NIF_LATIN1) == 0 || ((string)type != "funcall" 
            && (string)type != "dyncall" && (string)type != "ambcall"
            && (string)type != "modcall" && (string)type != "contain") ) 
            return enif_make_badarg(env);
    
    // find the source and the target node in the graph
    Node source = find_node(sourceType, sourceId);
    Node target = find_node(targetType, targetId);
    
    // add the arc
    if(source != INVALID && target != INVALID){
        if((string)type == "modcall"){
            Arc a = findArc(g, source, target, INVALID);
            if(a != INVALID)
                ArcMultipl[a]++;
            else{
                a = g.addArc(source, target);
                ArcType[a] = type;
                ArcMultipl[a] = 1;
                ArcLabel[a] = "";
            }
        }
        else{
            Arc a = g.addArc(source, target);
            ArcType[a] = type;
            ArcMultipl[a] = 0;
            ArcLabel[a] = "";
        }
    }
    
    return enif_make_int(env, 0);
}

// Create new graph
static ERL_NIF_TERM new_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // If the graph is not empty, we drop all the nodes of the graph,
    // then it will be empty
    ListDigraph::NodeIt n(g);
    while(n != INVALID){
        Node u = n;    
        ++n;
        g.erase(u);
    }
    
    return enif_make_int(env, 0);
}

// Create new modulblock graph
static ERL_NIF_TERM new_mb_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // If the graph is not empty, we drop all the nodes of the graph,
    // then it will be empty
    ListDigraph::NodeIt n(mbGraph);
    while(n != INVALID){
        Node u = n;    
        ++n;
        mbGraph.erase(u);
    }

    return enif_make_int(env, 0);
}

// Delete Modulblock Graph
static ERL_NIF_TERM delete_mbgraph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Delete the modulblock graph's nodes
    ListDigraph::NodeIt v(mbGraph);
    while(v != INVALID){
        Node u = v;    
        ++v;
        mbGraph.erase(u);
    }
    
    return enif_make_int(env, 0);
}

// Find a node in the graph from its type and id
Node find_node(const string type, const int id)
{
    ListDigraph::NodeIt n(g);
    while( n != INVALID && !(NodesId[n] == id && NodesType[n] == type) )
        ++n;
    return n;
}

// Find a node in the modulblock graph from its id
Node find_node(const int id)
{
    ListDigraph::NodeIt n(mbGraph);
    while(n != INVALID && GroupId[n] != id)
        ++n;
    return n;
}

// Find a node in the graph from its type and name
Node find_node_by_name(const string type, const string name)
{
    ListDigraph::NodeIt n(g);
    while( n != INVALID && !(NodesName[n] == name && NodesType[n] == type) )
        ++n;
    return n;
}

// Find a node's parent int the graph
Node find_parent_node(const Node child)
{
    ListDigraph::InArcIt a(g,child);
    while( a != INVALID && ArcType[a] != "contain")
        ++a;
    if(a != INVALID) return g.source(a);
    else return INVALID;
}
//  ============================================================================
//  Write graph to lgf file
//  ============================================================================

// Write the dependency graph to lgf file
static ERL_NIF_TERM write_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameter
    char file[1024];
    if (enif_get_string(env, argv[0], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // write the graph to file
    digraphWriter(g,(string)file).
        nodeMap("type", NodesType).
        nodeMap("id", NodesId).
        nodeMap("name", NodesName).
        arcMap("type", ArcType).
        arcMap("multipl", ArcMultipl).
        arcMap("cyclic", acyclicArcs).
        arcMap("arclabel", ArcLabel).
        run();  
        
    return enif_make_int(env, 0);
}

// Write the modulblock dependency graph to lgf file
static ERL_NIF_TERM write_mb_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameter
    char file[1024];
    if (enif_get_string(env, argv[0], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    convert_group_elements_to_str();
    
    // write the graph to file
    digraphWriter(mbGraph,(string)file).
        nodeMap("label", GroupLabel).
        nodeMap("id", GroupId).
        nodeMap("elements", GroupElementsStr).
        arcMap("multipl", ArcMp).
        arcMap("cyclic", AcyclicArc).
        run();  
        
    return enif_make_int(env, 0);
}

// Convert group elements to a string 
// (lgf can use only basic type maps to write out)
void convert_group_elements_to_str()
{
    for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n)
    {
        string elements = "";
        for(int i = 0; i < (int)GroupElements[n].size(); ++i)
        {
            elements += number_to_string(GroupElements[n][i]) + (string)" ";
        }
        GroupElementsStr[n] = elements;
    }
}

//  ============================================================================
//  Read graph from lgf file
//  ============================================================================

// Read dependency graph from lgf file
static ERL_NIF_TERM read_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameter
    char file[1024];
    if (enif_get_string(env, argv[0], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // read the graph
    digraphReader(g,(string)file).
        nodeMap("type", NodesType).
        nodeMap("id", NodesId).
        nodeMap("name", NodesName).
        arcMap("type", ArcType).
        arcMap("multipl", ArcMultipl).
        arcMap("cyclic", acyclicArcs).
        arcMap("arclabel", ArcLabel).
        run();  
        
    return enif_make_int(env, 0);
}

// Read modulblock graph from lgf file
static ERL_NIF_TERM read_mb_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameter
    char file[1024];
    if (enif_get_string(env, argv[0], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // read the graph
    digraphReader(mbGraph,(string)file).
        nodeMap("label", GroupLabel).
        nodeMap("id", GroupId).
        nodeMap("elements", GroupElementsStr).
        arcMap("multipl", ArcMp).
        arcMap("cyclic", AcyclicArc).
        run(); 
        
    convert_group_elements_from_str();
    
    return enif_make_int(env, 0);
}

// Make group elements from a string
// (lgf can use only basic type maps to read in) 
void convert_group_elements_from_str()
{
    for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n)
    {
        istringstream iss(GroupElementsStr[n]);
        vector<string> tokens;
        copy(istream_iterator<string>(iss),
            istream_iterator<string>(),
            back_inserter<vector<string> >(tokens));
            
        for(int i = 0; i < (int)tokens.size(); ++i)
        {
            GroupElements[n].push_back(atoi(tokens[i].c_str()));
        }
    }
}

//  ============================================================================
//  Filter graph
//  ============================================================================

// The graph will include the 'n' node and its children
void include_nodes(Node n, FilterType &nodeFilter, const string level)
{
    if(n != INVALID && !nodeFilter[n]){
        nodeFilter[n] = true;
        include_parent(n, nodeFilter);
        for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a){
			if((string)level != "func" || ArcType[a] != "modcall")
            	include_nodes(g.target(a),nodeFilter,level);
        }
    }
}

// The graph will exclude the 'n' node and its children
void exclude_nodes(Node n, FilterType &nodeFilter, const string level)
{
    if(n != INVALID && nodeFilter[n]){
        nodeFilter[n] = false;
        for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a){
			if((string)level != "func" || ArcType[a] != "modcall")
            	exclude_nodes(g.target(a),nodeFilter, level);
        }
    }
}

// The graph will include the 'n' node's parent
void include_parent(Node n, FilterType &nodeFilter)
{
    if(n != INVALID)
        for(ListDigraph::InArcIt a(g,n); a!= INVALID; ++a)
            if(ArcType[a] == "contain") {
                nodeFilter[g.source(a)] = true;
                include_parent(g.source(a),nodeFilter);
            }
}

// Select a group of arcs from the graph by type
void call_arc_filter(string type)
{
    // select modcall arcs
    if(type == "modcall"){
        for(ListDigraph::ArcIt a(g); a != INVALID; ++a)
            if(ArcType[a] == type || ArcType[a] == "icallarc" 
                || ArcType[a] == "callarc") 
                    typeArcFilter[a] = true;
    }
    // select funcall, dyncall, ambcall arcs 
    else{
        for(ListDigraph::ArcIt a(g); a != INVALID; ++a)
            if(ArcType[a] == "funcall" || ArcType[a] == "dyncall" 
                || ArcType[a] == "ambcall" || ArcType[a] == "callarc"
                    || ArcType[a] == "icallarc") 
                    typeArcFilter[a] = true;
    }
}

// Read an Erlang list, which contains (type,id) pairs
int read_param_id(vector<string> &nodeTypes, vector<int> &nodeIds, 
                    const ERL_NIF_TERM from, ErlNifEnv* env)
{
    ERL_NIF_TERM head, tail, list; // Erlang list type's variables
    char nodeType[256];
    int nodeId;
    list = from;
    while (enif_get_list_cell(env, list, &head, &tail))
    {
        // read a node's type
        if (enif_get_atom(env, head, (char*)nodeType, 
            sizeof(nodeType), ERL_NIF_LATIN1) == 0 
            || ((string)nodeType != "func" && (string)nodeType != "module")) 
                return 1;
        list = tail;
        
        // read a node's id
        if (enif_get_list_cell(env, list, &head, &tail))
            if (!enif_get_int(env, head, &nodeId) || nodeId < 0) 
                return 1;
        list = tail;
        
		if(find(nodeIds.begin(), nodeIds.end(), nodeId) != nodeIds.end()) continue;

        // put the read parameters in vectors
        nodeTypes.push_back(nodeType);
        nodeIds.push_back(nodeId);
    }
    return 0;
}

// Read an Erlang list, which contains atoms
int read_param_name(vector<string> &names, const ERL_NIF_TERM from, 
                    ErlNifEnv* env)
{
    ERL_NIF_TERM head, tail, list; // Erlang list type's variables
    char name[256];
    list = from;
    while (enif_get_list_cell(env, list, &head, &tail))
    {
        // read a node's name
        if (enif_get_atom(env, head, (char*)name, 
            sizeof(name), ERL_NIF_LATIN1) == 0) 
                return 1;
        list = tail;
        
        // put the node's name in the vector
        names.push_back(name);
    }
    return 0;
}

// Read the level of the graph
string read_graph_level(const ERL_NIF_TERM from, ErlNifEnv* env)
{
    // graphLevel can be: module | func | mb
    char graphLevel[256];
    if (enif_get_atom(env, from, (char*)graphLevel, 
        sizeof(graphLevel), ERL_NIF_LATIN1) == 0) 
            return "badarg";
    if((string)graphLevel != "module" && (string)graphLevel != "func"
        && (string)graphLevel != "mb") 
        return "badarg";
    return graphLevel;
}

// Erase the selected nodes from the graph
void erase_selected_nodes(const FilterType &nodeFilter)
{
    ListDigraph::NodeIt n(g);
    while(n!= INVALID)
    {
        if(!nodeFilter[n]){
            Node u = n;
			CNodes.erase(remove(CNodes.begin(), CNodes.end(), u), CNodes.end());
            ++n;
            g.erase(u);
        }
        else ++n;
    } 
}

// Erase modules with no children
void erase_isolated_modules()
{
	ListDigraph::NodeIt n(g);
    while(n != INVALID)
	{
		if(NodesType[n] == "module")
		{
			bool iso = true;
			for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a)
			{
				if(ArcType[a] == "contain")
				{
					iso = false;
					break;
				}
			}
			if(iso)
			{
				Node u = n;
				++n;
				g.erase(u);
			}
			else ++n;
		}
		else ++n;
	}
}

// Filter the graph by level
void filter_level(const string graphLevel)
{
    if((string)graphLevel == "module" || (string)graphLevel == "mb"){
    ListDigraph::NodeIt n(g);
    while(n != INVALID)
        if(NodesType[n] == "func"){ 
            Node u = n;
            ++n;
            g.erase(u);
        }
        else {
            ++n;
        }
    }
}

// Add nodes to the modulblock graph
void add_modulblock_nodes(const ERL_NIF_TERM from, ErlNifEnv* env)
{
    vector<string> groups;
    
    // read the modul groups
    int groupId = 1;
    string elements = "";
    ERL_NIF_TERM head, tail, list; // Erlang list type's variables
    list = from;
    while (enif_get_list_cell(env, list, &head, &tail))
    {
        // read one group
        if(read_param_name(groups,head,env) != 1){
            Node Id = mbGraph.addNode();
            
            // put the elements in the group
            for(int i = 0; i < (int)groups.size(); i++)
            {
                Node u = find_node_by_name("module",groups[i]);
                if(u != INVALID){
                    ModulBlockId[u].push_back(groupId);
                    GroupElements[Id].push_back(NodesId[u]);
                    elements = elements + groups[i] + (string)",";
                }
            }
            GroupId[Id] = groupId;
            GroupLabel[Id] = elements.substr(0,elements.length()-1);
            elements = "";
            groupId++;
            groups.clear();
        }
        list = tail;
    }
    
    // Modules of the graph, that is not in any groups will go to 0. modulblock
    // create 0. modulblock
    Node zero = mbGraph.addNode();
    GroupId[zero] = 0;

    for(ListDigraph::NodeIt n(g); n != INVALID; ++n){
        if(ModulBlockId[n].empty()){
            ModulBlockId[n].push_back(0);
            GroupElements[zero].push_back(NodesId[n]);
            elements = elements + NodesName[n] + (string)",";    
         }
    }
    if(!GroupElements[zero].empty())
        GroupLabel[zero] = elements.substr(0,elements.length()-1);
    else mbGraph.erase(zero);
    
    ListDigraph::NodeIt x(mbGraph);
    while(x != INVALID){
        if(GroupElements[x].empty()){
            Node u = x;
            ++x;
            mbGraph.erase(u);
        }
        else ++x;
    }
}

// Add arcs to the modulblock graph
void add_modulblock_arcs()
{
    for(ListDigraph::ArcIt a(g); a!= INVALID; ++a)
    {
        for(int i = 0; i < (int)ModulBlockId[g.source(a)].size(); ++i){
            for(int j = 0; j < (int)ModulBlockId[g.target(a)].size(); ++j){
                Node source = find_node(ModulBlockId[g.source(a)][i]);
                Node target = find_node(ModulBlockId[g.target(a)][j]);
                Arc b = INVALID;
                if(source != INVALID && target != INVALID){
                    b = findArc(mbGraph, source, target, INVALID);
                    if(b != INVALID){
                        ArcMp[b]++;
                    }
                    else{
                        b = mbGraph.addArc(source, target);
                        ArcMp[b] = 1;
                    }
                }
            }
        }
    }
}

// Filter the graph by cyclic subgraph on modulblock level
int filter_cyclic_mb(const ERL_NIF_TERM from, ErlNifEnv* env)
{
    // read parameter: Cyclic. It can be: true | false
    char cyclicGraph[256];
    if (enif_get_atom(env, from, (char*)cyclicGraph, 
        sizeof(cyclicGraph), ERL_NIF_LATIN1) == 0) 
            return 1;
    if((string)cyclicGraph != "true" && (string)cyclicGraph != "false") 
        return 1;
        
    // Find cyclic subgraph arcs
    // Find the modulblock graph's strongly connected components' cut arcs
    int cutArcs = stronglyConnectedCutArcs(mbGraph,AcyclicArc);
    (void)cutArcs;
    
    // Cyclic subgraph's nodes -- filter the graph
    if(cyclicGraph == (string)"true"){
        for(ListDigraph::ArcIt a(mbGraph); a != INVALID; ++a){
            if(!AcyclicArc[a]){
                cyclicNodeFilterMB[mbGraph.source(a)] = true;
                cyclicNodeFilterMB[mbGraph.target(a)] = true;
            }
        }
    }
    else{
        for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n)
            cyclicNodeFilterMB[n] = true;
    }
    
    // Erase selected nodes
    ListDigraph::NodeIt n(mbGraph);
    while(n!= INVALID)
    {
        if(!cyclicNodeFilterMB[n]){
            Node u = n;
            ++n;
            mbGraph.erase(u);
        }
        else ++n;
    } 
    
    return 0;
}

// Filter the graph by starting nodes
int filter_starting_nodes(const ERL_NIF_TERM from, ErlNifEnv* env, const string level)
{
    // read parameter: Starting Nodes
    vector<string> stNodeTypes;
    vector<int> stNodeIds;
    if(read_param_id(stNodeTypes,stNodeIds,from,env) != 0) 
        return 1;
    
    // Starting Nodes -- filter the graph
    if(stNodeIds.size() > 0)
        for(int i = 0; i < (int)stNodeTypes.size(); i++){
            Node startingNode = find_node(stNodeTypes[i], stNodeIds[i]);
			if(!stNodeFilter[startingNode])
            	include_nodes(startingNode, stNodeFilter, level);
        }
    else
        for(ListDigraph::NodeIt n(g); n != INVALID; ++n)
            stNodeFilter[n] = true;  
    stNodeIds.clear();
    stNodeTypes.clear();
    erase_selected_nodes(stNodeFilter);
    
    return 0;
}

// Filter the graph by exclude children
int filter_exclude_children(const ERL_NIF_TERM from, ErlNifEnv* env, const string level)
{
    // read parameter: Exclude Children
    vector<string> exLeavesNodeTypes;
    vector<int> exLeavesNodeIds;
    if(read_param_id(exLeavesNodeTypes,exLeavesNodeIds,from,env) != 0) 
        return 1;
    
    // Exclude Children -- filter the graph
    for (int i = 0; i < (int)exLeavesNodeTypes.size(); i++) {
        Node exl_lea_n = find_node(exLeavesNodeTypes[i],exLeavesNodeIds[i]);
        exclude_nodes(exl_lea_n, excludeFilter, level);
    }
	for (int i = 0; i < (int)exLeavesNodeTypes.size(); i++) {
        Node exl_lea_n = find_node(exLeavesNodeTypes[i],exLeavesNodeIds[i]);
        if(exl_lea_n != INVALID) excludeFilter[exl_lea_n] = true;
    }
	erase_selected_nodes(excludeFilter);
    exLeavesNodeIds.clear();
    exLeavesNodeTypes.clear();
    
    return 0;
}

// Filter the graph by exclude nodes
int filter_exclude_nodes(const ERL_NIF_TERM from, ErlNifEnv* env, const string level)
{
    // read parameter: Exclude Nodes
    vector<string> exNodeTypes;
    vector<int> exNodeIds;
    if(read_param_id(exNodeTypes,exNodeIds,from,env) != 0) 
        return 1;
    
    // Exclude Nodes -- filter the graph
    for (int i = 0; i < (int)exNodeTypes.size(); i++) {
        exclude_nodes(find_node(exNodeTypes[i],exNodeIds[i]), excludeFilter, level);
    }
    exNodeIds.clear();
    exNodeTypes.clear();
    return 0;
}

// Filter the graph by OTP modules
int filter_exclude_otp(const ERL_NIF_TERM from, ErlNifEnv* env)
{
    // read parameter: Exclude OTP    
    vector<string> otpNames;
    if(read_param_name(otpNames,from,env) != 0) 
        return 1;
    
    // Exclude OTP -- filter the graph
    for (int i = 0; i < (int)otpNames.size(); i++) {
        exclude_nodes(find_node_by_name("module",otpNames[i]), excludeFilter, "");
    }
    otpNames.clear();
    return 0;
}

// Filter the graph by excude libraries
int filter_exclude_libraries(const ERL_NIF_TERM from, ErlNifEnv* env, const string level)
{
    // read parameter: Exclude Libraries    
    vector<string> excLibNames;
    if(read_param_name(excLibNames,from,env) != 0) 
        return 1;
    
    // Exclude Libraries -- filter the graph
    for (int i = 0; i < (int)excLibNames.size(); i++) {
		Node n = find_node_by_name("module",excLibNames[i]);
		if(n != INVALID)
		{
			excludeFilter[n] = false;
			if((string)level == "func")
			{
				for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a)
				{
					if(ArcType[a] != "modcall")
						excludeFilter[g.target(a)] = false;
				}
			}
		}
		//exclude_nodes(find_node_by_name("module",excLibNames[i]),excludeFilter);
    }
    excLibNames.clear();
    
    return 0;
}

// Filter the graph by cyclic subgraph
int filter_cyclic(const ERL_NIF_TERM from, ErlNifEnv* env, 
                    const string graphLevel)
{
    // read parameter: Cyclic. It can be: true | false
    char cyclicGraph[256];
    if (enif_get_atom(env, from, (char*)cyclicGraph, 
        sizeof(cyclicGraph), ERL_NIF_LATIN1) == 0) 
            return 1;
    if((string)cyclicGraph != "true" && (string)cyclicGraph != "false") 
        return 1;
        
    // Find cyclic subgraph arcs
    // 1. Filter the graph by call arc type
    if((string)graphLevel == "func") call_arc_filter("funcall");
    else call_arc_filter("modcall");
    // 2. Create a subgraph which only contains calling arcs
    SubDigraph<ListDigraph> SubDepGraph(g,nodeAllFilter,typeArcFilter);
    // 3. Find the subgraph's strongly connected components' cut arcs
    int cutArcs = stronglyConnectedCutArcs(SubDepGraph,acyclicArcs);
    (void)cutArcs;
    
    // Cyclic subgraph's nodes -- filter the graph
    if(cyclicGraph == (string)"true"){
        for(SubDigraph<ListDigraph>::ArcIt a(SubDepGraph); a != INVALID; ++a){
            if(!acyclicArcs[a]){
                cyclicNodeFilter[g.source(a)] = true;
                include_parent(g.source(a), cyclicNodeFilter);
                cyclicNodeFilter[g.target(a)] = true;
                include_parent(g.target(a), cyclicNodeFilter);
            }
        }
    }
    else{
        for(SubDigraph<ListDigraph>::NodeIt n(SubDepGraph); n != INVALID; ++n)
            cyclicNodeFilter[n] = true;
    }
    
    erase_selected_nodes(cyclicNodeFilter);
    
    return 0;
}

// Set connection helper maps values
void set_conmaps_values()
{
    for(ListDigraph::NodeIt n(g); n!= INVALID; ++n)
    {
        IsArcBetCon[n] = false;
        IsVisitedCon[n] = false;
        ParentNodeCon[n] = -1;
    }
}

// Add connection arc tags
void add_con_tags()
{
    for(ListDigraph::ArcIt a(g); a != INVALID; ++a){
        if( NodesType[g.source(a)] == "module" && 
            NodesType[g.target(a)] == "func");
        else if(NodesConType[g.source(a)] == "pnode" || 
                NodesConType[g.target(a)] == "pnode")
                ArcType[a] = "pcallarc";
    }
}

// Filter the graph by connection
int filter_connection(const int ctype, const int maxdepth)
{
    if(CNodes.size() > 1) start_path_traversal(ctype,maxdepth);
    if(!CNodes.empty())
	{
		erase_selected_nodes(connectionFilter);
		add_con_tags();
	}
	CNodes.clear();

    return 0;
}

// Start path traversal from every connection node
void start_path_traversal(int ctype, int maxdepth)
{
	for(int i = 0; i < (int)CNodes.size(); ++i)
	{
		vector<int> Path;
		set_conmaps_values();
		int id = g.id(CNodes[i]);
		Path.push_back(id);
		path_traversal(CNodes[i], Path, ctype);
	}
	int concurentThreadsSupported = thread::hardware_concurrency();
	if(concurentThreadsSupported > 1)
	{
		stack<Node> st;
		for(int i = 0; i < (int)CNodes.size(); ++i) st.push(CNodes[i]);
		thread threads[concurentThreadsSupported];
		for(int i = 0; i < concurentThreadsSupported; ++i)
			threads[i] = thread(thread_man, &st, maxdepth, ctype);
		for(int i = 0; i < concurentThreadsSupported; ++i) threads[i].join();
	}
	else
	{
		map<Arc, string> labels;
		for(int i = 0; i < (int)CNodes.size(); ++i)
			add_conarcs(&(CNodes[i]), &labels, maxdepth, ctype);
		map<Arc, string>::iterator it;
		for(it = labels.begin(); it != labels.end(); ++it)
		{
			ArcLabel[it->first] = it->second;
		}
	}
}

// Travel a graph path from the given node
void path_traversal(Node s, vector<int> &Path, const int ctype)
{
	IsVisitedCon[s] = true;
	for(ListDigraph::OutArcIt a(g,s); a != INVALID; ++a)
	{
		Node target = g.target(a);
		if(!IsVisitedCon[target])
		{
			if(NodesConType[target] == "cnode")
			{
				Node start = g.nodeFromId(Path[0]);
				vector<int> v;
				for(int i = 1; i < (int)Path.size(); ++i)
				{
					Node n = g.nodeFromId(Path[i]);
					if(ctype == 1 && NodesConType[n] != "cnode" && NodesConType[n] != "pnode")
					{
						NodesConType[n] = "pnode";
						connectionFilter[n] = true;
						include_parent(n, connectionFilter);
					}
					v.push_back(Path[i]);
				}
				int id = g.id(target);
				v.push_back(id);
				vector<vector<int> > w;
				NodePath[start][target] = w;
				NodePath[start][target].push_back(v);
			}
			else
			{
				int id = g.id(target);
				Path.push_back(id);
				path_traversal(target,Path, ctype);
			}
		}		
	}
	IsVisitedCon[s] = false;
	Path.pop_back();
}

// Manage threads so there is no unused hardware thread
void thread_man(stack<Node>* s, const int maxdepth, const int ctype)
{
	bool done = false;
	map<Arc, string> labels;
	while(!done)
	{
		if(!s->empty())
		{
			mtx2.lock();
			Node node = s->top();
			s->pop();
			mtx2.unlock();
			thread t(add_conarcs, &node, &labels, maxdepth, ctype);
			t.join();
		}
		else done = true;
	}
	map<Arc, string>::iterator it;
	mtx3.lock();
	for(it = labels.begin(); it != labels.end(); ++it)
	{
		ArcLabel[it->first] = it->second;
	}
	mtx3.unlock();
}

// Add the indirect arcs to the graph
void add_conarcs(Node* node, map<Arc, string>* labels, const int maxdepth, const int ctype)
{
	ListDigraph::NodeMap<bool> IsDone(g);
	for(ListDigraph::NodeIt n(g); n!= INVALID; ++n) IsDone[n] = false;
	vector<int> Path;		
	int id = g.id((*node));
	Path.push_back(id);
	connect_path((*node), (*node), labels, Path, IsDone, 0, maxdepth, ctype);
}

// Find all paths between connection nodes
void connect_path(Node s, Node n, map<Arc, string>* labels, vector<int> &Path,
					ListDigraph::NodeMap<bool> &IsDone, int depth, const int maxdepth, const int ctype)
{
	if(ctype == 1 || depth <= maxdepth || maxdepth == 0)
	{
		map<Node, vector<vector<int> > >::iterator it;
		for(it = NodePath[n].begin(); it != NodePath[n].end(); ++it)
		{
			int id = g.id(it->first);
			if(find(Path.begin(), Path.end(), id) == Path.end())
			{
				for(int i = 0; i < (int)it->second.size(); ++i)
				{
					int re = 0;
					for(int j = 0; j < (int)it->second[i].size(); ++j)
					{
						Path.push_back(it->second[i][j]);
						++re;
					}
					connect_path(s, it->first, labels, Path, IsDone, (int)Path.size() - 1, maxdepth, ctype);
					for(int j = 0; j < re; ++j)
					{
						Path.pop_back();
					}
					if(ctype == 1) break;
				}
			}
		}
		Node target = g.nodeFromId(Path.back());
		if((int)Path.size() > 2 && !IsDone[target])
		{
			mtx.lock();
			Arc a = g.addArc(s, target);
			ArcType[a] = "icallarc";
			mtx.unlock();
			IsDone[target] = true;
		}
		if(ctype == 0 && (int)Path.size() > 2)
		{
			for(ListDigraph::OutArcIt a(g,s); a != INVALID; ++a)
			{
				if(target == g.target(a) && ArcType[a] == "icallarc")
				{
					string label = "";
					if(!(*labels)[a].empty())
					{
						label = ";";
					}
					for(int i = 1; i < (int)Path.size() - 1; ++i)
					{
						label = label + NodesName[g.nodeFromId(Path[i])];
						label = label + "->";
					}
					(*labels)[a]/*ArcLabel[a]*/ += label + NodesName[target];
				}
			}
		}
	}
}

// Get all connection nodes and initialise connection filter
int select_cnodes(const ERL_NIF_TERM from,ErlNifEnv* env, const string level)
{
    for(ListDigraph::NodeIt n(g); n != INVALID; ++n)
        connectionFilter[n] = false;
    
    // read parameter: Connection Between Nodes
    vector<string> conNodeTypes;
    vector<int> conNodeIds;
    if(read_param_id(conNodeTypes,conNodeIds,from,env) != 0) 
        return 1;
    
    
    // Connection Nodes -- add values to NodesConType arcmap
    if (conNodeTypes.size() > 0){
        for (int i = 0; i < (int)conNodeTypes.size(); i++) {
            Node n = find_node(conNodeTypes[i],conNodeIds[i]);
            if (n != INVALID && NodesConType[n] != "cnode")
            {
				if(((string)level != "module" || NodesType[n] != "func") &&
					((string)level != "func" || NodesType[n] != "module"))
				{
		            CNodes.push_back(n);
		            NodesConType[n] = "cnode";
		            connectionFilter[n] = true;
		            include_parent(n, connectionFilter);
		            include_nodes(n,connectionBeforeFilter,level);
				}
				else if((string)level == "func" && NodesType[n] == "module")
				{
					for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a)
					{
						if(ArcType[a] == "contain")
						{
							Node m = g.target(a);
							CNodes.push_back(m);
							NodesConType[m] = "cnode";
							connectionFilter[m] = true;
		            		include_parent(m, connectionFilter);
		            		include_nodes(m,connectionBeforeFilter,level);
						}
					}
				}
            }
        }
    }
    else{
        for(ListDigraph::NodeIt n(g); n != INVALID; ++n)
            connectionBeforeFilter[n] = true;  
    }
    conNodeTypes.clear();
    conNodeIds.clear();
    erase_selected_nodes(connectionBeforeFilter);
    
    return 0;	
}

// Filter the dependency graph
static ERL_NIF_TERM filter_graph_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // read parameter: graphLevel
    string graphLevel = read_graph_level(argv[0],env);
    if((string)graphLevel == "badarg")
        return enif_make_badarg(env);

    // Filter the graph by level
    filter_level(graphLevel);
    
    // mb level: create modulblock graph
    if((string)graphLevel == "mb"){
        add_modulblock_nodes(argv[7],env);
        add_modulblock_arcs();
        if(filter_cyclic_mb(argv[1],env) != 0)
            return enif_make_badarg(env);
    }
    
    // Func or module level: read parameters and filter the graph
    else{ 
        // Set filter variables' starting value
        for(ListDigraph::NodeIt n(g); n != INVALID; ++n){
            stNodeFilter[n]     = false;
            excludeFilter[n]    = true;
            cyclicNodeFilter[n] = false;
            nodeAllFilter[n]    = true;
        }

        /// Filter the graph

		// Connection filter, first step
		if(select_cnodes(argv[8],env,graphLevel) != 0)
			return enif_make_badarg(env);

        // Starting Nodes filter
        if(filter_starting_nodes(argv[6],env,graphLevel) != 0)
            return enif_make_badarg(env);
        
        // Exclude Children filter
        if(filter_exclude_children(argv[4],env,graphLevel) != 0)
            return enif_make_badarg(env);  
        // Exclude Leaves filter
        if(filter_exclude_nodes(argv[3],env,graphLevel) != 0)
            return enif_make_badarg(env);
        // Exclude OTP filter
        if(filter_exclude_otp(argv[2],env) != 0)
            return enif_make_badarg(env);
        // Exclude Libraries filter
        if(filter_exclude_libraries(argv[5],env,graphLevel) != 0)
            return enif_make_badarg(env);

        erase_selected_nodes(excludeFilter);

		// Cyclic subgraph filter
        if(filter_cyclic(argv[1],env,graphLevel) != 0)
            return enif_make_badarg(env);

        // Connection filter, second step
        int conntype = -1;
		int maxdepth = -1;
        if (!enif_get_int(env, argv[9], &conntype) 
                || conntype < 0 || conntype > 1) 
            return enif_make_badarg(env);
		if (!enif_get_int(env, argv[10], &maxdepth) 
                || maxdepth < 0 || maxdepth == 1) 
            return enif_make_badarg(env);
        filter_connection(conntype,maxdepth);

		if((string)graphLevel == "func")
			erase_isolated_modules();
    }
    
    return enif_make_int(env, 0);
}

//  ============================================================================
//  Write graph to dot file
//  ============================================================================
static ERL_NIF_TERM graph_to_dot_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ 
    // read parameter: graphLevel. It can be: module | func | mb.
    string graphLevel = read_graph_level(argv[0],env);
    if(graphLevel == "badarg") 
        return enif_make_badarg(env);

    // read parameter: file name
    char file[1024];
    if (enif_get_string(env, argv[1], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);

    // read parameter: dot parameters
    char dotpar[1024];
    if (enif_get_string(env, argv[2], (char*)dotpar, 
        sizeof(dotpar), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // read parameter: dot type. It can be: simple | complex | nomods.
    char dottype[256];
    if (enif_get_atom(env, argv[3], (char*)dottype, 
        sizeof(dottype), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    if((string)dottype != "simple" && (string)dottype != "complex"
        && (string)dottype != "nomods") 
            return enif_make_badarg(env);

    // Create output file
    string DotFileName = (string)file;
    ofstream OutDotFile;
    OutDotFile.open (DotFileName.c_str());

    /// Write the information of dependency graph to dot file
    OutDotFile << "digraph dependency_representation {\n";
    
    // Parameters for the whole graph's appearance
    OutDotFile << dotpar << "\n";
    
    // Simple dot graph
    if((string)dottype == "simple"){
        // Level: mb
        if((string)graphLevel =="mb")
            graph_to_dot_mb(OutDotFile);
        // Level: func | module
        else
            graph_to_dot_simple(graphLevel,OutDotFile);
    }
    // Complex dot graph
    else if((string)dottype == "complex"){
        // Level: mb
        if((string)graphLevel =="mb"){
            graph_to_dot_comp_mb(OutDotFile);
        }
        // Level: func | module
        else
            graph_to_dot_comp(graphLevel,OutDotFile);
    }
    // No moduls dot graph
    else{
        // Level: func
        if((string)graphLevel == "func")
            graph_to_dot_func(OutDotFile);
    }
    
    // Close file
    OutDotFile << "}";
    OutDotFile.close();
    
    return enif_make_int(env, 0);
}

// Write the graph to dot, when dot type is simple and level is func or module
void graph_to_dot_simple(const string graphLevel, ofstream &OutDotFile)
{   
    // root node 
    // We need a root node when graphLevel is func
    if((string)graphLevel == "func"){
        OutDotFile << "root [shape=\"triangle\", label=\"ROOT\", ";
        OutDotFile << "fontsize=\"18\", color=\"black\"  URL=\"#ok\", ";
        OutDotFile << "tooltip=\"node{'$gn', root, 0}\"]\n";
    }
    
    // Write graph's nodes to dot
    for(ListDigraph::NodeIt n(g); n != INVALID; ++n){
        string type = NodesType[n];
        string contype = NodesConType[n];
        int id = NodesId[n];
        bool IsRoot = true;
        OutDotFile << type << id;
        if(contype == "cnode")
        {
            OutDotFile << con_node(NodesName[n],make_tooltip(type,id)); 
            IsRoot = false;
        }
        else if (contype == "pnode")
        {    
            OutDotFile << path_node(NodesName[n],make_tooltip(type,id)); 
            IsRoot = false;
        }
        else if(type == "func") 
            // Function node
            OutDotFile << func_node(NodesName[n],make_tooltip(type,id)); 
        else{
            // Module node
            OutDotFile << module_node(NodesName[n],make_tooltip(type,id));
            
            // root -> module arc
            // We need this type of arc when graphLevel is func
            if(IsRoot && (string)graphLevel == "func"){
                OutDotFile << "root -> "; 
                OutDotFile << NodesType[n] << NodesId[n];
                OutDotFile << contain_arc("");
            }
        }
    }
    
    // Write graph's arcs to dot
    for(ListDigraph::ArcIt a(g); a != INVALID; ++a){
        Node source = g.source(a);
        Node target = g.target(a);
        // Icallarc
        if(ArcType[a] == "icallarc"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_icall_arc(ArcLabel[a]);
            else
                OutDotFile << cyclic_icall_arc(ArcLabel[a]);
        }            
        // Pcallarc
        else if(ArcType[a] == "pcallarc"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_pcall_arc("");
            else
                OutDotFile << cyclic_pcall_arc("");
        }
        // Funcall arc
        else if(ArcType[a] == "funcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_call_arc("funcall","");
            else
                OutDotFile << cyclic_call_arc("funcall","");
        }
        // Dyncall or ambcall arc
        else if(ArcType[a] == "dyncall" || ArcType[a] == "ambcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_dyncall_arc(ArcType[a],"");
            else
                OutDotFile << cyclic_call_arc(ArcType[a],"");
        }
        // Contain arc
        else if(ArcType[a] == "contain"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            OutDotFile << contain_arc("");
        }
        // Modcall arc
        else if(ArcType[a] == "modcall" && (string)graphLevel == "module"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_call_arc("modcall","");
            else
                OutDotFile << cyclic_call_arc("modcall","");
        }
    }
}

// Write the graph to dot, when dot type is complex and level is func or module
void graph_to_dot_comp(const string graphLevel, ofstream &OutDotFile)
{    
    // root 
    // We need a root node when graphLevel is func
    if((string)graphLevel == "func"){
        OutDotFile << "root [shape=\"triangle\", label=\"[{shape,triangle},";
        OutDotFile << "{name,\\\"ROOT\\\"},{node,{'$gn',root,0}},{type,root}]\", ";
        OutDotFile << "fontsize=\"18\", color=\"black\"]\n";
    }
    
    // Write graph's nodes to dot
    for(ListDigraph::NodeIt n(g); n != INVALID; ++n){
        string type = NodesType[n];
        int id = NodesId[n];
        OutDotFile << type << id;
        // Function node
        if(type == "func") {
            string label = "[{shape,hexagon},{name,\\\"" + NodesName[n] 
                            + (string)"\\\"},{node," + make_tooltip(type,id) 
                            + (string)"},{type,func}]";
            OutDotFile << func_node(label,""); 
        }
        // Module node
        else{
            string label = "[{shape,box},{name,\\\"" + NodesName[n] 
                            + (string)"\\\"},{node," + make_tooltip(type,id) 
                            + (string)"},{type,module}]";
            OutDotFile << module_node(label,"");
            
            // root -> module arc
            // We need this arc when graphLevel is func
            if((string)graphLevel == "func"){
                string label = "[{type,contain},{from,{'$gn',root,0}},{to," 
                                + make_tooltip(NodesType[n],NodesId[n]) 
                                + (string)"}]";
                OutDotFile << "root -> "; 
                OutDotFile << NodesType[n] << NodesId[n];
                OutDotFile << contain_arc(label);
            }
        }
    }
    
    // Write graph's arcs to dot
    for(ListDigraph::ArcIt a(g); a != INVALID; ++a){
        Node source = g.source(a);
        Node target = g.target(a);
        string label = "[{type," + ArcType[a] + (string)"},{from,"  
                        + make_tooltip(NodesType[source],NodesId[source])
                        + (string)"},{to," 
                        + make_tooltip(NodesType[target],NodesId[target]) 
                        + (string)"}]";
        // Funcall arc
        if(ArcType[a] == "funcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_call_arc("funcall",label);
            else
                OutDotFile << cyclic_call_arc("funcall",label);
        }
        // Dyncall or ambcall arc
        else if(ArcType[a] == "dyncall" || ArcType[a] == "ambcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_dyncall_arc(ArcType[a],label);
            else
                OutDotFile << cyclic_call_arc(ArcType[a],label);
        }
        // Contain arc
        else if(ArcType[a] == "contain"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            OutDotFile << contain_arc(label);
        }
        // Modcall arc
        else if(ArcType[a] == "modcall" && (string)graphLevel == "module"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_call_arc("modcall",label);
            else
                OutDotFile << cyclic_call_arc("modcall",label);
        }
    }
}

// Write the graph to dot, when dot type is nomods
void graph_to_dot_func(ofstream &OutDotFile)
{
    // Write graph's nodes to dot
    for(ListDigraph::NodeIt n(g); n != INVALID; ++n){
        // Function node
        if(NodesType[n] == "func"){
            Node parent = find_parent_node(n);
            string label = NodesName[parent] + (string)":" + NodesName[n];
            OutDotFile << "func" << NodesId[n];
            OutDotFile << 
                        func_node(label,make_tooltip(NodesType[n],NodesId[n])); 
        }
    }
    
    // Write graph's arcs to dot
    for(ListDigraph::ArcIt a(g); a != INVALID; ++a){
        Node source = g.source(a);
        Node target = g.target(a);
        // Funcall arc
        if(ArcType[a] == "funcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_call_arc("funcall","");
            else
                OutDotFile << cyclic_call_arc("funcall","");
        }
        // Dyncall and ambcall arc
        else if(ArcType[a] == "dyncall" || ArcType[a] == "ambcall"){
            OutDotFile << NodesType[source] << NodesId[source] << " -> "; 
            OutDotFile << NodesType[target] << NodesId[target];
            if(acyclicArcs[a])
                OutDotFile << simple_dyncall_arc(ArcType[a],"");
            else
                OutDotFile << cyclic_call_arc(ArcType[a],"");
        }
    }    
}

// Write the modulblock graph to dot, when dot type is simple
void graph_to_dot_mb(ofstream &OutDotFile)
{  
    // Graph
    OutDotFile << "\nsubgraph the_graph {\n";
    
    // Write graph's nodes to dot
    for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n){
        OutDotFile << GroupId[n] << " ";
        OutDotFile << func_node(number_to_string(GroupId[n]),GroupLabel[n]);
    }
    
    // Write graph's arcs to dot
    for(ListDigraph::ArcIt a(mbGraph); a != INVALID; ++a){
        Node source = mbGraph.source(a);
        Node target = mbGraph.target(a);
        OutDotFile << GroupId[source] << " -> " << GroupId[target] << " ";
        if(AcyclicArc[a]){
            OutDotFile << simple_call_arc("mbcall","");
        }
        else{
            OutDotFile << cyclic_call_arc("mbcall","");
        }
    }
    OutDotFile << "}\n";
    
    // Table
    OutDotFile << "\nsubgraph table {\n";
    OutDotFile << "MBElements [shape=none, margin=0, label=<\n";
    OutDotFile << "<TABLE BORDER=\"0\" CELLBORDER=\"1\" ";
    OutDotFile << "CELLSPACING=\"0\" CELLPADDING=\"4\">\n";
    for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n){
        OutDotFile << "<TR>\n";
        OutDotFile << "<TD>" << GroupId[n] << "</TD>\n";
        OutDotFile << "<TD align=\"left\">" << GroupLabel[n] << "</TD>\n";
        OutDotFile << "</TR>\n";
    }
    OutDotFile << "</TABLE>>];\n";
    OutDotFile << "}\n";
}

// Write the modulblock graph to dot, when dot type is complex
void graph_to_dot_comp_mb(ofstream &OutDotFile)
{  
    // Write graph's nodes to dot
    for(ListDigraph::NodeIt n(mbGraph); n != INVALID; ++n){
        string label = "[{shape,hexagon},{name,\\\"[" + GroupLabel[n] 
                + (string)"]\\\"},{node," + number_to_string(GroupId[n])
                + (string)"},{type,mb}]";
        OutDotFile << GroupId[n] << " ";
        OutDotFile << func_node(label,"");
    }
    
    // Write graph's arcs to dot
    for(ListDigraph::ArcIt a(mbGraph); a != INVALID; ++a){
        Node source = mbGraph.source(a);
        Node target = mbGraph.target(a);
        OutDotFile << GroupId[source] << " -> " << GroupId[target] << " ";
        string label = "[{type,mbcall},{from,"  
                + number_to_string(GroupId[source]) + (string)"},{to," 
                + number_to_string(GroupId[target]) + (string)"}]";
        if(AcyclicArc[a]){
            OutDotFile << simple_call_arc("mbcall",label);
        }
        else{
            OutDotFile << cyclic_call_arc("mbcall",label);
        }
    }
}

/// DOT file strings
// Module Node string
string module_node(const string label, const string tooltip)
{
    return  " [shape=\"box\", label=\"" + label 
            + "\", fontsize=\"18\", color=\"purple\"  URL=\"#ok\", tooltip=\"" 
            + tooltip + "\"]\n";
}
// Connection node string
string con_node(const string label, const string tooltip)
{
    return " [shape=\"box\", label=\"" + label 
            + "\", fontsize=\"14\", color=\"black\"  URL=\"#ok\", tooltip=\"" 
            + tooltip + "\"]\n";
}

// Path Node string
string path_node(const string label, const string tooltip)
{
    return  " [shape=\"ellipse\", label=\"(" + label  
            + ")\", fontsize=\"12\", color=\"grey\"  URL=\"#ok\", tooltip=\"" 
            + tooltip + "\"]\n";
}
// Function node string
string func_node(const string label, const string tooltip)
{
    return " [shape=\"hexagon\", label=\"" + label 
            + "\", fontsize=\"14\", color=\"black\"  URL=\"#ok\", tooltip=\"" 
            + tooltip + "\"]\n";
}

// Call arc string, which is a member of the cyclic subgraph
string cyclic_call_arc(const string type, const string label)
{
    return (string)" [color=\"red\",  style=\"dashed\", "
            + (string) "label=\"" + label + "\", "
            + (string) "arrowhead=\"invodot\", tooltip=\"" 
            + (string)type + "\"]\n";
}
// Call arc string, which is not a member of the cyclic subgraph
string simple_call_arc(const string type, const string label)
{
    return (string)" [color=\"black\",  style=\"dashed\", "
            + (string) "label=\"" + label + "\", " 
            + (string) "arrowhead=\"normal\", tooltip=\"" 
            + (string)type + "\"]\n";
}
// Dyncall arc string, which is not a member of the cyclic subgraph
string simple_dyncall_arc(const string type, const string label)
{
    return (string)" [color=\"grey\",  style=\"dotted\", "
            + (string) "label=\"" + label + "\", " 
            + (string) "arrowhead=\"normal\", tooltip=\"" 
            + (string)type + "\"]\n";
}
// Contain arc string
string contain_arc(const string label)
{
    return (string)" [color=\"black\",  style=\"solid\", "
            + (string) "label=\"" + label + "\", "
            + (string)"arrowhead=\"normal\", tooltip=\"contain\"]\n";
}
// Indirect callarc arc string
string simple_icall_arc(const string label)
{
    return (string)" [color=\"green\",  style=\"dashed\", "
            + (string) "label=\"" + label + "\", "
            + (string)"arrowhead=\"normal\", tooltip=\"indirect callarc\"]\n";
}
// Indirect callarc arc string
string cyclic_icall_arc(const string label)
{
    return (string)" [color=\"green\",  style=\"dashed\", "
            + (string) "label=\"" + label + "\", "
            + (string)"arrowhead=\"normal\", tooltip=\"indirect callarc\"]\n";
}
// Path simple callarc arc string
string simple_pcall_arc(const string label)
{
    return (string)" [color=\"grey\",  style=\"dotted\", "
            + (string) "label=\"" + label + "\", "
            + (string)"arrowhead=\"normal\", tooltip=\"callarc\"]\n";
}
// Path cyclic callarc arc string
string cyclic_pcall_arc(const string label)
{
    return (string)" [color=\"red\",  style=\"dotted\", "
            + (string) "label=\"" + label + "\", "
            + (string)"arrowhead=\"normal\", tooltip=\"callarc\"]\n";
}
// Make a tooltip from a node's type and id
string make_tooltip(const string type, const int id)
{
    string idStr = number_to_string(id);
    return "{'$gn'," + type + "," + idStr + "}";
}
// Convert integer to string
string number_to_string(int number)
{
    std::stringstream ss;
    ss << number;
    return ss.str();
}

//  ============================================================================
//  Write graph to Erlang name terms
//  ============================================================================
static ERL_NIF_TERM graph_to_terms_name_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{   
    // read parameter: graphLevel. It can be: module | func | mb.
    string graphLevel = read_graph_level(argv[0],env);
    if(graphLevel == "badarg") 
        return enif_make_badarg(env);
    
    // read parameter: output file
    char file[1024];
    if (enif_get_string(env, argv[1], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // Create output file
    string FileName = (string)file;
    ofstream OutFile;
    OutFile.open (FileName.c_str());
    
    // Level is func
    if((string)graphLevel == "func"){
        graph_to_terms_name_func(OutFile);
    }
    // Level is module
    else if((string)graphLevel == "module"){
        graph_to_terms_name_module(OutFile);
    }
    // Level is mb
    else if((string)graphLevel == "mb"){
        // Create mb output file
        string FileName = (string)file + "_mb";
        ofstream OutMBFile;
        OutMBFile.open (FileName.c_str());
        
        graph_to_terms_name_mb(OutFile, OutMBFile);
        
        OutMBFile.close();
    }
    // Close file 
    OutFile.close();
    
    return enif_make_int(env, 0);
}

// Write graph to Erlang name terms, when graph level is func
void graph_to_terms_name_func(ofstream &OutFile)
{
    vector<string> elements;
    string elem;

	OutFile << "[";
	bool first = true;
    for(ListDigraph::NodeIt n(g); n!= INVALID; ++n){
        if(NodesType[n] == "func"){
            if(first) first = false;
			else OutFile << "," << std::endl;
            // The function
            Node parent = find_parent_node(n);
            OutFile << "{\"";
            OutFile << NodesName[parent] << ":";
            OutFile << NodesName[n] << "\", [";
            
            // The functions which is called by 'n' node's function
            for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a){
                parent = find_parent_node(g.target(a));
                elem =  (string)"\"" 
                        + NodesName[parent] 
                        + (string)":" 
                        + NodesName[g.target(a)] 
                        + (string)"\"";
                elements.push_back(elem);
            }
            
            if(!elements.empty()) OutFile << elements[0];
            for(int i = 1; i < (int)elements.size(); ++i)
                OutFile << "," << elements[i];
            OutFile << "]}";
            elements.clear();
        }
    }
	OutFile << "].";
}

// Write graph to Erlang name terms, when graph level is module
void graph_to_terms_name_module(ofstream &OutFile)
{
    vector<string> elements;
    string elem; 

	OutFile << "[";
	bool first = true;
    for(ListDigraph::NodeIt n(g); n!= INVALID; ++n){
        if(NodesType[n] == "module"){
            if(first) first = false;
			else OutFile << ",";
            // The module 
            OutFile << "{\"" << NodesName[n] << "\", [";
            
            // The modules which is called by 'n' node's module
            for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a){
                if(ArcType[a] == "modcall"){
                    elem =  (string)"\"" 
                            + NodesName[g.target(a)] 
                            + (string)"\"";
                    elements.push_back(elem);
                }
            }
            
            if(!elements.empty()) OutFile << elements[0];
            for(int i = 1; i < (int)elements.size(); ++i)
                OutFile << "," << elements[i];
            OutFile << "]}"<< std::endl;
            elements.clear();
        }
    }
	OutFile << "].";
}

// Write graph to Erlang name terms, when graph level is modulblock
void graph_to_terms_name_mb(ofstream &OutFile, ofstream &OutMBFile)
{            
	OutFile << "[";
	bool first = true;
    for(ListDigraph::NodeIt n(mbGraph); n!= INVALID; ++n){
        if(first) first = false;
		else OutFile << ",";
        // The modulblock
        OutFile << "{" << GroupId[n] << ",[";
        
        // The modulblocks which is called by 'n' node's modulblock
        ListDigraph::OutArcIt a(mbGraph,n);
        if(a != INVALID){
            OutFile << GroupId[mbGraph.target(a)];
            ++a;
        }
        while( a != INVALID){
            OutFile << "," << GroupId[mbGraph.target(a)];
            ++a;
        }
        OutFile << "]}" << std::endl;
    }
    
    for(ListDigraph::NodeIt n(mbGraph); n!= INVALID; ++n){      
        OutMBFile << "{" << GroupId[n] << ",[";
        OutMBFile << GroupLabel[n] << "]}" << std::endl;
    }
	OutFile << "].";
}

//  ============================================================================
//  Write graph to Erlang node terms
//  ============================================================================

static ERL_NIF_TERM graph_to_terms_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{   
    // read parameter: graphLevel. It can be: module | func | mb.
    string graphLevel = read_graph_level(argv[0],env);
    if(graphLevel == "badarg") 
        return enif_make_badarg(env);
    
    // read parameter: output file
    char file[1024];
    if (enif_get_string(env, argv[1], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // Create output file
    string FileName = (string)file;
    ofstream OutFile;
    OutFile.open (FileName.c_str());
    
    /// Write the graph to the file in Erlang terms
    // Level is module or func
    if((string)graphLevel == "module" || (string)graphLevel == "func"){
        graph_to_terms_fun_or_mod(OutFile, graphLevel);
    }
    // Level is mb
    else{
        // Create mb output file
        string FileName = (string)file + "_mb";
        ofstream OutMBFile;
        OutMBFile.open (FileName.c_str());
        
        graph_to_terms_mb(OutFile, OutMBFile);
        
        OutMBFile.close();
    }
    
    // Close file
    OutFile.close();
    return enif_make_int(env, 0);
}

// Write graph to Erlang terms, when graph level is function or module
void graph_to_terms_fun_or_mod(ofstream &OutFile, const string graphLevel)
{
    vector<string> elements;
    string elem;
    
    for(ListDigraph::NodeIt n(g); n!= INVALID; ++n){
        if(NodesType[n] == (string)graphLevel){
            // The node
			int id = g.id(n);
            OutFile << "[{{" << id << ",[{'$gn'," << NodesType[n] << ",";
            OutFile << NodesId[n] << "}]}, [";
            
            // The nodes which is called by 'n' node
            for(ListDigraph::OutArcIt a(g,n); a != INVALID; ++a){
				int sid = g.id(g.target(a));
                string id = number_to_string(sid);
                string nodeId = number_to_string(NodesId[g.target(a)]);
                elem =  (string)"{" 
                        + id 
                        +(string)",[{'$gn'," 
                        + NodesType[g.target(a)] 
                        + (string)"," 
                        + nodeId 
                        + (string)"}]}";
                elements.push_back(elem);
            }
            
            if(!elements.empty()) OutFile << elements[0];
            for(int i = 1; i < (int)elements.size(); ++i)
                OutFile << "," << elements[i];
            OutFile << "]}]."<< std::endl;
            elements.clear();
        }
    }
}

// Write graph to Erlang terms, when graph level is modulblock
void graph_to_terms_mb(ofstream &OutFile, ofstream &OutMBFile)
{    
    for(ListDigraph::NodeIt n(mbGraph); n!= INVALID; ++n){
        // Modulblock
        OutFile << "[{" << GroupId[n] << ",[";
        
        // The modulblocks which is called by the 'n' node's modulblock
        ListDigraph::OutArcIt a(mbGraph,n);
        while( a != INVALID ){
            Node u = mbGraph.target(a);
            OutFile << GroupId[u];
            ++a;
            if(a!=INVALID) OutFile << ",";
        }                
        OutFile << "]}]."<< std::endl;
    }
    for(ListDigraph::NodeIt n(mbGraph); n!= INVALID; ++n){
        OutMBFile << "[{";
        OutMBFile << GroupId[n];
        OutMBFile << ",[";
        if(GroupElements[n].size() > 0){
            for(int i = 0; i < (int)GroupElements[n].size()-1; ++i){
                OutMBFile << "{'$gn',module," << GroupElements[n][i] << "},";
            }
            OutMBFile << "{'$gn',module,";
            OutMBFile << GroupElements[n][GroupElements[n].size()-1] << "}";
        }
        OutMBFile << "]}]." << std::endl;
    }
}

//  ============================================================================
//  Write graph to Erlang terms
//  ============================================================================
static ERL_NIF_TERM graph_to_terms_two_nif
    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{   
    // read parameter: graphLevel. It can be: module | func | mb.
    string graphLevel = read_graph_level(argv[0],env);
    if(graphLevel == "badarg") 
        return enif_make_badarg(env);
    
    // read parameter: output file
    char file[1024];
    if (enif_get_string(env, argv[1], (char*)file, 
        sizeof(file), ERL_NIF_LATIN1) == 0) 
            return enif_make_badarg(env);
    
    // Create output file
    string FileName = (string)file;
    ofstream OutFile;
    OutFile.open (FileName.c_str());
    
    // Level is func
    if((string)graphLevel == "func"){
        graph_to_terms_two_func(OutFile);
    }
    // Level is module
    else if((string)graphLevel == "module"){
        graph_to_terms_two_module(OutFile);
    }
    // Level is mb
    else if((string)graphLevel == "mb"){       
        graph_to_terms_two_mb(OutFile);     
    }
    // Close file 
    OutFile.close();
    
    return enif_make_int(env, 0);
}

// Write graph to Erlang terms, when graph level is func
void graph_to_terms_two_func(ofstream &OutFile)
{
    // The nodes
    OutFile << "[{nodes,[";
    
    ListDigraph::NodeIt n(g);
	while(n != INVALID)
	{
		if(NodesType[n] == "func" || NodesType[n] == "module")
		{
			int id = g.id(n);
			OutFile << "[{id,\"" << id << "\"},{name,\"";
			OutFile << NodesName[n] << "\"},{type,\"" << NodesType[n] << "\"}],";
		}
		++n;
	}
	OutFile << "[{id,\"-1\"},{name,\"ROOT\"},{type,\"root\"}]";

    // The arcs
    OutFile << "]},{links,[";
    ListDigraph::ArcIt a(g);
	while(a != INVALID)
	{
		if(ArcType[a] == "contain" || ArcType[a] == "funcall" || ArcType[a] == "icallarc")
		{
			int ids = g.id(g.source(a));
			int idt = g.id(g.target(a));
			OutFile << "[{source,\"" << ids;
        	OutFile << "\"},{target,\"" << idt;
			OutFile << "\"},{type,\"" << ArcType[a] << "\"}],";
		}
		++a;
	}
	ListDigraph::NodeIt m(g);
	bool first = true;
	while(m != INVALID)
	{
		if(NodesType[m] == "module")
		{
			if(first) first = false;
			else OutFile << ",";
			int id = g.id(m);
			OutFile << "[{source,\"-1\"},";
        	OutFile << "{target,\"" << id << "\"},";
			OutFile << "{type,\"contain\"}]";
		}
		++m;
	}
    OutFile << "]}].";
}



// Write graph to Erlang terms, when graph level is module
void graph_to_terms_two_module(ofstream &OutFile)
{
    // The nodes
    OutFile << "[{nodes,[";
    
    ListDigraph::NodeIt n(g);
    if(n != INVALID && NodesType[n] == "module"){
		int id = g.id(n);
        OutFile << "[{id,\"" << id << "\"},{name,\"";
        OutFile << NodesName[n] << "\"},{type,\"module\"}]";
        ++n;
    }
    while (n != INVALID){
        if(NodesType[n] == "module"){
			int id = g.id(n);
            OutFile << ",[{id,\"" << id << "\"},{name,\"";
            OutFile << NodesName[n] << "\"},{type,\"module\"}]";
        }
        ++n;
    }

    // The arcs
    OutFile << "]},{links,[";
    ListDigraph::ArcIt a(g);
	bool first = true;
    while(a != INVALID)
	{
		if(ArcType[a] == "modcall" || ArcType[a] == "icallarc")
		{
			if(first) first = false;
			else OutFile << ",";
			int ids = g.id(g.source(a));
			int idt = g.id(g.target(a));
			OutFile << "[{source,\"" << ids;
        	OutFile << "\"},{target,\"" << idt;
			OutFile << "\"},{type,\"" << ArcType[a] << "\"}]";
		}
        ++a;
    }
    OutFile << "]}].";
}

// Write graph to Erlang terms, when graph level is modulblock
void graph_to_terms_two_mb(ofstream &OutFile)
{    
    // The nodes
    OutFile << "[{nodes,[";
    
    ListDigraph::NodeIt n(mbGraph);
    if(n != INVALID){
        OutFile << "[{id,\"" << GroupId[n] << "\"},{name,\"";
        OutFile << GroupLabel[n] << "\"},{type,\"module\"}]";
        ++n;
    }
    while (n != INVALID){
        if(NodesType[n] == "module"){
            OutFile << ",[{id,\"" << GroupId[n] << "\"},{name,\"";
            OutFile << GroupLabel[n] << "\"},{type,\"module\"}]";
        }
        ++n;
    }

    // The arcs
    OutFile << "]},{links,[";
    ListDigraph::ArcIt a(mbGraph);
    if(a != INVALID){
        OutFile << "[{source,\"" << GroupId[mbGraph.source(a)];
        OutFile << "\"},{target,\"" << GroupId[mbGraph.target(a)] << "\"}]";
    }
    while( a != INVALID){
        OutFile << ",[{source,\"" << GroupId[mbGraph.source(a)];
        OutFile << "\"},{target,\"" << GroupId[mbGraph.target(a)] << "\"}]";
        ++a;
    }    
    OutFile << "]}].";
}

//  ============================================================================
//  ERL_NIF
//  ============================================================================
// Make connection between the cpp and Erlang functions 
extern "C" {
    static ErlNifFunc nif_funcs[] = {
        {"add_vertex", 3, add_vertex_nif},
        {"add_edge", 5, add_edge_nif},
        {"new_graph", 0, new_graph_nif},
        {"new_mb_graph", 0, new_mb_graph_nif},
        {"delete_mbgraph", 0, delete_mbgraph_nif},
        {"write_graph", 1, write_graph_nif},
        {"read_graph", 1, read_graph_nif},
        {"write_mb_graph", 1, write_mb_graph_nif},
        {"read_mb_graph", 1, read_mb_graph_nif},
        {"filter_graph", 11, filter_graph_nif},
        {"graph_to_dot", 4, graph_to_dot_nif},
        {"graph_to_terms", 2, graph_to_terms_nif},
        {"graph_to_terms_two", 2, graph_to_terms_two_nif},
        {"graph_to_terms_name", 2, graph_to_terms_name_nif},
    };


    int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) 
    {
        return 0;
    };

    int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
    {
        return 0;
    };

    int upgrade
        (ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
    {
        return 0;
    };

    void unload(ErlNifEnv* env, void* priv)
    {
        return;
    };

    ERL_NIF_INIT(refusr_dep_graph,nif_funcs,load,reload,upgrade,unload)
}
