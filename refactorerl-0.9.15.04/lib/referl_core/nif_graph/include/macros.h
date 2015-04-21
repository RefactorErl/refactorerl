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

#ifndef MACROS_H
#define	MACROS_H

#include <iostream>


//-----------------------------------------------------------------------------
// LOOPS

#define LOOP(CONTAINER, VAR) \
    size_type __LOOP__CONTAINER__size__ = CONTAINER.size();\
    for(size_type VAR = 0; VAR < __LOOP__CONTAINER__size__; ++VAR)

#define BACK_LOOP(CONTAINER, VAR) \
    size_type VAR = CONTAINER.size(); \
    while((VAR--) >= 1)

#define COND_LOOP(CONTAINER, VAR, COND) \
    size_type __COND__LOOP__CONTAINER__size__ = CONTAINER.size();\
    for(size_type VAR = 0; VAR < __COND__LOOP__CONTAINER__size__; ++VAR) if(COND)

#define COND_BACK_LOOP(CONTAINER, VAR, COND) \
    size_type VAR = CONTAINER.size(); \
    while((VAR--) >= 1) if(COND)


//-----------------------------------------------------------------------------
// ITERATORS

#define ITERATE(TYPE, VAR, CONTAINER) \
    TYPE::iterator __ITERATE__CONTAINER__end__ = CONTAINER.end(); \
    for (TYPE::iterator VAR = CONTAINER.begin(); \
         VAR != __ITERATE__CONTAINER__end__; ++VAR)

#define BACK_ITERATE(TYPE, VAR, CONTAINER) \
    TYPE::reverse_iterator __BACK__ITERATE__CONTAINER__end__ = CONTAINER.rend(); \
    for (TYPE::reverse_iterator VAR = CONTAINER.rbegin(); \
         VAR != __BACK__ITERATE__CONTAINER__end__; ++VAR)

#define COND_ITERATE(TYPE, VAR, CONTAINER, COND) \
    TYPE::iterator __COND__ITERATE__CONTAINER__end__ = CONTAINER.end(); \
    for (TYPE::iterator VAR = (CONTAINER).begin(); \
         VAR != __COND__ITERATE__CONTAINER__end__; ++VAR) if (COND(VAR->first))

#define COND_BACK_ITERATE(TYPE, VAR, CONTAINER, COND) \
    TYPE::reverse_iterator __COND__BACK__ITERATE__CONTAINER__end__ = CONTAINER.rend(); \
    for (TYPE::reverse_iterator VAR = (CONTAINER).rbegin(); \
         VAR != __COND__BACK__ITERATE__CONTAINER__end__; ++VAR) if (COND(VAR->first))

#define CONST_ITERATE(TYPE, VAR, CONTAINER) \
    TYPE::const_iterator __COND__ITERATE__CONTAINER__end__ = CONTAINER.end(); \
    for (TYPE::const_iterator VAR = CONTAINER.begin(); \
         VAR != __COND__ITERATE__CONTAINER__end__; ++VAR)

#define CONST_BACK_ITERATE(TYPE, VAR, CONTAINER) \
    TYPE::const_reverse_iterator __CONST__BACK__ITERATE__CONTAINER__end__ = CONTAINER.rend(); \
    for (TYPE::const_reverse_iterator VAR = CONTAINER.rbegin(); \
         VAR != __CONST__BACK__ITERATE__CONTAINER__end__; ++VAR)

#define COND_CONST_ITERATE(TYPE, VAR, CONTAINER, COND) \
    TYPE::const_iterator __COND__CONST__ITERATE__CONTAINER__end__ = CONTAINER.end(); \
    for (TYPE::const_iterator VAR = (CONTAINER).begin(); \
         VAR != __COND__CONST__ITERATE__CONTAINER__end__; ++VAR) if (COND(VAR->first))

#define COND_CONST_BACK_ITERATE(TYPE, VAR, CONTAINER, COND) \
    TYPE::const_reverse_iterator __COND__CONST__BACK__ITERATE__CONTAINER__end__ = CONTAINER.rend(); \
    for (TYPE::const_reverse_iterator VAR = (CONTAINER).rbegin(); \
         VAR != __COND__CONST__BACK__ITERATE__CONTAINER__end__; ++VAR) \
         if (COND(VAR->first))


//-----------------------------------------------------------------------------
// Global constants

#define GRAPHS_DIR_NAME     "graphs"
#define DB_FILE             "database"
#define DB_FILE_BAK         "database.b"
#define BEFORE_TRANS_FILE   "before_transformation"


//-----------------------------------------------------------------------------
// NIF Return values

#define OK(ENV)          str2atom(ENV, "ok")
#define NONE(ENV)        str2atom(ENV, "none")
#define NO_BACKUP(ENV)   str2atom(ENV, "no_backup")
#define TRUE(ENV)        str2atom(ENV, "true")
#define FALSE(ENV)       str2atom(ENV, "false")
#define BADARG(ENV)      enif_make_badarg(ENV)
#define INVALID_NAME(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "invalid_name"), \
                             str2atom(ENV, NAME))
#define INVALID_BACKUP(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "invalid_backup"), \
                             str2atom(ENV, NAME))
#define CORRUPTED_BACKUP(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "corrupted_backup"), \
                             str2atom(ENV, NAME))
#define GRAPH_ALREADY_EXIST(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "graph_already_exist"), \
                             str2atom(ENV, NAME))
#define GRAPH_NOT_EXIST(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "graph_not_exist"), \
                             str2atom(ENV, NAME))
#define GRAPH_LOAD_FAIL(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "graph_load_fail"), \
                             str2atom(ENV, NAME))
#define GRAPH_IS_IN_USE(ENV, NAME) \
            enif_make_tuple2(ENV, str2atom(ENV, "graph_is_in_use"), \
                             str2atom(ENV, NAME))
#define BAD_NODE(ENV, NODE_ID)  enif_make_tuple2(ENV, str2atom(ENV, "bad_node"), \
                                                 gnode_id2term(ENV, NODE_ID))
#define PROTECTED_NODE(ENV, NODE_ID) \
                enif_make_tuple2(ENV, str2atom(ENV, "protected_node"), \
                                 gnode_id2term(ENV, NODE_ID))
#define PROTECTED_LINK(ENV, NODE_ID1, LINK, NODE_ID2) \
                enif_make_tuple4(ENV, str2atom(ENV, "protected_link"), \
                                 gnode_id2term(ENV, NODE_ID1), \
                                 str2atom(ENV, LINK), \
                                 gnode_id2term(ENV, NODE_ID2))
#define NOT_EXISTS(ENV, NODE_ID1, LINK, NODE_ID2)  \
            enif_make_tuple4(ENV, str2atom(ENV, "not_exists"), \
                             gnode_id2term(ENV, NODE_ID1), str2atom(ENV,LINK), \
                             gnode_id2term(ENV, NODE_ID2))


//-----------------------------------------------------------------------------
// Others

#define CHECK_NAME(ENV, DIR, NAME) \
		if(contains(NAME, directory::separator())) { \
            return INVALID_NAME(ENV, NAME); \
        } \
        if(DIR.exist(NAME)) { \
            return GRAPH_ALREADY_EXIST(ENV, NAME); \
        }


#define DEFINE_EXCEPTION(EXCEPTION_NAME, MESSAGE) \
    class EXCEPTION_NAME : public exception_helper { \
        public: \
            EXCEPTION_NAME(const std::string& msg = MESSAGE) : \
                                                exception_helper(msg) { } \
    };

#define IS_WINDOWS    (defined _WIN32 || defined _WIN64)

#endif
