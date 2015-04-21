// -*- coding: latin-1 -*-

// This file is part of RefactorErl.
//
// RefactorErl is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// RefactorErl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
//
// The Original Code is RefactorErl.
//
// The Initial Developer of the Original Code is Eötvös Loránd University.
// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
// and Ericsson Hungary. All Rights Reserved.

//Author: Mátyás Kuti

#include "referlreceive.h"

ReferlReceive::ReferlReceive(RefErlConnect *ref_erl_connect) :
    QObject(), ref_erl_connect_(ref_erl_connect)
{
}

ReferlReceive::~ReferlReceive()
{
    erl_free_compound(ok);
    erl_free_compound(true_atom);
    erl_free_compound(deny);
    erl_free_compound(fatal);
    erl_free_compound(terminate);
    erl_free_compound(reset_db);
    erl_free_compound(get_envs);
    erl_free_compound(appbase);
    erl_free_compound(include);
    erl_free_compound(files);
    erl_free_compound(errors);
    erl_free_compound(progress);
    erl_free_compound(add);
    erl_free_compound(drop);
    erl_free_compound(completed);
    erl_free_compound(error);
    erl_free_compound(add_dir);
    erl_free_compound(drop_dir);
    erl_free_compound(cat_file);
    erl_free_compound(draw_svg);
    erl_free_compound(print_graph);
    erl_free_compound(get_graph);
    erl_free_compound(skeletons);
    erl_free_compound(db_hash);
    erl_free_compound(save_skeleton);
    erl_free_compound(delete_skeleton);
    erl_free_compound(modify_skeleton);
    erl_free_compound(skeleton_call_format);
    erl_free_compound(queries);
    erl_free_compound(delete_query);
    erl_free_compound(run_query);
    erl_free_compound(running_queries);
    erl_free_compound(modules);
    erl_free_compound(functions);
    erl_free_compound(autocomplete_query);
    erl_free_compound(predef_query);
    erl_free_compound(investigations);
    erl_free_compound(load_investigation);
    erl_free_compound(investigation_memo);
    erl_free_compound(investigation_query);
    erl_free_compound(delete_investigation);
    erl_free_compound(share_investigation);
    erl_free_compound(save_investigation);
    erl_free_compound(dupcode_algorithm_data);
    erl_free_compound(dupcode_search);
    erl_free_compound(previous_dupcode_names);
    erl_free_compound(is_client);
}

void ReferlReceive::Receive()
{
    bool running = true;
    while(running) {
        ErlMessage received;
        int message_type = ref_erl_connect_->ReceiveMessage(5000, &received);
        if(message_type != ERL_TICK || message_type != ERL_ERROR) {
            if( erl_match(terminate, received.msg) ) {
                running = false;
            } else if ( erl_match(fatal, received.msg) ) {
                emit ErrorMessageSignal("Unknown error!");
            } else if( erl_match(deny, received.msg) ) {
                emit ErrorMessageSignal(
                    "The database is busy, try again later. Request denied.");
            } else if( erl_match(is_client, erl_element(1, received.msg) ) ) {
                emit IsClientSignal( erl_match( erl_element(2, received.msg),
                                                true_atom ) );
            } else if( erl_match(reset_db, erl_element(1, received.msg)) ) {
                emit ResetDatabaseSignal( erl_match(ok, erl_element(2, received.msg)) );
            } else if( erl_match(get_envs, erl_element(1, received.msg)) ) {
                GetEnvs(received);
            } else if( erl_match(files, erl_element(1, received.msg)) ) {
                GetFiles(received);
            } else if( erl_match(errors, erl_element(1, received.msg)) ) {
                GetErrors(received);
            } else if( erl_match(modules, erl_element(1, received.msg) ) ) {
                GetModules(received);
            } else if( erl_match(functions, erl_element(1, received.msg) ) ) {
                GetFunctions(received);
            } else if( erl_match(progress, erl_element(1, received.msg)) ) {
                Progress(received);
            } else if( erl_match(add_dir, erl_element(1, received.msg)) ) {
                AddDir(received);
            } else if( erl_match(drop_dir, erl_element(1, received.msg)) ) {
                emit DroppedSignal();
            } else if( erl_match(db_hash, erl_element(1, received.msg)) ) {
                DatabaseHash(received);
            } else if( erl_match(cat_file, erl_element(1, received.msg)) ) {
                CatFile(received);
            } else if( erl_match(draw_svg, erl_element(1, received.msg)) ) {
                DrawSVG(received);
            } else if( erl_match(print_graph, erl_element(1, received.msg)) ) {
                PrintGraph(received);
            } else if( erl_match(get_graph, erl_element(1, received.msg)) ) {
                DrawGraph(received);
            } else if( erl_match(skeletons, erl_element(1, received.msg)) ) {
                GetSkeletons(received);
            } else if( erl_match(save_skeleton, erl_element(1, received.msg)) ) {
                SaveSkeleton(received);
            } else if( erl_match(delete_skeleton, erl_element(1, received.msg)) ) {
                DeleteSkeleton(received);
            } else if( erl_match(modify_skeleton, erl_element(1, received.msg)) ) {
                ModifySkeleton(received);
            } else if( erl_match(skeleton_call_format, erl_element(1, received.msg)) ) {
                SkeletonCallFormat(received);
            } else if( erl_match(queries, erl_element(1, received.msg)) ) {
                Queries(received);
            } else if( erl_match(run_query, erl_element(1, received.msg)) ) {
                RunQuery(received);
            } else if( erl_match(running_queries, erl_element(1, received.msg)) ) {
                RunningQueries(received);
            } else if( erl_match(autocomplete_query, erl_element(1, received.msg)) ) {
                AutoComplete(received);
            } else if( erl_match(dupcode_search, erl_element(1, received.msg) ) ) {
                DupcodeSearch(received);
            } else if( erl_match(dupcode_algorithm_data, erl_element(1, received.msg)) ) {
                DupcodeAlgorithmData(received);
            } else if( erl_match(previous_dupcode_names, erl_element(1, received.msg)) ) {
                DupcodePreviousNames(received);
            } else if( erl_match(predef_query, erl_element(1, received.msg)) ) {
                PredefQueries(received);
            } else if( erl_match(investigations, erl_element(1, received.msg)) ) {
                GetInvestigations(received);
            } else if( erl_match(load_investigation, erl_element(1, received.msg)) ) {
                LoadInvestigation(received);
            } else if( erl_match(investigation_memo, erl_element(1, received.msg)) ) {
                InvestigationMemo(received);
            } else if( erl_match(investigation_query, erl_element(1, received.msg)) ) {
                InvestigationQuery(received);
            } else if( erl_match(delete_investigation, erl_element(1, received.msg)) ) {
                DeleteInvestigation(received);
            } else if( erl_match(share_investigation, erl_element(1, received.msg)) ) {
                ShareInvestigation(received);
            } else if( erl_match(save_investigation, erl_element(1, received.msg)) ) {
                SaveInvestigation(received);
            }
        }
        erl_free_compound(received.msg);
        erl_free_compound(received.from);
        erl_eterm_release();
    }
}

void ReferlReceive::GetEnvs(const ErlMessage &received)
{
    ConfigurationList config_list;
    ETERM *list = erl_element(2, received.msg); //[{appbase|include, string()}]
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        ConfigurationType type;

        type =
            erl_match(appbase, erl_element(1, head) ) ?
            Appbase :
            Include;

        config_list << Configuration( type,
                            QString( erl_iolist_to_string( erl_element(2, head) ) ) );

        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(list);
    emit ConfigurationsSignal(config_list);
}

void ReferlReceive::GetFiles(const ErlMessage &received)
{
    QStringList file_list;
    ETERM *list = erl_element(2, received.msg); //[string()]
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        file_list << QString(erl_iolist_to_string( head ));

        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(head);
    emit FilesSignal(file_list);
}

void ReferlReceive::GetErrors(const ErlMessage &received)
{
    ErrorList error_list;
    ETERM *list = erl_element(2, received.msg);
        //[{string(), integer(), integer(), string()}]
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        char *path, *error;
        int start, end;

        path = erl_iolist_to_string( erl_element(1, head) );
        error = erl_iolist_to_string( erl_element(4, head) );
        start = ERL_INT_VALUE( erl_element(2, head) );
        end = start + ERL_INT_VALUE( erl_element(3, head) ) - 1;

        error_list << QPair<PathError, Position>(
                PathError(path, error), Position(start, end));

        list = erl_tl(list);
        erl_free_compound(head);
        delete path;
        delete error;
    }
    erl_free_compound(head);
    emit ErrorsSignal(error_list);
}

void ReferlReceive::GetModules(const ErlMessage &received)
{
    QStringList module_list;
    ETERM *list = erl_element(2, received.msg); //[string()]
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        module_list << QString( erl_iolist_to_string( head ) );
        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(head);
    emit ModulesSignal(module_list);
}

void ReferlReceive::GetFunctions(const ErlMessage &received)
{
    QStringList function_list;
    ETERM *list = erl_element(2, received.msg);
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        function_list << QString( erl_iolist_to_string( head ) );
        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(head);

    emit FunctionsSignal(function_list);
}

void ReferlReceive::Progress(const ErlMessage &received)
{
    ETERM *progress_msg = erl_element(2, received.msg);
        //{add, filepath, percent, formcount, formmax, speed}
        //{drop, filepath, 1, formcount, formmax, 0}
        //{completed, act, max}
    if ( erl_match(add, erl_element(1, progress_msg)) ) {
        char *path;
        path = erl_iolist_to_string( erl_element(2, progress_msg) );
        int percent;
        percent = static_cast<int>(
                    100 * ERL_FLOAT_VALUE( erl_element(3, progress_msg) ) );
        double speed;
        speed = ERL_FLOAT_VALUE( erl_element(6, progress_msg) );
        emit FileProgressSignal( QString(path), percent, speed);
        delete path;

    } else if( erl_match( drop, erl_element(1, progress_msg)) ) {
        char *path;
        path = erl_iolist_to_string( erl_element(2, progress_msg) );
        int percent;
        percent = static_cast<int>( 100 *
            ( static_cast<double>( ERL_INT_VALUE( erl_element(4, progress_msg) ) )
            /
            static_cast<double>( ERL_INT_VALUE( erl_element(5, progress_msg) ) ) )
        );
        emit FileProgressSignal(QString(path), percent, 0.0);
        delete path;

    } else if( erl_match( completed, erl_element(1, progress_msg) ) ) {
        int percent;
        percent = static_cast<int>( 100 *
            static_cast<double>( ERL_INT_VALUE( erl_element(3, progress_msg) ) )
            /
            static_cast<double>( ERL_INT_VALUE( erl_element(2, progress_msg) ) )
        );
        emit CompletedSignal(percent);
    }

    erl_free_compound(progress_msg);
}

void ReferlReceive::AddDir(const ErlMessage &received)
{
    ETERM *add_dir_msg = received.msg;
    ETERM *success = erl_element(2, add_dir_msg);
    if( erl_match(error, erl_element(1, success)) ) {
        emit ErrorInFileSignal();
    }

    erl_free_compound(add_dir_msg);
    erl_free_compound(success);
}

void ReferlReceive::CatFile(const ErlMessage &received)
{
    ETERM *cat_file_msg = erl_element(3, received.msg);
        //{ok | error, string()}
    char *path, *content;
    path = erl_iolist_to_string( erl_element(2, received.msg) ); //string()
    if( erl_match(ok, erl_element(1, cat_file_msg)) ) {
        content = erl_iolist_to_string( erl_element(2, cat_file_msg) );
        emit CatFileSignal( QString(path), QString(content));
        delete content;
    }
    erl_free_compound(cat_file_msg);
    delete path;
}

void ReferlReceive::DatabaseHash(const ErlMessage &received)
{
    ETERM *hash = erl_element(2, received.msg);
        //{{integer(), integer(), integer()}, integer()}
    int h1, h2, h3, h4;
    h1 = ERL_INT_VALUE( erl_element(1, erl_element(1, hash)));
    h2 = ERL_INT_VALUE( erl_element(2, erl_element(1, hash)));
    h3 = ERL_INT_VALUE( erl_element(3, erl_element(1, hash)));
    h4 = ERL_INT_VALUE( erl_element(2, hash) );
    DbHash db_hash(h1, h2, h3, h4);
    emit DbHashSignal(db_hash);

}

void ReferlReceive::SaveSkeleton(const ErlMessage &received)
{
    ETERM *success = erl_element(2, received.msg); //ok | error
    char *name = erl_iolist_to_string( erl_element(3, received.msg));
    if(erl_match(ok, success)) {
        emit SaveSkeletonSignal(QString(name));
    } else {
        emit ErrorMessageSignal(
            QString( erl_iolist_to_string(erl_element(2, success)) )
        );
    }
    delete name;
    erl_free_compound(success);
}

void ReferlReceive::GetSkeletons(const ErlMessage &received)
{
    SkeletonList skeleton_list;
        //[{string(), string(), string(), integer(), string()}]
    ETERM *list = erl_element(2, received.msg);
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        char *name, *body, *owner, *comment;
        name = erl_iolist_to_string( erl_element(1, head) );
        body = erl_iolist_to_string( erl_element(2, head) );
        owner = erl_iolist_to_string( erl_element(3, head) );
        comment = erl_iolist_to_string( erl_element(5, head) );
        skeleton_list << Skeleton(name, body, owner, comment);
        list = erl_tl(list);
        delete name;
        delete body;
        delete owner;
        delete comment;
        erl_free_compound(head);
    }
    erl_free_compound(head);
    emit SkeletonsSignal(skeleton_list);
}

void ReferlReceive::DeleteSkeleton(const ErlMessage &received)
{
    ETERM *success = erl_element(2, received.msg); //ok | error
    char *name = erl_iolist_to_string( erl_element(3, received.msg) );
        //string()
    if( erl_match(ok, success) ) {
        emit DeleteSkeletonSignal(QString(name));
    } else {
        emit ErrorMessageSignal(
            QString( erl_iolist_to_string(erl_element(2, success)) )
        );
    }
    delete name;
    erl_free_compound(success);
}

void ReferlReceive::ModifySkeleton(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //ok | {error, string()}
    char *name = erl_iolist_to_string( erl_element(3, received.msg) );
        //string()
    if(ERL_IS_TUPLE(result)) {
        emit ErrorMessageSignal(
            QString( erl_iolist_to_string(erl_element(2, result)) )
        );
    } else {
        ModifySkeletonSignal(QString(name));
    }
    delete name;
    erl_free_compound(result);
}

void ReferlReceive::SkeletonCallFormat(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg);
        //string() | {error, string()}
    if(ERL_IS_TUPLE(result)) {
        emit ErrorMessageSignal("The skeleton doesn't exist!");
    } else {
        QString format( erl_iolist_to_string( result ) );
        emit SkeletonCallFormatSignal(format);
    }
    erl_free_compound(result);
}

void ReferlReceive::Queries(const ErlMessage &received)
{
    QueryList query_list;
    ETERM *list = erl_element(2, received.msg);
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        char *query_str, *file;
        int position;
        list = erl_tl(list);
        query_str = erl_iolist_to_string( erl_element(1, head) );
        if( ERL_IS_ATOM( erl_element(2, head) ) ) {
            file = (char*)"";
        } else {
            file = erl_iolist_to_string( erl_element(2, head) );
        }
        if( ERL_IS_ATOM( erl_element(3, head) ) ) {
            position = -1;
        } else {
            position = ERL_INT_VALUE( erl_element(3, head) );
        }
        query_list << Query(query_str, file, position);
        erl_free_compound(head);
    }
    erl_free_compound(head);
    emit QueriesSignal(query_list);
}

void ReferlReceive::RunningQueries(const ErlMessage &received)
{
    QueryIdList queries; //[{integer(), string()}]
    ETERM *list = erl_element(2, received.msg);
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        int qid = ERL_INT_VALUE( erl_element(1, head) );
        QString qstr( erl_iolist_to_string( erl_element(2, head) ) );
        queries << QueryId(qid, qstr);
        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(head);
    emit RunningQueriesSignal(queries);
}

void ReferlReceive::RunQuery(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //[term()]
    char *query_str = erl_iolist_to_string( erl_element(3, received.msg) );
    if( ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result)) ) {
        QString error_message;
        error_message = QString( erl_iolist_to_string(erl_element(2, result)) );
        emit ErrorMessageSignal( error_message );
    } else {
        QueryResult query_result;
        query_result.SetResult(
                    QueryElem( FilePosition("", -1, -1) , QString(query_str) ) );
        ProcessList(query_result, result);
        emit RunQuerySignal( query_result );
    }
    delete query_str;
    erl_free_compound(result);
}

void ReferlReceive::ProcessList(QueryResult &result, ETERM *list)
{
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        QueryElem elem = ProcessQueryElem( erl_element(1, head) );
        QueryResult elem_result;
        elem_result.SetResult(elem);
        if(ERL_TUPLE_SIZE(head) == 2) {
            ProcessList(elem_result, erl_element(2, head));
        } else { //5
            QueryResult value_result;
            char *name;
            if( ERL_IS_ATOM(erl_element(3, head)) ) {
                name = ERL_ATOM_PTR( erl_element(3, head) );
            } else {
                name = erl_iolist_to_string( erl_element(3, head) );
            }
            ETERM *value = erl_element(4, head);
            QString value_str;
            if( ERL_IS_INTEGER(value) ) {
                value_str = QString::number(ERL_INT_VALUE(value));
            } else if( ERL_IS_ATOM(value) ) {
                value_str = QString( ERL_ATOM_PTR(value) );
            } else {
                value_str = QString( erl_iolist_to_string(value) );
            }
            QueryElem child_elem(FilePosition("", -1, -1),
                                 QString(name) + ": " + value_str);
            value_result.SetResult(child_elem);
            elem_result.AddChild(value_result);
            ProcessList(value_result, erl_element(5, head));
        }
        result.AddChild(elem_result);
        list = erl_tl(list);
        erl_free_compound(head);
    }
    erl_free_compound(head);
}

FilePosition ReferlReceive::ProcessPosition(const ETERM *position_tuple)
{
    FilePosition position("", -1, -1);

    if(ERL_IS_TUPLE(position_tuple)) {
        char *file = erl_iolist_to_string( erl_element(1, position_tuple) );
        int start_pos, end_pos;
        start_pos = ERL_INT_VALUE( erl_element(2, position_tuple) );
        end_pos = ERL_INT_VALUE( erl_element(3, position_tuple) );
        position = FilePosition(QString(file), start_pos, end_pos);
        delete file;
    }

    return position;
}

QueryElem ReferlReceive::ProcessQueryElem(const ETERM *query_elem)
{
    QueryElem elem(FilePosition("", -1, -1), "");
    if(ERL_IS_TUPLE(query_elem)) {
        char* elem_name = erl_iolist_to_string( erl_element(2, query_elem) );
        elem = QueryElem( ProcessPosition(erl_element(1, query_elem)),
                          QString(elem_name) );
        delete elem_name;
    }
    return elem;
}

void ReferlReceive::AutoComplete(const ErlMessage &received)
{
    ETERM *list = erl_element(2, received.msg); //[{string(), string()}]
    ETERM *head;
    QStringList autocomplete_list;
    QStringList completed_list;
    while( (head = erl_hd(list)) != NULL ) {
        autocomplete_list << QString( erl_iolist_to_string( erl_element(1, head) ) );
        completed_list << QString( erl_iolist_to_string( erl_element(2, head) ) );
        list = erl_tl(list);
        erl_free_compound(head);
    }
    emit AutoCompleteSignal(autocomplete_list, completed_list);
    erl_free_compound(head);
}

void ReferlReceive::PredefQueries(const ErlMessage &received)
{
    ETERM *list = erl_element(2, received.msg); //[{string(), string()}]
    ETERM  *head;
    QList<PredefQuery> predef_list;
    while( (head = erl_hd(list)) != NULL ) {
        predef_list << PredefQuery( erl_iolist_to_string(erl_element(1, head)),
                                    erl_iolist_to_string(erl_element(2, head)) );
        list = erl_tl(list);
        erl_free_compound(head);
    }
    emit PredefQuerySignal(predef_list);
    erl_free_compound(head);
}

void ReferlReceive::DrawSVG(const ErlMessage &received)
{
    //string() | {error, string()}
    if( ERL_IS_TUPLE( erl_element(2, received.msg) ) ) {
        ETERM *error_msg = erl_element(2, received.msg);
        QString error;
        error = QString( erl_iolist_to_string( erl_element(2, error_msg) ) );
        emit ErrorMessageSignal(error);
        erl_free_compound(error_msg);
    } else {
        emit SVGSignal(
            QString(
                erl_iolist_to_string( erl_element(2, received.msg) )
            )
        );
    }
}

void ReferlReceive::PrintGraph(const ErlMessage &received)
{
    //string() | {error, string()}
    if( ERL_IS_TUPLE( erl_element(2, received.msg) ) ) {
        ETERM *error_msg = erl_element(2, received.msg);
        QString error;
        error = QString( erl_iolist_to_string( erl_element(2, error_msg) ) );
        emit ErrorMessageSignal(error);
        erl_free_compound(error_msg);
    } else {
        emit GraphSignal(
            QString(
                erl_iolist_to_string( erl_element(2, received.msg) )
            )
        );
    }
}

void ReferlReceive::DrawGraph(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg);
    if( ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result))) {
        ETERM *error_msg = erl_element(2, received.msg);
        QString error;
        error = QString( erl_iolist_to_string( erl_element(2, error_msg) ) );
        emit ErrorMessageSignal(error);
        erl_free_compound(error_msg);
    } else {
        DependencyGraph *graph = new DependencyGraph();
        ETERM *nodes = erl_element(1, result);
        ETERM *edges = erl_element(2, result);

        ETERM *head;
        while( (head = erl_hd(nodes)) != NULL) {
            QString id = QString(erl_iolist_to_string( erl_element(1, head) ));
            QString label = QString(erl_iolist_to_string( erl_element(2, head) ));
            QString level_str = QString(erl_iolist_to_string( erl_element(3, head) ));
            DependencyLevel level;
            if(level_str == QString("module")) {
                level = Module;
            } else if(level_str == QString("func")) {
                level = Function;
            }

            Node *node = new Node(id, 0, 0, label, "", level);
            graph->AddNode(node);
            nodes = erl_tl(nodes);
        }

        while( (head = erl_hd(edges)) != NULL) {
            QString from = QString(erl_iolist_to_string( erl_element(1, head) ));
            QString to = QString(erl_iolist_to_string( erl_element(2, head) ));

            graph->AddEdge(from, to);

            edges = erl_tl(edges);
        }
        emit DrawGraphSignal(graph);
    }
}

void ReferlReceive::DupcodeSearch(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg);
    if( ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result)) ) {
        QString error_message;
        error_message = QString( erl_iolist_to_string(erl_element(2, result)) );
        emit ErrorMessageSignal( error_message );
        return;
    }
    ETERM *group_list = erl_element(3, result);
    QVector<DupcodeGroup> results;
    char *name = ERL_ATOM_PTR(erl_element(1, result));
    char *path = erl_iolist_to_string(erl_element(2, result));
    QString name_str(name == NULL ? "" : name);
    QString path_str(path == NULL ? "" : path);

    ETERM *head_list; //List!!!
    while( (head_list = erl_hd(group_list)) != NULL ) {
        ETERM *head;
        QVector<DupcodeElem> group;
        while( (head = erl_hd(head_list)) != NULL ) {
            ETERM *temp;
            temp = erl_hd(head);
            char* file_path = erl_iolist_to_string( erl_element(2, temp) );
            head = erl_tl(head);
            erl_free_compound(temp);

            temp = erl_hd(head);
            int start = ERL_INT_VALUE( erl_element(2, temp) );
            head = erl_tl(head);
            erl_free_compound(temp);

            temp = erl_hd(head);
            int end = ERL_INT_VALUE( erl_element(2, temp) );
            head = erl_tl(head);
            erl_free_compound(temp);

            DupcodeElem elem( file_path, start, end );
            group.push_back( elem );

            head_list = erl_tl(head_list);
            erl_free_compound(head);
            delete file_path;
        }
        erl_free_compound(head);
        group_list = erl_tl(group_list);
        erl_free_compound(head_list);
        results.push_back(group);
    }
    erl_free_compound(head_list);

    emit DupcodeSearchSignal(name_str, path_str, results);

}

void ReferlReceive::DupcodeAlgorithmData(const ErlMessage &received)
{
    ETERM *algorithm_list = erl_element(2, received.msg);

    QVector<DuplicatedCodeAlgorithm> algorithms;

    ETERM *head;
    while( (head = erl_hd(algorithm_list)) != NULL ) {
        char* label = ERL_ATOM_PTR( erl_element(1, head) );
        char* name = erl_iolist_to_string( erl_element(2, head) );
        ETERM *param_list = erl_element(3, head);
        ETERM *param_info;

        QList<Parameter> parameters;

        while( (param_info = erl_hd(param_list)) != NULL ) {
            ETERM *param = erl_element(2, param_info);
            ETERM *temp;
            temp = erl_hd(param);
            char* param_key = ERL_ATOM_PTR( erl_element(2, temp) );
            QString param_key_str(param_key);
            param = erl_tl(param);
            erl_free_compound(temp);

            temp = erl_hd(param);
            char* param_label = erl_iolist_to_string( erl_element(2, temp) );
            QString param_label_str(param_label);
            param = erl_tl(param);
            erl_free_compound(temp);

            temp = erl_hd(param);
            ETERM *DEFAULT_VALUE = erl_copy_term( erl_element(2, temp) );
            param = erl_tl(param);

            temp = erl_hd(param);
            char* param_type = ERL_ATOM_PTR( erl_element(2, temp) );
            QString param_type_str(param_type);
            param = erl_tl(param);

            if(param_type_str == "string") {
                char *val = erl_iolist_to_string(DEFAULT_VALUE);
                Parameter p(QVariant(QString(val == NULL ? "" : val)),
                                param_key_str, param_label_str, param_type_str);
                parameters.push_back(p);
            } else if(param_type_str == "atom") {
                char *val = ERL_ATOM_PTR(DEFAULT_VALUE);
                Parameter p(QVariant(QString(val == NULL ? "" : val)),
                                param_key_str, param_label_str, param_type_str, true);
                parameters.push_back(p);
            } else if(param_type_str == "integer") {
                Parameter p(QVariant(ERL_INT_VALUE(DEFAULT_VALUE)),
                                param_key_str, param_label_str, param_type_str);
                parameters.push_back(p);
            } else if(param_type_str == "float") {
                Parameter p(QVariant(ERL_FLOAT_VALUE(DEFAULT_VALUE)),
                                param_key_str, param_label_str, param_type_str);
                parameters.push_back(p);
            } else if(param_type_str == "boolean") {
                Parameter p(QVariant(erl_match(true_atom, DEFAULT_VALUE)),
                                param_key_str, param_label_str, param_type_str);
                parameters.push_back(p);
            } else if(param_type_str == "atoms") {
                Parameter p(QVariant(QStringList()), param_key_str, param_label_str, param_type_str, true);
                parameters.push_back(p);
            } else if(param_type_str == "enum") {
                temp = erl_hd(param);
                ETERM *options_list = erl_element(2, temp);
                param = erl_tl(param);

                temp = erl_hd(param);
                char* enum_type = ERL_ATOM_PTR(erl_element(2, temp));
                QString enum_type_str = QString(enum_type);

                QStringList options;
                ETERM *option;
                while( (option = erl_hd(options_list)) != NULL ) {

                    if(enum_type_str == "string") {
                        char *val = erl_iolist_to_string(option);
                        options.push_back( val == NULL ? "" : val );
                    } else if(enum_type_str == "atom") {
                        char *val = ERL_ATOM_PTR(option);
                        options.push_back( val == NULL ? "" : val );
                    }

                    erl_free_compound(option);
                    options_list = erl_tl(options_list);
                }

                Parameter p(QVariant(options), param_key_str, param_label_str, param_type_str, enum_type_str == "atom", true);
                parameters.push_back(p);
            }

            param_list = erl_tl(param_list);
            erl_free_compound(param_info);
        }
        algorithms.push_back( DuplicatedCodeAlgorithm(label, name, parameters) );
        algorithm_list = erl_tl(algorithm_list);
        erl_free_compound(head);
    }
    emit DupcodeAlgorithmsSignal(algorithms);
}

void ReferlReceive::DupcodePreviousNames(const ErlMessage &received)
{
    ETERM *names_list = erl_element(2, received.msg);
    ETERM *head;
    QStringList names;
    while( (head = erl_hd(names_list)) != NULL ) {
        names << QString( ERL_ATOM_PTR(head) );
        names_list = erl_tl(names_list);
    }
    erl_free_compound(head);
    emit DupcodePreviousNamesSignal(names);
}

void ReferlReceive::GetInvestigations(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //[{string(), [string()]}]
    InvestigationList investigation_list;
    ETERM *head;
    while( (head = erl_hd(result)) != NULL ) {
        QString name( erl_iolist_to_string( erl_element(1, head) ) );
        QStringList users;
        ETERM *user_list = erl_element(2, head);
        ETERM* user;
        while( (user = erl_hd(user_list)) != NULL ) {
            users << QString( erl_iolist_to_string(user) );
            user_list = erl_tl(user_list);
            erl_free_compound(user);
        }
        erl_free_compound(user);

        result = erl_tl(result);
        erl_free_compound(head);
        investigation_list << Investigation(name, users);
    }
    erl_free_compound(head);
    emit InvestigationsSignal(investigation_list);
}

void ReferlReceive::LoadInvestigation(const ErlMessage &received)
{
    InvestigationGraph graph;
    QList<QPair<QString, QString> > edges;
    ETERM *result = erl_element(2, received.msg);
    if(ERL_IS_TUPLE(result) && erl_match( erl_element(1, result), error )) {
        emit ErrorMessageSignal(
                    QString( erl_iolist_to_string( erl_element(2, result) ) ) );
        return;
    }
    ETERM *inv_name = erl_element(2, result);
    char *inv_name_str = erl_iolist_to_string(inv_name);
    if(inv_name_str != 0) {
        graph.SetName( QString( inv_name_str ) );
    }
    delete inv_name_str;

    ETERM *hash = erl_element(3, result);
    int h1, h2, h3, h4;
    h1 = ERL_INT_VALUE( erl_element(1, erl_element(1, hash)));
    h2 = ERL_INT_VALUE( erl_element(2, erl_element(1, hash)));
    h3 = ERL_INT_VALUE( erl_element(3, erl_element(1, hash)));
    h4 = ERL_INT_VALUE( erl_element(2, hash) );
    DbHash db_hash(h1, h2, h3, h4);
    graph.SetDbHash(db_hash);

    ETERM *list = erl_element(5, result);
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        ETERM *pid = erl_element(1, head);
        ETERM *parent = erl_element(2, head);
        ETERM *invnode = erl_element(3, head);
        ETERM *children = erl_element(4, head);
        ETERM *name = erl_element(2, invnode);
        ETERM *shown = erl_element(3, invnode);
        ETERM *node = erl_element(4, invnode);
        ETERM *text = erl_element(5, invnode);
        ETERM *edge_label = erl_element(6, invnode);
        ETERM *memo = erl_element(7, invnode);
        ETERM *file_position = erl_element(8, invnode);
        ETERM *display_position = erl_element(9, invnode);
        bool is_memo = erl_match( true_atom, memo );
        QString gn, type;
        int num;
        if(ERL_IS_TUPLE(node)) {
            gn = QString( ERL_ATOM_PTR(erl_element(1, node)) );
            type = QString( ERL_ATOM_PTR(erl_element(2, node)) );
            num = ERL_INT_VALUE(erl_element(3, node));
        } else {
            gn = "memo";
            num = -1;
        }
        GraphNode gnode( gn, type, num );
        InvestigationNode *new_node =
                new InvestigationNode(
                    QString(erl_iolist_to_string(pid)), gnode, is_memo);

        if(name == NULL) {
            new_node->SetName(QString());
        } else {
            new_node->SetName(QString(erl_iolist_to_string(name)));
        }
        new_node->SetParent( QString( erl_iolist_to_string(parent) ) );
        new_node->SetShown( erl_match( true_atom, shown ) );
        new_node->SetText( QString( erl_iolist_to_string(text) ) );
        new_node->SetEdgeLabel( QString( erl_iolist_to_string(edge_label) ) );
        new_node->SetX( ERL_INT_VALUE( erl_element(1, display_position) ) );
        new_node->SetY( ERL_INT_VALUE( erl_element(2, display_position) ) );
        char *path = erl_iolist_to_string( erl_element(1, file_position) );
        if(path == NULL) {
            new_node->SetPath( QString() );
        } else {
            new_node->SetPath( QString(path) );
        }
        if(is_memo) {
            new_node->SetLineNumber( -1 );
            new_node->SetOffset( -1 );
            new_node->SetLabel( QString() );
            new_node->SetStartPosition( -1 );
            new_node->SetEndPosition( -1 );
        } else {
            new_node->SetLineNumber(
                        ERL_INT_VALUE( erl_element(3, file_position) ) );
            new_node->SetOffset(
                        ERL_INT_VALUE( erl_element(2, file_position) ) );
            new_node->SetLabel(
                        QString(erl_iolist_to_string( erl_element(5, file_position) )) );
            new_node->SetStartPosition(
                        ERL_INT_VALUE( erl_element(1, erl_element(4, file_position)) ) );
            new_node->SetEndPosition(
                        ERL_INT_VALUE( erl_element(2, erl_element(4, file_position)) ) );
        }

        ETERM *child;
        while( (child = erl_hd(children)) != NULL ) {
            edges <<
                 QPair<QString, QString>(
                        QString( erl_iolist_to_string(pid) ),
                        QString( erl_iolist_to_string(child) ) );
            children = erl_tl(children);
            erl_free_compound(child);
        }
        erl_free_compound(child);

        list = erl_tl(list);
        erl_free_compound(head);
        graph.AddNode(new_node);
    }
    erl_free_compound(head);
    typedef QPair<QString, QString> InvEdge;
    for(InvEdge edge : edges) {
        graph.AddEdge(edge.first, edge.second);
    }

    emit LoadInvestigationSignal(graph);
}

void ReferlReceive::InvestigationMemo(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //{string(), string()}
    char *id = erl_iolist_to_string( erl_element(1, result) );
    char *parent = erl_iolist_to_string( erl_element(2, result) );
    QString id_str(id);
    QString parent_str(parent);
    delete id;
    delete parent;

    emit InvestigationMemoSignal(id_str, parent_str);
}

void ReferlReceive::InvestigationQuery(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //[term()]
    if(ERL_IS_TUPLE(result) && erl_match( erl_element(1, result), error )) {
        emit ErrorMessageSignal(
                    QString( erl_iolist_to_string( erl_element(2, result) ) ) );
        return;
    }
    InvestigationNodeList nodes;
    ETERM *list = result;
    ETERM *head;
    while( (head = erl_hd(list)) != NULL ) {
        ETERM *pid = erl_element(1, head);
        ETERM *parent = erl_element(2, head);
        ETERM *invnode = erl_element(3, head);
        //ETERM *children = erl_element(4, head);
        ETERM *name = erl_element(2, invnode);
        ETERM *shown = erl_element(3, invnode);
        ETERM *node = erl_element(4, invnode);
        ETERM *text = erl_element(5, invnode);
        ETERM *edge_label = erl_element(6, invnode);
        ETERM *memo = erl_element(7, invnode);
        ETERM *file_position = erl_element(8, invnode);
        ETERM *display_position = erl_element(9, invnode);
        bool is_memo = erl_match( true_atom, memo );
        QString gn, type;
        int num;
        if(ERL_IS_TUPLE(node)) {
            gn = QString( ERL_ATOM_PTR(erl_element(1, node)) );
            type = QString( ERL_ATOM_PTR(erl_element(2, node)) );
            num = ERL_INT_VALUE(erl_element(3, node));
        } else {
            gn = "memo";
            num = -1;
        }
        GraphNode gnode( gn, type, num );
        InvestigationNode *new_node =
                new InvestigationNode(
                    QString(erl_iolist_to_string(pid)), gnode, is_memo);
        if(name == NULL) {
            new_node->SetName(QString());
        } else {
            new_node->SetName(QString(erl_iolist_to_string(name)));
        }
        new_node->SetParent( QString( erl_iolist_to_string(parent) ) );
        new_node->SetShown( erl_match( true_atom, shown ) );
        new_node->SetText( QString( erl_iolist_to_string(text) ) );
        new_node->SetEdgeLabel( QString( erl_iolist_to_string(edge_label) ) );
        new_node->SetX( ERL_INT_VALUE( erl_element(1, display_position) ) );
        new_node->SetY( ERL_INT_VALUE( erl_element(2, display_position) ) );
        char *path = erl_iolist_to_string( erl_element(1, file_position) );
        if(path == NULL) {
            new_node->SetPath( QString() );
        } else {
            new_node->SetPath( QString(path) );
        }
        if(is_memo) {
            new_node->SetLineNumber( -1 );
            new_node->SetOffset( -1 );
            new_node->SetLabel( QString() );
            new_node->SetStartPosition( -1 );
            new_node->SetEndPosition( -1 );
        } else {
            new_node->SetLineNumber(
                        ERL_INT_VALUE( erl_element(3, file_position) ) );
            new_node->SetOffset(
                        ERL_INT_VALUE( erl_element(2, file_position) ) );
            new_node->SetLabel(
                        QString(erl_iolist_to_string( erl_element(5, file_position) )) );
            new_node->SetStartPosition(
                        ERL_INT_VALUE( erl_element(1, erl_element(4, file_position)) ) );
            new_node->SetEndPosition(
                        ERL_INT_VALUE( erl_element(2, erl_element(4, file_position)) ) );
        }

        list = erl_tl(list);
        erl_free_compound(head);
        nodes.push_back(new_node);
    }
    erl_free_compound(head);
    emit InvestigationQuerySignal(nodes);
}

void ReferlReceive::DeleteInvestigation(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //string(), {error, string()}
    if(ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result))) {
        emit ErrorMessageSignal(
                    QString( erl_iolist_to_string( erl_element(2, result) ) ) );
        return;
    }
    emit DeleteInvestigationSignal(
                QString( erl_iolist_to_string(erl_element(3, received.msg)) ) );
    erl_free_compound(result);
}

void ReferlReceive::ShareInvestigation(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //string(), {error, string()}
    if(ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result))) {
        emit ErrorMessageSignal(
                    QString( erl_iolist_to_string( erl_element(2, result) ) ) );
        return;
    }
    emit ShareInvestigationSignal(
                QString( erl_iolist_to_string(erl_element(3, received.msg)) ),
                QString( erl_iolist_to_string(erl_element(4, received.msg)) ) );
    erl_free_compound(result);
}

void ReferlReceive::SaveInvestigation(const ErlMessage &received)
{
    ETERM *result = erl_element(2, received.msg); //string(), {error, string()}
    if(ERL_IS_TUPLE(result) && erl_match(error, erl_element(1, result))) {
        emit ErrorMessageSignal(
                    QString( erl_iolist_to_string( erl_element(2, result) ) ) );
        return;
    }
    emit SaveInvestigationSignal(
                QString( erl_iolist_to_string(erl_element(3, received.msg)) ) );
    erl_free_compound(result);
}
