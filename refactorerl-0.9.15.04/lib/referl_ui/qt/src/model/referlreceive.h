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

#ifndef REFERLRECEIVE_H
#define REFERLRECEIVE_H

#include <QObject>
#include <QStringList>
#include <QString>
#include <QList>
#include <QLineEdit>
#include <QComboBox>
#include <QSpinBox>
#include <QDoubleSpinBox>
#include <QCheckBox>

#include "referlconnect.h"
#include "common.h"
#include "queryresult.h"
#include "investigations/investigationgraph.h"
#include "model/dependencygraph/dependencygraph.h"

class ReferlReceive : public QObject
{
        Q_OBJECT

    private:
        //Pointer to the RefErlConnect class which allows message receiveing
        RefErlConnect *ref_erl_connect_ = NULL;

        //Atom patterns used to identify the messages and their purpose
        ETERM *ok = erl_mk_atom("ok");
        ETERM *true_atom = erl_mk_atom("true");
        ETERM *deny = erl_mk_atom("deny");
        ETERM *fatal = erl_mk_atom("fatal");
        ETERM *terminate = erl_mk_atom("terminate");
        ETERM *reset_db = erl_mk_atom("reset_db");
        ETERM *get_envs = erl_mk_atom("get_envs");
        ETERM *appbase = erl_mk_atom("appbase");
        ETERM *include = erl_mk_atom("include");
        ETERM *files = erl_mk_atom("files");
        ETERM *errors = erl_mk_atom("errors");
        ETERM *progress = erl_mk_atom("progress");
        ETERM *add = erl_mk_atom("add");
        ETERM *drop = erl_mk_atom("drop");
        ETERM *completed = erl_mk_atom("completed");
        ETERM *error = erl_mk_atom("error");
        ETERM *add_dir = erl_mk_atom("add_dir");
        ETERM *drop_dir = erl_mk_atom("drop_dir");
        ETERM *cat_file = erl_mk_atom("cat_file");
        ETERM *draw_svg = erl_mk_atom("draw_svg");
        ETERM *print_graph = erl_mk_atom("print_graph");
        ETERM *get_graph = erl_mk_atom("get_graph");
        ETERM *skeletons = erl_mk_atom("skeletons");
        ETERM *db_hash = erl_mk_atom("db_hash");
        ETERM *save_skeleton = erl_mk_atom("save_skeleton");
        ETERM *delete_skeleton = erl_mk_atom("delete_skeleton");
        ETERM *modify_skeleton = erl_mk_atom("modify_skeleton");
        ETERM *skeleton_call_format = erl_mk_atom("skeleton_call_format");
        ETERM *queries = erl_mk_atom("queries");
        ETERM *delete_query = erl_mk_atom("delete_query");
        ETERM *run_query = erl_mk_atom("run_query");
        ETERM *running_queries = erl_mk_atom("running_queries");
        ETERM *modules = erl_mk_atom("modules");
        ETERM *functions = erl_mk_atom("functions");
        ETERM *autocomplete_query = erl_mk_atom("autocomplete_query");
        ETERM *predef_query = erl_mk_atom("predef_query");
        ETERM *investigations = erl_mk_atom("investigations");
        ETERM *load_investigation = erl_mk_atom("load_investigation");
        ETERM *investigation_memo = erl_mk_atom("investigation_memo");
        ETERM *investigation_query = erl_mk_atom("investigation_query");
        ETERM *delete_investigation = erl_mk_atom("delete_investigation");
        ETERM *share_investigation = erl_mk_atom("share_investigation");
        ETERM *save_investigation = erl_mk_atom("save_investigation");
        ETERM *dupcode_algorithm_data = erl_mk_atom("dupcode_algorithm_data");
        ETERM *dupcode_search = erl_mk_atom("dupcode_search");
        ETERM *previous_dupcode_names = erl_mk_atom("previous_dupcode_names");
        ETERM *is_client = erl_mk_atom("is_client");

    private:
        /********** File and database management **********/
        void GetEnvs(const ErlMessage &received);
        void GetFiles(const ErlMessage &received);
        void GetErrors(const ErlMessage &received);
        void GetModules(const ErlMessage &received);
        void GetFunctions(const ErlMessage &received);
        void Progress(const ErlMessage &received);
        void AddDir(const ErlMessage &received);
        void CatFile(const ErlMessage &received);
        void DatabaseHash(const ErlMessage &received);
        /********** File and database management **********/

        /********** Semantic queries **********/
        void SaveSkeleton(const ErlMessage &received);
        void GetSkeletons(const ErlMessage &received);
        void DeleteSkeleton(const ErlMessage &received);
        void ModifySkeleton(const ErlMessage &received);
        void SkeletonCallFormat(const ErlMessage &received);
        void Queries(const ErlMessage &received);
        void RunningQueries(const ErlMessage &received);
        void RunQuery(const ErlMessage &received);
        void ProcessList(QueryResult &result, ETERM *list);
        FilePosition ProcessPosition(const ETERM* position_tuple);
        QueryElem ProcessQueryElem(const ETERM *query_elem);
        void AutoComplete(const ErlMessage &received);
        void PredefQueries(const ErlMessage &received);
        /********** Semantic queries **********/

        /********** Dependency graph drawing **********/
        void DrawSVG(const ErlMessage &received);
        void DrawGraph(const ErlMessage &received);
        void PrintGraph(const ErlMessage &received);
        /********** Dependency graph drawing **********/

        /********** Duplicated code analysis **********/
        void DupcodeSearch(const ErlMessage &received);
        void DupcodeAlgorithmData(const ErlMessage &received);
        void DupcodePreviousNames(const ErlMessage &received);
        /********** Duplicated code analysis **********/

        /********** Investigations **********/
        void GetInvestigations(const ErlMessage &received);
        void LoadInvestigation(const ErlMessage &received);
        void InvestigationMemo(const ErlMessage &received);
        void InvestigationQuery(const ErlMessage &received);
        void DeleteInvestigation(const ErlMessage &received);
        void SaveInvestigation(const ErlMessage &received);
        void ShareInvestigation(const ErlMessage &received);
        /********** Investigations **********/

    public:
        //Constructor
        explicit ReferlReceive(RefErlConnect *ref_erl_connect);

        //Destructor
        ~ReferlReceive();

    public slots:
        void Receive();

    signals:
        //Error message
        void ErrorMessageSignal(const QString &message);

        /********** File and database management **********/
        void ResetDatabaseSignal(const bool &success);
        void ConfigurationsSignal(const ConfigurationList &configurations);
        void FilesSignal(const QStringList &files);
        void ErrorsSignal(const ErrorList &errors);
        void ModulesSignal(const QStringList &modules);
        void FunctionsSignal(const QStringList &functions);
        void FileProgressSignal(const QString &path,
                                const int &percent,
                                const double &speed);
        void CompletedSignal(const int &percent);
        void DroppedSignal();
        void ErrorInFileSignal();
        void CatFileSignal(const QString &path,
                           const QString &content);
        void DbHashSignal(const DbHash &db_hash);
        void IsClientSignal(const bool &is_client);
        /********** File and database management **********/

        /********** Dependency graph drawing **********/
        void SVGSignal(const QString &path);
        void GraphSignal(const QString &dot_path);
        void DrawGraphSignal(DependencyGraph *graph);
        /********** Dependency graph drawing **********/

        /********** Semantic queries **********/
        void SkeletonsSignal(const SkeletonList &skeletons);
        void SaveSkeletonSignal(const QString &name);
        void DeleteSkeletonSignal(const QString &name);
        void ModifySkeletonSignal(const QString &name);
        void QueriesSignal(const QueryList &queries);
        void RunningQueriesSignal(const QueryIdList &running_queries);
        void RunQuerySignal(const QueryResult &result);
        void SkeletonCallFormatSignal(const QString &format);
        void AutoCompleteSignal(const QStringList &autcomplete_list,
                                const QStringList &completed_list_);
        void PredefQuerySignal(const QList<PredefQuery> &predef_queries);
        /********** Semantic queries **********/

        /********** Duplicated code analysis **********/
        void DupcodeSearchSignal(const QString &name,
                                 const QString &path,
                                 const QVector<DupcodeGroup> &groups);
        void DupcodeAlgorithmsSignal(const QVector<DuplicatedCodeAlgorithm> &algorithms);
        void DupcodePreviousNamesSignal(const QStringList &names);
        /********** Duplicated code analysis **********/

        /********** Investigations **********/
        void InvestigationsSignal(const InvestigationList &investigation_list);
        void LoadInvestigationSignal(const InvestigationGraph &graph);
        void InvestigationMemoSignal(const QString &id,
                                     const QString &parent);
        void InvestigationQuerySignal(const InvestigationNodeList &nodes);
        void DeleteInvestigationSignal(const QString &name);
        void SaveInvestigationSignal(const QString &name);

        void ShareInvestigationSignal(const QString &name, const QString &user);
        /********** Investigations **********/
};

#endif // REFERLRECEIVE_H
