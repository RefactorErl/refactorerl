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

#ifndef REFACTORERLMODEL_H
#define REFACTORERLMODEL_H

#include <QObject>
#include <QThread>
#include <QStandardItemModel>
#include <QStringListModel>
#include "errorstandarditemmodel.h"
#include "skeletonstandarditemmodel.h"
#include "querystandarditemmodel.h"
#include "referlconnect.h"
#include "referlreceive.h"
#include "common.h"

//The class allows the user to connect to a RefactorErl node, send queries
// and receive the answers (results). After a result is received a signal
// is emitted or simply processed and stored in the proper model.
//The models containing file, error, module, query, etc. data can be connected
//to view classes to display them.
class RefErlModel : public QObject
{
    Q_OBJECT

    private:
        //Object managing the connection and message sending
        RefErlConnect *ref_erl_connect_;
        //Object receiveing the messages emitting signals with the results
        ReferlReceive *ref_erl_receive_;
        //Separate thread for receiving
        QThread *receive_thread_;

        //Model storing the list of files
        QStandardItemModel *file_model_;
        //Model storing the list of errors
        QStandardItemModel *error_model_;
        //Model storing the environment variables
        QStandardItemModel *configuration_model_;
        //Model storing the list of modules
        QStringListModel *module_model_;
        //Model storing the list of functions
        QStringListModel *function_model_;

        //Model storing the latest query result
        QStandardItemModel *query_result_model_;
        //Model storing the query skeletons
        QStandardItemModel *skeleton_model_;
        //Model storing the cached queries for the current user
        QStandardItemModel *queries_model_;
        //Model storing the currently running queries
        QStandardItemModel *running_queries_model_;
        //Model storing the predefined queries
        QStandardItemModel *predef_queries_model_;
		
        //Model storing the list of investigations
        QStandardItemModel *investigations_model_;

        //Model storing the list of previous dupcode results
        QStringListModel *dupcode_results_;

        QVector<DuplicatedCodeAlgorithm> *dupcode_algorithms_;

        DbHash db_hash_;

        bool is_client_ = false;

    private:
        //Used to build up the latest query result model by recursively adding
        // sub-elements to the data item in the first parameter
        void AddChildren(QStandardItem *item, 
						const QueryResult &result);

    public:
        //Constructor
        explicit RefErlModel(QObject *parent = 0);
        //Destructor. Closes the connection to RefactorErl and stops
        // the receiveing thread.
        ~RefErlModel();
        //Starts a connection to the given node with the given cookie
        bool StartConnection(const std::string &node_name, 
							const std::string &cookie);
        //Returns the node name of the connected RefactorErl node
        const QString GetNodeName() const;
        //Returs the user name
        const QString GetUserName() const;
        //Emits a signal for the ref_erl_receive_ member to start receiveing
        // in a separate thred
        void EmitStartReceiving();

        /********** Model accesses **********/
        QStandardItemModel* FileModel() const;
        QStandardItemModel* ErrorModel() const;
        QStringListModel* ModuleModel() const;
        QStringListModel *FunctionModel() const;
        QStandardItemModel* ConfigurationModel() const;
        QStandardItemModel* QueryResultModel() const;
        QStandardItemModel* SkeletonModel() const;
        QStandardItemModel* QueriesModel() const;
        QStandardItemModel* RunningQueriesModel() const;
        QStandardItemModel* PredefQueriesModel() const;
        QStandardItemModel* InvestigationsModel() const;
        QStringListModel* DupcodeResultsModel() const;
        QVector<DuplicatedCodeAlgorithm>* DuplicatedCodeAlgorithms() const;
        const DbHash &DatabaseHash() const;
        const bool &IsClient() const;
        /********** Model accesses **********/


        /********** File and database management **********/
        void ResetDatabase(const PositioningMode &pos_mode) const;
        void AddPath(const QString &path) const;
        void DropFiles(const QStringList &path_list) const;
        void AddAppbase(const QString &path) const;
        void AddInclude(const QString &path) const;
        void DeleteEnv(const ConfigurationType &type,
                       const QString &path) const;
        void SyncronizeDatabase() const;
        void GetConfigurations() const;
        void GetFiles() const;
        void GetErrors() const;
        void GetModules() const;
        void GetFunctions() const;
        void CatFile(const QString &path) const;
        void GetDbHash() const;
        /********** File and database management **********/

        /********** Dependency graph drawing **********/
        void DrawGraph(const QString &path,
            const DependencyLevel &level,
            const DependencyType &type,
            const bool &exclude_otp = false,
            const QStringList &starting_nodes = QStringList(),
			      const QStringList &connection_nodes = QStringList(),
            const QStringList &excluded_nodes = QStringList(),
            const QStringList &excluded_leaves = QStringList(),
			      const QStringList &excluded_lib = QStringList(),
			      const QStringList &groups = QStringList(),
            const DependencyDrawMethod &method = GraphViz) const;
        /********** Dependency graph drawing **********/

        /********** Semantic queries **********/
        void GetQueries() const;
        void GetRunningQueries() const;
        void RunQuery(const QString &query) const;
        void RunQuery(const QString &query,
                      const QString &file,
                      const int &position) const;
        void DeleteQuery(const QString &query) const;
        void DeleteQuery(const QString &query,
                         const QString &file,
                         const int &position) const;
        void KillQuery(const int &qid) const;
        void AutoComplete(const QString &query) const;
        void GetPredefQueries(const QString &path,
                              const int &position) const;
        void GetSkeletons() const;
        void GetSkeletonCallFormat(const QString &name) const;
        void SaveSkeleton(const QString &name,
                          const QString &skeleton,
                          const QString &comment) const;
        void DeleteSkeleton(const QString &name) const;
        void ModifySkeleton(const QString &name,
                            const QString &skeleton,
                            const QString &comment) const;
        /********** Semantic queries **********/

        /********** Duplicated code analysis **********/
        void DuplicatedCodeSearch(const QString &algorithm_key,
                                  const QList<Parameter> &parameters) const;
        void DuplicatedCodeSelectedSearch(const QString &algorithm_key,
                                          const QString& file_path,
                                          const int &start,
                                          const int&end) const;
        void TriggerDupcodeSelectedSearch(const QString &algorithm_key,
                                          const QString& file_path,
                                          const int &start,
                                          const int&end);
        void DuplicatedCodeAlgorithmData() const;
        void GetDupcodeResultNames() const;
        void DuplicatedCodePreviousSearch(const QString &name) const;
        /********** Duplicated code analysis **********/

        /********** Investigations **********/
        void GetInvestigations() const;
        void LoadInvestigation(const QString &name) const;
        void SaveInvestigation(const InvestigationGraph &graph,
                               const QString &name) const;
        void DeleteInvestigation(const QString &name) const;
        void ShareInvestigation(const QString &name,
                                const QString &user) const;
        void StartInvestigation(const QString &module,
                                const QString &function,
                                const int &arity) const;
        void InvestigationQuery(const QString &parent_id,
                                const QString &query,
                                const QString &file,
                                const int &position) const;
        void InvestigationMemo(const QString &parent_id) const;
        /********** Investigations **********/

    private slots:
        /********* File and database management ********/
        void GetConfigurationsFinished(const ConfigurationList &config_list);
        void GetFilesFinished(const QStringList &file_list);
        void GetErrorsFinished(const ErrorList &error_list);
        void GetModulesFinished(const QStringList &module_list);
        void GetFunctionsFinished(const QStringList &function_list);
        void GetSkeletonsFinished(const SkeletonList &skeleton_list);
        void GetDbHashFinished(const DbHash &db_hash);
        void SetIsClient(const bool &is_client);
        /********* File and database management ********/
        
        /********* Queries ********/
        void GetQueriesFinished(const QueryList &query_list);
        void GetRunningQueriesFinished(const QueryIdList &list);
        void RunQueryFinished(const QueryResult &result);
        void PredefQueriesFinished(const QList<PredefQuery> &predef_list);
        /********* Queries ********/
        
        /********* Investigations ********/
        void GetInvestigationsFinished(
                        const InvestigationList &investigation_list);
        /********* Investigations ********/

        /********* Duplicated code *********/
        void GetDupcodeResultNamesFinished(const QStringList &names);
        void DuplicatedCodeAlgorithmDataFinished(const QVector<DuplicatedCodeAlgorithm> &algorithms);
        /********* Duplicated code *********/

        /********* Dependency graph *********/
        void DrawGraph(DependencyGraph *graph);
        /********* Dependency graph *********/

    signals:
        //Error message
        void ErrorMessageSignal(const QString &message);
        //Receiving start
        void StartReceivingSignal();
        
        /********* File and database management ********/
        void ResetDatabaseSignal(const bool &success);
        void FileProgressSignal(const QString &path,
                                const int &percent,
                                const double &speed);
        void CompletedSignal(const int &percent);
        void DroppedSignal();
        void ErrorInFileSignal();
        void CatFileSignal(const QString &path, const QString &content);
        void DatabaseChangedSignal();
        /********* File and database management ********/
        
        /********* Dependency graph ********/
        void SVGSignal(const QString &path);
        void GraphSignal(const QString &dot_path);
        void DrawGraphSignal(DependencyGraph *graph);
        /********* Dependency graph ********/
        
        /********* Queries ********/
        void SaveSkeletonSignal(const QString &name);
        void DeleteSkeletonSignal(const QString &name);
        void ModifySkeletonSignal(const QString &name);
        void SkeletonCallFormatSignal(const QString &format);
        void AutoCompleteSignal(const QStringList &autcomplete_list,
                                const QStringList &completed_list);
        void QueryFinishedSignal();
        /********* Queries ********/

        /********* Duplicated code ********/
        void DupcodeSearchSignal(const QString &name,
                                 const QString &path,
                                 const QVector<DupcodeGroup> &groups);
        void DupcodeAlgorithmsSignal();
        void DupcodeSelectedSearchSignal(const QString &algorithm_key,
                                         const QString &file_path,
                                         const int &start,
                                         const int &end);
        /********* Duplicated code ********/
        
        /********* Investigations ********/
        void LoadInvestigationSignal(const InvestigationGraph &graph);
        void InvestigationMemoSignal(const QString &id, const QString &parent);
        void InvestigationQuerySignal(const InvestigationNodeList &nodes);
        void DeleteInvestigationSignal(const QString &name);
        void ShareInvestigationSignal(const QString &name, const QString &user);
        void SaveInvestigationSignal(const QString &name);
        /********* Investigations ********/

};
#endif // REFACTORERLMODEL_H
