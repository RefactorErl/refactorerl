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

#include "referlmodel.h"

RefErlModel::RefErlModel(QObject *parent) :
    QObject(parent)
{
    ref_erl_connect_ = new RefErlConnect();
    receive_thread_ = new QThread();
    ref_erl_receive_ = new ReferlReceive(ref_erl_connect_);
    ref_erl_receive_->moveToThread( receive_thread_ );

    configuration_model_ = new QStandardItemModel(this);
    file_model_ = new QStandardItemModel(this);
    error_model_ = new ErrorStandardItemModel(this);

    module_model_ = new QStringListModel(this);
    function_model_ = new QStringListModel(this);

    query_result_model_ = new QueryStandardItemModel(this);
    query_result_model_->setHorizontalHeaderLabels( 
                            QStringList {"Query result"} );

    skeleton_model_ = new SkeletonStandardItemModel(this);
    //skeleton_model_->setHorizontalHeaderLabels(
        //QStringList { "Name", "Owner" } );

    queries_model_ = new QStandardItemModel(this);

    running_queries_model_ = new QStandardItemModel(this);
    running_queries_model_->setHorizontalHeaderLabels( 
                            QStringList {"Query ID", "Query"} );

    predef_queries_model_ = new QStandardItemModel(this);

    investigations_model_ = new QStandardItemModel(this);
    investigations_model_->setHorizontalHeaderLabels( 
                            QStringList {"Name", "Own"} );

    dupcode_results_ = new QStringListModel(this);

    dupcode_algorithms_ = new QVector<DuplicatedCodeAlgorithm>();

    /********** Meta type registrations **********/
    //These meta type registrations are required for cross-thread signaling
    qRegisterMetaType<ConfigurationList>("ConfigurationList");
    qRegisterMetaType<ErrorList>("ErrorList");
    qRegisterMetaType<SkeletonList>("SkeletonList");
    qRegisterMetaType<DbHash>("DbHash");
    qRegisterMetaType<QueryList>("QueryList");
    qRegisterMetaType<QueryResult>("QueryResult");
    qRegisterMetaType<QueryIdList>("QueryIdList");
    qRegisterMetaType<QList<PredefQuery> >("QList<PredefQuery>");
    qRegisterMetaType< QVector<DupcodeGroup> >("QVector<DupcodeGroup>");
    qRegisterMetaType< QList< Position > >("QList< Position >");
    qRegisterMetaType<InvestigationList>("InvestigationList");
    qRegisterMetaType<InvestigationGraph>("InvestigationGraph");
    qRegisterMetaType<InvestigationNodeList>("InvestigationNodeList");
    qRegisterMetaType<QVector<DuplicatedCodeAlgorithm> >("QVector<DuplicatedCodeAlgorithm>");
    qRegisterMetaType<DependencyGraph>("DependencyGraph");
    /********** Meta type registrations **********/

    /********* Receiving signal connections ********/
    //Ensure the deletion of the receiving thread
    connect( receive_thread_, SIGNAL( finished() ), 
        ref_erl_receive_, SLOT( deleteLater() ) );
    connect( this, SIGNAL( StartReceivingSignal() ), 
        ref_erl_receive_, SLOT( Receive() ) );
    /********* Receiving signal connections ********/
    
    /********* File and database management signal connections ********/
    //Reset database
    connect( ref_erl_receive_, SIGNAL( ResetDatabaseSignal(bool) ),
        this, SIGNAL( ResetDatabaseSignal(bool) ) );
    //List of configurations
    connect( ref_erl_receive_, SIGNAL( ConfigurationsSignal(ConfigurationList) ),
        this, SLOT( GetConfigurationsFinished(ConfigurationList) ) );
    //List of files
    connect( ref_erl_receive_, SIGNAL( FilesSignal(QStringList) ),
        this, SLOT( GetFilesFinished( QStringList) ) );
    //List of modules
    connect( ref_erl_receive_, SIGNAL( ModulesSignal(QStringList) ),
        this, SLOT( GetModulesFinished(QStringList) ) );
    //List of functions
    connect( ref_erl_receive_, SIGNAL( FunctionsSignal(QStringList) ),
        this, SLOT( GetFunctionsFinished(QStringList) ) );
    //List of errors
    connect( ref_erl_receive_, SIGNAL( ErrorsSignal(ErrorList) ),
        this, SLOT( GetErrorsFinished(ErrorList) ) );
    //File adding/dropping progress
    connect( ref_erl_receive_, SIGNAL( FileProgressSignal(QString,int,double) ),
        this, SIGNAL( FileProgressSignal(QString,int,double) ) );
    //File adding/dropping completion
    connect( ref_erl_receive_, SIGNAL( CompletedSignal(int) ),
        this, SIGNAL( CompletedSignal(int) ) );
    //File dropped
    connect( ref_erl_receive_, SIGNAL( DroppedSignal() ),
        this, SIGNAL( DroppedSignal() ) );
    //Errors present in the added files
    connect( ref_erl_receive_, SIGNAL( ErrorInFileSignal() ),
        this, SIGNAL( ErrorInFileSignal() ) );
    //File contents
    connect( ref_erl_receive_, SIGNAL( CatFileSignal(QString, QString) ),
        this, SIGNAL( CatFileSignal(QString, QString) ) );
    //Database hash
    connect( ref_erl_receive_, SIGNAL( DbHashSignal(DbHash) ),
        this, SLOT( GetDbHashFinished(DbHash) ) );
    //Is client
    connect( ref_erl_receive_, SIGNAL( IsClientSignal(bool) ),
        this, SLOT( SetIsClient(bool) ) );
    /********* File and database management signal connections ********/

    //Error message signal connection
    connect( ref_erl_receive_, SIGNAL( ErrorMessageSignal(QString) ),
        this, SIGNAL( ErrorMessageSignal(QString) ) );
    
    /********* Dependency graph signal connections ********/
    //SVG generated
    connect( ref_erl_receive_, SIGNAL( SVGSignal(QString) ),
        this, SIGNAL( SVGSignal(QString) ) );
    //Plain-ext dot file generated
    connect( ref_erl_receive_, SIGNAL( GraphSignal(QString) ),
        this, SIGNAL( GraphSignal(QString) ) );
    connect( ref_erl_receive_, SIGNAL( DrawGraphSignal(DependencyGraph*) ),
        this, SLOT( DrawGraph(DependencyGraph*) ) );
    /********* Dependency graph signal connections ********/

    /********* Query signal connections ********/
    //List of skeletons
    connect( ref_erl_receive_, SIGNAL( SkeletonsSignal(SkeletonList) ),
        this, SLOT( GetSkeletonsFinished(SkeletonList) ) );
    //Skeleton saved
    connect( ref_erl_receive_, SIGNAL( SaveSkeletonSignal(QString) ),
        this, SIGNAL( SaveSkeletonSignal(QString) ) );
    //Skeleton deleted
    connect( ref_erl_receive_, SIGNAL( DeleteSkeletonSignal(QString) ),
        this, SIGNAL( DeleteSkeletonSignal(QString) ) );
    //Skeleton modified
    connect( ref_erl_receive_, SIGNAL( ModifySkeletonSignal(QString) ),
        this, SIGNAL( ModifySkeletonSignal(QString) ) );
    //List of cached queries
    connect( ref_erl_receive_, SIGNAL( QueriesSignal(QueryList) ),
        this, SLOT( GetQueriesFinished(QueryList) ) );
    //Query finished
    connect( ref_erl_receive_, SIGNAL( RunQuerySignal(QueryResult) ),
        this, SLOT( RunQueryFinished(QueryResult) ) );
    //Skeleton call format
    connect( ref_erl_receive_, SIGNAL( SkeletonCallFormatSignal(QString) ),
        this, SIGNAL( SkeletonCallFormatSignal(QString) ) );
    //List of running queries
    connect( ref_erl_receive_, SIGNAL( RunningQueriesSignal(QueryIdList) ),
        this, SLOT( GetRunningQueriesFinished(QueryIdList) ) );
    //List of autocomplete possibilities
    connect( ref_erl_receive_, SIGNAL( AutoCompleteSignal(QStringList, QStringList) ),
        this, SIGNAL( AutoCompleteSignal(QStringList, QStringList) ) );
    //List of predefined queries
    connect( ref_erl_receive_, SIGNAL( PredefQuerySignal(QList<PredefQuery>) ),
        this, SLOT( PredefQueriesFinished(QList<PredefQuery>) ) );
    /********* Query signal connections ********/

    /********* Duplicated code signal connections ********/
    //Duplicated code analysis
    connect( ref_erl_receive_, SIGNAL( DupcodeSearchSignal(QString, QString, QVector<DupcodeGroup> ) ),
        this, SIGNAL( DupcodeSearchSignal(QString, QString, QVector<DupcodeGroup>) ) );
    connect( ref_erl_receive_, SIGNAL( DupcodeAlgorithmsSignal(QVector<DuplicatedCodeAlgorithm>) ),
        this, SLOT( DuplicatedCodeAlgorithmDataFinished(QVector<DuplicatedCodeAlgorithm>)  ) );
    connect( ref_erl_receive_, SIGNAL( DupcodePreviousNamesSignal(QStringList) ),
             this, SLOT(GetDupcodeResultNamesFinished(QStringList)) );
    /********* Duplicated code signal connections ********/

    /********* Investigations signal connections ********/
    //List of investigations
    connect( ref_erl_receive_, SIGNAL( InvestigationsSignal(InvestigationList) ),
        this, SLOT( GetInvestigationsFinished(InvestigationList) ) );
    //Loaded investigation
    connect( ref_erl_receive_, SIGNAL( LoadInvestigationSignal(InvestigationGraph) ),
        this, SIGNAL( LoadInvestigationSignal(InvestigationGraph) ) );
    //Add memo
    connect( ref_erl_receive_, SIGNAL( InvestigationMemoSignal(QString, QString) ),
        this, SIGNAL( InvestigationMemoSignal(QString, QString) ) );
    //Investigation query result
    connect( ref_erl_receive_,
        SIGNAL( InvestigationQuerySignal(InvestigationNodeList) ),
        this, SIGNAL( InvestigationQuerySignal(InvestigationNodeList) ) );
    //Delete investigation
    connect( ref_erl_receive_, SIGNAL( DeleteInvestigationSignal(QString) ),
             this, SIGNAL( DeleteInvestigationSignal(QString) ) );
    //Share investigation
    connect( ref_erl_receive_, SIGNAL( ShareInvestigationSignal(QString, QString) ),
             this, SIGNAL( ShareInvestigationSignal(QString, QString) ) );
    //Save investigation
    connect( ref_erl_receive_, SIGNAL( SaveInvestigationSignal(QString) ),
             this, SIGNAL( SaveInvestigationSignal(QString) ) );
    /********* Investigations signal connections ********/

    receive_thread_->start(); //Start the separate thread for receiving
                              //Idle until connecting

}

RefErlModel::~RefErlModel()
{
    ref_erl_connect_->CloseConnection();
    receive_thread_->quit();
    receive_thread_->wait();
    ref_erl_connect_->CloseSocket();
    delete receive_thread_;
    delete ref_erl_connect_;
    delete dupcode_algorithms_;
}

bool RefErlModel::StartConnection(const std::string &node_name,
                                  const std::string &cookie)
{
    ref_erl_connect_->SetCookie(cookie);
    return ref_erl_connect_->ConnectToNode(node_name);
}

const QString RefErlModel::GetNodeName() const
{
    return QString::fromStdString(ref_erl_connect_->GetNodeName());
}

const QString RefErlModel::GetUserName() const
{
    return QString::fromStdString(ref_erl_connect_->GetUserName());
}

void RefErlModel::EmitStartReceiving()
{
    emit StartReceivingSignal();
}

QStandardItemModel* RefErlModel::FileModel() const
{
    return file_model_;
}

QStandardItemModel* RefErlModel::ErrorModel() const
{
    return error_model_;
}

QStringListModel *RefErlModel::ModuleModel() const
{
    return module_model_;
}

QStringListModel* RefErlModel::FunctionModel() const
{
    return function_model_;
}

QStandardItemModel* RefErlModel::ConfigurationModel() const
{
    return configuration_model_;
}

QStandardItemModel* RefErlModel::QueryResultModel() const
{
    return query_result_model_;
}

QStandardItemModel* RefErlModel::SkeletonModel() const
{
    return skeleton_model_;
}

QStandardItemModel* RefErlModel::QueriesModel() const
{
    return queries_model_;
}

QStandardItemModel* RefErlModel::RunningQueriesModel() const
{
    return running_queries_model_;
}

QStandardItemModel* RefErlModel::PredefQueriesModel() const
{
    return predef_queries_model_;
}

QStandardItemModel* RefErlModel::InvestigationsModel() const
{
    return investigations_model_;
}

QStringListModel* RefErlModel::DupcodeResultsModel() const
{
    return dupcode_results_;
}

QVector<DuplicatedCodeAlgorithm>* RefErlModel::DuplicatedCodeAlgorithms() const
{
    return dupcode_algorithms_;
}

const DbHash &RefErlModel::DatabaseHash() const
{
    return db_hash_;
}

const bool &RefErlModel::IsClient() const
{
    return is_client_;
}

void RefErlModel::ResetDatabase(const PositioningMode &pos_mode) const
{
    ETERM *reset_msg[2], *tuple_msg;
    reset_msg[0] = erl_mk_atom("reset_db");

    switch(pos_mode)
    {
        case Absolute:
        {
            reset_msg[1] = erl_mk_atom("abs");
            break;
        }
        case Relative:
        {
            reset_msg[1] = erl_mk_atom("rel");
            break;
        }
    }

    tuple_msg = erl_mk_tuple(reset_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::GetConfigurations() const
{
    ETERM *get_envs = erl_mk_atom("get_envs");
    ref_erl_connect_->SendMessage(get_envs);
    erl_free_compound(get_envs);
}

void RefErlModel::GetFiles() const
{
    ETERM *files = erl_mk_atom("files");
    ref_erl_connect_->SendMessage(files);
    erl_free_compound(files);
}

void RefErlModel::GetErrors() const
{
    ETERM *errors = erl_mk_atom("errors");
    ref_erl_connect_->SendMessage(errors);
    erl_free_compound(errors);
}

void RefErlModel::GetModules() const
{
    ETERM *modules = erl_mk_atom("modules");
    ref_erl_connect_->SendMessage(modules);
    erl_free_compound(modules);
}

void RefErlModel::GetFunctions() const
{
    ETERM *functions = erl_mk_atom("functions");
    ref_erl_connect_->SendMessage(functions);
    erl_free_compound(functions);
}

void RefErlModel::AddPath(const QString &path) const
{
    ETERM *add_path_msg[2], *tuple_msg;
    add_path_msg[0] = erl_mk_atom("add_dir");
    add_path_msg[1] = erl_mk_estring(path.toStdString().c_str(), path.length());
    tuple_msg = erl_mk_tuple(add_path_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

    GetFiles();
    GetErrors();
    GetFunctions();
    GetModules();
    GetDbHash();
}

void RefErlModel::DropFiles(const QStringList &path_list) const
{
    ETERM *drop_dir_msg[2], *tuple_msg, *path_estring_list[path_list.size()];
    drop_dir_msg[0] = erl_mk_atom("drop_dir");

    int path_count = 0;
    for(QString str : path_list) {
        path_estring_list[path_count] = erl_mk_estring(str.toStdString().c_str(), str.length());

        ++path_count;
    }

    drop_dir_msg[1] = erl_mk_list(path_estring_list, path_list.size());
    tuple_msg = erl_mk_tuple(drop_dir_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

    GetFiles();
    GetErrors();
    GetFunctions();
    GetModules();
    GetDbHash();
}

void RefErlModel::AddAppbase(const QString &path) const
{
    ETERM *add_appbase_msg[2], *tuple_msg;
    add_appbase_msg[0] = erl_mk_atom("add_appbase");
    add_appbase_msg[1] = erl_mk_estring(path.toStdString().c_str() , path.length());
    tuple_msg = erl_mk_tuple(add_appbase_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

    GetConfigurations();
}

void RefErlModel::AddInclude(const QString &path) const
{
    ETERM *add_include_msg[2], *tuple_msg;
    add_include_msg[0] = erl_mk_atom("add_include");
    add_include_msg[1] = erl_mk_estring(path.toStdString().c_str() , path.length());
    tuple_msg = erl_mk_tuple(add_include_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

    GetConfigurations();
}

void RefErlModel::DeleteEnv(const ConfigurationType &type, const QString &path) const
{
    ETERM *del_env_msg[3], *tuple_msg;
    del_env_msg[0] = erl_mk_atom("del_env");
    del_env_msg[1] = erl_mk_estring(path.toStdString().c_str(), path.length());
    del_env_msg[2] = erl_mk_atom( type == Appbase ? "appbase" : "include" );
    tuple_msg = erl_mk_tuple(del_env_msg, 3);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

    GetConfigurations();
}

void RefErlModel::SyncronizeDatabase() const
{
    ETERM *sync_db;
    sync_db = erl_mk_atom("sync_db");
    ref_erl_connect_->SendMessage(sync_db);
    erl_free_compound(sync_db);

    GetConfigurations();
    GetFiles();
    GetErrors();
    GetFunctions();
    GetModules();
    GetDbHash();
}

void RefErlModel::CatFile(const QString &path) const
{
    ETERM *cat_file_msg[2], *tuple_msg;
    cat_file_msg[0] = erl_mk_atom("cat_file");
    cat_file_msg[1] = erl_mk_estring(path.toStdString().c_str(), path.length());
    tuple_msg = erl_mk_tuple(cat_file_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::GetDbHash() const
{
    ETERM *db_hash = erl_mk_atom("db_hash");
    ref_erl_connect_->SendMessage(db_hash);
    erl_free_compound(db_hash);
}

void RefErlModel::SetIsClient(const bool &is_client)
{
    is_client_ = is_client;
}

void RefErlModel::GetConfigurationsFinished(const ConfigurationList &config_list)
{
    configuration_model_->clear();
    configuration_model_->setColumnCount(2);
    configuration_model_->setRowCount(config_list.size());
    configuration_model_->setHorizontalHeaderLabels(
                QStringList {"Type", "Path"} );
    int line_count = 0;
    for(Configuration config : config_list){
        QStandardItem *type =
            new QStandardItem(
                config.first == Appbase ? QString("appbase") :
                                          QString("include")
            );
        QStandardItem *path =
            new QStandardItem( config.second );
        configuration_model_->setItem(line_count, 0, type);
        configuration_model_->setItem(line_count, 1, path);
        ++line_count;
    }
}

void RefErlModel::GetFilesFinished(const QStringList &file_list)
{
    file_model_->clear();
    file_model_->setColumnCount(2);
    file_model_->setRowCount(file_list.size());
    file_model_->setHorizontalHeaderLabels(
                QStringList {"File name", "File path"} );
    int line_count = 0;
    for(QString str : file_list) {
        QStringList components = str.split("/");
        QStandardItem *name =
            new QStandardItem( components.at( components.length() - 1 ) );
        QStandardItem *path =
                new QStandardItem( str );
        file_model_->setItem(line_count, 0, name);
        file_model_->setItem(line_count, 1, path);
        ++line_count;
    }
}

void RefErlModel::GetErrorsFinished(const ErrorList &error_list)
{
    error_model_->clear();
    error_model_->setColumnCount(5);
    error_model_->setRowCount(error_list.size());
    error_model_->setHorizontalHeaderLabels(
        QStringList {"File name", "File path", "Error", "Position"} );
    int line_count = 0;
    for(Error err : error_list) {
        QStringList l = err.first.first.split("/");
        QStandardItem *name =
            new QStandardItem( l.at(l.size() - 1) );
        QStandardItem *path =
            new QStandardItem( err.first.first );
        QStandardItem *error =
            new QStandardItem( err.first.second.trimmed() );
        QStandardItem *start_position =
            new QStandardItem( QString::number( err.second.first ));
        QStandardItem *end_position =
            new QStandardItem( QString::number( err.second.second ));
        error_model_->setItem(line_count, 0, name);
        error_model_->setItem(line_count, 1, path);
        error_model_->setItem(line_count, 2, error);
        error_model_->setItem(line_count, 3, start_position);
        error_model_->setItem(line_count, 4, end_position);
        ++line_count;
    }
}

void RefErlModel::GetModulesFinished(const QStringList &module_list)
{
    module_model_->setStringList(QStringList() << "" << module_list);
}

void RefErlModel::GetFunctionsFinished(const QStringList &function_list)
{
    function_model_->setStringList(QStringList() << "" << function_list);
}

void RefErlModel::GetSkeletonsFinished(const SkeletonList &skeleton_list)
{
    QString name, body, owner, comment;
    skeleton_model_->clear();
    skeleton_model_->setColumnCount(4);
    skeleton_model_->setRowCount(0);
    skeleton_model_->setHorizontalHeaderLabels(
        QStringList {"Name", "Owner"} );
    int line_count = 0;
    for(Skeleton skel : skeleton_list) {
        std::tie(name, body, owner, comment) = skel;
        QStandardItem *name_item = new QStandardItem( name );
        QStandardItem *body_item = new QStandardItem( body );
        QStandardItem *owner_item = new QStandardItem( owner );
        QStandardItem *comment_item = new QStandardItem( comment );
        skeleton_model_->setItem(line_count, 0, name_item);
        skeleton_model_->setItem(line_count, 1, body_item);
        skeleton_model_->setItem(line_count, 2, owner_item);
        skeleton_model_->setItem(line_count, 3, comment_item);
        ++line_count;
    }
}

void RefErlModel::GetDbHashFinished(const DbHash &db_hash)
{
    db_hash_ = db_hash;
    emit DatabaseChangedSignal();
}

void RefErlModel::GetQueriesFinished(const QueryList &query_list)
{
    QString query_str, file;
    int position;
    queries_model_->clear();
    queries_model_->setRowCount(0);
    queries_model_->setColumnCount(3);
    queries_model_->setHorizontalHeaderLabels(
                QStringList {"Query", "File", "Position"} );
    int line_count = 0;
    for(Query query : query_list) {
        std::tie(query_str, file, position) = query;
        QStandardItem *query_item = new QStandardItem( query_str );
        QStandardItem *file_item = new QStandardItem( file );
        QStandardItem *position_item;
        if(position == -1) {
            position_item = new QStandardItem( QString("") );
        } else {
            position_item = new QStandardItem( QString::number(position) );
        }
        queries_model_->setItem(line_count, 0, query_item);
        queries_model_->setItem(line_count, 1, file_item);
        queries_model_->setItem(line_count, 2, position_item);
        ++line_count;
    }
}

void RefErlModel::DrawGraph(const QString &path,
    const DependencyLevel &level,
    const DependencyType &type,
    const bool &exclude_otp,
    const QStringList &starting_nodes,
	  const QStringList &connection_nodes,
    const QStringList &excluded_nodes,
    const QStringList &excluded_leaves,
	  const QStringList &excluded_lib,
	  const QStringList &groups,
    const DependencyDrawMethod &method) const
{
	  bool modul_block_level = false;
    ETERM *draw_graph_msg[3], *tuple_msg;
    switch(method) {
        case GraphViz:
            draw_graph_msg[0] = erl_mk_atom("print_graph");
            break;
        case GraphVizSVG:
            draw_graph_msg[0] = erl_mk_atom("draw_svg");
            break;
        case BuiltIn:
            draw_graph_msg[0] = erl_mk_atom("get_graph");
            break;
    }

	  draw_graph_msg[1] = erl_mk_estring(path.toStdString().c_str(), path.length());
	  ETERM *option_list = erl_mk_empty_list();
	  
	  //Level
	  ETERM *level_msg[2];
	  level_msg[0] = erl_mk_atom("level");

    switch(level) {
        case Module: {
            level_msg[1] = erl_mk_atom("mod");
            break;
        }
        case ModuleGroup: {
			modul_block_level = true;
            level_msg[1] = erl_mk_atom("mb");
            break;
        }
        case Function: {
            level_msg[1] = erl_mk_atom("func");
            break;
        }
    }
	option_list = erl_cons( erl_mk_tuple(level_msg, 2), option_list );

	//Type
	ETERM *type_msg[2];
	type_msg[0] = erl_mk_atom("type");
    type_msg[1] = type == All ? erl_mk_atom("all") : erl_mk_atom("cyclic");
	option_list = erl_cons( erl_mk_tuple(type_msg, 2), option_list );

	if(!modul_block_level)
	{
		//Starting nodes
		ETERM *start_node_msg[2];
		start_node_msg[0] = erl_mk_atom("starting_nodes");
		start_node_msg[1] = erl_mk_empty_list();

		for(QString str : starting_nodes) {
		    start_node_msg[1] = erl_cons( erl_mk_estring(str.toStdString().c_str(),
		                                           str.length()), start_node_msg[1] );
		}
		option_list = erl_cons( erl_mk_tuple(start_node_msg, 2), option_list );

		//Connection nodes
		ETERM *connection_node_msg[2];
		connection_node_msg[0] = erl_mk_atom("connection");
		connection_node_msg[1] = erl_mk_empty_list();

		for(QString str : connection_nodes) {
		    connection_node_msg[1] = erl_cons( erl_mk_estring(str.toStdString().c_str(),
		                                           str.length()), connection_node_msg[1] );
		}
		option_list = erl_cons( erl_mk_tuple(connection_node_msg, 2), option_list );

		//Exclude otp
		ETERM *exclude_otp_msg[2];
		exclude_otp_msg[0] = erl_mk_atom("exclude_otp");
		exclude_otp_msg[1] = erl_mk_atom( exclude_otp ? "true" : "false" );
		option_list = erl_cons( erl_mk_tuple(exclude_otp_msg, 2), option_list );

		//Excluded nodes
		ETERM *excluded_msg[2];
		excluded_msg[0] = erl_mk_atom("exclude");
		excluded_msg[1] = erl_mk_empty_list();

		for(QString str : excluded_nodes) {
		    excluded_msg[1] = erl_cons( erl_mk_estring(str.toStdString().c_str(),
		                                           str.length()), excluded_msg[1] );
		}
		option_list = erl_cons( erl_mk_tuple(excluded_msg, 2), option_list );

		//Excluded module leaves
		ETERM *excluded_leaves_msg[2];
		excluded_leaves_msg[0] = erl_mk_atom("exclude_children");
		excluded_leaves_msg[1] = erl_mk_empty_list();

		for(QString str : excluded_leaves) {
		    excluded_leaves_msg[1] = erl_cons( erl_mk_estring(str.toStdString().c_str(),
		                                    str.length()) , excluded_leaves_msg[1] );
		}
		option_list = erl_cons( erl_mk_tuple(excluded_leaves_msg, 2), option_list );

		//Excluded libraries
		ETERM *excluded_lib_msg[2];
		excluded_lib_msg[0] = erl_mk_atom("exclude_lib");
		excluded_lib_msg[1] = erl_mk_empty_list();

		for(QString str : excluded_lib) {
		    excluded_lib_msg[1] = erl_cons( erl_mk_estring(str.toStdString().c_str(),
		                                    str.length()) , excluded_lib_msg[1] );
		}
		option_list = erl_cons( erl_mk_tuple(excluded_lib_msg, 2), option_list );
	}
	else
	{
		//Modul groups
		ETERM *groups_msg[2];
		groups_msg[0] = erl_mk_atom("groups");
		groups_msg[1] = erl_mk_empty_list();

		for(QString str : groups) {
			QStringList list = str.split(QRegExp("\\W+"), QString::SkipEmptyParts);
			if(list.size() == 1)
			{
				groups_msg[1] = erl_cons( erl_mk_estring(list[0].toStdString().c_str(),
		                                    	list[0].length()) , groups_msg[1] );
			}
			else
			{
				ETERM *group = erl_mk_empty_list();
				for(QString sstr : list)
				{
					group = erl_cons( erl_mk_estring(sstr.toStdString().c_str(),
											sstr.length()) , group );
				}
				groups_msg[1] = erl_cons( group, groups_msg[1] );
			}
		    
		}
		option_list = erl_cons( erl_mk_tuple(groups_msg, 2), option_list );
	}

    draw_graph_msg[2] = option_list;
    tuple_msg = erl_mk_tuple(draw_graph_msg, 3);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);

}

void RefErlModel::GetQueries() const
{
    ETERM *get_queries_msg[2], *tuple_msg;
    get_queries_msg[0] = erl_mk_atom("queries");
    std::string user_name = GetUserName().toStdString();
    get_queries_msg[1] = erl_mk_estring(user_name.c_str(), user_name.length());
    tuple_msg = erl_mk_tuple(get_queries_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::GetRunningQueries() const
{
    ETERM *running_queries = erl_mk_atom("running_queries");
    ref_erl_connect_->SendMessage(running_queries);
    erl_free_compound(running_queries);
}

void RefErlModel::RunQuery(const QString &query) const
{
    QString safe_query = query.simplified();
    safe_query.replace( " ", "" );
    ETERM *run_query_msg[5], *tuple_msg;
    std::string user_name = GetUserName().toStdString();
    run_query_msg[0] = erl_mk_atom("run_query");
    run_query_msg[1] = erl_mk_estring(user_name.c_str(), user_name.length());
    run_query_msg[2] = erl_mk_estring(safe_query.toStdString().c_str(),
                                      safe_query.length());
    run_query_msg[3] = erl_mk_atom("no_file");
    run_query_msg[4] = erl_mk_atom("no_pos");
    tuple_msg = erl_mk_tuple(run_query_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::RunQuery(const QString &query,
                           const QString &file,
                           const int &position) const
{
    QString safe_query = query.simplified();
    safe_query.replace( " ", "" );
    ETERM *run_query_msg[5], *tuple_msg;
    std::string user_name = GetUserName().toStdString();
    run_query_msg[0] = erl_mk_atom("run_query");
    run_query_msg[1] = erl_mk_estring(user_name.c_str(), user_name.length());
    run_query_msg[2] = erl_mk_estring(safe_query.toStdString().c_str(),
                                      safe_query.length());
    run_query_msg[3] = erl_mk_estring(file.toStdString().c_str(),
                                      file.length());
    run_query_msg[4] = erl_mk_int(position);
    tuple_msg = erl_mk_tuple(run_query_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::DeleteQuery(const QString &query) const
{
    ETERM *delete_query_msg[4], *tuple_msg;
    delete_query_msg[0] = erl_mk_atom("delete_query");
    delete_query_msg[1] = erl_mk_estring(query.toStdString().c_str(),
                                         query.length());
    delete_query_msg[2] = erl_mk_atom("no_file");
    delete_query_msg[3] = erl_mk_atom("no_pos");
    tuple_msg = erl_mk_tuple(delete_query_msg, 4);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::DeleteQuery(const QString &query,
                              const QString &file,
                              const int &position) const
{
    ETERM *delete_query_msg[4], *tuple_msg;
    delete_query_msg[0] = erl_mk_atom("delete_query");
    delete_query_msg[1] = erl_mk_estring(query.toStdString().c_str(),
                                         query.length());
    delete_query_msg[2] = erl_mk_estring(file.toStdString().c_str(),
                                         file.length());
    delete_query_msg[3] = erl_mk_int(position);
    tuple_msg = erl_mk_tuple(delete_query_msg, 4);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::KillQuery(const int &qid) const
{
    ETERM *kill_query_msg[2], *tuple_msg;
    kill_query_msg[0] = erl_mk_atom("kill_query");
    kill_query_msg[1] = erl_mk_int(qid);
    tuple_msg = erl_mk_tuple(kill_query_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::GetSkeletons() const
{
    ETERM *skeletons = erl_mk_atom("skeletons");
    ref_erl_connect_->SendMessage(skeletons);
    erl_free_compound(skeletons);
}

void RefErlModel::GetSkeletonCallFormat(const QString &name) const
{
    ETERM *skeleton_call_format_msg[2], *tuple_msg;
    skeleton_call_format_msg[0] = erl_mk_atom("skeleton_call_format");
    skeleton_call_format_msg[1] = erl_mk_estring(name.toStdString().c_str(),
                                                 name.length());
    tuple_msg = erl_mk_tuple(skeleton_call_format_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::SaveSkeleton(const QString &name,
                               const QString &skeleton,
                               const QString &comment) const
{
    ETERM *save_skeleton_msg[5], *tuple_msg;
    std::string user_name = GetUserName().toStdString();
    save_skeleton_msg[0] = erl_mk_atom("save_skeleton");
    save_skeleton_msg[1] = erl_mk_estring(name.toStdString().c_str(),
                                          name.length());
    save_skeleton_msg[2] = erl_mk_estring(skeleton.toStdString().c_str(),
                                          skeleton.length());
    save_skeleton_msg[3] = erl_mk_estring(comment.toStdString().c_str(),
                                          comment.length());
    save_skeleton_msg[4] = erl_mk_estring(user_name.c_str(),
                                          user_name.length());
    tuple_msg = erl_mk_tuple(save_skeleton_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::DeleteSkeleton(const QString &name) const
{
    ETERM *delete_skeleton_msg[2], *tuple_msg;
    delete_skeleton_msg[0] = erl_mk_atom("delete_skeleton");
    delete_skeleton_msg[1] = erl_mk_estring(name.toStdString().c_str(), name.length());
    tuple_msg = erl_mk_tuple(delete_skeleton_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::ModifySkeleton(const QString &name,
                                 const QString &skeleton,
                                 const QString &comment) const
{
    ETERM *modify_skeleton_msg[5], *tuple_msg;
    std::string user_name = GetUserName().toStdString();
    modify_skeleton_msg[0] = erl_mk_atom("modify_skeleton");
    modify_skeleton_msg[1] = erl_mk_estring(name.toStdString().c_str(),
                                            name.length());
    modify_skeleton_msg[2] = erl_mk_estring(skeleton.toStdString().c_str(),
                                            skeleton.length());
    modify_skeleton_msg[3] = erl_mk_estring(comment.toStdString().c_str(),
                                            comment.length());
    modify_skeleton_msg[4] = erl_mk_estring(user_name.c_str(),
                                            user_name.length());
    tuple_msg = erl_mk_tuple(modify_skeleton_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::GetPredefQueries(const QString &path, const int &position) const
{
    ETERM *predef_msg[3], *tuple_msg;
    predef_msg[0] = erl_mk_atom("predef_query");
    predef_msg[1] = erl_mk_estring(path.toStdString().c_str(), path.length());
    predef_msg[2] = erl_mk_int(position);
    tuple_msg = erl_mk_tuple(predef_msg, 3);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::AutoComplete(const QString &query) const
{
    ETERM *autocomplete_msg[2], *tuple_msg;
    autocomplete_msg[0] = erl_mk_atom("autocomplete_query");
    autocomplete_msg[1] = erl_mk_estring(query.toStdString().c_str(),
                                         query.length());

    tuple_msg = erl_mk_tuple(autocomplete_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::AddChildren(QStandardItem *item, const QueryResult &result)
{
    const QList< QueryResult > top_children = result.GetChildren();
    for(QueryResult res : top_children) {
        QueryElem elem = res.GetResult();
        QString file, text;
        int start_pos, end_pos;
        std::tie(file, start_pos, end_pos) = elem.first;
        text = elem.second;
        QStandardItem *text_item = new QStandardItem( text );
        QStandardItem *file_item = new QStandardItem( file );
        QStandardItem *start_item = new QStandardItem( QString::number(start_pos) );
        QStandardItem *end_item = new QStandardItem( QString::number(end_pos) );
        item->appendRow(QList<QStandardItem*>() <<
                        text_item <<
                        file_item <<
                        start_item <<
                        end_item);
        AddChildren(item->child(item->rowCount()-1), res);
    }
}

void RefErlModel::RunQueryFinished(const QueryResult &result)
{
    query_result_model_->clear();
    query_result_model_->setColumnCount(4);
    query_result_model_->setRowCount(1);
    query_result_model_->setHorizontalHeaderLabels( QStringList {"Query result"} );
    QueryElem top = result.GetResult();
    QString top_file, top_text;
    int top_start_pos, top_end_pos;
    std::tie(top_file, top_start_pos, top_end_pos) = top.first;
    top_text = top.second;
    QStandardItem *top_text_item = new QStandardItem( top_text );
    QStandardItem *top_file_item = new QStandardItem( top_file );
    QStandardItem *top_start_item = new QStandardItem( QString::number(top_start_pos) );
    QStandardItem *top_end_item = new QStandardItem( QString::number(top_end_pos) );
    query_result_model_->setItem(0, 0, top_text_item);
    query_result_model_->setItem(0, 1, top_file_item);
    query_result_model_->setItem(0, 2, top_start_item);
    query_result_model_->setItem(0, 3, top_end_item);
    AddChildren(top_text_item, result);
    GetRunningQueries();
    GetQueries();
    emit QueryFinishedSignal();
}

void RefErlModel::PredefQueriesFinished(const QList<PredefQuery> &predef_list)
{
    predef_queries_model_->clear();
    predef_queries_model_->appendRow(
                QList<QStandardItem*>() <<
                new QStandardItem("") <<
                new QStandardItem(""));
    for(PredefQuery predef : predef_list) {
        QStandardItem *name_item = new QStandardItem( predef.first );
        QStandardItem *query_item = new QStandardItem( predef.second );
        predef_queries_model_->appendRow(
                    QList<QStandardItem*>() <<
                    name_item <<
                    query_item);
    }
}

void RefErlModel::GetRunningQueriesFinished(const QueryIdList &list)
{
    running_queries_model_->clear();
    running_queries_model_->setColumnCount(2);
    running_queries_model_->setRowCount(list.size());
    running_queries_model_->setHorizontalHeaderLabels( QStringList {"Query ID", "Query"} );
    int line_count = 0;
    for(QueryId qid : list) {
        QStandardItem *id = new QStandardItem( QString::number( qid.first ) );
        QStandardItem *str = new QStandardItem( qid.second );
        running_queries_model_->setItem(line_count, 0, id);
        running_queries_model_->setItem(line_count, 1, str);
        ++line_count;
    }
}

void RefErlModel::DuplicatedCodeSearch(const QString &algorithm_key,
                                       const QList<Parameter> &parameters) const
{
    ETERM *options = erl_mk_empty_list();
    ETERM *alg_name_item[2];
    alg_name_item[0] = erl_mk_atom("algorithm");
    alg_name_item[1] = erl_mk_atom( algorithm_key.toStdString().c_str());
    options = erl_cons( erl_mk_tuple(alg_name_item, 2), options );

    for(const Parameter &param : parameters) {
        ETERM *param_tuple[2];
        QVariant value = param.GetValue();
        bool ok;
        param_tuple[0] = erl_mk_atom(param.GetKey().toStdString().c_str());
        if(param.GetType() == "boolean") {
            param_tuple[1] = value.toBool() ? erl_mk_atom("true") : erl_mk_atom("false");
        } else if(param.GetType() == "float") {
            param_tuple[1] = erl_mk_float(value.toDouble(&ok));
        } else if(param.GetType() == "integer") {
            param_tuple[1] = erl_mk_int(value.toInt(&ok));
        } else if(param.GetType() == "atom") {
            if(value.toString().isEmpty()) {
                continue;
            }
            param_tuple[1] = erl_mk_atom(value.toString().toStdString().c_str());
        } else if(param.GetType() == "string") {
            if(value.toString().isEmpty()) {
                continue;
            }
            param_tuple[1] = erl_mk_estring(value.toString().toStdString().c_str(), value.toString().size());
        } else if(param.GetType() == "enum") {
            if(param.GetIsAtom()) {
                param_tuple[1] = erl_mk_atom(value.toString().toStdString().c_str());
            } else {
                param_tuple[1] = erl_mk_estring(value.toString().toStdString().c_str(), value.toString().size());
            }
        } else if(param.GetType() == "atoms") {
            QStringList values = value.toStringList();
            if(values.size() == 0) {
                continue;
            }
            ETERM *list = erl_mk_empty_list();
            for(QString &str : values) {
                list = erl_cons(erl_mk_atom(str.toStdString().c_str()), list);
            }
            param_tuple[1] = list;
        }

        options = erl_cons(erl_mk_tuple(param_tuple, 2), options);
    }

    ETERM *dupcode_tuple[2];
    dupcode_tuple[0] = erl_mk_atom("dupcode_search");
    dupcode_tuple[1] = options;
    ref_erl_connect_->SendMessage(erl_mk_tuple(dupcode_tuple, 2));
}

void RefErlModel::DuplicatedCodeSelectedSearch(const QString &algorithm_key,
                                               const QString &file_path,
                                               const int &start,
                                               const int &end) const
{
    ETERM *param_tuple[5];
    param_tuple[0] = erl_mk_atom("selected_dupcode");
    param_tuple[1] = erl_mk_atom(algorithm_key.toStdString().c_str());
    param_tuple[2] = erl_mk_estring(file_path.toStdString().c_str(), file_path.size());
    param_tuple[3] = erl_mk_int(start);
    param_tuple[4] = erl_mk_int(end);
    ref_erl_connect_->SendMessage(erl_mk_tuple(param_tuple, 5));
}

void RefErlModel::TriggerDupcodeSelectedSearch(const QString &algorithm_key,
                                               const QString& file_path,
                                               const int &start,
                                               const int&end)
{
    emit DupcodeSelectedSearchSignal(algorithm_key,
                                     file_path,
                                     start,
                                     end);
}

void RefErlModel::DuplicatedCodeAlgorithmData() const
{
    ETERM *dupcode_alg_data = erl_mk_atom("dupcode_algorithm_data");
    ref_erl_connect_->SendMessage(dupcode_alg_data);
}

void RefErlModel::GetDupcodeResultNames() const
{
    ETERM *previous_names = erl_mk_atom("previous_dupcode_names");
    ref_erl_connect_->SendMessage(previous_names);
}

void RefErlModel::DuplicatedCodePreviousSearch(const QString &name) const
{
    ETERM *previous_tuple[2];
    previous_tuple[0] = erl_mk_atom("previous_dupcode_search");
    previous_tuple[1] = erl_mk_atom(name.toStdString().c_str());
    ref_erl_connect_->SendMessage( erl_mk_tuple(previous_tuple, 2) );
}

void RefErlModel::GetInvestigations() const
{
    ETERM *investigations = erl_mk_atom("investigations");
    ref_erl_connect_->SendMessage(investigations);
}

void RefErlModel::GetInvestigationsFinished(const InvestigationList &investigation_list)
{
    investigations_model_->clear();
    investigations_model_->setRowCount(investigation_list.size());
    investigations_model_->setHorizontalHeaderLabels( QStringList {"Name", "Own"} );
    int line_count = 0;
    for(Investigation inv : investigation_list) {
        QStandardItem *name_item = new QStandardItem( inv.first );
        bool own = inv.second.contains(GetUserName());
        QStandardItem *own_item = new QStandardItem( own ? "True" : "False" );
        investigations_model_->setItem(line_count, 0, name_item);
        investigations_model_->setItem(line_count, 1, own_item);
        ++line_count;
    }
}

void RefErlModel::GetDupcodeResultNamesFinished(const QStringList &names)
{
    dupcode_results_->setStringList(QStringList() << "" << names);
}

void RefErlModel::DuplicatedCodeAlgorithmDataFinished(const QVector<DuplicatedCodeAlgorithm> &algorithms)
{
    delete dupcode_algorithms_;
    dupcode_algorithms_ = new QVector<DuplicatedCodeAlgorithm>(algorithms);
}

void RefErlModel::LoadInvestigation(const QString &name) const
{
    ETERM *inv_msg[2], *tuple_msg;
    inv_msg[0] = erl_mk_atom("load_investigation");
    inv_msg[1] = erl_mk_estring(name.toStdString().c_str(), name.size());
    tuple_msg = erl_mk_tuple(inv_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::SaveInvestigation(const InvestigationGraph &graph,
                                    const QString &name) const
{
    ETERM *save_msg[5], *tuple_msg;
    save_msg[0] = erl_mk_atom("save_investigation");
    save_msg[1] = erl_mk_estring(name.toStdString().c_str(), name.size());
    save_msg[2] = erl_mk_estring(GetUserName().toStdString().c_str(),
                                 GetUserName().size());
    int h1, h2, h3, h4;
    std::tie(h1, h2, h3, h4) = graph.GetDbHash();
    ETERM *hash, *hash_tuple_first[3], *hash_tuple[2];
    hash_tuple_first[0] = erl_mk_int(h1);
    hash_tuple_first[1] = erl_mk_int(h2);
    hash_tuple_first[2] = erl_mk_int(h3);
    hash_tuple[0] = erl_mk_tuple(hash_tuple_first, 3);
    hash_tuple[1] = erl_mk_int(h4);
    hash = erl_mk_tuple(hash_tuple, 2);
    save_msg[3] = hash;
    ETERM *inv_list = erl_mk_empty_list();
    for(InvestigationNode *node : graph.GetNodes()) {
        ETERM *inv_item[4], *inv_tuple;

        inv_item[0] = erl_mk_estring( node->GetId().toStdString().c_str(),
                                      node->GetId().size() );
        inv_item[1] = erl_mk_estring( node->GetParent().toStdString().c_str(),
                                      node->GetParent().size() );

        ETERM *inv_node[9];
        inv_node[0] = erl_mk_atom("invnode");
        inv_node[1] = erl_mk_estring( node->GetName().toStdString().c_str(),
                                      node->GetName().size() );
        inv_node[2] = node->IsShown() ? erl_mk_atom("true") : erl_mk_atom("false");
        QString gn, type;
        int num;
        std::tie(gn, type, num) = node->GetNode();
        if(node->IsMemo()) {
            inv_node[3] = erl_mk_atom("memo");
        } else {
            ETERM *gnode[3];
            gnode[0] = erl_mk_atom(gn.toStdString().c_str());
            gnode[1] = erl_mk_atom(type.toStdString().c_str());
            gnode[2] = erl_mk_int(num);
            inv_node[3] = erl_mk_tuple(gnode, 3);
        }
        inv_node[4] = erl_mk_estring(node->GetText().toStdString().c_str(),
                                     node->GetText().size());
        inv_node[5] = erl_mk_estring(node->GetEdgeLabel().toStdString().c_str(),
                                     node->GetEdgeLabel().size());
        inv_node[6] = node->IsMemo() ? erl_mk_atom("true") : erl_mk_atom("false");
        ETERM *file_pos_tuple[5];
        file_pos_tuple[0] = erl_mk_estring(node->GetPath().toStdString().c_str(),
                                           node->GetPath().size());
        file_pos_tuple[1] = erl_mk_int(node->GetOffset());
        file_pos_tuple[2] = erl_mk_int(node->GetLineNumber());
        ETERM *pos_tuple[2];
        pos_tuple[0] = erl_mk_int(node->GetStartPosition());
        pos_tuple[1] = erl_mk_int(node->GetEndPosition());
        file_pos_tuple[3] = erl_mk_tuple(pos_tuple, 2);
        file_pos_tuple[4] = erl_mk_estring(node->GetLabel().toStdString().c_str(),
                                           node->GetLabel().size());
        inv_node[7] = erl_mk_tuple(file_pos_tuple, 5);
        ETERM *displ_pos_tuple[2];
        displ_pos_tuple[0] = erl_mk_int(node->GetX());
        displ_pos_tuple[1] = erl_mk_int(node->GetY());
        inv_node[8] = erl_mk_tuple(displ_pos_tuple, 2);

        inv_item[2] = erl_mk_tuple(inv_node, 9);

        ETERM *children = erl_mk_empty_list();
        for(InvestigationNode *child : graph.GetEdges()[node->GetId()]) {
            children = erl_cons( erl_mk_estring(child->GetId().toStdString().c_str(),
                                                child->GetId().size()) , children);
        }
        inv_item[3] = children;

        inv_tuple = erl_mk_tuple(inv_item, 4);
        inv_list = erl_cons(inv_tuple, inv_list);
    }

    save_msg[4] = inv_list;
    tuple_msg = erl_mk_tuple(save_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::DeleteInvestigation(const QString &name) const
{
    ETERM *delete_msg[3], *tuple_msg;
    delete_msg[0] = erl_mk_atom("delete_investigation");
    delete_msg[1] = erl_mk_estring(name.toStdString().c_str(), name.size());
    delete_msg[2] = erl_mk_estring(GetUserName().toStdString().c_str(),
                                   GetUserName().size());
    tuple_msg = erl_mk_tuple(delete_msg, 3);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::ShareInvestigation(const QString &name, const QString &user) const
{
    ETERM *share_msg[3], *tuple_msg;
    share_msg[0] = erl_mk_atom("share_investigation");
    share_msg[1] = erl_mk_estring(name.toStdString().c_str(), name.size());
    share_msg[2] = erl_mk_estring(user.toStdString().c_str(), user.size());
    tuple_msg = erl_mk_tuple(share_msg, 3);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::StartInvestigation(const QString &module,
                                     const QString &function,
                                     const int &arity) const
{
    ETERM *start_msg[5], *tuple_msg;
    start_msg[0] = erl_mk_atom("start_investigation");
    start_msg[1] = erl_mk_estring(module.toStdString().c_str(), module.size());
    start_msg[2] = erl_mk_estring(function.toStdString().c_str(), function.size());
    start_msg[3] = erl_mk_int(arity);
    start_msg[4] = erl_mk_estring(GetUserName().toStdString().c_str(),
                                  GetUserName().size());
    tuple_msg = erl_mk_tuple(start_msg, 5);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::InvestigationQuery(const QString &parent_id,
                                     const QString &query,
                                     const QString &file,
                                     const int &position) const
{
    ETERM *query_msg[6], *tuple_msg;
    query_msg[0] = erl_mk_atom("investigation_query");
    query_msg[1] = erl_mk_estring(parent_id.toStdString().c_str() ,
                                  parent_id.size() );
    query_msg[2] = erl_mk_estring(query.toStdString().c_str() , query.size() );
    query_msg[3] = erl_mk_estring(file.toStdString().c_str() , file.size() );
    query_msg[4] = erl_mk_int(position);
    query_msg[5] = erl_mk_estring(GetUserName().toStdString().c_str() ,
                                  GetUserName().size() );
    tuple_msg = erl_mk_tuple(query_msg, 6);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::InvestigationMemo(const QString &parent_id) const
{
    ETERM *memo_msg[2], *tuple_msg;
    memo_msg[0] = erl_mk_atom("investigation_memo");
    memo_msg[1] = erl_mk_estring(parent_id.toStdString().c_str(),
                                 parent_id.size());
    tuple_msg = erl_mk_tuple(memo_msg, 2);
    ref_erl_connect_->SendMessage(tuple_msg);
    erl_free_compound(tuple_msg);
}

void RefErlModel::DrawGraph(DependencyGraph *graph)
{
    graph->Arrange();
    emit DrawGraphSignal(graph);
}
