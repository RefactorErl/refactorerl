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

#ifndef DUPLICATEDCODEWIDGET_H
#define DUPLICATEDCODEWIDGET_H

#include <QWidget>
#include <QStandardItemModel>
#include <QCompleter>
#include <QProgressDialog>
#include "model/referlmodel.h"
#include "view/codebrowser/codebrowserwidget.h"
#include "model/common.h"

namespace Ui {
class DuplicatedCodeWidget;
}

//Provides user interface for the duplicated code analysis functionality
class DuplicatedCodeWidget : public QWidget
{
    Q_OBJECT

    private:
        Ui::DuplicatedCodeWidget *ui;
        RefErlModel *ref_erl_model_;
        CodeBrowserWidget *left_browser_;
        CodeBrowserWidget *right_browser_;
        int latest_start_ = -1;
        int latest_end_= -1;
        bool load_left_ = false;
        bool load_right_ = false;
        bool load_file_ = false;
        QVector<DupcodeGroup> last_result_;

        QCompleter *names_completer_;
        QProgressDialog *dialog_;

    public:
        explicit DuplicatedCodeWidget(QWidget *parent = 0,
                                      RefErlModel *ref_erl_model = 0);
        ~DuplicatedCodeWidget();

    public slots:
        void DupcodeSelectedSearch(const QString &algorithm_key,
                                   const QString &file_path,
                                   const int &start,
                                   const int &end);

    private slots:
        void RunButtonClicked();
        void LoadButtonClicked();
        void DupcodeSearch(const QString &name,
                           const QString &path,
                           const QVector<DupcodeGroup> &results);
        void ShowFile(const QString &path, const QString &content);
        void GroupComboboxCurrentIndexChanged(int index);
        void LeftComboboxCurrentIndexChanged(int index);
        void RightComboboxCurrentIndexChanged(int index);
        void DisplayAlgorithms();
};

#endif // DUPLICATEDCODEWIDGET_H
