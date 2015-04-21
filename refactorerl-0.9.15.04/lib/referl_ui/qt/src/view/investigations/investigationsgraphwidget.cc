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

//Author: Matyas Kuti

#include "investigationsgraphwidget.h"
#include <QInputDialog>
#include <QMessageBox>

InvestigationsGraphWidget::InvestigationsGraphWidget(QWidget *parent,
                                                     RefErlModel *ref_erl_model) :
    QGraphicsView(parent), ref_erl_model_(ref_erl_model)
{
    QGraphicsScene *scene = new QGraphicsScene(this);
    scene->setItemIndexMethod(QGraphicsScene::NoIndex);
    setScene(scene);
    setCacheMode(CacheBackground);
    setViewportUpdateMode(BoundingRectViewportUpdate);
    setRenderHint(QPainter::Antialiasing);
    setTransformationAnchor(AnchorUnderMouse);
    scale(qreal(0.8), qreal(0.8));

    connect( ref_erl_model_, SIGNAL( InvestigationMemoSignal(QString,QString) ),
        this, SLOT( AddMemo(QString,QString) ));
    connect( ref_erl_model_, SIGNAL( DatabaseChangedSignal() ),
        this, SLOT( DatabaseChanged() ) );
}

void InvestigationsGraphWidget::SetIsNew(const bool &is_new)
{
    is_new_ = is_new;
    changed_ = false;
}

const bool &InvestigationsGraphWidget::GetIsNew() const
{
    return is_new_;
}

const bool &InvestigationsGraphWidget::GetChanged() const
{
    return changed_;
}

const QString &InvestigationsGraphWidget::GetGraphName()
{
    return graph_.GetName();
}

void InvestigationsGraphWidget::Changed()
{
    changed_ = true;
    emit ChangedSignal();
}

void InvestigationsGraphWidget::DrawInvestigationGraph(const InvestigationGraph &graph)
{
    graph_ = graph;
    bool query_enabled = graph_.GetDbHash() == ref_erl_model_->DatabaseHash();
    scene()->clear();
    QMap<QString, InvestigationProxyItem*> node_items;
    for(InvestigationNode *node : graph_.GetNodes()) {
        node->SetQueryEnabled(query_enabled);
        InvestigationItem *item = new InvestigationItem(node, ref_erl_model_);
        node_items[ node->GetId() ] = new InvestigationProxyItem(this, item);
        scene()->addItem( node_items[ node->GetId() ] );
        node_items[node->GetId()]->setPos( node->GetX(), node->GetY() );
        connect( item, SIGNAL( DeleteSignal(QString) ),
            this, SLOT( Delete(QString) ), Qt::QueuedConnection);
        connect( item, SIGNAL( NewSignal(QString) ),
            this, SLOT( New(QString) ), Qt::QueuedConnection);
        connect( item, SIGNAL( MoveToNewSignal(QString) ),
            this, SLOT( MoveToNew(QString) ), Qt::QueuedConnection);
        connect( item, SIGNAL( ChangedSignal() ),
            this, SLOT( Changed() ), Qt::QueuedConnection);
    }
    for(QList<InvestigationNode*> edges : graph.GetEdges()) {
        for(InvestigationNode* node : edges) {
            if(node->GetParent() != "no_parent") {
                scene()->addItem(
                    new InvestigationEdgeItem( node_items[node->GetParent()],
                            node_items[node->GetId()] ) );
            }
        }
    }
    if(!query_enabled) {
        QMessageBox::warning(this, "Warning",
                   "The database has changed. No querying is allowed");
    }
}

void InvestigationsGraphWidget::AddMemo(const QString &id, const QString &parent)
{
    GraphNode gnode("memo", QString(), -1);
    InvestigationNode *memo = new InvestigationNode(id, gnode, true);
    InvestigationNode *parent_node = graph_.GetNodes()[parent];
    graph_.AddNode(memo);
    graph_.AddEdge( parent, memo->GetId());
    memo->SetParent(parent);
    memo->SetX( parent_node->GetX() + 100 );
    memo->SetY( parent_node->GetY() + 100 );
    memo->SetEdgeLabel("Memo");

    DrawInvestigationGraph(graph_);
}

void InvestigationsGraphWidget::AddNode(InvestigationNode* node)
{
    if(graph_.GetNodes().count(node->GetParent()) == 0) {
        emit NoSourceNode();
        return;
    }
    graph_.AddNode(node);
    InvestigationNode *parent_node = graph_.GetNodes()[node->GetParent()];
    node->SetX( parent_node->GetX() + 100 );
    node->SetY( parent_node->GetY() + 100 );
    graph_.AddEdge(node->GetParent(), node->GetId());
    DrawInvestigationGraph(graph_);
}

void InvestigationsGraphWidget::Save()
{
    if(is_new_) {
        QString name = QInputDialog::getText(this, "Save investigation", "Investigation name");
        if(name.isEmpty()) return;
        graph_.SetName(name);
        ref_erl_model_->SaveInvestigation(graph_, name);
        is_new_ = false;
        changed_ = false;
        emit NewName(name);
    } else {
        ref_erl_model_->SaveInvestigation(graph_, graph_.GetName());
        changed_ = false;
    }
}

void InvestigationsGraphWidget::SaveAs()
{
    QString name = QInputDialog::getText(this, "Save investigation", "Investigation name");
    if(name.isEmpty()) return;
    graph_.SetName(name);
    ref_erl_model_->SaveInvestigation(graph_, name);
    is_new_ = false;
    changed_ = false;
    emit NewName(name);
}

void InvestigationsGraphWidget::DatabaseChanged()
{
    bool query_enabled = graph_.GetDbHash() == ref_erl_model_->DatabaseHash();
    for(InvestigationNode *node : graph_.GetNodes()) {
        node->SetQueryEnabled(query_enabled);
    }
}

void InvestigationsGraphWidget::Delete(const QString &id)
{
    graph_.RemoveNode(id);

    DrawInvestigationGraph(graph_);
    Changed();
}

void InvestigationsGraphWidget::New(const QString &id)
{
    InvestigationGraph new_graph;

    new_graph.SetDbHash(graph_.GetDbHash());

    for(InvestigationNode *node : graph_.GetNodes()) {
        if(node->GetId() == id) { //If the node is the given one, add
                                  //it to the new graph
            new_graph.AddNode( node );
            node->SetParent("no_parent");
        } else {
            delete node;
        }
    }

    DrawInvestigationGraph(new_graph);
    emit NewName("Unsaved");
    is_new_ = true;
}

void InvestigationsGraphWidget::MoveToNew(const QString &id)
{
    graph_.CreateSubGraph(id);
    graph_.SetName(QString());

    DrawInvestigationGraph(graph_);
    emit NewName("Unsaved");
    is_new_ = true;
}

void InvestigationsGraphWidget::wheelEvent(QWheelEvent *event)
{
    if(event->modifiers() & Qt::ControlModifier) {
        scaleView(pow((double)2, event->delta() / 240.0));
    } else {
        QGraphicsView::wheelEvent(event);
    }
}

void InvestigationsGraphWidget::scaleView(qreal scaleFactor)
{
    qreal factor =
            transform().scale(scaleFactor, scaleFactor)
            .mapRect(QRectF(0, 0, 1, 1)).width();
    if (factor < 0.07 || factor > 100)
        return;

    scale(scaleFactor, scaleFactor);
}

void InvestigationsGraphWidget::zoomIn()
{
    scaleView(qreal(1.2));
}

void InvestigationsGraphWidget::zoomOut()
{
    scaleView( 1 /qreal(1.2));
}
