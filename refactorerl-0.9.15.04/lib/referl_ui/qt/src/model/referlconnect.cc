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

#include "referlconnect.h"

RefErlConnect::RefErlConnect()
{
    erl_init(NULL, 0); //Initialize the Erl Interface
}

RefErlConnect::~RefErlConnect()
{
    delete cookie_;
    delete node_name_;
    delete user_name_;
}

void RefErlConnect::SetCookie(const std::string &new_cookie)
{
    delete cookie_;
    cookie_ = new char[new_cookie.length() + 1];
    std::strcpy(cookie_, new_cookie.c_str());
}

const std::string RefErlConnect::GetCookie() const
{
    return std::string(cookie_);
}

const std::string RefErlConnect::GetNodeName() const
{
    return std::string(node_name_);
}

const std::string RefErlConnect::GetUserName() const
{
    return std::string(user_name_);
}

bool RefErlConnect::ConnectToNode(const std::string &node_name)
{
    delete node_name_;
    node_name_ = new char[node_name.length() + 1];
    std::strcpy(node_name_, node_name.c_str());

    //Initialize the connection and return false if connecting
    //  fails. This class uses short node names, the node name will be
    //  c1@localhost
    erl_connect_init(1, cookie_, 0);
    if( (socket_fd_ = erl_connect(node_name_)) < 0 ) {
        return false;
    }
    return SendPid(); //Send the current process Id to RefactorErl and wait for
                      // answer: {ok, <user name>}
}

void RefErlConnect::CloseConnection()
{
    ETERM *terminate;
    terminate = erl_mk_atom("terminate");
    SendMessage(terminate);
    erl_free_compound(terminate);
}

void RefErlConnect::CloseSocket()
{
    erl_close_connection(socket_fd_);
}

bool RefErlConnect::SendMessage(ETERM *message) const
{
    return erl_reg_send(socket_fd_, (char*)"referl_qt", message);
}

int RefErlConnect::ReceiveMessage(const int &buffer_size, 
                                    ErlMessage *message) const
{
    unsigned char buffer[buffer_size];
    int message_type = erl_receive_msg(socket_fd_, buffer, buffer_size, message);
    return message_type;
}

bool RefErlConnect::SendPid()
{
    //Construct a {<PID>, pid} message
    ETERM *arr[2], *tuple_msg;
    arr[0] = erl_mk_pid(erl_thisnodename(), socket_fd_, 0, erl_thiscreation());
    arr[1] = erl_mk_atom("pid");
    tuple_msg = erl_mk_tuple(arr, 2);
    erl_reg_send(socket_fd_, (char*)"referl_qt", tuple_msg);
    erl_free_compound(tuple_msg);
    //Wait for answer
    ErlMessage answer;
    unsigned char buffer[1000];
    erl_receive_msg(socket_fd_, buffer, 1000, &answer);
    ETERM *atom_ok = erl_mk_atom("ok");
    user_name_ = erl_iolist_to_string(erl_element(2, answer.msg));
    return erl_match(atom_ok, erl_element(1, answer.msg));
}
