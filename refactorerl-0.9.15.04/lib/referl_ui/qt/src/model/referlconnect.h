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

#ifndef REFERLCONNECT_H
#define REFERLCONNECT_H

#include <string>
#include <cstring>
#include "ei.h"
#include "erl_interface.h"

//Allows to connect to an arbitrary RefactorErl node running a
// process registered as referl_qt
class RefErlConnect
{
    private:
        //Cookie string
        char *cookie_ = NULL;

        //Socket file descriptor for communication
        //  with the Erlang node
        int socket_fd_;

        //Connected node name
        char *node_name_ = NULL;

        //User name
        char *user_name_ = NULL;

    private:
        //Sends the C node's process ID (PID) to the referl_qt
        //  process, returns true if receives an ok answer
        bool SendPid();

    public:
        //Constructor
        explicit RefErlConnect();

        //Destructor, deletes the dinamically allocated members
        ~RefErlConnect();

        //Sets the cookie to be used in the connection
        void SetCookie(const std::string &cookie);

        //Return the cookie as a string
        const std::string GetCookie() const;

        //Return the node name as a string
        const std::string GetNodeName() const;

        //Return the username as a string
        const std::string GetUserName() const;

        //Tries to establish a connection to the node, given in the
        //  node_name parameter
        bool ConnectToNode(const std::string &node_name);

        //Terminates the connection by sending a terminate message
        //  to the referl_qt process
        void CloseConnection();

        //Closes the socket using the proper erl interface function
        void CloseSocket();

        //Sends message to the referl_qt process
        bool SendMessage(ETERM *message) const;

        //Receives a message from the referl_qt process into the message
        //  parameter
        //Returns the message type
        int ReceiveMessage(const int &buffer_size, ErlMessage *message) const;

};
#endif // REFERLCONNECT_H
