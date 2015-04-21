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

#include <iostream>
#include <fstream>
#include <cctype>
#include <exception>

#include "algorithms.h"
#include "conversions.h"
#include "backup.h"
#include "directory.h"
#include "globals.h"

using namespace std;


state::state(const string& name) {
    set_name(name);
}


//-----------------------------------------------------------------------------
// File read/write methods

void state::write_name(fstream& f) const {
    f << name;
}

void state::read_name(fstream& f) {
    string name;

    f >> name;
    set_name(name);
}


//-----------------------------------------------------------------------------
// Getters and setters

string state::get_path() const {
    return globals::ngraph->get_path() + name;
}

void state::set_name(const string& name) {
    this->name = name;
}

string state::get_name() const {
    return name;
}
