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
#include <algorithm>
#include <list>
#include <vector>
#include <sstream>
#include <string>

#include "algorithms.h"

using namespace std;

strings_t str_split(const string& str, const string& delimiter) {
    strings_t tokens;
    if(str.size() > 0) {
        string::size_type lastPos = str.find_first_not_of(delimiter, 0);
        string::size_type pos     = str.find_first_of(delimiter, lastPos);

        while (string::npos != pos || string::npos != lastPos) {
            tokens.push_back(str.substr(lastPos, pos - lastPos));
            lastPos = str.find_first_not_of(delimiter, pos);
            pos = str.find_first_of(delimiter, lastPos);
        }
    }

    return tokens;
}

bool str_starts_with(const string& str, const string& prefix) {
    if(str.size() >= prefix.size()) {
        return (str.compare(0, prefix.size(), prefix) == 0);
    } else {
        return false;
    }
}

bool str_contains(const string& str, const char* infix) {
    return str.find(infix, 0) != string::npos;
}

bool str_contains(const string& str, const string& infix) {
    return str_contains(str, infix.c_str());
}

bool str_ends_with(const string& str, const string& suffix) {
    if(str.size() >= suffix.size()) {
        return (str.compare(str.size() - suffix.size(), str.size(), suffix) == 0);
    } else {
        return false;
    }
}
