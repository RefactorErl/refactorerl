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


#ifndef __algorithms_h_
#define __algorithms_h_

#include <iostream>
#include <algorithm>
#include <list>
#include <vector>
#include <string>
#include <sstream>

#include "macros.h"
#include "types.h"
#include "exception_helper.h"


//-----------------------------------------------------------------------------
// Container functions

/** Removes an object from the given container.
 *
 *  @param container The container to be modified.
 *  @param elem The element to be removed.
 */
template <class _container_type, class _elem_type>
void remove(_container_type& container, const _elem_type& elem) {
    typename _container_type::iterator invalid =
                          std::remove(container.begin(), container.end(), elem);

    container.erase(invalid, container.end());
}

/** Checks whether the given container contains the given element.
 *  Note that the container has to have operator[].
 *
 *  @param container The container to examine.
 *  @param elem The element to search.
 *  @return If the container contains the element, then returns true,
 *          otherwise false.
 */
template <class _container_type, class _elem_type>
bool contains(_container_type container, const _elem_type& elem) {
    LOOP(container, i) {
        if(container[i] == elem) {
            return true;
        }
    }

    return false;
}


//-----------------------------------------------------------------------------
// String functions

/** Converts the given object to string.
 *
 *  @param t The object to be converted.
 *  @return The object converted to string.
 */
template <class T>
std::string to_string(const T& t) {
    std::stringstream ss;

    ss << t;

    return ss.str();
}

/** Splits a string by the given delimiter,
 *  and returns the split strings in a vector.
 *
 *  @param str The string to be split.
 *  @param delimiter The delimiter, which used in the split.
 *  @return The split strings.
 */
strings_t str_split(const std::string& str,
                                   const std::string& delimiters = " ");

/** Checks that the given string starts with the given prefix.
 *
 *  @param str The string to be examined.
 *  @param prefix The string prefix.
 *  @return True, if the specified string starts with the given prefix,
 *          otherwise false.
 */
bool str_starts_with(const std::string& str, const std::string& prefix);

/** Checks that the given string contains the given infix.
 *
 *  @param str The string to be examined.
 *  @param infix The string infix.
 *  @return True, if the specified string contains the given infix,
 *          otherwise false.
 */
bool str_contains(const std::string& str, const char* infix);
bool str_contains(const std::string& str, const std::string& infix);

/** Checks that the given string ends with the given suffix.
 *
 *  @param str The string to be examined.
 *  @param suffix The string suffix.
 *  @return True, if the specified string ends with the given suffix,
 *          otherwise false.
 */
bool str_ends_with(const std::string& str, const std::string& suffix);

#endif
