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

#ifndef __state_h_
#define	__state_h_


#include <iostream>
#include <fstream>

#include "algorithms.h"
#include "types.h"
#include "exception_helper.h"

/** The base of the backup class. */
class state {
public:

    /** Constructor.
     *
     *  @param name The state's name. Default: (empty string)
     */
    state(const std::string& name = "");


    //-------------------------------------------------------------------------
    // File read/write methods

    /** Writes out the name of the state to the given file,
     *  if the name is not empty.
     *
     *  @param f The text file into which the function writes.
     */
    void write_name(std::fstream& f) const;

    /**  Reads the state's name from the given file.
     *
     *   @param f The text file from which the function reads.
     */
    void read_name(std::fstream& f);


    //-------------------------------------------------------------------------
    // Getters and setters

    /** Returns the path of the state. */
    std::string get_path() const;

    /** Sets the name of the state, which is the backup's file name also.
     *
     *  @param name The new name of the backup.
     */
    virtual void set_name(const std::string& name);

    /** Returns the name of the state. */
    std::string get_name() const;

    virtual ~state() {}

protected:

    //-------------------------------------------------------------------------
    // Properties

    /** The name of the state, that is the file's name also. */
    std::string name;

};


#endif
