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

#ifndef __directory_h_
#define __directory_h_

#include <iostream>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>

#include "bin_file.h"
#include "types.h"


/** A class that facilitate directory operations. */
class directory
{
public:

    //-------------------------------------------------------------------------
    // Constructors and open operations

    directory();

    /** Creates and opens a directory object.
     *
     *  @param path The absolute path to be opened.
     */
    directory(const std::string& path);
    directory(const char* path);

    /** Opens the directory.
     *
     *  @param path The absolute path to be opened.
     *  @return Returns true if the directory opened successfully,
     *          otherwise false.
     */
    bool open(const std::string& path);
    bool open(const char* path);


    //-------------------------------------------------------------------------
    // Static functions

    /** Returns the absolute path of the current, working directory. */
    static std::string get_working_dir();

    /** Returns the file's name from the given path.
     *
     *  @param path The path of the file.
     *  @return The string of the file's name.
     */
    static std::string get_file_name(std::string path);

    /** Returns the directory separator ('\' or '/') character, depending
    *  on the operating system.
    */
    static char separator();

    
    //-------------------------------------------------------------------------
    // Iterator operations

    /** Positions the directory to the first directory element.
     *  A directory element can be a file or a directory.
     *
     *  @return The first directory element.
     */
    std::string first();

    /** Positions the directory to the next directory element, and returns that.
     *
     *  @return The next directory element.
     */
    std::string next();

    /** Returns the current directory element. */
    std::string current();

    /** Checks the directory, whether it is empty.
     *
     *  @return Returns true, if there is no more directory element to iterate,
     *          otherwise false.
     */
     bool end_of_dir() const;


    //-------------------------------------------------------------------------
    // Directory modifying methods

    /** Makes a directory in the directory.
     *
     *  @param dir_name The name of the new directory.
     *  @param mode The privileges of the directory.
     */
#if IS_WINDOWS
    void make_dir(const std::string& dir_name);
#else
    void make_dir(const std::string& dir_name,
                  const mode_t& mode = S_IRWXU | S_IRGRP | S_IXGRP |
                                       S_IROTH | S_IXOTH);
#endif

    /** In the directory, renames the given directory.
     *
     *  @param old_name The old name of the directory.
     *  @param new_name The new name of the directory.
     *  @return If the renaming was successful then returns true,
     *          otherwise false.
     */
    bool rename(const std::string& old_name, const std::string& new_name);

    /** In the directory, removes a file or a directory.
     *  If the given name is a directory, then removes everything from it.
     *
     *  @param name The name of a file or a directory.
     *  @return If the removing was successful then returns true,
     *          otherwise false.
     */
    bool remove(const std::string& name);

    /** Removes everything from the directory. */
    void remove_all();


    //-------------------------------------------------------------------------
    // Query functions

    /** Returns the absolute path of the directory. */
    std::string get_path() const;

    /** Checks that the directory is opened.
     *
     *  @return Returns true, if the directory has opened successfully,
     *          otherwise false.
     */
    bool is_open() const;

    /** Returns true if the given name is a directory's name
     *  in the directory, otherwise false.
     *
     *  @param name The name of a directory, but it can be a file's name also.
     */
    bool is_directory(const std::string& name) const;
    bool is_directory(const char* name) const;

    /** Returns true if the given name is a file's name
     *  in the directory, otherwise false.
     *
     *  @param name The name of a file, but it can be a folder's name also.
     */
    bool is_file(const std::string& name) const;
    bool is_file(const char* name) const;

    /** Returns true if a file or directory exists with
     *  the given name, otherwise false.
     *
     *  @param name The name of a file or directory.
     */
    bool exist(const std::string& name) const;
    bool exist(const char* name) const;

    /** Returns the names of files or directories
     *  that has exactely the given name.
     *
     *  @param name The name to check.
     *  @return Strings, that contains the found files or directories.
     */
    strings_t get_names(const std::string& name) const;

    /** Returns the names of files or directories
     *  that starts with the given prefix.
     *
     *  @param prefix The prefix to check.
     *  @return Strings, that contains the found files or directories.
     */
    strings_t get_names_by_prefix(const std::string& prefix) const;

    /** Returns the names of files or directories
     *  that ends with the given suffix.
     *
     *  @param suffix The suffix to check.
     *  @return Strings, that contains the found files or directories.
     */
    strings_t get_names_by_suffix(const std::string& suffix) const;

    /** Returns the count of files and folders in the directory. */
    size_type count() const;

    
    //-------------------------------------------------------------------------
    // Other functions

    /** Closes the directory.
     *
     *  @return Returns true, if the closing was successful, otherwise false.
     */
    bool close();

    /** Destructor :
     *  Closes the directory, if it is not closed.
     */
    ~directory();


private:

    //-------------------------------------------------------------------------
    // Properties

    /** The path of the directory.*/
    std::string path;

    /** Used during iteration. */
    DIR* d;
    /** Used during iteration. */
    dir_elem_t dir;
    

    //-------------------------------------------------------------------------
    // Other private functions

    /** Reinitializes the directory object. */
    void reinit();

    /** Returns an absolute path, supplemented with the given name. */
    std::string get_full_path(const std::string& name) const;

    /** If the given path does not end with a '\' or '/' character
        * (depending on the operating system), then it places a prior
        * to the end of the given path.
        *
        * @param path_to_fix The path that needs to be fixed.
        */
    void fix_path(std::string& path_to_fix);
    
};

#endif
