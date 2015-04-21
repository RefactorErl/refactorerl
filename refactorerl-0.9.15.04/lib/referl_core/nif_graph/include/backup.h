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


#ifndef __backup_h_
#define __backup_h_

#include <iostream>
#include <fstream>

#include "state.h"
#include "algorithms.h"
#include "types.h"
#include "exception_helper.h"

/** Represents a backup, that stores information about the graph on the disk.
 *  Note that this class only holds information about the backup's name,
 *  and the functions of that class operates with that, so it does not store
 *  informations about the graph.
 *  The form of a backup looks like this: backup.A .
 *  You can read more about backups in persistence.h. */
class backup : public state {
public:

    /** Constructor.
     *
     *  @param name The backup's name. Default: (empty string)
     */
    backup(const std::string& name = "");

    /** Constructor.
     *
     *  @param cp_num The backup's checkpoint number.
     */
    backup(const backup_num_t& cp_num);

    
    //-------------------------------------------------------------------------
    // Static functions

    /** Checks whether a backup exists, with the given checkpoint number.
     *
     * @param checkpoint_num The backup's checkpoint number.
     * @return Returns true if exists, otherwise false.
     */
    static bool exist(const backup_num_t& checkpoint_num);

    /** Checks whether a backup exists, with the given name.
     *
     * @param name The backup's name.
     * @return Returns true if exists, otherwise false.
     */
    static bool exist(const std::string& name);

    /** Generates just the name of a checkpoint, with the given parameters.
     *
     *  @param checkpoint The number of checkpoint.
     *  @return The name of a backup, that has the specified parameters.
     */
    static std::string gen_name(const backup_num_t& checkpoint_num);

    /** Returns backups from the given directory.
     *
     * @param path The path of the directory.
     * @return A vector of backups (which are in the given directory).
     */
    static backups_t get_backups_from_dir(const std::string& path);


    //-------------------------------------------------------------------------
    // File read/write methods
    
    /** Sets the commit-log of this backup.
     *  Actually it writes out the given commit log to a commit-log file.
     *
     *  @param commit_log The commit-log to write.
     *
     *  @exception invalid_name
     */
    void set_commit_log(const std::string& commit_log) const;

    /** Returns the commit log that belongs to this backup.
     *
     *  @exception invalid_name
     */
    std::string get_commit_log() const;


    //-------------------------------------------------------------------------
    // Other functions

    /** Checks whether the backup exists on the disk.
     *
     *  @return Returns true, if the backup can be found on the proper folder,
     *          otherwise false.
     */
    bool exist() const;

    /** Returns the checkpoint number of the backup.
     *
     *  @exception invalid_name
     */
    backup_num_t get_checkpoint_num() const;

    /** Checks whether the backup's name is valid.
     *  Invalid backup name can cause invalid_name exception,
     *  in other functions.
     *
     *  @return Returns true, if the name is valid, otherwise false.
     */
    bool is_valid_name() const;


    //-------------------------------------------------------------------------
    // Exceptions

    /** Thrown when the backup's name is invalid and someone (or a function)
     *  wants to know the chekpoint number of the backup.
     *  A backup's name has to look like this: 'backup.A', where A is an
     *  unsigned integer value.
     */
    DEFINE_EXCEPTION(invalid_name, "The name of the backup is invalid!")

private:

    //-------------------------------------------------------------------------
    // Constants

    /** Every backup's name starts with that string. */
    static std::string BACKUP_PREFIX();
    
    /** Every commit log's name starts with that string. */
    static std::string COMMIT_LOG_PREFIX();

    /** The separator used to separate the backup's
     *  properties from each other.
     */
    static std::string SEPARATOR();


    //-------------------------------------------------------------------------
    // Other functions

    /** Returns the path of the commit log that belongs to the backup.
     *
     *  @exception invalid_name
     */
    std::string commit_log_path() const;

    /** Returns only the file's name of the commit log
     *  that belongs to the backup.
     *
     *  @exception invalid_name
     */
    std::string commit_log_file() const;
};

#endif
