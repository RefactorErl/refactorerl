'use strict';

angular.module('referl.services').factory('database', function($http, channel, $rootScope) {

	var modification = null;

	var setModification = function(newModification) {
		if (newModification) {
			modification = newModification;
			$rootScope.$broadcast("database:modification-start", modification);
		} else {
			if (modification) {
				modification.done = true;
				$rootScope.$broadcast("database:modification-done", modification);
				modification = null;
			}
		}
	};

	var modify = function(type, url, path, isFolder) {
		setModification({
			direct: true,
			byWeb2: true,
			type: type,
			path: path,
			isFolder: isFolder
		});
		$http.post(url, {path: path, isFolder: isFolder});
	};

	channel.on({

		db_start: function(event, newModification) {
			if (!modification) {
				setModification(newModification);
			}
		},

		db_progress: function(event, progress) {
			if (progress.percent >= 1) { // sometimes I got 1 even if it is not finished yet...
				progress.percent = progress.formCount / progress.formMax;
			}
			if (!modification) {
				setModification({
					byWeb2: true,
					type: progress.type,
					progress: progress
				});
			} else {
				modification.byWeb2 = true;
				modification.type = progress.type;
				modification.progress = progress;
			}
		},

		db_done: function(event, args) {
			if (args.global) {
				if (modification && modification.byWeb2) {
					// we will receive a non-global one which
					// has the result
					return;
				}
			} else {
				if (modification) {
					modification.result = args;
				}
			}
			setModification(null);
		}

	});

	channel.send("request_database_status");

	var database = {

		add: function(path, isFolder) {
			modify("add", "api/database/add", path, isFolder);
		},

		drop: function(path, isFolder) {
			modify("drop", "api/database/drop", path, isFolder);
		},

		update: function(path, isFolder) {
			modify("update", "api/database/update", path, isFolder);
		},

		addAppBase: function(path) {
			$http.post("api/database/addAppBase", {
				path: path
			});
		},

		addInclude: function(path) {
			$http.post("api/database/addInclude", {
				path: path
			});
		}

	};

	return database;

});
