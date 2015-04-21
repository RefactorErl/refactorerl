'use strict';

function DatabaseCtrl($scope, $http, predefineds, locationSearch, queries, database, $window) {
	$scope.options = {filter: "", onlyErlang: true, showHidden: false};
	$scope.isDatabase = true;
	$scope.selected = null;
    $scope.funlist = [];
    $scope.tablist = [];
    var positions = { };

	var forEach = function(nodes, fn, arrayForSure) {
		if (arrayForSure || angular.isArray(nodes)) {
			angular.forEach(nodes, function(node) {
				fn(node);
				if (node.children && node.children.length) {
					forEach(node.children, fn, true);
				}
			});
		} else {
			forEach([nodes], fn, true);
		}
	};

	var toArray = function(promise) {
		var array = [];
		array.loading = true;
		promise.then(
			function(response) {
                array.push.apply(array, response.data.files);
				array.loading = false;
			},
			function(error) {
				array.failed = error;
				array.loading = false;
			}
		);
		return array;
	};

	$scope.openFolder = function(node) {
		node.open = true;
		var url = $scope.isDatabase ? "api/database/getFiles" : "api/filesystem/getFiles";
		node.children = toArray($http.post(url, {path: node.path}));
	};

	$scope.close = function(node) {
		node.open = false;
		if (node.children) {
			forEach(node.children, $scope.unselect);
		}
		node.children = [];
	};

	$scope.toggleOpen = function(node) {
        if (!node.folder) return;
		node.open ? $scope.close(node) : $scope.openFolder(node);
	};

	$scope.click = function(node) {
		if (node.folder) {
            $scope.toggleOpen(node)
        } else {
            $scope.openFile(node);
        }
	};

	var openFile = function(path) {
		locationSearch.set("file", path);
        var pos = positions[path];
        if (pos && pos.last) {
            locationSearch.set("pos", null); // remove highlighting
        }
        else {
            locationSearch.set("pos");
        }
	};

    var downloadFile = function(path) {
        window.open("api/download/download_from_server?path="
                    + encodeURIComponent(path), "_blank");
    };

    $scope.openFile = function(node) {
        openFile(node.path);
    };
    $scope.downloadFile = function(node) {
        downloadFile(node.path);
    };

	$scope.unselect = function(node) {
		if ($scope.selected === node) {
			$scope.selected = null;
		}
	};

	$scope.toggleSelect = function(node) {
		if ($scope.selected === node) {
			$scope.selected = null;
		} else {
			$scope.selected = node;
		}
	};

	$scope.unselectAll = function() {
		$scope.selected = null;
	};

	$scope.selectedType = function() {
		if (!$scope.selected) return "";
		return $scope.selected.folder ? "folder" : "file";
	};

	$scope.collapse = function() {
		// enought only on first level
		angular.forEach($scope.tree, $scope.close);
	};

	$scope.contextMenuForNode = function(node) {
		var items = [];
		if (!node) return items;
		$scope.selected = node;
		if (node.folder) {
			if (node.open) {
				items.push({name: "Close Folder", callback: function() {
					$scope.close(node);
				}});
			} else {
				items.push({name: "Open Folder", callback: function() {
					$scope.openFolder(node);
				}});
			}
		}
		if (!node.folder) {
			items.push({name: "Open File", callback: function() {
				$scope.openFile(node);
			}});
            items.push({name: "Download File", callback: function() {
                $scope.downloadFile(node);
            }});
		}
		var type = node.folder ? "Folder" : "File";
		if ($scope.isDatabase) {
			items.push({name: "Remove " + type + " From Database", key: "drop", callback: function() {
				$scope.drop(node);
			}});
			items.push({name: "Update/Reload In Database", key: "update", callback: function() {
				$scope.update(node);
			}});
		} else {
			items.push({name: "Add " + type + " To Database", key: "add", callback: function() {
				$scope.add(node);
			}});
			if (node.folder) {
				items.push({name: "Add As Include Folder", key: "addInclude", callback: function() {
					database.addInclude(node.path);
				}});
				items.push({name: "Add As Appbase Folder", key: "addAddBase", callback: function() {
					database.addAppBase(node.path);
				}});
			} else {
				items.push({name: "Add Parent As Include Folder", key: "addParentInclude", callback: function() {
					var parentPath = node.path.replace(/[\/\\][^\/\\]+$/, "");
					database.addInclude(parentPath);
				}});
			}
		}
		return items;
	};

	$scope.toDatabase = function() {
		$scope.options.filter = "";
		$scope.isDatabase = true;
		$scope.tree = toArray($http.get("api/database/getFiles"));
	};

	$scope.toFileSystem = function() {
		$scope.options.filter = "";
		$scope.isDatabase = false;
		$scope.tree = toArray($http.get("api/filesystem/getFiles"));
	};

	$scope.add = function(node) {
		database.add(node.path, node.folder);
	};

	$scope.drop = function(node) {
		// setTimeout is to leave $digest cycle
		$window.setTimeout(function() {
			if ($window.confirm("Are you sure to remove " + node.path + " from database?")) {
				$scope.$apply(function() {
					database.drop(node.path, node.folder);
				});
			}
		}, 0);
	};

	$scope.update = function(node) {
		database.update(node.path, node.folder);
	};

	var search = function(filter) {
		var url = $scope.isDatabase ? "api/database/search" : "api/filesystem/search";
		$scope.tree = toArray($http.post(url, {filter: filter}));
	};

	var filterChange = function(filter) {
		$scope.regexpFilter = filter.replace(/\*/g, ".*").replace(/\?/g, ".");
		if (filter == "") {
			$scope.isDatabase ? $scope.toDatabase() : $scope.toFileSystem();
		} else {
			search(filter);
		}
	};

	$scope.fsFilter = function(node) {
        var ret = true;
		if ($scope.options.onlyErlang) {
			ret &= node.folder || (node.ext == "erl" || node.ext == "hrl");
		}
        if (! $scope.options.showHidden) {
            if ($scope.options.filter) {
                ret &= _.every(node.path.split("\/"), function(part){return part[0] != "."});
            } else {
                ret &= node.name[0] != ".";
            }
        }
		return ret;
	};

	$scope.$watch("options.filter", filterChange);
	$scope.$watch("tree", $scope.unselectAll);

	// reload
	$scope.$on("database:modification-done", function(event, modification) {
		if ($scope.isDatabase) {
			$scope.toDatabase();
		}
	});

	// file handling
	$scope.codeMirrorConfig = {
		lineNumbers: true,
		mode: 'erlang',
		readOnly: true,
		styleSelectedText: true
	};

	$scope.contextMenuFor = function(file, selection) {
		return predefineds.getAsContextMenu(file, selection, true);
	};

	$scope.file =
	$scope.fileName =
	$scope.fileContent = "";
	$scope.$watch(locationSearch.getterFor("file"), function(path) {
		if (path) {
            var thePath = path;
			$http.get("api/getFile", {params: {file: path}}).success(function(response) {
				if (thePath != path) return;
				$scope.file = path;
				$scope.fileName = path.replace(/.*\//, "");
				$scope.fileError = response.error;
				$scope.fileContent = response.content;
			})
            change_funlist(path);
		} else {
            console.log("I'm out");
			$scope.file = "";
			$scope.fileError = "";
			$scope.fileContent = "";
		}
	});

    var change_funlist = function(path) {
        $scope.funlist = toArray($http.post("api/getFuns", {path: path}));
    };

	$scope.$watch(locationSearch.getterFor("pos"), function(pos) {
		$scope.pos = pos;
		$scope.highlight = pos && pos.split(",");
	});

	var toPosParameter = function(pos) {
		return (pos.first - 1) + "," + pos.last;
	};

    $scope.regenerate = function() {
        var file = $scope.file;
        $http.post("api/regenerateFile", {file: file}).then(function(response) {
            positions[file] = undefined;
            openFile(file);
        });
    };

    $scope.removeHighlighting = function() {
        locationSearch.set("pos", null);
    };

    angular.extend($scope, {
		codeMirrorConfig: {
			lineNumbers: true,
			mode: 'erlang',
			readOnly: true,
			styleSelectedText: true
		},

        href: function(item) {
            if (item.pos.first) {
                var idPart = locationSearch.getPart("id");
                var href = "#/database?" + idPart + "&file=" + encodeURIComponent(item.pos.file);
                if (item.pos.last) {
                    href += "&pos=" + toPosParameter(item.pos);
                }
                return href;
            } else {
                return "";
            }
        },

		isCurrent: function(item) {
			if (item.pos) {
                var isCurrentFile = (item.pos.file === $scope.file);
				if (item.pos.last) {
					return isCurrentFile && (toPosParameter(item.pos) === $scope.pos);
				} else {
					return isCurrentFile && !$scope.pos;
				}
			}
			return false;
		},

        open: function($event, item) {
            try {
                if (!item.pos) return;
                locationSearch.set("file", item.pos.file);
                if (item.pos.last && (item.pos.error == "")) {
                    locationSearch.set("pos", (item.pos.first - 1) + "," + item.pos.last);
                    positions[item.pos.file] = item.pos;
                } else {
                    locationSearch.set("pos");
                    positions[item.pos.file] = item.pos;
                }
            } finally {
                $event.preventDefault();
                $event.stopPropagation();
            }
        },

        isPositioned: function(item) {
            if (item.pos.error == "") { return true; } else { return false; }
        },

        contextMenuFor: function(file, selection) {
            return predefineds.getAsContextMenu(file, selection, true);
        },

        defaultForDoubleClick: function(file, index) {
            predefineds.get(file, index).then(function(response) {
                queries.execute({
                    from: response.node,
                    queryText: response.queries[0].query
                });
            });
        }
    });

// *** Experiment ends here!
}
