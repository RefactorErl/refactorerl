'use strict';

function QueriesCtrl($scope, $rootScope, queries, locationSearch, $http, predefineds, smartLocation, jobs, jobType, search) {

	queries.setupNavigationIn($scope);

	$scope.historyTypes = ['queries', 'search'];
	$scope.jobCancelled = false;
	$scope.lastHeaderName = null;
	$scope.newFile = false;

	$scope.showQueryResults = true;
	$scope.showDatabaseBrowser = false;
	$scope.showFunctionQuicklist = false;

	$scope.file =
	$scope.fileName =
	$scope.fileContent = "";
    $scope.$watch(locationSearch.getterFull(), function() {
        var path = locationSearch.get("file");
        $scope.newFile = ($scope.file != path);
        if($scope.newFile) {
		    if (path) {
                var thePath = path;
			    $http.get("api/getFile", {params: {file: path}}).success(function(result) {
				    if (thePath != path) return;
				    $scope.file = path;
				    $scope.fileName = path.replace(/.*\//, "");
				    $scope.fileError = result.error;
				    $scope.fileContent = result.content;
			    });
			    change_funlist(path);
		    } else {
			    $scope.file =
			    $scope.fileName =
			    $scope.fileContent = "";
		    }
        }

        var pos = locationSearch.get("pos");
		$scope.pos = pos;
		$scope.highlight = pos && pos.split(",");

        var show = locationSearch.get("show");
        showBox(show);

        var scrollPos = locationSearch.get("scroll");
        set_scroll(scrollPos);

        var page = locationSearch.get("page");
		page = (page) ? page : 1;
        if($scope.results() && $scope.results().page && $scope.results().page != page) {
		    setPage(page);
        }
    });

	var run_again = function() {
    // NOTE: hack
    // search.parameters should contain queries.parameters when its type is search
    if ($scope.results().data.type === 'search_result') {
      search.execute(queries.parameters(), false);
    } else {
      queries.execute(queries.parameters(), false);
    }
	}

	var check_validity = function() {
		var results = queries.lastResults();
		var state = queries.resultState();

		if(results && !results.error) {
			if(state.dbchanged) {
				$rootScope.$broadcast("message:show",
					{	title: "Database changed",
						msg: "The database has changed since the query was run. The results may be incomplete and incorrect.",
						large: "Would you like to run this query again?",
						buttons: [	{text: "Run", call: run_again},
									{text: "Cancel", default: true}],
						disableable: "queryResultsInvalid"});
			}
		}
	}

	var result_id_getter = function() {
		var state = queries.resultState();
		if(state) {
			return state.id;
		}
		return null;
	}

	$scope.$watch(result_id_getter, check_validity);

    $scope.resultScroll = 0;
    $scope.resultScrollPos = 0;
    var set_scroll = function(pos) {
        if(!pos) {
            pos = 0;
        }
		$scope.resultScroll = pos;
    };

    var setPage = function(page) {
		var id = smartLocation.getJobId();
		jobs.getResultsToDisplay(id, page)
			.then(function(job) {
        locationSearch.set('page', page);
        queries.updateResults(job);
      });
    };

    var change_funlist = function(path) {
        $scope.funlist = toArray($http.post("api/getFuns", {path: path}));
    };

	var setClosedAll = function(closed) {
    if ($scope.results().data.type === 'search_result') {
		  angular.forEach($scope.results().data.result, function(file) {
		  	file.closed = closed;
		  });
    } else if ($scope.results().data) {
		  angular.forEach($scope.results().data.groups, function(group) {
		  	group.closed = closed;
		  });
    } else {
		  angular.forEach($scope.results().files, function(file) {
		  	file.closed = closed;
		  });
    }
	};

	var toPosParameter = function(pos) {
		return (pos.start - 1) + "," + pos.end;
	};

	angular.extend($scope, {

		isWaiting:        queries.waiting,
		results:          queries.lastResults,

    queriesIsWaiting:  queries.waiting,
    queriesJobResults: queries.lastResults,

    searchIsWaiting:  search.waiting,
    searchJobResults: search.lastResults,

		codeMirrorConfig: {
			lineNumbers: true,
			mode: 'erlang',
			readOnly: true,
			styleSelectedText: true
		},

		collapseAll: function() {
			setClosedAll(true);
		},

		expandAll: function() {
			setClosedAll(false);
		},

		toggleOpen: function(group) {
			group.closed = !group.closed;
		},

		href: function(item, page) {
			if (item.pos) {
				var idPart = locationSearch.getPart("id");
				var href = "#/queries?" + idPart + "&file=" + encodeURIComponent(item.pos.file);
                href += "&page=" + ((page) ? page : 1);
				if (item.pos.end) {
					href += "&pos=" + toPosParameter(item.pos);
				}
				return href;
			} else {
				return "";
			}
		},

    searchHref: function(file, chunk, page) {
			if (chunk.startingPosition) {
				var idPart = locationSearch.getPart("id");
				var href = "#/queries?" + idPart + "&file=" + encodeURIComponent(file.filePath);
        href += "&page=" + ((page) ? page : 1) + "&pos=" + (chunk.startingPosition) + "," + (parseInt(chunk.startingPosition) + chunk.chunk.length);

				return href;
			} else {
				return "";
			}
    },

		FLhref: function(item) {
			if (item.pos) {
				var idPart = locationSearch.getPart("id");
				var href = "#/queries?" + idPart + "&file=" + encodeURIComponent(item.pos.file);
				if (item.pos.last) {
					href += "&pos=" + (item.pos.first - 1) + "," + item.pos.last;
				}
                href += "&show=funlist";
				return href;
			} else {
				return "";
			}
		},

		isCurrent: function(item) {
			if (item.pos) {
				var isCurrentFile = (item.pos.file === $scope.file);
				if (item.pos.end) {
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
                $scope.newFile = ($scope.file != item.pos.file);

				locationSearch.set("file", item.pos.file);
				locationSearch.set("page", $scope.results().page);
				locationSearch.set("scroll", $scope.resultScrollPos);
                $scope.resultScroll = $scope.resultScrollPos;
				if (item.pos.end) {
					locationSearch.set("pos", (item.pos.start - 1) + "," + item.pos.end);
				} else {
					locationSearch.set("pos");
				}
			} finally {
				$event.preventDefault();
				$event.stopPropagation();
			}
		},

		openQuicklist: function($event, item) {
            try {
                if (!item.pos) return;
                $scope.newFile = ($scope.file != item.pos.file);
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

		contextMenuFor: function(file, selection) {
			return predefineds.getAsContextMenu(file, selection, true);
		},

		isPositioned: function(item) {
            if (item.pos.error == "") {
            	return true;
              } else {
            	return false;
        	 }
        },
	});

// File tree code starts here!
	$scope.options = {filter: "", onlyErlang: true};
	$scope.isDatabase = true;
	$scope.selected = null;
    $scope.funlist = [];
    var positions = {};

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

    var downloadFile = function(path) {
        window.open("api/download/download_from_server?path="
                    + encodeURIComponent(path), "_blank");
    };

    $scope.downloadFile = function(node) {
        downloadFile(node.path);
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

	$scope.nodeToggleOpen = function(node) {
        if (!node.folder) return;
		node.open ? $scope.close(node) : $scope.openFolder(node);
	};

	$scope.click = function(node) {
		if (node.folder) {
            $scope.nodeToggleOpen(node)
        } else {
            $scope.openFile(node);
        }
	};

	$scope.openFile = function(node) {
		locationSearch.set("pos", null);
		locationSearch.set("file", node.path);
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

	var searchFilter = function(filter) {
		var url = $scope.isDatabase ? "api/database/search" : "api/filesystem/search";
		$scope.tree = toArray($http.post(url, {filter: filter}));
	};

	var filterChange = function(filter) {
		$scope.regexpFilter = filter.replace(/\*/g, ".*").replace(/\?/g, ".");
		if (filter == "") {
			$scope.isDatabase ? $scope.toDatabase() : $scope.toFileSystem();
		} else {
			searchFilter(filter);
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

    $scope.removeHighlighting = function() {
        locationSearch.set("pos", null);
    };

    $scope.regenerate = function() {
    	var file = $scope.file;
    	$http.post("api/regenerateFile", {file: file}).then(function(response) {
	        positions[file] = undefined;
	        openFile(file);
        });
    };

    $scope.redirectExportResults = function() {
		var id = smartLocation.getJobId();
		window.open("api/jobs/results/" + id +"?format=text", "_blank");
    };

    $scope.cancelJob = function() {
    	var id = smartLocation.getJobId();
		jobs.remove(id)
			.then(function(response) {
		        $scope.jobCancelled = true;
        	});

	};

	$scope.getNext = function() {
		if($scope.results().isLastPage)
		{
			return;
		}
        setPage($scope.results().page + 1);
        $scope.resultScroll = 0;
	};

	$scope.getPrev = function() {
		if($scope.results().isFirstPage)
		{
			return;
		}
        setPage($scope.results().page - 1);
        $scope.resultScroll = 0;
	};

    var showBox = function(box) {
        var index = {"browser": "showDatabaseBrowser",
            "funlist": "showFunctionQuicklist",
            "results": "showQueryResults"};

        box = (box) ? box : "results";
        for(var i in index) {
            $scope[index[i]] = (i == box);
        }
    };

	$scope.manageAccordion = function(box) {
        showBox(box);
        locationSearch.set("show", box);
	};

    window.onresize = function(){
        $scope.$apply();
    }

	$scope.getQueryResultsNavigationContetnStyle = function() {
        var max = Math.max(50, ($(".inner2").height() - 280));
    	return {  "max-height": max + "px" };
	};

	$scope.getNavigationContetnStyle = function() {
        var max = Math.max(50, ($(".inner2").height() - 170));
    	return {  "max-height": max + "px" };
	};

	var jobStarted = function() {
		$scope.jobCancelled = false;
	};

	var jobFinished = function() {
        locationSearch.set("show", "results");
		$scope.showDatabaseBrowser = false;
		$scope.showFunctionQuicklist = false;
		$scope.showQueryResults = true;
	};

	$scope.$watch("options.filter", filterChange);
	$scope.$watch("tree", $scope.unselectAll);

	// reload
	$scope.$on("database:modification-done", function(event, modification) {
		if ($scope.isDatabase) {
			$scope.toDatabase();
		}
	});
	$scope.$on("jobs:started", jobStarted);
	$scope.$on("jobs:finished", jobFinished);

  $scope.$watch('searchIsWaiting()', function() {
    $scope.isWaiting = $scope.searchIsWaiting;
    $scope.results   = $scope.searchJobResults;
  });

  $scope.$watch('searchJobResults()', function() {
    $scope.results = $scope.searchJobResults;
  });

  $scope.$watch('queriesIsWaiting()', function() {
    $scope.isWaiting = $scope.queriesIsWaiting;
    $scope.results   = $scope.queriesJobResults;
  });

  $scope.$watch('queriesJobResults()', function() {
    $scope.results = $scope.queriesJobResults;
  });
}
