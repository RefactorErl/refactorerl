'use strict';

function DuplicatesCtrl($scope, $rootScope, autoComplete, duplicates, predefineds, $http, locationSearch, $location, shortener) {

	duplicates.setupNavigationIn($scope);

	$scope.historyType = "duplicates";

	// used by the view to extend options
	$scope.extend = angular.extend;

	// The main model
	$scope.algorithm = "suffix_tree";

    $scope.ready = false;
    $scope.algorithms = null;
    $scope.errors = null;

    $scope.findByKey = function(arr, key) {
    	return _.find(arr, function(elem){ return elem.key == key; });
    }

	$scope.contextMenuFor = function(file, selection) {
		return predefineds.getAsContextMenu(file, selection, true);
	}

    var getAlgorithms = function() {
		$http.get("api/getDupcodeAlgorithms", {params: null}).success(function(data) {
			$scope.algorithms = data;
            $scope.ready = true;
		}).error(function(a,b,c,d) {
            $scope.ready = true;
            $scope.errors = ["Unknown error occured. Try refreshing!"];
        });
    }
    getAlgorithms();

	$scope.subjectsConfig = {
		multiple: true,
		minimumInputLength: 1,
		query: autoComplete.createModuleQuery()
	};

	$scope.searchNameConfig = {
		multiple: false,
		minimumInputLength: 1,
		query: autoComplete.createDupcodeSearchQuery()
	};

	// Parameters binding
	$scope.$watch(duplicates.parameters, function(parameters, old) {
		if (old === parameters || parameters == undefined) return; // don't clear with the initial empty parameters
        var alg = $scope.findByKey($scope.algorithms, parameters.key);
        if(alg) {
        	alg.args = parameters.args;
        	$scope.algorithm = parameters.key;
        }
	});

    $scope.download = function() {
		window.open("api/downloadDuplicatesResult?path="+$scope.results().output, "_blank");
    };

	// Results
	$scope.waiting = duplicates.waiting;
	$scope.results = duplicates.lastResults;
	$scope.idHref = locationSearch.getterForPart("id");

	$scope.generate = function() {
        var params = $scope.findByKey($scope.algorithms, $scope.algorithm);
        if(checkParams(params.args)) {
		    duplicates.execute(params);
        }
	};

    /*$scope.getResultByName = function(sname) {
        duplicates.execute({name: sname.text});
    }*/

    $scope.getResultByName = function(sname) {
        if(sname) {
            $http.get("api/getDupcodeJobIdByName", {params: {name: sname.text}}).success(function(data) {
			    if(data.id) {
window.location = window.location+"&id="+shortener.fromInt(data.id);
                } else {
                    duplicates.execute({name: sname.text});
                }
		    });
        }
    };

    function checkParams(args) {
        $scope.errors = null;
        for(var i in args) {
            var type = args[i].type;
            var value = args[i].default;
            var label = args[i].label;
            var enumtype = args[i].enumtype;
            if(type == 'integer') {
                checkType(value, parseInt, label+" has to be an integer.");
            }
            if(type == 'float') {
                checkType(value, parseFloat, label+" has to be a float.");
            }
            if(type == 'enum' && (enumtype=='integer' || enumtype=='float')) {
                var selected = args[i].selected;
                var chkfun = (enumtype=='float') ? parseFloat : parseInt;
                var t = (enumtype=='float') ? "floats." : "integers.";
                for(var j in selected) {
                    if(checkType(selected[j], chkfun, label+" has to be a list of "+t)) {
                        break;
                    }
                }
            }
        }
        return !$scope.errors;
    }

    function checkType(value, fun, msg) {
        if( value != fun(value).toString() ) {
            if($scope.errors == null) $scope.errors = [];
            $scope.errors.push(msg);
            return false;
        }
        return true;
    }

	$scope.codeMirrorConfig = {
		lineNumbers: true,
		mode: 'erlang',
		readOnly: true,
		styleSelectedText: true
	};
	$scope.panels = [
		{ path: "path1", content: "" },
		{ path: "path2", content: "" }
	];
	$scope.itemsConfig = {
		query: function(query) {
			var group = $scope.currentGroup;
			query.callback({results: group ? group.items : []});
		}
	};

	var prevId = null;

	var update = function() {
		if ($location.path() != "/duplicates") return;

		var group = locationSearch.get("group");
		if (!group) {
			group = 0;
			locationSearch.set("group", group);
			$location.replace();
		}
		var items = locationSearch.get("items");
		if (!items) {
			items = "0,1";
			locationSearch.set("items", items);
			$location.replace();
		}

		var results = duplicates.lastResults();
		var state = duplicates.resultState();
		if (results) {
			var currentGroup = results.groups[group];
			$scope.currentGroup = currentGroup;
			if (currentGroup) {
				angular.forEach(currentGroup.items, function(item, index) {
					item.id = index;
					var fileName = item.path.replace(/.*\//, "");
					item.text = fileName + ":" + (item.start.line+1);
				});
				var indexes = items.split(",");
				angular.forEach(indexes, function(itemIndex, panelIndex) {
					$scope.panels[panelIndex].item = currentGroup.items[itemIndex];
				});
			}

			var cId = locationSearch.get("id");
			if(cId != prevId) {
				prevId = cId;

				if(state.dbchanged && !results.error) {
					if(state.forSelection) {
						$rootScope.$broadcast("message:show",
							{	title: "Database changed",
								msg: "The database has changed since the analysis was run. The groups are not valid and the shown results are probably off.",
								buttons: [	{text: "OK", default: true}],
								disableable: "duplicatesResultsInvalid"});
					} else {
						$rootScope.$broadcast("message:show",
							{	title: "Database changed",
								msg: "The database has changed since the analysis was run. The groups are not valid and the shown results are probably off.",
								large: "Would you like to run the same duplicates search on the current database?",
								buttons: [	{text: "Run", call: run_again},
											{text: "Cancel", default: true}],
								disableable: "duplicatesResultsInvalid"});
					}
				}
			}
		}
	};

	var run_again = function() {
		$scope.generate();
	}

	var getItems = function() {
		return _.map($scope.panels, function(panel) {
			return panel.item || $scope.currentGroup.items[0];
		});
	};

	var getItemIndexes = function() {
		return _.map(getItems(), function(item) { return item.id; }).join(",");
	};

	// for synchronized scroll position
	var suffix =
		"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" +
		"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";

	angular.forEach($scope.panels, function(panel, panelIndex) {
		var item = function() { return panel.item };
		$scope.$watch(item, function(item) {
			if (item) {
				locationSearch.set("items", getItemIndexes());
				item.highlight = [item.start, item.end];
				angular.forEach($scope.currentGroup.items, function(item) {
					item.multiHighlight = [];
				});
				$http.get("api/getFile", {params: {file: item.path}}).success(function(file) {
					item.content = file.content + suffix;
				});
			}
		});
	});

	$scope.showDiff = function() {
		var items = getItems();
		var url = "api/duplicates/diff/" + duplicates.foregroundId() + "/" + locationSearch.get("group");
		$http.get(url, {params: {items: getItemIndexes()}}).success(function(response) {
			angular.forEach(response.ranges, function(range, index) {
				items[index].multiHighlight = range;
			});
		});
	};

	$scope.$on("$routeUpdate", update);

	$scope.$watch(duplicates.lastResults, update);

}
