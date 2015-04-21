'use strict';

function GraphsCtrl($scope, $rootScope, autoComplete, graphs, locationSearch, smartLocation, $http, $q, $cookies, $timeout) {

	graphs.setupNavigationIn($scope);

	$scope.historyType = "graphs";

	// used by the view to extend options
	$scope.extend = angular.extend;

	// The main model
	$scope.parameters = {
		level: "module",
		type: "all",
		format: "smart",
		excludeOtp: false
	};

	// Options
	$scope.levels = [
		{value: "function",      text: "Function"},
		{value: "module",        text: "Module"},
		{value: "functionblock", text: "Set of modules"}
	];
	$scope.types = [
		{value: "all",           text: "Whole graph"},
		{value: "cycles",        text: "Cyclic sub-graph"}
	];
	$scope.formats = [
		{value: "text",          text: "Plain textual relations"},
		{value: "svg",           text: "Svg file"},
		{value: "smart",         text: "Smart graph"},
		{value: "collapsible",   text: "Collapsible graph (beta)"}
	];

    var allowRegexp = function(term, data) {
        if ( _.filter(data, function(elem) {
            return term.localeCompare(elem.text)===0;
        }).length===0) {
            return {id:term, text:term};
        }
    };

	// Function level
	$scope.rootFunctionConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createFunctionQuery(),
    	multiple: true,
		minimumInputLength: 1
	};
	$scope.connectionFunctionConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createFunctionQuery(),
    	multiple: true,
		minimumInputLength: 1
	};
	$scope.excludeFunctionsConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createFunctionQuery(),
		multiple: true,
		minimumInputLength: 1
	};
	/*$scope.functionConnectionsConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createFunctionQuery(),
		multiple: true,
		allowClear: true,
		minimumInputLength: 1
	};*/
	$scope.noLeafFunctionsConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createFunctionQuery(),
		multiple: true,
		minimumInputLength: 1
	};

	// Module level
	$scope.rootModuleConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createModuleQuery(),
    	multiple: true,
		minimumInputLength: 1
	};
	$scope.connectionModuleConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createModuleQuery(),
    	multiple: true,
		minimumInputLength: 1
	};
	$scope.excludeModulesConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createModuleQuery(),
		multiple: true,
		minimumInputLength: 1
	};
	/*$scope.moduleConnectionsConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createModuleQuery(),
		multiple: true,
		allowClear: true,
		minimumInputLength: 1
	};*/
	$scope.noLeafModulesConfig = {
        createSearchChoice: allowRegexp,
		query: autoComplete.createModuleQuery(),
		multiple: true,
		minimumInputLength: 1
	};

	// Set of modules (functionblock) level

	var directories;
	var getDirectories = function() {
		if (directories) return $q.when(directories);
		return $http
			.get("api/getDirectories")
			.then(function(response) {
				return directories = _.map(response.data, function(dir) {
					return { id: dir, text: dir };
				});
			});
	};

    //Would be better to use allowRegexp instead of this
    //Also would be better to derive the configs from a default one
    var directoryQuery = function(query) {
		$scope.$apply(function() {
			getDirectories().then(function(directories) {
				var options = [];
				if (query.term.length) {
					options.push({
						id: query.term,
						text: query.term
					});
				}
				options.push.apply(options, directories);
				query.callback({
					results: options,
					more: false
				});
			});
		});
	};

	$scope.subjectsConfig = {
		multiple: true,
		query: directoryQuery
	};


	$scope.excludeLibsConfig = {
		multiple: true,
		query: directoryQuery,
		allowClear: true
	};

	// Visibilities
	$scope.rootFunctionVisible = function() {
		return (
			($scope.parameters.level == 'function') &&
			($scope.parameters.connectionFunction == '')
		);
	};
	$scope.connectionFunctionVisible = function() {
		return (
			($scope.parameters.level == 'function') &&
			($scope.parameters.rootFunction == '')
		);
	};
	$scope.excludeFunctionsVisible = function() {
		return (
			($scope.parameters.level == 'function')
		);
	};
	$scope.rootModuleVisible = function() {
		return (
			($scope.parameters.level == 'module') &&
			($scope.parameters.connectionModule == '')
		);
	};
	$scope.connectionModuleVisible = function() {
		return (
			($scope.parameters.level == 'module') &&
			($scope.parameters.rootModule == '')
		);
	};
	$scope.excludeModulesVisible = function() {
		return (
			($scope.parameters.level == 'module')
		);
	};
	$scope.excludeLibsVisible = function() {
		return (
			($scope.parameters.level != 'functionblock')
		);
	};
	$scope.excludeOtpVisible = function() {
		return (
			($scope.parameters.level != 'functionblock')
		);
	};
	$scope.noLeafFunctionsVisible = function() {
		return (
			($scope.parameters.level == 'function')
		);
	};
	$scope.noLeafModulesVisible = function() {
		return (
			($scope.parameters.level == 'module')
		);
	};
	$scope.subjectsVisible = function() {
		return (
			($scope.parameters.level == 'functionblock')
		);
	};

	// Parameters binding
	$scope.$watch(graphs.parameters, function(parameters, old) {
		if (old === parameters) return; // don't clear with the initial empty parameters
		$scope.parameters = parameters;
	});

  // rootModule and rootFunction bindinds
  $scope.$watch(locationSearch.getterFor('rootModule'), function(rootModule) {
    if (!rootModule) { return; }
    $scope.parameters.format = 'collapsible';
    $scope.parameters.level = 'module';
    $scope.parameters.rootModule = { id: rootModule, text: rootModule };
    $("#rootModule").select2("val", rootModule);
  });

  $scope.$watch(locationSearch.getterFor('rootFunction'), function(rootFunction) {
    if (!rootFunction) { return; }
    $scope.parameters.format = 'collapsible';
    $scope.parameters.level = 'function';
    $scope.parameters.rootFunction = { id: rootFunction, text: rootFunction };
    $("#rootFunction").select2("val", rootFunction);
  });

  $scope.$watch(locationSearch.getterFor('execute'), function(execute) {
    if (execute) {
      $timeout(function() {
        locationSearch.set('execute');
        locationSearch.set('rootFunction');
        locationSearch.set('rootModule');
        $scope.generate();
      });
    }
  });

  var run_again = function() {
    $scope.generate();
  };

  var check_validity = function(results) {
    if(results && !results.error) {
      var state = graphs.resultState();
      if(state.dbchanged) {
        $rootScope.$broadcast("message:show", {
          title: "Database changed",
          msg: "The database has changed since the analysis was run. Nodes and relations might be invalid or missing.",
          large: "Would you like to run the same dependency analysis on the current database?",
          buttons: [{text: "Run", call: run_again}, {text: "Cancel", default: true}],
          disableable: "graphResultsInvalid"
        });
      }
    }
  }

  $scope.$watch(graphs.lastResults, check_validity);


	// Results
	$scope.waiting = graphs.waiting;
	$scope.results = graphs.lastResults;

	$scope.generate = function() {
		if ($scope.parameters.format == "text" && $scope.parameters.level == "functionblock") {
			alert('Sorry, textual representation is not supported for "Set of modules" level.');
		} else {
			locationSearch.set("svg", false);
			graphs.execute($scope.parameters);
		}
	};

	$scope.seeSvg = function($event) {
		try {
			locationSearch.set("svg", true);
		} finally {
			$event.preventDefault();
		}
	};

	$scope.$watch(locationSearch.getterFor("svg"), function(svg) {
		$scope.displaySvg = svg;
	});

  // Collapsible graph
  $scope.updateCollapsibleSettings = function() {
    $cookies.collapsibleMaxDepth  = $('#collapsible-max-depth').val();
    $cookies.collapsibleDirection = $('#collapsible-direction').val();

    // Close modal
    $("#collapsible-modal").modal("hide");
  };

  $scope.collapsibleMaxDepth = function() {
    // Default max-depth is 2
    if (typeof($cookies.collapsibleMaxDepth) === 'undefined') {
      return 2;
    } else {
      return parseInt($cookies.collapsibleMaxDepth);
    }
  };

  $scope.collapsibleDirection = function() {
    // Default direction is 'both'
    return $cookies.collapsibleDirection || 'both';
  };

  $scope.collapsibleDirections = [
    {value: 'forward',  text: 'Forward'},
    {value: 'backward', text: 'Backward'},
    {value: 'both',     text: 'Both'}
  ];

  $scope.collapsibleSettings = {
    maxDepth:  $scope.collapsibleMaxDepth(),
    direction: $scope.collapsibleDirection()
  };

  $scope.collapsibleAnimation = true;

  $scope.toggleCollapsibleAnimation = function() {
    var $graphContainer = $('.results-collapsible'),
        cGraph           = $graphContainer.data('graph');

    if ($graphContainer.length > 0) {
      if ($scope.collapsibleAnimation) {
        cGraph.stop();
        $scope.collapsibleAnimation = false;
      } else {
        cGraph.start();
        $scope.collapsibleAnimation = true;
      }
    }
  };

  // Search related vars
  $scope.searchResults = [];

  $scope.clearSearchResults = function() {
    $scope.searchResults = [];
    $('#graph-search').val('');
    d3.selectAll('circle').classed('search-result', false)
  };

}
