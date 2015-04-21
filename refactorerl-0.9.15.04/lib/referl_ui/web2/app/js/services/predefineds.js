'use strict';

angular.module('referl.services').factory('predefineds', function($http, $q, duplicates, jobs, smartLocation, queries, $rootScope, $location, locationSearch) {


	var execute = function(node, query) {
		queries.execute({
			from: node,
			queryText: query.query
		});
	};

    var queryFrom = function(node) {
        var parameters = queries.parameters();
        parameters.from = node;
        $rootScope.$broadcast("focusQueryInput");
    };

/*    var sendAsDynfun = function(node) {
    $http.post("api/saveAsDynfun", {
      ref: node.ref,
      display: node.display
    })
    };*/

    var selectionDupcodeAlgorithms = ["matrix", "suffix_tree", "filtered_suffix_tree", "sw_metrics", "matrixfilter"];
    var selectionDupcodeTexts = {
        "matrix" : "Using The Matrix Algorithm", //"Find Duplicates of Selection Using The Matrix Algorithm",
        "suffix_tree" : "Using The Suffix Tree Algorithm", //"Find Duplicates of Selection Using The Suffix Tree Algorithm",
        "filtered_suffix_tree" : "Using The Suffix Tree Algorithm With Filters", //"Find Duplicates of Selection Using The Suffix Tree Algorithm With Filters",
        "sw_metrics" : "Using The Software Metrics Algorithm", //"Find Duplicates of Selection Using The Software Metrics Algorithm",
        "matrixfilter" : "Using The Software Metrics With Syn. Filters Algorithm", //"Find Duplicates of Selection Using The Software Metrics With Syn. Filters Algorithm"
    };
    var dupcodeFor = function(file, selection, usingAlgorithm) {
        var parameters = {
            forSelection: true,
            algorithm: usingAlgorithm,
            file: file,
            from: selection.from,
            to: selection.to,
            fromIndex: selection.fromIndex,
            toIndex: selection.toIndex,
            text: selection.text
        };

        duplicates.execute(parameters);

        /*function execute() {
            return $http.post("api/duplicates/execute", parameters);
        }

        var prevId = jobs.foregroundId();

        jobs.newForeground(execute())
            .then(jobs.waitTillFinished)
            .then(jobs.getResultsToDisplay)
            .then(function(job){
                console.log(job.result);
                if(job.result.groups.length) {
                    duplicates.displayJob(job);

                    smartLocation.setPage("duplicates");
                    smartLocation.setJobId(job.id);
                } else {
                    jobs.toForeground(prevId);
                    $rootScope.$broadcast("message:show", {title: "Search finished", msg: "No duplicates were found."});
                }
            });*/
    };

    var predefineds = {
        get: function(file, index) {
            return $http
                .post("api/getPredefineds", {
                    file: file,
                    index: index + 1
                })
                .then(function(response) {
                    return response.data;
                });
        },

        getAsContextMenu: function(file, selection, addSeparator) {
            return predefineds.get(file, selection.cursorIndex).then(function(data) {
                var items = [];
                if (data.node) {
                    angular.forEach(data.queries, function(query) {
                        items.push({name: query.label, className: "wrap",
                            callback: function() {
                                if(query.query != "") execute(data.node, query);
                            }});
                    });
                    /*items.push({name: "Send \""+data.node.display+"\" as dynfun",
                        className: "wrap",
                        callback: function() {
                            sendAsDynfun(data.node);
                    }});*/
                    items.push({name: "Custom Query From Here", callback: function() {
                        queryFrom(data.node);
                    }});
                    items.push({name: "Collapsible Graph From Here", callback: function() {
                      $location.path('/graphs');
                      locationSearch.clear();

                      switch (data.type) {
                        case 'module':
                          var rootModule = data.node.display.replace(/\-module\(/, '').replace(/\)\./, '');
                          locationSearch.set('rootModule', rootModule);
                          locationSearch.set('execute', 'true');
                          break;

                        case 'fundef':
                          var rootFunction = data.node.display;
                          locationSearch.set('rootFunction', rootFunction);
                          locationSearch.set('execute', 'true');
                          break;

                        default:
                          console.log('unknown type: ', data.type);
                          break;
                      }
                    }});
                } else {
                    if(data.error) {
                        if(data.error=="nofile") {
                            items.push({name: "This file hasn't been added to the database",
                                className: "warning", disabled: true, callback: function() {

                            }});
                        }
                        if(data.error=="nonode") {
                            items.push({name: "No entity at this position",
                                className: "warning", disabled: true, callback: function() {

                            }});
                        }
                    }
                }

                if(selection.somethingSelected && (!data.error || data.error != "nofile") ) {
                    if (addSeparator && items.length) {
                        items.push("-----");
                    }

                    var callbackFor = function(file, selection, algorithm) {
                        return function() {
                            dupcodeFor(file, selection, algorithm);
                        }
                    }
                    var dupcode_items = [];
                    for(var i in selectionDupcodeAlgorithms) {
                        var algorithm = selectionDupcodeAlgorithms[i];
                        var text = selectionDupcodeTexts[algorithm];
                        dupcode_items.push({"name": text, "callback": callbackFor(file, selection, algorithm)});
                    }
                    items.push({"name": "Find Duplicates of Selection", "items": dupcode_items});
                }

                if (addSeparator && items.length) {
                    items.push("-----");
                }

                return items;
            });
        }
    };

    return predefineds;

});
