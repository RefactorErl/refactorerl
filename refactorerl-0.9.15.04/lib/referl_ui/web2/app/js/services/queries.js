'use strict';

angular.module('referl.services').factory('queries', function(jobType) {

	var queries = jobType('queries');

	var superExecute = queries.execute;
	queries.execute = function(parameters) {
		if (!/^\s*@/.test(parameters.queryText)) {
			delete parameters.from;
		}
		return superExecute.apply(this, arguments);
	};

	return queries;

});
