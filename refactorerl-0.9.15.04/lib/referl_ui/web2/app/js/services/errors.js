'use strict';

angular.module('referl.services').service('errors', function($http, $rootScope) {

	var count = null;
	var errors = null;

	$rootScope.$on("database:modification-done", function() {
		count = null;
		errors = null;
	});

	return {

		count: function() {
			if (!count) {
				count = $http.get("api/errors/count").then(function(response) {
					return response.data.count;
				});
			}
			return count;
		},

		errors: function() {
			if (!errors) {
				errors = $http.get("api/errors").then(function(response) {
					return response.data;
				});
			}
			return errors;
		}

	};

});
