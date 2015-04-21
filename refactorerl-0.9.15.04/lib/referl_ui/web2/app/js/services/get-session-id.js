'use strict';

angular.module('referl.services').factory('getSessionId', function($q, $http, $window) {
	return function() {
		var defer = $q.defer();
		if ($window.sessionId) {
			defer.resolve($window.sessionId);
		} else {
			$http.get("api/session/id").success(function(sessionId) {
				defer.resolve(sessionId);
			});
		}
		return defer.promise;
	};
});
