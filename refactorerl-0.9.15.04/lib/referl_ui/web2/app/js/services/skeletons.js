'use strict';

angular.module('referl.services').factory('skeletons', function($http) {

	var skeletons = {
		save: function(skeleton) {
			return $http.post("api/skeletons/save", skeleton).then(function(response) {
				return response.data;
			});
		}
	};
	return skeletons;

});
