'use strict';

angular.module('referl.services').factory('smartLocation', function(locationSearch, $location, shortener) {

	var smartLocation = {

		setJobId: function(id) {
			locationSearch.setShort("id", id);
		},

		getJobId: function() {
			return locationSearch.getShort("id");
		},

		getResultPage: function() {
			return locationSearch.get("page");
		},

		createJobIdPart: function(id) {
			return "id=" + shortener.fromInt(id);
		},

		setPage: function(pageName) {
			return $location.path("/" + pageName);
		},

		getPage: function() {
			return $location.path().substring(1);
		}

	};

	return smartLocation;

});
