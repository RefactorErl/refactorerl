'use strict';

angular.module('referl.services').factory('locationSearch', function($location, shortener) {

	var locationSearch = {

		set: function(name, value) {
			var search = $location.search();
			if (!value && value !== 0) {
				delete search[name];
			} else {
				search[name] = value;
			}
			$location.search(search);
		},

  clear: function() {
    var search = $location.search();
    for (var i in search) { $location.search(i, null); }
  },

		get: function(name) {
			var search = $location.search();
			return search[name];
		},

		getPart: function(name) {
			return name + "=" + locationSearch.get(name);
		},

        getterFull: function() {
            return function() {
                return $location.absUrl();
            };
        },

		getterFor: function(name) {
			return function() {
				return locationSearch.get(name);
			};
		},

		getterForPart: function(name) {
			return function() {
				return locationSearch.getPart(name);
			};
		},

		setShort: function(name, intValue) {
			locationSearch.set(name, shortener.fromInt(intValue));
		},

		getShort: function(name) {
			return shortener.toInt(locationSearch.get(name));
		},

		getShortPart: function(name) {
			return name + "=" + locationSearch.getShort(name);
		}

	};

	return locationSearch;

});
