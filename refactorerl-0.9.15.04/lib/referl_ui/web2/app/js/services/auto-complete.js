'use strict';

angular.module('referl.services').factory('autoComplete', function($http, $timeout) {

	var factory = function(path, throttle) {
		if (typeof throttle !== "number") {
			throttle = 100;
		}
		var
			timeout,             // current scheduled but not yet executed request
			requestSequence = 0; // sequence used to drop out-of-order responses
		return function(query) {
			$timeout.cancel(timeout);
			timeout = $timeout(function() {
				requestSequence += 1; // increment the sequence
				var requestNumber = requestSequence; // this request's sequence number
				$http
					.post(path, {
						term: query.term,
						context: query.context
					})
					.success(function(response) {
						if (requestNumber < requestSequence) return;
						//response.results.unshift({id: query.term, text: query.term});
						query.callback(response);
					});
			}, throttle);
		};
	};

	var autoComplete = {

		createModuleQuery: function(throttle) {
			return factory("api/autocomplete/module", throttle);
		},

		createFunctionQuery: function(throttle) {
			return factory("api/autocomplete/function", throttle)
		},

		createFullModuleQuery: function(throttle) {
			return factory("api/autocomplete/fullModule", throttle);
		},

		queryInputQuery: function(throttle) {
			return factory("api/autocomplete/query", throttle);
		},

		createDupcodeSearchQuery: function(throttle) {
			return factory("api/autocomplete/dupcodeNames", throttle);
		},

		commonPrefix: function(strings) {
			if (strings.length < 2) return strings[0] || "";
			var sorted = strings.slice(0).sort();
			var prefix = sorted[0];
			var last   = sorted[sorted.length - 1];
			var length = prefix.length;
			while (length && last.slice(0, length) != prefix) {
				prefix = prefix.substring(0, --length);
			}
			return prefix;
		},

		/**
		 * In case of
		 *    item.id = fixPrefix + item.text
		 * the function returns the fixPrefix, otherwise the item.id.
		 * See Spec in the unit tests.
		 **/
		fixPrefix: function(item) {
			return autoComplete.fixPrefix2(item.id, item.text);
		},

		fixPrefix2: function(string, suffix) {
			var stringLen = string.length, suffixLen = suffix.length;
			if (string.substring(stringLen - suffixLen) === suffix) {
				return string.substring(0, stringLen - suffixLen);
			} else {
				return string;
			}
		},

		/**
		 * Gets all the fix prefixes of the results, finds the common
		 * prefixes of these, removes that from all the results, then
		 * returns this prefix.
		 */
		cleanPrefixes: function(results) {
			var fixPrefixes = [];
			angular.forEach(results, function(item) {
				var fixPrefix = autoComplete.fixPrefix(item);
				if (!_.contains(fixPrefixes, fixPrefix)) {
					fixPrefixes.push(fixPrefix);
				}
			});
			var common = autoComplete.commonPrefix(fixPrefixes);
			if (common) {
				angular.forEach(results, function(item) {
					item.id = item.id.substring(common.length);
				});
			}
			return common;
		}

	};

	return autoComplete;

});
