'use strict';

angular.module('referl.directives').directive('reTimeAgo', function($timeout) {
	return {

		restrict: 'A',

		link: function(scope, element, attrs) {
			var timeout;
			var update = function() {
				if (timeout) $timeout.cancel(timeout);
				var value = moment(scope.$eval(attrs.reTimeAgo));
				element.text(value.fromNow());
				var delay;
				var elapsed = moment().diff(value);
				if (elapsed < 45 * 1000) { // 45 seconds
					delay = 10 * 1000; // 10 seconds
				} else if (elapsed < 45 * 60 * 1000) { // 45 minutes
					delay = 60 * 1000; // 1 minute
				} else {
					delay = 60 * 60 * 1000; // 1 hour
				}
				timeout = $timeout(update, delay);
			};
			scope.$watch(attrs.reTimeAgo, update);
		}

	};
});
