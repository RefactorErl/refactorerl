'use strict';

angular.module('referl.directives').directive('reAutoFocus', function($window) {
	var timer;

	return function(scope, elem, attr) {
		if (timer) $window.clearTimeout(timer);

		// why digest needlessly? => setTimeout instead $timeout
		timer = $window.setTimeout(function() {
			elem[0].focus();
		}, 0);
	};
});
