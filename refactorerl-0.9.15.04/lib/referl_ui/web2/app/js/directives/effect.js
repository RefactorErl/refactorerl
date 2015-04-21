'use strict';

angular.module('referl.directives').directive('reEffect', function(effects) {
	return {

		restrict: 'A',

		link: function(scope, elem, attrs) {
			scope.$on(attrs.reEffectTrigger, function() {
				effects.apply(attrs.reEffect, elem);
			});
		}

	};
});
