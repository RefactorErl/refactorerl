'use strict';

angular.module('referl.directives').directive('reScrollToHere', function() {
	return {

		restrict: 'A',

		link: function(scope, elem, attrs) {
			var parent = elem.parent();
			scope.$watch(attrs.reScrollToHere, function(condition) {
				if (condition) {
					var viewportTop = parent.scrollTop();
					var viewportHeight = parent.height();
					var viewportBottom = viewportTop + viewportHeight;
					var elemTop = viewportTop + elem.position().top;
					var elemBottom = elemTop + elem.outerHeight();
					if (elemBottom > viewportBottom) {
						viewportTop = elemBottom - viewportHeight;
					}
					if (viewportTop > elemTop) {
						viewportTop = elemTop;
					}
					parent.scrollTop(viewportTop);
				}
			});
		}

	};
});
