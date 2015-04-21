'use strict';

angular.module('referl.directives').directive('reClickOutside', function($document) {
	return {
		link: function(scope, element, attrs) {
			element.bind("mouseup", function(event) {
				event.stopPropagation();
			});
			var onDocMouseUp = function() {
				scope.$apply(attrs.reClickOutside);
			};
			$document.bind("mouseup", onDocMouseUp);
			scope.$on("$destroy", function() {
				$document.unbind("mouseup", onDocMouseUp);
			});
		}
	};
});
