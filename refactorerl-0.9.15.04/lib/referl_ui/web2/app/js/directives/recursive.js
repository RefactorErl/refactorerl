'use strict';

angular.module('referl.directives').directive('reRecursive', function() {
	return {

		restrict: 'A',
		scope: true,

		compile: function compile(elem, attrs, linker) {
			var template = elem.html();

			return function(scope, elem, attrs, ctrl) {
				var expression = attrs.reRecursive;
				var match = expression.match(/^\s*(.+)\s+as\s+(.*)\s*$/);
				if (!match) {
					throw new Error("Expected reRecursive in form of '_data_ as _name_' but got '" + expression + "'.");
				}
				var dataExpression = match[1];
				var name = match[2];

				// for the reRecursion
				scope.$$reRecursiveName = name;
				scope.$$reRecursiveTemplate = template;

				scope.$watch(dataExpression, function(data) {
					scope[name] = data;
				});
			};
		}

	};
});

angular.module('referl.directives').directive('reRecursion', function($compile) {
	return {

		restrict: 'A',
		scope: true,

		link: function(scope, elem, attrs) {
			var dataExpression = attrs.reRecursion;
			var name = scope["$$reRecursiveName"];
			scope.$watch(dataExpression, function(data) {
				scope[name] = data;
			});

			elem.html(scope.$$reRecursiveTemplate);
			var linker = $compile(elem.contents());
			linker(scope);
		}

	};
});
