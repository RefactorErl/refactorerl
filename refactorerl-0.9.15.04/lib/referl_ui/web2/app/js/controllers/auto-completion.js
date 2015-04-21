'use strict';

function AutoCompletionCtrl($scope, $timeout) {
	var closer;
	$scope.$on("open", function() {
		if (closer) {
			$timeout.cancel(closer);
			closer = false;
		}
		$scope.visible = true;
	});

	$scope.$on("close", function() {
		if (closer) $timeout.cancel(closer);
		closer = $timeout(function() {
			$scope.visible = false;
		}, 150);
	});

	$scope.$on("select", function(event, hint) {
		$scope.selected = hint;
	});

	$scope.sourceHighlighter = "\\$[^\\$]*\\$";
}
