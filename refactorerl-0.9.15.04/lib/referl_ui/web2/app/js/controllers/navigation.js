'use strict';

function NavigationCtrl($scope, errors) {
	$scope.$watch(errors.count, function(count) {
		count.then(function(value) {
			$scope.errorsCount = value;
		});
	});

	$scope.$watch("errorsCount", function() {
		$scope.$broadcast("bounceErrors");
	});
}
