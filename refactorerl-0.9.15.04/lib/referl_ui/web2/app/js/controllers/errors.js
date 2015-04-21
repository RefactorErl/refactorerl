'use strict';

function ErrorsCtrl($scope, $http, locationSearch, errors) {
	$scope.loading = true;
	$scope.errors = [];

	$scope.codeMirrorConfig = {
		lineNumbers: true,
		mode: 'erlang',
		readOnly: true,
		styleSelectedText: true
	};

	var openIfNeeded = function() {
		var index = locationSearch.get("error");
		angular.forEach($scope.errors, function(error) {
			error.open = false;
		});
		var current = $scope.errors[index];
		if (current) {
			open(current);
		}
	};

	var open = function(error) {
		error.open = true;
		var parameters = {
			file: error.file
		};
		$http.get("api/getFile", {params: parameters}).success(function(file) {
			error.content = file.content;
		})
	};

	$scope.$watch(locationSearch.getterFor("error"), openIfNeeded);

	var load = function(promise) {
		$scope.errors = [];
		promise.then(function(errors) {
			angular.forEach(errors, function(error, index) {
				error.href = "#/errors?error=" + index;
			});
			$scope.errors = errors;
			$scope.loading = false;
			openIfNeeded();
		});
	};

	//load();
	$scope.$watch(errors.errors, load);

}
