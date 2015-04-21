'use strict';

function ModalsCtrl($scope, $cookies, database) {

	$scope.open = function() {
		$('#main-modal').on('hide.bs.modal', function (e) {
			if(!$scope.closeable) {
				e.preventDefault();
			}
		});

		$('#main-modal').on('hidden.bs.modal', function () {
			$scope.closeable = false;
			$scope.isOpen = false;
		});

		$("#main-modal").modal("show");
		$scope.isOpen = true;
	};

	$scope.close = function() {
		if($scope.closeable) {
			$("#main-modal").modal("hide");
			$scope.closeable = false;
			$scope.isOpen = false;
		}
	};

	$scope.isOpen = false;
	$scope.closeable = false;
	$scope.content = {};
	$scope.settings = {disabled: false};

	$scope.$on("database:modification-start", function(event, modification) {
		$scope.open();
		$scope.closeable = false;
		$scope.content = modification;
	});

	$scope.$on("database:modification-done", function(event, modification) {
		$scope.closeable = true;
		if (!modification.direct) {
			$scope.close();
		}
	});

	$scope.$on("message:show", function(event, msg){
		if(! $scope.isOpen && !$cookies[msg.disableable]) {
			$scope.open();
			$scope.closeable = true;
			$scope.content = msg;
		}
	});

	$scope.percentDisplay = function() {
		if ($scope.content.progress) {
			return Math.round($scope.content.progress.percent * 100) + "%";
		} else {
			return "";
		}
	};

	$scope.percentValue = function() {
		if ($scope.content.progress) {
			return $scope.content.progress.percent * 100 + "%";
		} else {
			return "100%";
		}
	};

	var disabled_changed = function() {
		if($scope.content.disableable) {
			if($scope.settings.disabled) {
				$cookies[$scope.content.disableable] = "disabled";
			} else {
				delete $cookies[$scope.content.disableable];
			}
		}
	}

	$scope.$watch("settings.disabled", disabled_changed, true);

}
