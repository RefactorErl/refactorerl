'use strict';

function EditSkeletonCtrl($scope, skeletons) {

	var saveInProgress = false;
	if (!$scope.skeleton) {
		$scope.skeleton = {};
	}

	if ($scope.skeleton.name) {
		$scope.mode = "edit";
		$scope.skeleton.new = false;
	} else if ($scope.skeleton.source) {
		$scope.mode = "create-from";
		$scope.skeleton.new = true;
	} else {
		$scope.mode = "create";
		$scope.skeleton.new = true;
	}

	$scope.canSave = function() {
		return (
			!saveInProgress &&
			$scope.skeleton.name &&
			$scope.skeleton.source
		);
	};

	$scope.save = function() {
		if (!$scope.canSave()) return;
		saveInProgress = true;
		skeletons.save($scope.skeleton).then(function(result) {
			saveInProgress = false;
			if (result.ok) {
        $("#skeleton-modal").modal("hide");
			} else {
				alert(result.error);
			}
		});
	};

	$scope.sourceTab = function() {
		$scope.$broadcast("focusCancel");
	};

	$scope.sourceEnter = function() {
		$scope.$save();
	};

}
