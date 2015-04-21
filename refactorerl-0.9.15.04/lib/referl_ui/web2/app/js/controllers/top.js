'use strict';

function TopCtrl($scope, queries, jobs, $window, search) {
	$scope.parameters = {};
	$scope.skeleton = {};

	$scope.$watch(queries.parameters, function(parameters) {
		$scope.parameters = parameters;
	});

	var getUnseenFinishedCount = function() {
		return (
			jobs.getBackgroundUnseenCount() -
			jobs.getBackgroundRunningCount()
		);
	};
	$scope.$watch(getUnseenFinishedCount, function(current, old) {
		if (current == old) { // on very first call
			old = 0;
		}
		if (current > old) {
			$scope.$broadcast("bounce");
		}
	});

	angular.extend($scope, {

		isReadOnly: queries.waiting,
		getBackgroundRunningCount: jobs.getBackgroundRunningCount,
		getBackgroundUnseenCount:  jobs.getBackgroundUnseenCount,

		canExecute: function() {
			return !queries.waiting();
		},

		canMoveToBackground: function() {
			return queries.waiting();
		},

		canCancel: function() {
			return queries.waiting();
		},

		cancel: function() {
			return queries.cancelForeground();
		},

		execute: function(inBackground) {
			if (queries.waiting()) {
				queries.toBackground();
				return;
			}
			var params = $scope.parameters;
			params.queryText = $.trim(params.queryText || "");
			if (!params.queryText) {
				$window.alert("Can not execute an empty query!");
				return false;
			}
			if (/^\s*@/.test(params.queryText) && !params.from) {
				$window.alert("Can not execute this query without context!");
				return false;
			}
			queries.execute(params, inBackground);
			return true;
		},

		executeInBackground: function() {
			$scope.execute(true);
		},

		toBackground: queries.toBackground,

		onQueryTab: function() {
			$scope.$broadcast("focusExecute");
		},

    search: function() {
      var params = $scope.parameters;

      params.queryText = $.trim(params.queryText || "");

      if (params.queryText.length < 3) {
        $window.alert('Query must be greater than 3.');
        return false;
      }

      search.execute(params);
      return true;
    },

		saveAsSkeleton: function() {
			var queryText = $.trim($scope.parameters.queryText || "");
			$scope.skeleton = {
				source: queryText
			};
			$scope.editSkeletonOpen = true;
		},

		openQueue: function() {
			$scope.queueOpen = true;
		},

		closeQueue: function() {
			$scope.queueOpen = false;
		},

		closeEditSkeleton: function() {
			$scope.editSkeletonOpen = false;
		}


	});

}
