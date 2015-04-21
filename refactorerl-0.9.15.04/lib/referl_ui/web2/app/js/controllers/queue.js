'use strict';

function QueueCtrl($scope, $http, $location, smartLocation, $rootScope, jobs) {

	$scope.queue = false;

	var removeForeground = function() {
		$scope.queue.items = _.filter($scope.queue.items, function(item) {
			return item.id != jobs.foregroundId();
		});
	};

	var toDate = function(values) {
		var secs = values[0] * 1000000 + values[1];
		return new Date(secs * 1000);
	};

	var reload = function() {
		$http.get("api/jobs/queue").then(function(response) {
			angular.forEach(response.data.items, function(item) {
				item.launchedDate = toDate(item.launched);
				if (item.finished) {
					item.finishedDate = toDate(item.finished);
				}
			});
			$scope.available = true;
			$scope.queue = response.data;
			removeForeground();
		});
	};

	$scope.goto = function(item) {
		if (item.removed) return;
		smartLocation.setPage(item.page);
		smartLocation.setJobId(item.id);
		$scope.closeQueue();
	};

	$scope.remove = function(job) {
		job.removed = true;
		jobs.remove(job.id);
	};

	$scope.$on("jobs:backgroundChange", function() {
		removeForeground();
		reload();
	});

	reload();

}
