'use strict';

angular.module('referl.services').factory('jobType', function(jobs, $http, $q, waiter, smartLocation) {

	function jobType(name) {
		var resultsWaiter = waiter();
		var lastResults = null; // set by display function
		var parameters = {};
		var resultState = null;

		function displayResults(job) {
			lastResults = job.result;
			resultState = job.state;
			jobs.markSeen(job);
			return job.id;
		}

		function displayParameters(_parameters_) {
			parameters = _parameters_;
		}

		var instance = {

			foregroundId: jobs.foregroundId,

			waiting:     resultsWaiter.waiting,
			lastResults: function() { return lastResults; },
			resultState: function() { return resultState; },
			parameters:  function() { return parameters; },

			displayJob: function(job){ displayResults(job); },

			/**
			 * Executes the job defined by the parameters. If the current location
			 * is the job type, the job is execute in the foreground, otherwise
			 * in the background.
			 */
			execute: function(parameters, inBackground) {
				function execute() {
					return $http.post("api/" + name + "/execute", parameters);
				}

				if (!inBackground && instance.isActive()) {
					lastResults = null;
					displayParameters(parameters);
					resultsWaiter.waitFor(
						jobs.newForeground(execute())
							.then(jobs.waitTillFinished)
							.then(jobs.getResultsToDisplay)
							.then(displayResults)
							.then(function(id){jobs.getParametersToDisplay(id).then(displayParameters)})
					);
				} else {
					jobs.newBackground(execute());
				}
			},

			isActive: function() {
				return smartLocation.getPage() === name;
			},

			toBackground: function() {
				lastResults = null;
				resultsWaiter.reset();
				jobs.toBackground();
			},

			toForeground: function(id) {
				if (id == jobs.foregroundId()) return;
				if (!instance.isActive()) return;
				lastResults = null;
				resultsWaiter.reset();
				jobs.toForeground(id);
                var page = smartLocation.getResultPage();
                page = (page != "") ? page : 1;
				if (id) {
					resultsWaiter.waitFor($q.all([
						jobs.getParametersToDisplay(id).then(displayParameters),
						jobs.getResultsToDisplay(id, page).then(displayResults)
					]));
				}
			},

			cancelForeground: function() {
				resultsWaiter.reset();
				jobs.cancelForeground();
			},

			updateResults: function(job) {
				lastResults = job.result;
				jobs.markSeen(job);
			},

			/**
			 * Sets up the navigation until the scope is not destroyed:
			 *   - bring the job into foreground when location contains a job id
			 *   - put the foreground id into the location
			 *   - put the job into background when scope is destroyed
			 */
			setupNavigationIn: function($scope) {
				$scope.$watch(smartLocation.getJobId, instance.toForeground);

				$scope.$watch(jobs.foregroundId, function(id) {
					// condition to not have an empty job id in the url
					// during the short period when there is no id yet
					// because we are waiting for the backend to return it
					if (id || !instance.waiting()) {
						smartLocation.setJobId(id);
					}
				});

				$scope.$on("$destroy", instance.toBackground);
			}

		};

		return instance;
	}

	return jobType;

});
