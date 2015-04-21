'use strict';

angular.module('referl.services').factory('jobs', function($q, channel, suspendable, auth, $rootScope, $http) {

	var foregroundId = null;
	var foregroundIdPromise = null; // resolved when id is received

	var all        = { running: [], unseen: [] }; // all = background + foreground
	var background = { running: [], unseen: [] }; // background = all - foreground

	var updateBackground = function() {
		angular.forEach(all, function(list, category) {
			background[category] = _.without(list, foregroundId);
		});
	};

	var isUnseenFinished = function(id, ids) {
		return _.contains(ids.unseen, id) && !_.contains(ids.running, id);
	};

	var on = {

		ready: function() {
			channel.send("get_queue_ids");
		},

		queue_ids: suspendable(function(event, ids) {
			all = ids;
			updateBackground();
		}),

		job_started:function(event, id) {
			$rootScope.$broadcast("jobs:started", id);
		},

		job_finished: function(event, id) {
			$rootScope.$broadcast("jobs:finished", id);
		}

	};

	channel.on(on);

	// reload queue ids after login/logout
	$rootScope.$watch(auth.getUser, on.ready);

	var backgroundIds = function() {
		return background.running + "|" + background.unseen;
	};
	$rootScope.$watch(backgroundIds, function() {
		$rootScope.$broadcast("jobs:backgroundChange", background.running, background.unseen);
	});

	var rejectIfNotForegroundFor = function(id) {
		return function(input) {
			if (id !== foregroundId) return $q.reject();
			return input;
		};
	};
	var downloadIfInForeground = function(id, download) {
		var rejectIfNotForeground = rejectIfNotForegroundFor(id);
		var returnData = function(response) {
			return response.data;
		};
		return $q.when(id)
			.then(rejectIfNotForeground)
			.then(download)
			.then(rejectIfNotForeground)
			.then(returnData);
	};

	var jobs = {

		foregroundId:              function() { return foregroundId; },
		getBackgroundRunningCount: function() { return background.running.length; },
		getBackgroundUnseenCount:  function() { return background.unseen.length; },

		/**
		 * Sets the foreground job to a new job. Id has to be defined by passing a http promise
		 * which resolves to a request returning a json having an id attribute. Ex. {id: 123}
		 */
		newBackground: function(httpPromise) {
			// Don't have to do anything indeed. The backend will send the new list of queue_ids,
			// so the counters will be updated automatically.
		},

		/**
		 * Sets the foreground job to a new job. Id has to be defined by passing a http promise
		 * which resolves to a request returning a json having an id attribute. Ex. {id: 123}
		 */
		newForeground: function(httpPromise) {
			var idPromise = httpPromise.then(function(httpResponse) {
				return httpResponse.data.id;
			});
			return jobs.newForegroundWithId(idPromise);
		},

		/**
		 * Sets the foreground job to a new job. Id has to be defined by passing the id
		 * or a promise resolving to an id.
		 */
		newForegroundWithId: function(idPromise) {
			jobs.toBackground(); // clears the current foregroundId

			foregroundIdPromise = idPromise = $q.when(idPromise);

			// We have to suspend processing of these until we don't know the
			// foregroundId to not display it as a background job.
			// See unit test about "defer queue counting" for better explanation.
			on.queue_ids.suspend();
			idPromise.then(function(id) {
				if (foregroundIdPromise === idPromise) {
					jobs.toForeground(id);
				}
				on.queue_ids.allow();
			});
			return idPromise;
		},

		/**
		 * Returns a promise which will be resolved when the job identified by the id 
		 * will be finished.
		 * It works safely only if the job was already "seen" by the user.
		 */
		waitTillFinished: function(id) {
			if (isUnseenFinished(id, all)) {
				return id;
			} else {
				var defer = $q.defer();
				var deregister = channel.on("queue_ids", function(event, ids) {
					if (isUnseenFinished(id, ids)) {
						deregister();
						defer.resolve(id);
					}
				});
				return defer.promise;
			}
		},

		/** Brings the job identified by the id to the foreground */
		toForeground: function(id) {
			if (foregroundId != id) {
				foregroundId = id;
				updateBackground();
			}
			// id might be still null if we are waiting for it, so these
			// can not be depend on the condition above
			foregroundIdPromise = $q.when(id);
		},

		/** Sends the current foreground job to background - if there is one */
		toBackground: function() {
			jobs.toForeground(null);
		},

		markSeen: function(job) {
			$http.post("api/jobs/markSeen", {id: job.id});
		},

		cancelForeground: function() {
			if (foregroundId) {
				jobs.remove(foregroundId).then(jobs.toBackground);
			}
		},

		/**
		 * "To display" means: reject the returned promise if the requested
		 * job is not in foreground.
		 */
		getResultsToDisplay: function(id, page) {
			return (
				downloadIfInForeground(id, function() {
					var page_ = page ? page : 1;
					return $http.get("api/jobs/results/" + id + "?page=" + page_);
				})
				.then(function(job) {
					if (job.notFinished) {
						return $q.when(id)
							.then(jobs.waitTillFinished)
							.then(jobs.getResultsToDisplay);
					} else {
						return job;
					}
				})
			);
		},

		/**
		 * "To display" means: reject the returned promise if the requested
		 * job is not in foreground.
		 */
		getParametersToDisplay: function(id) {
			return downloadIfInForeground(id, function() {
				return $http.get("api/jobs/parameters/" + id);
			});
		},

		remove: function(id) {
			return $http.post("api/jobs/remove", {
				id: id
			});
		}

	};

	return jobs;

});
