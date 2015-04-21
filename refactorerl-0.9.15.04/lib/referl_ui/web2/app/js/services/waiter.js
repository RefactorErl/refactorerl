'use strict';

angular.module('referl.services').factory('waiter', function() {

	function waiter() {
		var waitingFor = null;

		return {
			/**
			 * Sets the instance to a waiting mode until the passed task
			 * is not resolved.
			 */
			waitFor: function(task) {
				waitingFor = task;
				var reset = function() {
					if (waitingFor === task) {
						waitingFor = null;
					}
				};
				task.then(reset, reset);
			},

			/** Sets the instance to normal (not waiting) mode. */
			reset: function() {
				waitingFor = null;
			},

			/** Returns true if the instance is waiting. */
			waiting: function() {
				return waitingFor !== null;
			}
		};
	}

	return waiter;

});
