'use strict';

describe('service jobs', function() {

	beforeEach(module('referl.services'));
	beforeEach(module('referl.mocks.misc'));
	beforeEach(module("referl.mocks.channel"));

	// TODO: after auth login/logout channel reconnect whatever is fixed, this can be removed
	beforeEach(module(function($provide) {
		$provide.constant("auth", { getUser: angular.noop });
	}));

	it('should count simple queue items', inject(function(jobs, channelEvents) {

		expect(jobs.getBackgroundRunningCount()).toBe(0);
		expect(jobs.getBackgroundUnseenCount()).toBe(0);

		channelEvents.$broadcast("queue_ids", {
			running: [3, 4, 5],
			unseen:  [1, 2]
		});

		expect(jobs.getBackgroundRunningCount()).toBe(3);
		expect(jobs.getBackgroundUnseenCount()).toBe(2);

		channelEvents.$broadcast("queue_ids", {
			running: [5],
			unseen:  [1, 2, 3, 4]
		});

		expect(jobs.getBackgroundRunningCount()).toBe(1);
		expect(jobs.getBackgroundUnseenCount()).toBe(4);

	}));

	it('should count queue items with foreground job', inject(function(jobs, channelEvents) {

		channelEvents.$broadcast("queue_ids", {
			running: [3, 4, 5],
			unseen:  [1, 2]
		});

		jobs.toForeground(3);

		expect(jobs.getBackgroundRunningCount()).toBe(2);
		expect(jobs.getBackgroundUnseenCount()).toBe(2);

		jobs.toForeground(1);

		expect(jobs.getBackgroundRunningCount()).toBe(3);
		expect(jobs.getBackgroundUnseenCount()).toBe(1);

		jobs.toBackground();

		expect(jobs.getBackgroundRunningCount()).toBe(3);
		expect(jobs.getBackgroundUnseenCount()).toBe(2);

	}));

	it('should defer queue counting when foreground id is not yet available', inject(function(jobs, channelEvents, $q, $rootScope) {
		var id = $q.defer();

		jobs.newForegroundWithId(id.promise);

		expect(jobs.getBackgroundRunningCount()).toBe(0);

		channelEvents.$broadcast("queue_ids", {
			running: [10, 11]
		});

		// should be still 0 as the service can't know what is the foreground id yet
		expect(jobs.getBackgroundRunningCount()).toBe(0);

		id.resolve(11);
		$rootScope.$apply(); // propagate promise resolutions

		// should be 1, because the only background id is the 10 as the 11 is in the foreground
		expect(jobs.getBackgroundRunningCount()).toBe(1);
	}));

});
