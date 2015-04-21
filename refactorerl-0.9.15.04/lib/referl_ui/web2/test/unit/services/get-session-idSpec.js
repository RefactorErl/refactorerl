'use strict';

describe('service getSessionId', function() {

	beforeEach(module('referl.services'));
	beforeEach(module('referl.mocks.misc'));

	var SESSION_ID = "theSessionId";

	it('should call the backend and propagate in a promise', inject(function($httpBackend, $rootScope, getSessionId, miscMocks) {
		$httpBackend.expectGET('api/session/id').respond(SESSION_ID);
		var spy = miscMocks.spyPromise(getSessionId());
		$rootScope.$apply(); // propagate promise resolutions
		expect(spy.onResolve).not.toHaveBeenCalled();

		$httpBackend.flush();

		$rootScope.$apply(); // propagate promise resolutions
		expect(spy.onResolve).toHaveBeenCalledWith(SESSION_ID);
	}));

});
