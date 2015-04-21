'use strict';

describe('service auth', function() {

	beforeEach(module('referl.services'));
	beforeEach(module('referl.mocks.misc'));

	var USER = {name: "testUser"};

	afterEach(inject(function($httpBackend) {
		$httpBackend.verifyNoOutstandingExpectation();
		$httpBackend.verifyNoOutstandingRequest();
	}));

	it('should return no user', inject(function($injector, $httpBackend, $rootScope, miscMocks) {
		$httpBackend.expectGET('api/getUser').respond(false);
		var auth = $injector.get("auth");

		var userSpy = miscMocks.spyPromise(auth.getUserPromise(), "userSpy");

		expect(auth.getUser()).toBe(false);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy.onResolve).not.toHaveBeenCalled();

		$httpBackend.flush();

		expect(auth.getUser()).toBe(false);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy.onResolve).toHaveBeenCalledWith(false);
	}));

	it('should return logged in user', inject(function($injector, $httpBackend, $rootScope, miscMocks) {
		$httpBackend.expectGET('api/getUser').respond(USER);
		var auth = $injector.get("auth");

		var userSpy = miscMocks.spyPromise(auth.getUserPromise(), "userSpy");

		expect(auth.getUser()).toBe(false);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy.onResolve).not.toHaveBeenCalled();

		$httpBackend.flush();

		expect(auth.getUser()).toEqual(USER);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy.onResolve).toHaveBeenCalledWith(USER);
	}));

	it('should login then logout the user', inject(function($injector, $httpBackend, $rootScope, miscMocks) {
		$httpBackend.expectGET('api/getUser').respond(false);
		var auth = $injector.get("auth");
		$httpBackend.flush();

		$httpBackend.expectGET(/^api\/login\?.+$/).respond(USER);
		auth.login(USER.name, "THEPASS");
		$httpBackend.flush();

		var userSpy1 = miscMocks.spyPromise(auth.getUserPromise(), "userSpy1");

		expect(auth.getUser()).toBe(USER);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy1.onResolve).toHaveBeenCalledWith(USER);

		$httpBackend.expectGET('api/logout').respond(false);
		auth.logout();
		$httpBackend.flush();

		var userSpy2 = miscMocks.spyPromise(auth.getUserPromise(), "userSpy2");

		expect(auth.getUser()).toBe(false);
		$rootScope.$apply(); // propagate promise resolutions
		expect(userSpy2.onResolve).toHaveBeenCalledWith(false);
	}));

	it('should call navigateToBeforeLogin after login if needed', inject(function($injector, $httpBackend, $rootScope, miscMocks) {
		$httpBackend.expectGET('api/getUser').respond(false);
		var auth = $injector.get("auth");
		$httpBackend.flush();

		spyOn(auth, "navigateToBeforeLogin").andCallThrough();

		$httpBackend.expectGET(/^api\/login\?.+$/).respond(USER);
		auth.login(USER.name, "THEPASS");
		$httpBackend.flush();

		expect(auth.navigateToBeforeLogin).not.toHaveBeenCalled();
		auth.navigateToBeforeLogin.reset();

		$httpBackend.expectGET(/^api\/login\?.+$/).respond(USER);
		auth.login(USER.name, "THEPASS", true);
		$httpBackend.flush();

		expect(auth.navigateToBeforeLogin).toHaveBeenCalled();
		auth.navigateToBeforeLogin.reset();

		$httpBackend.expectGET(/^api\/login\?.+$/).respond(false);
		auth.login(USER.name, "THEPASS", true);
		$httpBackend.flush();

		expect(auth.navigateToBeforeLogin).not.toHaveBeenCalled();
	}));

	describe('newPromise', function() {

		beforeEach(inject(function($location) {
			spyOn($location, 'replace').andCallThrough();
			spyOn($location, 'path').andCallThrough();
		}));

		it('should return a rejecting promise for no logged user', inject(function($injector, $httpBackend, $rootScope, $location, miscMocks) {
			$httpBackend.expectGET('api/getUser').respond(false);
			var auth = $injector.get("auth");

			$httpBackend.flush();

			$location.path("/test");
			var spy = miscMocks.spyPromise(auth.newPromise());
			$rootScope.$apply(); // propagate promise resolutions
			expect(spy.onReject).toHaveBeenCalled();
			expect($location.replace).toHaveBeenCalled();
			expect($location.path).toHaveBeenCalledWith("/login");

			$location.replace.reset();
			auth.navigateToBeforeLogin();
			expect($location.replace).toHaveBeenCalled();
			expect($location.path).toHaveBeenCalledWith("/test");
		}));

		it('should return a resolving promise for logged user', inject(function($injector, $httpBackend, $rootScope, $location, miscMocks) {
			$httpBackend.expectGET('api/getUser').respond(USER);
			var auth = $injector.get("auth");

			$httpBackend.flush();

			var spy = miscMocks.spyPromise(auth.newPromise());
			expect($location.path).not.toHaveBeenCalledWith("/login");
			$location.path.reset();

			$rootScope.$apply(); // propagate promise resolutions
			expect(spy.onResolve).toHaveBeenCalled();
			expect($location.path).not.toHaveBeenCalled();
		}));

	});

});
