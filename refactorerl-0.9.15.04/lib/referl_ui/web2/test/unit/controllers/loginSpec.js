'use strict';

describe('LoginCtrl', function() {

	beforeEach(module('referl'));

	it('should login', inject(function($rootScope, $controller) {
		var loginPromise = jasmine.createSpyObj('loginPromise', ['then']);
		var authMock = jasmine.createSpyObj('auth', ['login']);
		authMock.login.andReturn(loginPromise);

		var $scope = $rootScope.$new();
		var ctrl = $controller("LoginCtrl", {
			$scope: $scope,
			auth: authMock
		});

		$scope.credentials.username = "theUsername";
		$scope.credentials.password = "thePassword";

		expect(authMock.login).not.toHaveBeenCalled();
		$scope.login();
		expect(authMock.login).toHaveBeenCalledWith("theUsername", "thePassword", true);
	}));

});
