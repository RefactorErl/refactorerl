'use strict';

describe('TopCtrl', function() {

	beforeEach(module('referl'));

	var bounceSpy;
	var scope;
	var backgroundRunningCount;
	var backgroundUnseenCount;

	beforeEach(module('referl', function($provide) {
		backgroundRunningCount = 0;
		backgroundUnseenCount = 0;
		$provide.value('jobs', {
			getBackgroundRunningCount: function() {
				return backgroundRunningCount;
			},
			getBackgroundUnseenCount: function() {
				return backgroundUnseenCount;
			}
		});
	}));

	beforeEach(inject(function($rootScope) {
		bounceSpy = jasmine.createSpy('bounce');
		scope = $rootScope.$new();
		scope.$on("bounce", bounceSpy);
	}));

	it('should not bounce initially #1', inject(function($rootScope, $controller) {
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		expect(bounceSpy).not.toHaveBeenCalled();
	}));

	it('should not bounce initially #2', inject(function($rootScope, $controller) {
		backgroundUnseenCount = 1;
		backgroundRunningCount = 1;
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		expect(bounceSpy).not.toHaveBeenCalled();
	}));

	it('should bounce initially', inject(function($rootScope, $controller) {
		backgroundUnseenCount = 1;
		backgroundRunningCount = 0;
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		expect(bounceSpy).toHaveBeenCalled();
	}));

	it('should not bounce on first background job', inject(function($rootScope, $controller) {
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		backgroundUnseenCount = 1;
		backgroundRunningCount = 1;
		scope.$digest();
		expect(bounceSpy).not.toHaveBeenCalled();
	}));

	it('should not bounce on new background job', inject(function($rootScope, $controller) {
		backgroundUnseenCount = 1;
		backgroundRunningCount = 1;
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		backgroundUnseenCount++;
		backgroundRunningCount++;
		scope.$digest();
		expect(bounceSpy).not.toHaveBeenCalled();
	}));

	it('should bounce on new finished, unseen job', inject(function($rootScope, $controller) {
		backgroundUnseenCount = 1;
		backgroundRunningCount = 1;
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		backgroundRunningCount--;
		scope.$digest();
		expect(bounceSpy).toHaveBeenCalled();
	}));

	it('should not bounce when job is removed', inject(function($rootScope, $controller) {
		backgroundUnseenCount = 1;
		backgroundRunningCount = 1;
		$controller("TopCtrl", {$scope: scope});
		scope.$digest();
		backgroundUnseenCount--;
		backgroundRunningCount--;
		scope.$digest();
		expect(bounceSpy).not.toHaveBeenCalled();
	}));

});
