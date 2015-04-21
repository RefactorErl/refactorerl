angular.module('referl.mocks.misc', []);

angular.module('referl.mocks.misc').factory('miscMocks', function() {
	return {
		spyPromise: function(promise, name) {
			var spy = jasmine.createSpyObj(name || 'promiseSpy', ['onResolve', 'onReject']);
			promise.then(spy.onResolve, spy.onReject);
			return spy;
		}
	};
});
