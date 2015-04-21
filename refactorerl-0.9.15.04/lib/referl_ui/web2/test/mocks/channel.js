angular.module('referl.mocks.channel', []);

angular.module('referl.mocks.channel').factory("channelEvents", function($rootScope) {
	return $rootScope.$new(true);
});

angular.module('referl.mocks.channel').factory("channel", function(channelEvents, $rootScope) {
	var events = channelEvents, channel;
	return channel = {
		on: function(event, handler) {
			if (angular.isString(event)) {
				return events.$on(event, function() {
					var args = arguments;
					$rootScope.$apply(function() {
						handler.apply(channel, args);
					});
				});
			} else {
				var deregisters = [];
				angular.forEach(event, function(handler, event) {
					deregisters.push(channel.on(event, handler));
				});
				return function() {
					angular.forEach(deregisters, function(deregister) {
						deregister();
					});
				};
			}
		},
		send: jasmine.createSpy("channel.send")
	};
});
