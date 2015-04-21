'use strict';

angular.module('referl.services').factory('channel', function($rootScope, $q, getSessionId, $window) {
	var WebSocket = $window.WebSocket || $window.MozWebSocket;
	if (!WebSocket) alert("No WebSocket support, sorry, it's required.");

	var events = $rootScope.$new(true);
	var socket;
	var buffer = [];

	var channel = {

		/** Registers an event handler. */
		on: function(event, handler) {
			if (angular.isString(event)) {
				return events.$on(event, handler);
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

		/** Sends an [event, parameter] message. */
		send: function(event, parameter) {
			buffer.push(angular.toJson([event, parameter]));
		}

	};

	channel.on("ready", function() {
		buffer.push = function(message) {
			socket.send(message);
		};
		angular.forEach(buffer, buffer.push);
		buffer.length = 0;
	});

	var connect = function() {
		getSessionId().then(function(sessionId) {
			// localhost can be much slower than 127.0.0.1. See:
			// http://code.google.com/p/chromium/issues/detail?id=175237
			var host = location.host.replace("localhost", "127.0.0.1");
			socket = new WebSocket("ws://" + host + "/api/ws");
			socket.onopen = function() {
				socket.send(angular.toJson(["hello", {sessionId: sessionId}]));
			};
			socket.onmessage = function(message) {
				if (message.data) {
					var broadcast = function() {
						var parsed = angular.fromJson(message.data);
						events.$broadcast(parsed[0], parsed[1]);
						$rootScope.$broadcast("channel:" + parsed[0], parsed[1]);
					};
					$rootScope.$$phase ? broadcast() : $rootScope.$apply(broadcast);
				}
			};
			socket.onclose = function() {
				buffer = [];
				socket = null;
				events.$broadcast("close");
				connect();
			};
		});
	};

	connect();
	return channel;
});
