'use strict';

angular.module('referl.services').factory('suspendable', function() {

	return function(func) {
		var buffer = [];
		var wrapped = function() {
			buffer.push(arguments);
		};
		wrapped.suspend = function() {
			delete buffer.push;
			return wrapped;
		};
		wrapped.allow = function() {
			buffer.push = function(args) {
				func.apply(wrapped, args);
			};
			angular.forEach(buffer, buffer.push);
			buffer.length = [];
			return wrapped;
		};
		return wrapped.allow();
	};

});
