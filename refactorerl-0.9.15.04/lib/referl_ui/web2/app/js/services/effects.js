'use strict';

/**
 * Manipulates DOM element using jQuery. Therefore it should be used only in directives.
 */
angular.module('referl.services').factory('effects', function() {
	// based on easing equation from Robert Penner (http://www.robertpenner.com/easing)
	var elastic = function(p) {
		return p === 0 || p === 1 ? p :
			-Math.pow( 2, 8 * (p - 1) ) * Math.sin( ( (p - 1) * 80 - 7.5 ) * Math.PI / 15 );
	};

	$.easing["reEaseOutElastic"] = function(p) {
		return 1 - elastic(1 - p);
	};

	var effects = {
		bounce: function(elem, params) {
			params = params || {};
			var offset = params.offset || 15;
			var step0 = {top: 0, position: "relative"};
			var step1 = {top: (-1*offset) + "px" };
			var step2 = {top: 0 };
			elem
				.clearQueue()
				.css(step0)
				.animate(step1, {duration: 100})
				.animate(step2, {
					duration: 150,
					specialEasing: {
						top: "reEaseOutElastic"
					}
				});
		}
	};

	return {
		apply: function(name, elem, params) {
			var effect = effects[name];
			effect(elem, params);
		}
	};
});
