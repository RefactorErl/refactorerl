'use strict';

angular.module('referl.directives').directive('reResizable', function($parse) {

	var stored = {};

	return {

		restrict: 'A',

		link: function(scope, elem, attrs) {
			var options = $parse(attrs.reResizable)(scope) || {};
			var offset;
			var setValue = function(value) {
				if (options.relative) {
					value = value / elem.offsetParent().width() * 100 + "%";
				}
				elem.width(value);
				stored[options.persist] = value;
			};
			var onMouseDown = function(e) {
				offset = e.clientX - elem.width();
				$(document).on(documentHandlers);
				return false;
			};
			var onMouseMove = function(e) {
				var value = offset + e.clientX;
				elem.width(value);
			};
			var onMouseUp = function() {
				$(document).off(documentHandlers);
				setValue(elem.width());
			};
			var onDoubleClick = function() {
				var scrollBarWidth = 17;
				var inner2 = elem.find(".inner2");
				var diff = elem.width() - inner2.innerWidth();
				elem.width("80%"); // that is the maximum acceptable width
				inner2.css({position: "absolute", display: "block", width: "auto", left: "0", right: "auto", top: "0"});
				var width = inner2.innerWidth();
				inner2.css({position: "", display: "", width: "", left: "", right: "", top: ""});
				setValue(width + diff + scrollBarWidth + 1);
			};
			var documentHandlers = {
				mousemove: onMouseMove,
				mouseup:   onMouseUp,
			};
			$("<div/>")
				.addClass("re-resizer")
				.prependTo(elem)
				.on({
					mousedown: onMouseDown,
					dblclick:  onDoubleClick
				});
			if (options.persist && stored[options.persist]) {
				elem.width(stored[options.persist]);
			}
		}

	};
});
