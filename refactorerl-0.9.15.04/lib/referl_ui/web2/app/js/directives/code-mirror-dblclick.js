'use strict';

/**
 * Requires CodeMirror version 3.14.0 or newer.
 */
angular.module('referl.directives').directive('reCodeMirrorDblclick', function($q, $parse) {
	return {

		require: 'reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			var handler = $parse(attrs.reCodeMirrorDblclick);

			var onDoubleClick = function(cm, event) {
				var locals = {
					x: event.clientX,
					y: event.clientY,
					$event: event
				};
				try {
					locals.pos = cm.coordsChar({left: locals.x, top: locals.y});
					locals.index = cm.getDoc().indexFromPos(locals.pos);
				} catch (e) {}
				handler(scope, locals);
			};

			var lastX, lastY, lastDownTime;
			reCodeMirror.on("mousedown", function(cm, event) {
				var now = +new Date();
				if (lastDownTime > now - 400) {
					if (lastX == event.clientX && lastY == event.clientY) {
						onDoubleClick(cm, event);
						lastDownTime = 0;
						return;
					}
				}
				lastDownTime = now;
				lastX = event.clientX;
				lastY = event.clientY;
			});

			// can be fired on double click when context menu is open
			reCodeMirror.on("dblclick", onDoubleClick);
		}

	};
});
