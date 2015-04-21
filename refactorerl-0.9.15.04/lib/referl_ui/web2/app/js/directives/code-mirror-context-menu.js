'use strict';

/**
 * You can not use re-context-menu directive directly with re-code-mirror, because CodeMirror
 * will replace the existing textarea with a new DOM elements and the handlers should be
 * attached to that element. You have to add this as well directive to solve that issue.
 * Additionally it will define a "pos" local for the "re-context-menu" directive which has
 * the {line, ch} position of the clicked character.
 */
angular.module('referl.directives').directive('reCodeMirrorContextMenu', function($q, $parse) {
	return {

		require: ['reCodeMirror', 'reContextMenu'],

		controller: function($attrs) {
			$attrs.reContextMenuManual = true;
		},

		link: function(scope, elem, attrs, ctrls) {
            var reCodeMirror = ctrls[0];
			var reContextMenu = ctrls[1];

			reCodeMirror.on("ready", function(cm) {
				var wrapper = angular.element(cm.getWrapperElement());
				reContextMenu.create(scope, wrapper, attrs, function(locals) {
					try {
						locals.pos = cm.coordsChar({left: locals.x, top: locals.y});
						locals.index = cm.getDoc().indexFromPos(locals.pos);
                        locals.selection = {
                            cursorIndex: locals.index,
                            somethingSelected: cm.getDoc().somethingSelected(),
                            fromIndex: cm.getDoc().indexFromPos(cm.getDoc().sel.from),
                            toIndex: cm.getDoc().indexFromPos(cm.getDoc().sel.to),
                            from: {line: cm.getDoc().sel.from.line+1, ch: cm.getDoc().sel.from.ch+1},
                            to: {line: cm.getDoc().sel.to.line+1, ch: cm.getDoc().sel.to.ch+1},
                            text : cm.getDoc().getSelection()
                        };
					} catch (e) {}
				});
			});
		}

	};
});
