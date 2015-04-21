'use strict';

angular.module('referl.directives').directive('reCodeMirrorSelectAll', function($parse) {

	return {

		require: ['reContextMenu', 'reCodeMirror'],

		link: function(scope, elem, attrs, ctrls) {
			var reContextMenu = ctrls[0];
			var reCodeMirror  = ctrls[1];

			reCodeMirror.on("ready", function(cm) {
				reContextMenu.pushTransformer(function(items) {
					// means automatically add an item
					if (attrs.reCodeMirrorSelectAll) {
						items.push({name: "Select All", type: "selectAll"});
					}
					angular.forEach(items, function(item) {
						if (item.type == "selectAll") {
							delete item.type; // to make it the default text item
							item.callback = function() {
								var doc = cm.getDoc();
								doc.setSelection(
									{line: 0, ch: 0},
									{line: doc.lastLine() + 1, ch: 0}
								);
							};
						}
					});
				});
			});
		}

	};
});
