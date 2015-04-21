'use strict';

angular.module('referl.directives').directive('reHighlight', function() {
	return {

		restrict: 'A',
		require: 'reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			var context = 3;
			reCodeMirror.on("ready", function(cm) {

				var lastMark = null;

				var highlight = function(range) {
					if (lastMark) {
						lastMark.clear();
						lastMark = null;
					}
					if (!range) return;
					if (!cm.getValue()) return;

					var doc  = cm.getDoc();

					var from = range[0];
					var to   = range[1];

					if (typeof from != "object") {
						from = doc.posFromIndex(range[0]);
					}
					if (typeof to != "object") {
						to = doc.posFromIndex(range[1]);
					}

					var startLine = from.line - context;

					var startY = cm.charCoords({line: startLine}, "local").top + 4;
					cm.scrollTo(0, startY);

					lastMark = doc.markText(from, to, { className: "highlight" });
				};

				scope.$watch(attrs.reHighlight, function(reHighlight) {
					var newFile =  scope.$eval(attrs.reNewFile);
					if(!newFile) {
						highlight(reHighlight);
					}	
				});

				reCodeMirror.on("change", function(cm) {
					highlight(scope.$eval(attrs.reHighlight));
				});

			});
		}

	};
});
