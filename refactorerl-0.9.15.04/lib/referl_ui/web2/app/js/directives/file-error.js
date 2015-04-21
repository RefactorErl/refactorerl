'use strict';

angular.module('referl.directives').directive('reFileError', function() {
	return {

		restrict: 'A',
		require: 'reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			var context = 5;

			var error = scope.$eval(attrs.reFileError);

			// Skip empty lines
			var findStart = function(doc, index) {
				var pos = doc.posFromIndex(index);
				while (doc.posFromIndex(++index).line === pos.line + 1) {
					pos = doc.posFromIndex(index);
				}
				return pos;
			};

			reCodeMirror.on("change", function(cm) {
				var value = cm.getValue();
				if (!value) return;

				var doc  = cm.getDoc();
				var from = findStart(doc, error.start);
				var to   = doc.posFromIndex(error.start + error.length - 1);

				var startLine = from.line - context;
				var endLine   = to.line + context - 1;

				var startY = cm.charCoords({line: startLine}, "local").top + 4;
				var endY   = cm.charCoords({line: endLine  }, "local").bottom + 4;

				cm.setSize(null, endY - startY);
				cm.scrollTo(0, startY);

				doc.markText(from, to, { className: "highlight" });
			});
		}

	};
});
