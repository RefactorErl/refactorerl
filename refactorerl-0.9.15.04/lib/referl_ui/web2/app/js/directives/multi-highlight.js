'use strict';

angular.module('referl.directives').directive('reMultiHighlight', function() {
	return {

		restrict: 'A',
		require: 'reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			reCodeMirror.on("ready", function(cm) {

				var doc  = cm.getDoc();
				var marks = [];

				var highlightAll = function(highlights) {
					angular.forEach(marks, function(mark) {
						mark.clear();
					});
					marks = [];
					if (!highlights) return;
					angular.forEach(highlights, highlight);
				};

				var highlight = function(range) {
					if (!range) return;
					if (!cm.getValue()) return;

					var from = range[0];
					var to   = range[1];

					if (typeof from != "object") {
						from = doc.posFromIndex(range[0]);
					}
					if (typeof to != "object") {
						to = doc.posFromIndex(range[1]);
					}

					marks.push(doc.markText(from, to, { className: "multi-highlight" }));
				};

				scope.$watch(attrs.reMultiHighlight, function(highlights) {
					highlightAll(highlights);
				});

				reCodeMirror.on("change", function(cm) {
					highlightAll(scope.$eval(attrs.reMultiHighlight));
				});

			});
		}

	};
});
