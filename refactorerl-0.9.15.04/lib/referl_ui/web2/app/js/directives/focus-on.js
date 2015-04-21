'use strict';

angular.module('referl.directives').directive('reFocusOn', function() {
	return {

		restrict: 'A',
		require: '?reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			var cm;
			if (reCodeMirror) {
				reCodeMirror.on("ready", function(_cm_) {
					cm = _cm_;
				});
			}
			scope.$on(attrs.reFocusOn, function() {
				if (cm) {
					cm.focus();
					var doc = cm.getDoc();
					doc.setSelection(
						doc.posFromIndex(0),
						doc.posFromIndex(cm.getValue().length)
					);
				} else {
					elem.focus();
				}
			});
		}

	};
});
