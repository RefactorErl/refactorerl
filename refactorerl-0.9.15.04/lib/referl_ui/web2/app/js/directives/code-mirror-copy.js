'use strict';

angular.module('referl.directives').directive('reCodeMirrorCopy', function($parse) {

	var supported = swfobject.getFlashPlayerVersion().major;

	$.contextMenu.types.reCodeMirrorCopy = function(item, opt, root) {
		var $this = angular.element(this);
		if (!supported) return $this.hide();

		$('<span>' + item.name + '</span>').appendTo(this);

		if (item.disabled) return;

		var clip = new ZeroClipboard($this, {
			moviePath: "lib/zeroclipboard/ZeroClipboard.swf",
			hoverClass: "hover"
		});
		clip.setHandCursor(false); // does not seem to work always, so better disable it

		var destroy = function() {
			try {
				clip.unglue($this);
			} catch(e) {}
			try {
				clip.resetBridge();
			} catch(e) {}
			clip.off('complete', on.complete);
			clip.off('dataRequested', on.complete);
		};

		var on = {
			complete: function() {
				$this.trigger("mouseup");
				destroy();
			},
			dataRequested: function() {
				clip.setText(item.getText());
			}
		};

		clip.on('complete', on.complete);
		clip.on('dataRequested', on.dataRequested);

		root.$trigger.on("contextmenu:hidden", destroy);
	};

	return {

		require: ['reContextMenu', 'reCodeMirror'],

		link: function(scope, elem, attrs, ctrls) {
			var reContextMenu = ctrls[0];
			var reCodeMirror  = ctrls[1];

			reCodeMirror.on("ready", function(cm) {
				reContextMenu.pushTransformer(function(items) {
					// means automatically add an item
					if (attrs.reCodeMirrorCopy) {
						items.push({name: "Copy", type: "copy"});
					}
					angular.forEach(items, function(item) {
						if (item.type == "copy") {
							item.type = "reCodeMirrorCopy";
							if (!cm.getDoc().somethingSelected()) {
								item.disabled = true;
							}
							item.getText = function() {
								return cm.getDoc().getSelection();
							};
						}
					});
				});
			});
		}

	};
});
