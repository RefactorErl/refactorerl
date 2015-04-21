'use strict';

/**
 * = About target element =
 * Please note that the context menu will be triggered only for the child
 * elements. The reason of this limitation is that the jQuery-contextMenu
 * requires a selector to set. To circumvent this you can use a wrapper
 * element or register the directive to the parent element and define
 * a "re-context-menu-selector" attribute refering to the children.
 *
 * = About callback =
 * Do not wrap callbacks in scope.$apply, it is already done.
 */
angular.module('referl.directives').directive('reContextMenu', function($q, $parse) {
	return {

		controller: function() {
			var transformers = [];
			this.pushTransformer = function(transformer) {
				transformers.push(transformer);
			};
			this.create = function(scope, elem, attrs, localsEnhancer) {
				var getter = $parse(attrs.reContextMenu);
				elem.on("mouseup", "*", function(e) {
					if (e.which == 3) {
						var $this = angular.element(this);
						scope.$apply(function() {
							e.stopPropagation();
							$this.contextMenu(); // won't show yet, but will close the open one if there is

							var dataProvider = $this
								.closest("[data-re-has-context-menu-data]")
								.data("$reContextMenuDataProvider");
							var locals = {
								x: e.pageX,
								y: e.pageY,
								data: dataProvider && dataProvider()
							};
							if (localsEnhancer) {
								localsEnhancer(locals);
							}
							var items = getter(scope, locals);
							$q.when(items).then(function(items) {
								if (items && items.length) {
									angular.forEach(transformers, function(tansformer) {
										var result = tansformer(items);
										if (result) items = result;
									});

									var wrapCallbacks = function(items) {
										angular.forEach(items, function(item) {
											var cb = item.callback;
											if (cb) {
												item.callback = function() {
													scope.$apply(cb);
												};
											}
											if(item.items) {
												wrapCallbacks(item.items);
											}
										});
									};
									wrapCallbacks(items);

									$this.trigger($.Event("contextmenu", {
										pageX: e.pageX,
										pageY: e.pageY,
										reContextMenuGenerated: {
											callback: angular.noop,
											items: items
										}
									}));
								}
							});
						});
					}
				});

				elem.contextMenu({
					trigger: 'none', // we have our custom trigger to evaluate the data
					selector: attrs.reContextMenuSelector || "*",
					className: attrs.reContextMenuClass,
					build: function($trigger, e) {
						return e.reContextMenuGenerated || false;
					}
				});
			};
		},

		link: function(scope, elem, attrs, ctrl) {
			if (!attrs.reContextMenuManual) {
				ctrl.create(scope, elem, attrs);
			}
		}

	};
});

angular.module('referl.directives').directive('reContextMenuData', function($parse) {
	return {

		link: function(scope, elem, attrs, ctrl) {
			var getter = $parse(attrs.reContextMenuData);
			elem.attr("data-re-has-context-menu-data", true);
			elem.data("$reContextMenuDataProvider", function() {
				return getter(scope);
			});
		}

	};
});
