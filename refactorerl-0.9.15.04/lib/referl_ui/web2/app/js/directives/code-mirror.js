'use strict';

angular.module('referl.directives').directive('reCodeMirror', function($http, $window) {
	return {

		require: ['reCodeMirror', 'ngModel'],

		controller: function() {
			var cm = null;

			var syncs = {

				setOptions: function(options) {
					angular.forEach(options, function(value, name) {
						cm.setOption(name, value);
					});
				},

				on: function(event, handler) {
					if (event == "ready") {
						handler(cm);
					} else {
						cm.on(event, handler);
					}
				}

			};

			var buffer = [];

			var that = this;
			angular.forEach(syncs, function(method, name) {
				that[name] = function() {
					buffer.push({method: method, args: arguments});
				};
			});

			that.init = function(_cm_) {
				cm = _cm_;
				angular.extend(that, syncs);
				angular.forEach(buffer, function(command) {
					command.method.apply(that, command.args);
				});
			};
		},

		link: function(scope, elem, attrs, ctrls) {
			var ctrl = ctrls[0];
			var ngModel = ctrls[1];

			var options = scope.$eval(attrs.reCodeMirror) || {};
			var cm = CodeMirror.fromTextArea(elem[0], options);

			cm.on("change", function() {
				var newValue = cm.getValue();
				var oldValue = ngModel.$viewValue;
				if (typeof oldValue === "string") {
					oldValue = oldValue.replace(/\r/g, "");
				}
				if (newValue !== oldValue) {
					ngModel.$setViewValue(newValue);
					scope.$$phase || scope.$apply();
				}
			});

			ngModel.$formatters.push(function(value) {
				if (angular.isUndefined(value) || value === null) {
					return "";
				} else {
					return value;
				}
			});

			ngModel.$render = function() {
				cm.setValue(ngModel.$viewValue);
			};

			ctrl.init(cm);

			// watch visibility and refresh if changed to visible
			var lastVisible = undefined;
			var wrapper = $(cm.getWrapperElement());
			var intervalId = $window.setInterval(function() {
				var currentVisible = wrapper.is(":visible");
				if (currentVisible !== lastVisible) {
					lastVisible = currentVisible;
					if (currentVisible) {
						cm.refresh();
					}
				}
			}, 100);

			scope.$on("$destroy", function() {
				$window.clearInterval(intervalId);
			});
		}

	};
});
