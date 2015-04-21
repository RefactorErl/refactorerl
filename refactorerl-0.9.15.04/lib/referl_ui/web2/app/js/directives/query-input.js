'use strict';

angular.module('referl.directives').directive('reQueryInput', function(autoComplete, $http, $templateCache, $compile, $controller, $document, $parse) {

	var autoCompleteQuery = autoComplete.queryInputQuery(0);

	var dispatchMouseEvent = function(name, target) {
		var event = $document[0].createEvent("MouseEvents");
		event.initMouseEvent(name, true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		$(target)[0].dispatchEvent(event);
	};

	var toNextSkeletonArgument = function(editor, startPos) {
		var doc = editor.getDoc();
		var content = editor.getValue();
		if (angular.isUndefined(startPos)) {
			startPos = doc.indexFromPos(editor.getCursor());
		}
		var placeholderPos = content.indexOf("`_`", startPos);
		if (placeholderPos != -1) {
			doc.setSelection(
				doc.posFromIndex(placeholderPos + 1),
				doc.posFromIndex(placeholderPos + 2)
			);
			return true;
		}
		return false;
	};

	var getHints = function(editor, showHints, options) {
		var cursor = editor.getCursor();
		var from   = cursor;
		var to     = cursor;
		var term   = editor.getDoc().getRange(CodeMirror.Pos(0, 0), cursor);

		var scope = options.autoCompletionScope;

		var aligned = false;
		var alignToItsContainer = function(child) {
			if (aligned) return;
			var hintsNode = $(child).closest(".CodeMirror-hints");
			options.autoCompletionDiv.css({
				left:   hintsNode.css("left"),
				top:    hintsNode.css("top"),
				//zIndex: hintsNode.css("zIndex")
				zIndex: 1051
			});
			hintsNode.css({visibility: "hidden"});

			options.autoCompletionDiv
				.off("mousedown")
				.on("mousedown", function() {
					dispatchMouseEvent("mousedown", hintsNode);
				});

			hintsNode.after(options.autoCompletionDiv);
			scope.$broadcast("open");

			var parentNode = hintsNode[0].parentNode;
			hintsNode.wrap("<div/>");
			var wrapperNode = hintsNode[0].parentNode;
			wrapperNode.removeChild = function() {
				scope.$broadcast("close");
				scope.$$phase || scope.$apply();
				parentNode.removeChild(wrapperNode);
			};
			aligned = true;
		};

		autoCompleteQuery({
			term: term,
			callback: function(data) {
				var runnable = _.any(data.results, function(res){return res.text === ""});
				var results = _.filter(data.results, function(res){return res.text});
				if (!results.length) return;

				var prefixes = _.uniq(_.pluck(results, "prefix"));
				if (prefixes.length == 1) {
					var prefix = prefixes[0];
					if (prefix && term.substring(term.length - prefix.length === prefix)) {
						angular.forEach(results, function(result) {
							result.id = result.prefix + result.id;
							result.prefix = "";
						});
						var doc = editor.getDoc();
						var toIndex = doc.indexFromPos(to);
						from = doc.posFromIndex(toIndex - prefix.length);
					}
				}

				var hints = [];
				angular.forEach(results, function(item) {
					hints.push({
						displayText: item.text,
						text: item.id,
						details: item.details,
						render: function(elt, data, completion) {
							completion.$codeMirrorHintNode = elt;
							$(elt).closest(".CodeMirror-hints").addClass("for-auto-completion");
						},
						hint: function(editor, data, completion) {
							editor.replaceRange(completion.text, data.from, data.to);
							CodeMirror.signal(data, "inserted");
							toNextSkeletonArgument(editor, data.from);
						}
					});
				});

				var hintsData = {
					list: hints,
					from: from,
					to:   to,
					runnable: runnable
				};

				scope.hints = hints;

				CodeMirror.on(hintsData, "select", function(item, node) {
					alignToItsContainer(node);
					scope.$broadcast("select", item);
					scope.$$phase || scope.$apply();
				});

				scope.select = function(hint) {
					dispatchMouseEvent("click", hint.$codeMirrorHintNode);
				};

				scope.insert = function(hint) {
					dispatchMouseEvent("dblclick", hint.$codeMirrorHintNode);
				};

				showHints(hintsData);
			}
		});
	};

	return {

		require: 'reCodeMirror',

		link: function(scope, elem, attrs, reCodeMirror) {
			var autoCompletionScope = scope.$new();
			var autoCompletionDiv = $("<div/>").css({position: "absolute"});
			var autoCompletionHtml = $http.get("partials/auto-completion.html", {cache: $templateCache}).success(function(response) {
				var contents = autoCompletionDiv.html(response).contents();
				$compile(contents)(autoCompletionScope);
				$controller("AutoCompletionCtrl", {
					$scope: autoCompletionScope
				});
			});

			var lastSeq = 0, lastValue;
			var forcedAutoComplete = function(editor) {
				lastValue = editor.getValue();
				var currentSeq = ++lastSeq;
				angular.element(".CodeMirror-hints").hide();

				var getHintsLocal = function(editor, callback, options) {
					autoCompletionScope.selectedChanged = false;
					var myCallback = function(data) {
						if (currentSeq == lastSeq) {
							autoCompletionScope.acData = data;
							callback(data);
							CodeMirror.on(data, "inserted", function() {
								// so we won't do autocomplete immediately after insert
								lastValue = editor.getValue();
							});
						}
					};
					getHints(editor, myCallback, options);
				};

				CodeMirror.showHint(editor, getHintsLocal, {
					async: true,
					autoCompletionScope: autoCompletionScope,
					autoCompletionDiv: autoCompletionDiv,
					completeSingle: false,
					alignWithWord: true,
					// Always close immediately after change. We will reopen with a
					// new call to doAutoComplete if needed
					closeCharacters: {
						test: function() {
							return true;
						}
					},
					extraKeys: {
						Home : function() {
							return CodeMirror.Pass;
						},
						End: function() {
							return CodeMirror.Pass;
						},
						Up: function(cm, handle) {
							handle.moveFocus(-1);
							autoCompletionScope.selectedChanged = true;
						},
						Down: function(cm, handle) {
							handle.moveFocus(1);
							autoCompletionScope.selectedChanged = true;
						},
						Enter: function(cm, handle) {
							//newer release of CodeMirror exposes completion data as handle.data
							if (autoCompletionScope.acData.runnable && !autoCompletionScope.selectedChanged) {
								if (attrs.reOnEnter) {
									handle.close();
									scope.$eval(attrs.reOnEnter);
									scope.$$phase || scope.$apply();
									return;
								}
							}
							handle.pick();
						}
					}
				});
			};

			var doAutoComplete = function(editor) {
				if (!editor.hasFocus()) return;
				if (!editor.getValue()) return;
				if (editor.getValue() === lastValue) return;
				forcedAutoComplete(editor);
			};

			reCodeMirror.setOptions({
				lineNumbers: false,
				mode: "text/plain",
				autoCloseBrackets: true,
				viewportMargin: Infinity,
				lineWrapping: true,
				styleSelectedText: true,
				extraKeys: {
					Enter: function(cm) {
						if (attrs.reOnEnter) {
							scope.$eval(attrs.reOnEnter);
							scope.$$phase || scope.$apply();
						}
					},
					Tab: function(editor) {
						if (toNextSkeletonArgument(editor)) {
							// jumped to the next skeleton argument
						} else {
							scope.$eval(attrs.reOnTab);
							scope.$$phase || scope.$apply();
						}
					},
					"Ctrl-Enter": "newlineAndIndent",
					"Ctrl-Space": forcedAutoComplete
				}
			});
			reCodeMirror.on("cursorActivity", doAutoComplete);

			scope.$watch(attrs.reReadOnly, function(value) {
				reCodeMirror.setOptions({readOnly: value});
			});

			var blurTimeout;
			reCodeMirror.on("blur", function(editor) {
				// show-hint can make fake blurs
				if (blurTimeout) {
					clearTimeout(blurTimeout);
					blurTimeout = false;
				}
				blurTimeout = setTimeout(function() {
					if (!editor.hasFocus()) {
						editor.setSelection({line: 0, ch: 0}, {line: 0, ch: 0});
					}
				}, 100);
			});
		}

	};
});
