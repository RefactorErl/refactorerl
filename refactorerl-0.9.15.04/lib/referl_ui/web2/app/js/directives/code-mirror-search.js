'use strict';

angular.module('referl.directives').directive('reCodeMirrorRememberSearch', function($q, $http) {
	return {

		require: ['reCodeMirror', 'reCodeMirrorRememberSearch'],

		controller: function($attrs) {
			this.term = "";
            this.saveTerm = function(query) {
                if(this.term != query) {
                    this.term = query;
                    this.storeSearchTerm(query);
                }
            };

            var that = this;

            this.updateSearchTerm = function() {
                $http.get("api/getDataFromSession", {params: {key: $attrs.reCodeMirrorRememberSearch}})
	                .success(function(response) {
                        if(response.data == null) {
                            that.term = "";
                        } else {
		                    that.term = response.data;
                        }
	                });
            };
            this.updateSearchTerm();

            this.storeSearchTerm = function(term) {
                $http.post("api/storeDataInSession", {key: $attrs.reCodeMirrorRememberSearch, data: term});
            };

            var queryDialog =
                'Search: <input type="text" style="width: 10em" onfocus="var val = this.value; this.value = \'\'; this.value = val;" value="[value]"/> <span style="color: #888">(Find next: Enter,  Find previous: Shift-Ctrl-G, Use /re/ syntax for regexp search)</span>';
            this.doSearch = function(cm, rev) {
                var state = getSearchState(cm);
                if (state.query) return findNext(cm, rev);
                dialog(cm, queryDialog, that.term, "Search for:", function(query) {
                    cm.operation(function() {
                        if (!query || state.query) return;
                        that.saveTerm(query);
                        state.query = parseQuery(query);
                        cm.removeOverlay(state.overlay);
                        state.overlay = searchOverlay(state.query);
                        cm.addOverlay(state.overlay);
                        state.posFrom = state.posTo = cm.getCursor();
                        findNext(cm, rev);
                    });
                });
            }

            this.clearSearch = function(cm) {cm.operation(function() {
                var state = getSearchState(cm);
                if (!state.query) return;
                state.query = null;
                cm.removeOverlay(state.overlay);
            });}

            function findNext(cm, rev) {cm.operation(function() {
                var state = getSearchState(cm);
                var cursor = getSearchCursor(cm, state.query, rev ? state.posFrom : state.posTo);
                if (!cursor.find(rev)) {
                    cursor = getSearchCursor(cm, state.query, rev ? CodeMirror.Pos(cm.lastLine()) : CodeMirror.Pos(cm.firstLine(), 0));
                    if (!cursor.find(rev)) return;
                }
                cm.setSelection(cursor.from(), cursor.to());
                state.posFrom = cursor.from(); state.posTo = cursor.to();
            });}

            function searchOverlay(query) {
                if (typeof query == "string") return {token: function(stream) {
                    if (stream.match(query)) return "searching";
                    stream.next();
                    stream.skipTo(query.charAt(0)) || stream.skipToEnd();
                }};
                return {token: function(stream) {
                    if (stream.match(query)) return "searching";
                    while (!stream.eol()) {
                        stream.next();
                        if (stream.match(query, false)) break;
                    }
                }};
            }

            function getSearchState(cm) {
                return cm.state.search || (cm.state.search = new SearchState());
            }

            function getSearchCursor(cm, query, pos) {
                // Heuristic: if the query string is all lowercase, do a case insensitive search.
                return cm.getSearchCursor(query, pos, typeof query == "string" && query == query.toLowerCase());
            }

            function SearchState() {
                this.posFrom = this.posTo = this.query = null;
                this.overlay = null;
            }

            function dialog(cm, text, defValue, shortText, f) {
                if (cm.openDialog) cm.openDialog(text.split("[value]").join(defValue), f);
                else f(prompt(shortText, defValue));
            }

            function parseQuery(query) {
                var isRE = query.match(/^\/(.*)\/([a-z]*)$/);
                return isRE ? new RegExp(isRE[1], isRE[2].indexOf("i") == -1 ? "" : "i") : query;
            }
		},

		link: function(scope, elem, attrs, ctrls) {
            var reCodeMirror = ctrls[0];
            var ctrl = ctrls[1];

            reCodeMirror.on("ready", function(cm) {
                cm.addKeyMap(
                    {
                      "Ctrl-F": function(cm) {
                        ctrl.clearSearch(cm); ctrl.doSearch(cm);
                      }
                    }, false);
			});
		}

	};
});
