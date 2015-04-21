'use strict';

angular.module('referl.directives').directive('reFollowCursor', function($timeout, $document) {
    return {

        restrict: 'A',

        link: function(scope, elem, attrs) {
            scope.ReFollowing.window = elem;
            scope.timeout = null;

            $document.bind("mousemove", function(e) {
                /*var width = scope.ReFollowing.window.width() + 4; //+140 for buttons
                var height = scope.ReFollowing.window.height() + 4; //+4 for borders*/
                
                scope.ReFollowing.pos.top = e.pageY;
                scope.ReFollowing.pos.left = e.pageX;
            });

            var set_visibility = function() {
                if(scope.ReFollowing.ids.length < 1) {
                    scope.ReFollowing.window.css("visibility", "hidden");

                    scope.ReFollowing.window.css("bottom", "auto");
                    scope.ReFollowing.window.css("right", "auto");
                    scope.ReFollowing.window.css("top", 0);
                    scope.ReFollowing.window.css("left", 0);
                } else {
                    scope.ReFollowing.window.css("visibility", "visible");
                }
            };

            scope.$watchCollection("ReFollowing.ids", function() {
                $timeout.cancel(scope.timeout);
                if(scope.ReFollowing.ids.length) {
                    scope[attrs.reFollowCursor] = scope.ReFollowing.ids[0];
                }
                scope.timeout = $timeout(set_visibility, 100);
            });

            elem.bind("mouseenter", function(e) {
                scope.ReFollowing.ids.unshift(scope.ReFollowing.ids[0]);
                set_visibility();
            });

            elem.bind("mouseleave", function(e) {
                var leave = function(){
                    scope.ReFollowing.ids.pop();
                };
                $timeout(leave, 0);
            });
        }

    };
});

angular.module('referl.directives').directive('reFollowElement', function($timeout) {

    return {

        restrict: 'A',

        link: function(scope, elem, attrs) {
            if(scope.historyType == 'duplicates') {
                elem.bind("mouseenter", function(e) {
                    var addId = function(){
                        scope.ReFollowing.ids.unshift(attrs.reFollowElement);
                    };
                    $timeout(addId, 0);
                    var setPos = function(){
                        var width = scope.ReFollowing.window.width() + 4;
                        var height = scope.ReFollowing.window.height() + 4; //+4 for borders
                        var docwidth = $(document).width();
                        var docheight = $(document).height(); //+4 for borders

                        if((scope.ReFollowing.pos.left + 15 + width) > docwidth) {
                            scope.ReFollowing.window.css("right", (docwidth - scope.ReFollowing.pos.left) + 15);
                            scope.ReFollowing.window.css("left", "auto");
                        } else {
                            scope.ReFollowing.window.css("right", "auto");
                            scope.ReFollowing.window.css("left", scope.ReFollowing.pos.left + 15);
                        }

                        if((scope.ReFollowing.pos.top + height) > docheight) {
                            scope.ReFollowing.window.css("bottom", (docheight - scope.ReFollowing.pos.top));
                            scope.ReFollowing.window.css("top", "auto");
                        } else {
                            scope.ReFollowing.window.css("bottom", "auto");
                            scope.ReFollowing.window.css("top", scope.ReFollowing.pos.top);
                        }
                    }
                    $timeout(setPos, 80);
                });

                elem.bind("mouseleave", function(e) {
                    var popId = function(){
                        scope.ReFollowing.ids.pop();
                    };
                    $timeout(popId, 100);
                });

                scope.domTimeout = null;
                elem.bind('DOMSubtreeModified', function() { //For handling event delete/cancel
                    var clearIds = function() {
                        while(scope.ReFollowing.ids.length) {
                            scope.ReFollowing.ids.pop();
                        }
                    }
                    $timeout.cancel(scope.domTimeout);
                    scope.domTimeout = $timeout(clearIds, 100);
                });
            }
        }

    };
});