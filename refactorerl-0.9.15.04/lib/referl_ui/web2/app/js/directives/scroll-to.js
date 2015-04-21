'use strict';

angular.module('referl.directives').directive('reScrollTo', function () {
    return {
        link:
            function(scope, element, attrs) {
                element.bind("scroll", function() {
                    scope[attrs.reScrollTo] = element.scrollTop();
                });

                scope["DOMNodeInsertedTimeout"] = 0;
                element.bind('DOMNodeInserted', function() {
                    clearTimeout(scope["DOMNodeInsertedTimeout"]);
                    scope["DOMNodeInsertedTimeout"] = setTimeout(scroller, 100);
                });

                var scroller = function() {
	                element.scrollTop(scope[attrs.reScrollSaved]);
                    scope[attrs.reScrollTo] = scope[attrs.reScrollSaved];
                }

                scope.$watch(attrs.reScrollSaved, function(pos) {
		            element.scrollTop(pos);
                });
            }
    }
});
