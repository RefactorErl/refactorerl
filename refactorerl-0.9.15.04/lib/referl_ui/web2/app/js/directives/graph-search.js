'use strict';

angular.module('referl.directives').directive('reGraphSearch', function($document) {
  return {
    restrict: 'A',
    link: function(scope, element, attrs) {
      var nodes           = scope.results().data.nodes || [],
          nodeLabels      = nodes.map(function(node) { return node.name; }),
          minSearchLength = 3;

      $('#graph-search').on('keyup', function(e) {
        var keyCode      = e.keyCode,
            searchString = e.target.value;

        if (searchString.length < minSearchLength) { return; }

        var matches = [];

        nodes.forEach(function(node) {
          if (node.name.match(new RegExp(searchString, 'i'))) {
            matches.push(node.id);
          }
        });

        scope.$apply(function() {
          scope.searchResults = matches;
        });
      });

    }
  }
});
