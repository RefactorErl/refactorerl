'use strict';

angular.module('referl.directives').directive('reSigmaGraph', function($document) {
	return {

		restrict: 'A',

		link: function(scope, element, attrs) {
			var graph = sigma.init(element[0]);

			graph.drawingProperties({
				defaultLabelColor: '#000',
				font: 'Arial',
				edgeColor: 'source',
				defaultEdgeArrow: 'target'
			});

			graph.bind("overnodes", function(event) {
				var nodes = event.content;
				var neighbors = {};
				graph
					.iterEdges(function(e) {
						if (nodes.indexOf(e.source) < 0 && nodes.indexOf(e.target) < 0) {
							e.hidden = 1;
						} else {
							neighbors[e.source] = 1;
							neighbors[e.target] = 1;
						}
					})
					.iterNodes(function(n) {
						n.hidden = !neighbors[n.id] && nodes.indexOf(n.id) != 0;
					})
					.draw(2, 2, 2);
			});

			graph.bind('outnodes', function() {
				graph
					.iterEdges(function(e) {
						e.hidden = 0;
					})
					.iterNodes(function(n) {
						n.hidden = 0;
					})
					.draw(2, 2, 2);
			});

			scope.$watch(attrs.reSigmaGraph, function(value) {
				angular.forEach(value.nodes, function(node) {
					if (node.type == "function") {
						node.size = 4;
						node.color = "#666";
					} else if (node.type == "module") {
						node.size = 6;
						node.color = "#000";
					} else {
						node.size = 10;
						node.color = "#000";
					}
				});
				angular.forEach(value.edges, function(edge) {
					if (edge.color == "green") edge.type = 'curve';
				});
				graph.emptyGraph();
				graph.pushGraph(value);
				graph.draw();
			});
		}

	};
});
