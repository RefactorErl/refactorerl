'use strict';

angular.module('referl.directives').directive('reD3Graph', function($document) {
  return {
    restrict: 'A',
    link: function(scope, element, attrs) {

      // Initialize d3 Graph
      var container = element[0],
          width     = $(container).width(),
          height    = $(container).height(),
          color     = d3.scale.category20();

      var force = d3.layout.force()
                    .charge(-500)
                    .linkDistance(100)
                    .size([width, height]);

      $('.results-collapsible').data('graph', force);

      var zoomed = function() {
        svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
      }

      var dragStart = function() {
        d3.select('.results-collapsible').classed('dragging', true);
      }

      var dragEnd = function() {
        d3.select('.results-collapsible').classed('dragging', false);
      }

      var drag = d3.behavior.drag()
        .on('dragstart', dragStart)
        .on('dragend', dragEnd);

      var svg = d3.select(container).append("svg")
                  .attr('width', width - 10)
                  .attr('height', height - 10)
                  .call(d3.behavior.zoom().on('zoom', zoomed))
                  .call(drag)
                  .append('g');

      svg.append('svg:defs').append('svg:marker')
         .attr('id', 'end-arrow1')
         .attr('viewBox', '0 -5 10 10')
         .attr('refX', 6)
         .attr('markerWidth', 3)
         .attr('markerHeight', 3)
         .attr('orient', 'auto')
         .append('svg:path')
         .attr('d', 'M0,-5L10,0L0,5')
         .attr('fill', '#000');
	  svg.append('svg:defs').append('svg:marker')
         .attr('id', 'end-arrow2')
         .attr('viewBox', '0 -5 10 10')
         .attr('refX', 6)
         .attr('markerWidth', 3)
         .attr('markerHeight', 3)
         .attr('orient', 'auto')
         .append('svg:path')
         .attr('d', 'M0,-5L10,0L0,5')
         .attr('fill', '#080');

      var path, circle, text, selectedNode,
          direction = scope.collapsibleDirection() ,
          maxDepth  = scope.collapsibleMaxDepth(),
          nodes     = [],
          links     = [];

      scope.$watch(attrs.reD3Graph, function(graph) {
        if (graph.nodes === '') { $('.results-collapsible').addClass('no-results'); }

        // D3 initialization require source and target node ids
        var nodeIds = (graph.nodes || []).map(function(n) { return n.id; });

        if (graph.links === '') {
          graph.links = []
        }

        graph.links.forEach(function(edge) {
          edge.source = nodeIds.indexOf(edge.source.id || edge.source);
          edge.target = nodeIds.indexOf(edge.target.id || edge.target);
        });

        // Initialize graph data
        nodes = d3.values(graph.nodes);
        links = graph.links;

        // Update view
        var rootFunction = scope.parameters.rootFunction[0],
            rootModule   = scope.parameters.rootModule[0];

        if (typeof(rootFunction) !== 'undefined') {
          var node = nodes.filter(function(node) {
            var matchResult = rootFunction.id.match(/:(\w+\/\d+)/);
            if (matchResult) {
              return node.name === matchResult[1];
            } else {
              // rootFunction is a regexp
              return node.name.match(rootFunction.id) != null;
            }
          })[0];
          staticUpdate(function() {
            selectedNode = node;
            collapseNode(node);
          });
        } else if (typeof(rootModule) !== 'undefined') {
          var node = nodes.filter(function(node) { return node.name === rootModule.id; })[0];
          staticUpdate(function() {
            selectedNode = node;
            collapseNode(node);
          });
        } else {
          staticUpdate();
        }

        // Collapse nodes from
        //selectedNode = nodes.filter(function(node) { return node.type == 'root'; })[0];
        //collapseNode(selectedNode);

        // Watch settings
        scope.$watch(scope.collapsibleMaxDepth, function(collapsibleMaxDepth) {
          maxDepth = collapsibleMaxDepth;

          // TODO: should we change the state of the graph, when maxDepth changes?
          //if (typeof(selectedNode) !== 'undefined') {
          //  collapseNode(selectedNode);
          //} else {
          //  staticUpdate();
          //}
        });

        scope.$watch(scope.collapsibleDirection, function(collapsibleDirection) {
          direction = collapsibleDirection;
          // TODO: should we change the state of the graph, when maxDepth changes?
          //staticUpdate();
        });

        scope.$watchCollection('searchResults', function(results) {
          var circles = d3.selectAll('circle');
          circles.classed('search-result', false);
          results.forEach(function(id) {
            d3.select('#circle-' + id).classed('search-result', true);
          });
        });

        function update() {
          if (scope.collapsibleAnimation) {
            dynamicUpdate();
          } else {
            staticUpdate();
          }
        }

        function staticUpdate(callback) {
          d3.select('.results-collapsible').classed('loading', true);
          setTimeout(function() {
            dynamicUpdate();
            force.start();
            for (var i = 50; i > 0; --i) force.tick();
            force.stop();
            d3.select('.results-collapsible').classed('loading', false);

            if (typeof(callback) !== 'undefined') { callback(); }
          });
        }

        function dynamicUpdate() {
          force
            .nodes(nodes) // These nodes are the displayed ones
            .links(links)
            .on('tick', tick)
            .start();

          if (path) { d3.select('.paths').remove(); }

          path = svg.append('g')
            .attr('class', 'paths')
            .selectAll('path')
            .data(force.links())
            .enter()
            .append('path')
            .attr('class', 'link')
            .style('stroke', function(d) { return d.type == 'icallarc' ? 'green' : 'grey'; })
            .style('stroke-dasharray', function(d) { return d.type == 'icallarc' ? '6,6' : ''; })
            .attr('marker-end', function(d) { return d.type == 'icallarc' ? 'url(#end-arrow2)' : 'url(#end-arrow1)'; })
            .attr('d', 'M0,0L0,0');

          if (circle) { d3.select('.circles').remove(); }
          circle = svg.append('g')
            .attr('class', 'circles')
            .selectAll('circle')
            .data(force.nodes())
            .enter()
            .append('circle')
            .on('click', click)
            .attr('class', function(d) { return 'node node-' + d.type + (d.recent ? ' recent' : ''); })
            .attr('r', function(d) { return d.type == 'root' ? 9 : 6; })
            .attr('fill', function(d) { return d.color; })
            .attr('id', function(d) { return 'circle-' + d.id })
            .attr('name', function(d) { return d.id });

          if (text) { d3.select('.texts').remove(); }
          text = svg.append('g')
            .attr('class', 'texts')
            .selectAll('text')
            .data(force.nodes())
            .enter()
            .append('text')
            .attr('x', 8)
            .attr('y', '.31em')
            .text(function(d) { return d.name });
        }

        function click(d) {
          if (d3.event.defaultPrevented) return; // ignore drag

          selectedNode = d;

          collapseNode(d);
        }

        function collapseNode(node) {
          nodes = [];
          links = [];

          var graphNodes = graph.nodes;
          var graphLinks = graph.links;

          graphNodes = graphNodes.map(function(node) {
            node.visited = false;
            node.recent  = false;
            return node;
          });

          nodes.push(node);

          // BACKWARD:
          // I.) Put every node from forward way into `nodes` to draw them
          // II.) Every node from backward (where link.target = this)
          //      when their distance is lesse or equals to maxDepth, should have visible state, when

          if (direction == 'both') {
            // First part of the algorythm
            drawActualDirection(node, nodes, links, graphNodes, graphLinks);
          } else {
            // First part of the algorythm
            drawOppositeDirection(node, nodes, links, graphNodes, graphLinks);
            // Second part of the algorythm
            drawActualDirection(node, nodes, links, graphNodes, graphLinks);
          }

          update();
        }

        function drawOppositeDirection(node, nodes, links, graphNodes, graphLinks) {
          // I.)
          // Loop edges from selectedNode
          // 1.) Links are going from this
          var sourceLinks = getLinksFromNode(node, graphLinks, oppositeDirection(direction));
          sourceLinks.forEach(function(link) { links.push(link); });

          // 2.) Put every node from this edge into `nodes`, because we need to show them
          sourceLinks.forEach(function(link) {
            // TODO REFACTOR
            // we should use a function to push elements to nodes
            // and check if it is in the array already
            if (nodes.indexOf(link.source) == -1) {
              nodes.push(link.source);
            }
            if (nodes.indexOf(link.target) == -1) {
              nodes.push(link.target);
            }
          });

          // 3.) Loop through this new node.
          while (nodes.filter(function(node) { return !node.visited; }).length != 0) {
            nodes.forEach(function(node) {
              if (!node.visited) {
                var sourceLinks = getLinksFromNode(node, graphLinks, 'both');
                sourceLinks.forEach(function(link) {
                  if (links.indexOf(link) == -1) {
                    links.push(link);
                  }
                });
                sourceLinks.forEach(function(link) {
                  // TODO REFACTOR
                  // we should use a function to push elements to nodes
                  // and check if it is in the array already
                  if (nodes.indexOf(link.source) == -1) {
                    nodes.push(link.source);
                  }
                  if (nodes.indexOf(link.target) == -1) {
                    nodes.push(link.target);
                  }
                });
              }
            });
          }
        }

        function drawActualDirection(node, nodes, links, graphNodes, graphLinks) {
          // II.) Step
          node.visited = false;
          var newNodes = [node];
          for (var i = 0; i < maxDepth; i++) {
            var notVisitedNodes = newNodes.filter(function(node) { return !node.visited; });

            notVisitedNodes.forEach(function(node) {
              if (nodes.indexOf(node) == -1) {
                nodes.push(node);
              }
              node.visited = true;
              // TODO FIXME
              var targetLinks = getLinksFromNode(node, graphLinks, i == 0 ? direction : 'both');

              targetLinks.forEach(function(link) {
                if (links.indexOf(link) == -1) {
                  links.push(link);
                }
                var sourceNode = link.source,
                targetNode = link.target;

              if (newNodes.indexOf(sourceNode) == -1) {
                newNodes.push(link.source);
              }
              if (newNodes.indexOf(targetNode) == -1) {
                newNodes.push(link.target);
              }

              });
            });
          }
          newNodes.forEach(function(node) { node.recent = true; });

          var notVisitedNodes = newNodes.filter(function(node) { return !node.visited; });
          notVisitedNodes.forEach(function(node) { if (nodes.indexOf(node) == -1) { nodes.push(node); } });
        }

        // Returns the opposite direction
        function oppositeDirection(direction) {
          switch(direction) {
            case 'forward':
              return 'backward';
              break;
            case 'backward':
              return 'forward';
              break;
            case 'both':
              return 'both';
              break;
          }
        }

        // Returns those links that have node as source or target node, according
        // to the specified direction
        function getLinksFromNode(node, links, direction) {
          if (!node) { return []; }
          node.visited = true;

          return links.filter(function(link) {
            switch(direction) {
              case 'forward':
                return link.source.id === node.id;
                break;
              case 'backward':
                return link.target.id === node.id;
                break;
              case 'both':
                return link.target.id === node.id || link.source.id === node.id;
                break;
            }
          });
        }

        // Returns those links that have both nodes in nodes array
        function filterLinks(links, nodes) {
          return links.filter(function(link) {
            return nodes.indexOf(link.source) > -1 && nodes.indexOf(link.target) > -1;
          });
        }

        function drawSiblings(node, graphNodes, graphLinks) {
          node.visited = true;

          graphLinks.forEach(function(link) {
            switch(direction) {
              case 'both':
                if (link.source == node || link.target == node) {
                  links.push(link);
                }
                break;
              case 'forward':
                if (link.source == node) {
                  links.push(link);
                }
                break;
              case 'backward':
                if (link.target == node) {
                  links.push(link);
                }
                break;
            };
          });

          var linkNodes = [];
          links.forEach(function(link) {
            linkNodes.push(link.source);
            linkNodes.push(link.target);
          });

          graphNodes.forEach(function(node) {
            if (linkNodes.indexOf(node) > -1 && nodes.indexOf(node) == -1)  {
              nodes.push(node);
            }
          });

        }

        function tick() {
          path.attr('d', function(d) {
            var deltaX = d.target.x - d.source.x,
                deltaY = d.target.y - d.source.y,
                dist   = Math.sqrt(deltaX * deltaX + deltaY * deltaY),
                normX  = deltaX / dist,
                normY  = deltaY / dist,
                bezierX = (d.target.x + d.source.x) * 0.5 + normY * dist * -0.1,
                bezierY = (d.target.y + d.source.y) * 0.5 + normX * dist * 0.1,
                sourcePadding = 6,
                targetPadding = 9,
                sourceX = d.source.x + (sourcePadding * normX),
                sourceY = d.source.y + (sourcePadding * normY),
                targetX = d.target.x - (targetPadding * normX),
                targetY = d.target.y - (targetPadding * normY);

            if (deltaX === 0 && deltaY === 0) {
              var drx = 20,
                  dry = 20,
                  sourceX   = d.source.x + sourcePadding - 1,
                  sourceY   = d.source.y + sourcePadding - 1,
                  targetX   = d.target.x - targetPadding + 2,
                  targetY   = d.target.y - targetPadding + 2,
                  xRotation = -45,
                  largeArc  = 1;

              return 'M' + sourceX + ',' + sourceY + 'A' + drx + ',' + dry + ' ' + xRotation + ',' + largeArc + ',' + 1 + ' ' + targetX + ',' + targetY;
            }

            return d.type == 'icallarc' ? 'M' + (sourceX || 0) + ',' + (sourceY || 0) + 'Q' + bezierX + ','
                                              + bezierY + ',' + (targetX || 0) + ',' + (targetY || 0) :
                   'M' + (sourceX || 0) + ',' + (sourceY || 0) + 'L' + (targetX || 0) + ',' + (targetY || 0);
          });

          circle.attr('transform', function(d) { return 'translate(' + d.x + ',' + d.y + ')'; });
          text  .attr('transform', function(d) { return 'translate(' + d.x + ',' + d.y + ')'; });
        }
      });

    }
  };
});
