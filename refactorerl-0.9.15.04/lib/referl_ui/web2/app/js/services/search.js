'use strict';

angular.module('referl.services').factory('search', function(jobType, smartLocation) {

  var search = jobType('search');

  var superExecute = search.execute;
  search.execute = function(parameters) {
    return superExecute.apply(this, arguments);
  }

  search.isActive = function() {
    return smartLocation.getPage() === 'queries';
  }

  return search;
});
