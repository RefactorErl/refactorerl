'use strict';

angular.module('referl.filters').filter('fileName', function() {
    return function(filePath) {
        if(typeof filePath == "string" ) {
            return filePath.substr(filePath.lastIndexOf("/")+1);
        } else {
            return filePath;
        }
    }
});
