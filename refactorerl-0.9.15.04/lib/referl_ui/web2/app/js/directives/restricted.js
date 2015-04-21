'use strict';

angular.module('referl.directives').directive('reStricted', function(effects) {
	return {

		restrict: 'A',

		link: function(scope, elem, attrs) {
            elem.addClass('hide');
            scope.$watch('auth.getUser()', function(user){
                if(user && user.rights == attrs.reStricted){
                    elem.removeClass('hide');
                } else {
                    elem.addClass('hide');
                }
            });
		}

	};
});
