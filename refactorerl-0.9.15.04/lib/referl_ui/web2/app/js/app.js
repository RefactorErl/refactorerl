'use strict';

angular.module('referl', ['ui', 'ngCookies', 'referl.directives', 'referl.services', 'referl.filters']);

angular.module('referl').config(function($routeProvider, authPromise, rightPromise) {
	$routeProvider
		.when("/login", {
			controller: "LoginCtrl",
			templateUrl: "partials/login.html"
		})
		.when("/queries", {
			controller: "QueriesCtrl",
			templateUrl: "partials/queries.html",
			reloadOnSearch: false,
			resolve: {
                auth: authPromise
			}
		})
		.when("/database", {
			controller: "DatabaseCtrl",
			templateUrl: "partials/database.html",
			reloadOnSearch: false,
			resolve: {
				auth: rightPromise({right:"admin", redirect:"/queries"})
			}
		})
		.when("/errors", {
			controller: "ErrorsCtrl",
			templateUrl: "partials/errors.html",
			reloadOnSearch: false,
			resolve: {
				auth: rightPromise({right:"admin", redirect:"/queries"})
			}
		})
		.when("/graphs", {
			controller: "GraphsCtrl",
			templateUrl: "partials/graphs.html",
			reloadOnSearch: false,
			resolve: {
				auth: authPromise
			}
		})
		.when("/duplicates", {
			controller: "DuplicatesCtrl",
			templateUrl: "partials/duplicates.html",
			reloadOnSearch: false,
			resolve: {
				auth: authPromise
			}
		})
		.otherwise({redirectTo: "/queries"});
});

angular.module('referl').run(function($route) {
	// https://github.com/angular/angular.js/issues/1213#issuecomment-9152458
	$route.reload();
});

// keep session alive
angular.module('referl').run(function($http, $timeout) {
	var keepSession = function() {
		$http.get("api/session/keep");
		$timeout(keepSession, 5 * 60 * 1000);
	};
	$timeout(keepSession, 5 * 60 * 1000);
});
