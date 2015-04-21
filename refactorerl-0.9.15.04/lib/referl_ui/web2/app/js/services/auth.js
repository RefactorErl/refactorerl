'use strict';

angular.module('referl.services').constant('authPromise', function(auth) {
	return auth.newPromise();
});

angular.module('referl.services').constant('rightPromise', function(params) {
    return function(auth, $q, $location) {
        var adminDefer = $q.defer();
        
        auth.newPromise().then(function(user){
            if(user && user.rights==params.right) {
                adminDefer.resolve(user);
            } else {
                $location.replace().path(params.redirect);
                adminDefer.reject();
            }
        }, function(){
            adminDefer.reject();
        });

        return adminDefer.promise;
    }
});

angular.module('referl.services').service('auth', function($q, $http, $location, $rootScope, $window) {
	var ready = false;
	var user = false;
	var userDefer = $q.defer();
	var beforeLogin = "/";

    var userRights = function() {
        if(!window.restrictedMode || user.name=="admin") {
            user.rights = "admin";
        } else {
            user.rights = "standard";
        }
    };

	if ($window.loggedInUser) {
		user = {name: $window.loggedInUser};
        userRights();
		ready = true;
		userDefer.resolve(user);
	} else {
		$http.get("api/getUser")
			.success(function(response) {
				user = response != "false" && response;
				ready = true;
				userDefer.resolve(user);
			})
			.error(userDefer.reject);
	}

	var auth = {

		/**
		 * Returns false if we are not ready yet or if the user
		 * is not logged in.
		 */
		getUser: function() {
			return ready && user;
		},

		/**
		 * Resolved when user information is available (even if
		 * that means an unauthorized user).
		 */
		getUserPromise: function() {
			if (!ready) {
				return userDefer.promise;
			} else {
				var defer = $q.defer();
				defer.resolve(user);
				return defer.promise;
			}
		},

		/**
		 * Returns a promise failing in case of not successful login.
		 *
		 * navigateOnSuccess: navigate on success to the page where
		 * newPromise failed or to the homepage.
		 */
		login: function(username, password, navigateOnSuccess) {
			var parameters = {
				username: username,
				password: password
			};
			var loginDefer = $q.defer();
			$http.get("api/login", {params: parameters})
				.success(function(response) {
                    if(response.error) {
                        alert(response.error);
						loginDefer.reject(response);
                    } else {
					    user = response;
					    ready = true;
					    userDefer.resolve(user);
                        userRights();
					    loginDefer.resolve(user);
					    if (navigateOnSuccess) {
						    auth.navigateToBeforeLogin();
					    }
                    }
				});
			return loginDefer.promise;
		},

		navigateToBeforeLogin: function() {
			if (beforeLogin != "/") {
				$location.replace();
			}
			$location.path(beforeLogin);
			beforeLogin = "/";
		},

		logout: function() {
			$http.get("api/logout").then(function() {
				user = false;
			});
		},

		/**
		 * Returns a promise which will be resolved if the user 
		 * is _currently_ logged in. Otherwise redirects to
		 * /login location and saves the current path to restore
		 * it after succesful login.
		 */
		newPromise: function() {
			var defer = $q.defer();
			var localBeforeLogin = $location.path();
			var login = function() {
				beforeLogin = localBeforeLogin;
				$location.replace().path("/login");
				defer.reject();
			};
			auth.getUserPromise()
				.then(function(user) {
					if (user) {
						defer.resolve(user);
					} else {
						login();
					}
				}, login);
			return defer.promise;			
		}

	};
	$rootScope.auth = auth;
	return auth;
});
