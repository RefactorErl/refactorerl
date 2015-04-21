'use strict';

function LoginCtrl($scope, auth) {
	var credentials = {
		username: "",
		password: ""
	};
	$scope.credentials = credentials;
    $scope.restricted = window.restrictedMode;
    $scope.needpass = function(){
        var val = $scope.restricted && $scope.credentials.username=="admin";
        if(!val) {
            $scope.credentials.password="";
        }
        return val;
    };
	$scope.login = function() {
        if( $scope.needpass() && $scope.credentials.password.length<3 ) {
            alert("Password required!");
            return false;
        }
        if( $scope.credentials.username.length<1 ) {
            alert("Username required!");
            return false;
        }
		auth.login($scope.credentials.username, $scope.credentials.password, true);
	};
}
