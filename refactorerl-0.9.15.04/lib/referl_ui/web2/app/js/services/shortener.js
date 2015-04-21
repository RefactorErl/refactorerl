'use strict';

angular.module('referl.services').factory('shortener', function() {
	// http://kvz.io/blog/2009/06/10/create-short-ids-with-php-like-youtube-or-tinyurl/

	var index = "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ0123456789";
	var count = index.length;

	function bcpow(a, b) {
		return Math.floor(Math.pow(parseFloat(a), parseInt(b)));
	}

	return {

		fromInt: function(input) {
			if (!input) return "";
			var result = "";
			var outputLength = Math.floor(
				Math.log(parseInt(input)) / Math.log(count)
			);
			for (var i = outputLength; i >=0; i--) {
				result += index.substr(
					(Math.floor(parseInt(input) / bcpow(count, i)) % count), 1
				);
			}
			return result;
		},

		toInt: function(string) {
			if (!string) return null;
			var result = 0;
			for (var i = 0; i <= (string.length - 1); i++) {
				result +=
					index.indexOf(string.substr(i, 1)) *
					(bcpow(index.length, (string.length - 1) - i));
			}
			return result;
		}

	};

});
