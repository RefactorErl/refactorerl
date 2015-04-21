'use strict';

describe('service shortener', function() {

	beforeEach(module('referl.services'));

	var numbers = [
		1, 2, 10, 19, 99, 123, 999, 987465, 1234567890123456, 187459958056134,
		8752901, 6917673, 2629553, 8089942, 8986726, 8553109, 4410539, 6469720,
		5444614, 6042764, 2336624, 9944744, 8990070, 9609255, 4909973, 5246681,
		5391416, 3993860, 7139241, 8026445, 7935465, 777769, 8554142, 8550784,
		683755, 3118130, 6633231, 3623604, 704166, 9876719, 15998056134,
		45272984103, 35580022144, 55819906752, 71443208943, 91484064451,
		824114250, 24406097519, 30477649757, 1336486034
	];

	it('should be lossless', inject(function(shortener) {
		numbers.forEach(function(number) {
			var string = shortener.fromInt(number);
			expect(shortener.toInt(string)).toEqual(number);
		});
	}));

	it('should not make longer output', inject(function(shortener) {
		numbers.forEach(function(number) {
			var string = shortener.fromInt(number);
			expect(string.length).not.toBeGreaterThan(number.toString().length);
		});
	}));

	it('should generate fair shorter output', inject(function(shortener) {
		var inputLengths = 0;
		var outputLengths = 0;
		numbers.forEach(function(number) {
			var string = shortener.fromInt(number);
			inputLengths  += number.toString().length;
			outputLengths += string.length;
		});
		expect(outputLengths / inputLengths).not.toBeGreaterThan(0.70);
	}));

	it('should generate URI-safe strings', inject(function(shortener) {
		numbers.forEach(function(number) {
			var string = shortener.fromInt(number);
			expect(encodeURIComponent(string)).toEqual(string);
		});
	}));

	// poor man's profanity filter
	it('should not use vowel', inject(function(shortener) {
		var vowels = "aeiou";
		vowels += vowels.toUpperCase();
		vowels = vowels.split("");
		numbers.forEach(function(number) {
			var string = shortener.fromInt(number);
			vowels.forEach(function(vowel) {
				expect(string).not.toContain(vowel);
			});
		});
	}));

});
