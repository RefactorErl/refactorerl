'use strict';

describe('service autoComplete', function() {

	beforeEach(module('referl.services'));

	describe('commonPrefix', function() {

		it('should be correct', inject(function(autoComplete) {
			var cp = autoComplete.commonPrefix;

			expect(cp([])).toBe('');
			expect(cp(['a', 'ab'])).toBe('a');
			expect(cp(['b', 'ba'])).toBe('b');
			expect(cp(['a', 'ba'])).toBe('');
			expect(cp(['b', 'ab'])).toBe('');
			expect(cp(['foo'])).toBe('foo');
			expect(cp(['foo', 'bar'])).toBe('');
			expect(cp(['interspecies', 'intersteltar', 'interstate'])).toBe('inters');
		}));

	});

	describe('fixPrefix', function() {

		it('should be correct', inject(function(autoComplete) {
			var fp = autoComplete.fixPrefix;

			expect(fp({id: "foobar",  text: "bar"})).toBe("foo");
			expect(fp({id: "fooxbar", text: "bar"})).toBe("foox");
			expect(fp({id: "foobar",  text: "foo"})).toBe("foobar");
			expect(fp({id: "foobar",  text: "par"})).toBe("foobar");
			expect(fp({id: "empty",   text: ""   })).toBe("empty");
			expect(fp({id: "",        text: ""   })).toBe("");
			expect(fp({id: "",        text: "x"  })).toBe("");
			expect(fp({id: "x",       text: "x"  })).toBe("");
		}));

	});

	describe('cleanPrefixes', function() {

		it('should handle the usual case', inject(function(autoComplete) {
			var results = [
				{id: "mods[a", text: "a"},
				{id: "mods[b", text: "b"}
			];
			var expected = [
				{id: "a", text: "a"},
				{id: "b", text: "b"}
			];
			expect(autoComplete.cleanPrefixes(results)).toBe("mods[");
			expect(results).toEqual(expected);
		}));

		it('should handle common prefix', inject(function(autoComplete) {
			var results = [
				{id: "mods.funs[a",    text: "a"},
				{id: "mods.records[b", text: "b"}
			];
			var expected = [
				{id: "funs[a",    text: "a"},
				{id: "records[b", text: "b"}
			];
			expect(autoComplete.cleanPrefixes(results)).toBe("mods.");
			expect(results).toEqual(expected);
		}));

		it('should handle unrelated results', inject(function(autoComplete) {
			var results = [
				{id: "mods[a", text: "a"},
				{id: "file[b", text: "b"}
			];
			var expected = [
				{id: "mods[a", text: "a"},
				{id: "file[b", text: "b"}
			];
			expect(autoComplete.cleanPrefixes(results)).toBe("");
			expect(results).toEqual(expected);
		}));

		it('should work for empty results', inject(function(autoComplete) {
			var results = [];
			var expected = [];
			expect(autoComplete.cleanPrefixes(results)).toBe("");
			expect(results).toEqual(expected);
		}));

	});

});
