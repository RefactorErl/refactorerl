'use strict';

describe('service suspendable', function() {

	beforeEach(module('referl.services'));

	var calls, wrapped;

	beforeEach(inject(function(suspendable) {
		calls = [];
		wrapped = suspendable(function(param) {
			calls.push(param);
		});
	}));

	it('should allow calls normally', function() {
		wrapped("call1");
		expect(calls).toEqual(["call1"]);

		wrapped("call2");
		expect(calls).toEqual(["call1", "call2"]);
	});

	it('should suspend calls then continue', function() {
		wrapped.suspend();

		wrapped("call1");
		expect(calls).toEqual([]);

		wrapped("call2");
		expect(calls).toEqual([]);

		wrapped.allow();
		expect(calls).toEqual(["call1", "call2"]);

		wrapped("call3");
		expect(calls).toEqual(["call1", "call2", "call3"]);
	});

	it('should allow double suspend', function() {
		wrapped.suspend();
		wrapped("call1");
		wrapped.suspend();
		wrapped("call2");

		wrapped.allow();
		expect(calls).toEqual(["call1", "call2"]);
	});

});
