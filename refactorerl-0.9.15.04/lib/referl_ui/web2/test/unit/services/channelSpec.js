'use strict';

describe('service channel', function() {

	beforeEach(module('referl.services'));
	beforeEach(module("referl.mocks.channel"));

	// TODO: not so nice that I am testing the mock instead the real channel
	//  - even if I know the related part is just copy-pasted
	it('should register then deregister handlers', inject(function(channelEvents, channel) {

		var handler1 = jasmine.createSpy("handler1");
		var dereg1 = channel.on("event1", handler1);
		expect(handler1).not.toHaveBeenCalled();

		channelEvents.$broadcast("event1", {});
		expect(handler1).toHaveBeenCalled();
		
		handler1.reset();
		dereg1();
		channelEvents.$broadcast("event1", {});
		expect(handler1).not.toHaveBeenCalled();

		var handler2 = jasmine.createSpy("handler2");
		var handler3 = jasmine.createSpy("handler3");
		var dereg23 = channel.on({
			event2: handler2,
			event3: handler3
		});
		expect(handler2).not.toHaveBeenCalled();
		expect(handler3).not.toHaveBeenCalled();

		channelEvents.$broadcast("event2", {});
		expect(handler2).toHaveBeenCalled();
		expect(handler3).not.toHaveBeenCalled();

		handler2.reset();
		handler3.reset();
		channelEvents.$broadcast("event3", {});
		expect(handler2).not.toHaveBeenCalled();
		expect(handler3).toHaveBeenCalled();

		handler2.reset();
		handler3.reset();
		dereg23();
		channelEvents.$broadcast("event2", {});
		channelEvents.$broadcast("event3", {});
		expect(handler2).not.toHaveBeenCalled();
		expect(handler3).not.toHaveBeenCalled();

	}));

});
