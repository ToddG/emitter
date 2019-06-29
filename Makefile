.PHONY: all
all: build

.PHONY: build
build: 
	cd wavegen && rebar3 do escriptize, eunit
	cd wavegen && _build/default/bin/wavegen

#.PHONY: monitor_start
#monitor_start: 
#	cd monitor && $(MAKE) start
#
#.PHONY: monitor_stop
#monitor_stop: 
#	cd monitor && $(MAKE) stop


