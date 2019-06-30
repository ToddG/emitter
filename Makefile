.PHONY: help
help:
	# -----------------------------------------------------------------------------
	# Targets:
	# 	start :		run tests by building wavegen and then starting it with
	# 			monitoring components so we can easily analyze the
	# 			performance. opens a browser window to grafana.
	#
	# 	stop :		stops the test and the monitoring components 
	#
	# 	build:          builds the 'wavegen' inside a docker container 
	# -----------------------------------------------------------------------------

.PHONY: start
start: build
	$(MAKE) monitor_start

.PHONY: build
build:  
	docker build -t toddg/erlang-wavegen .

.PHONY: log
log: 
	cd monitor && docker-compose logs -f --tail="all"

.PHONY: rebuild
rebuild:  
	docker build -t toddg/erlang-wavegen . --no-cache=true

.PHONY: stop
stop: monitor_stop

.PHONY: monitor_start
monitor_start: 
	# runs the toddg/erlang-wavegen (specified in the docker-compose.yaml)
	cd monitor && $(MAKE) start

.PHONY: monitor_stop
monitor_stop: 
	cd monitor && $(MAKE) stop
