.PHONY: elvis test bench redis

elvis:
	elvis --config elvis.config

bench:
	rebar3 ct -v --dir test --suite esnowflake_SUITE --group bench

dialyzer:
	rebar3 dialyzer

start-redis:
	if [ "$(shell docker ps -q -f=name=esnowflake_redis)" = "" ] ; then \
		docker run -d --rm --name esnowflake_redis -p 26379:6379 redis:4; \
	fi

stop-redis:
	docker stop esnowflake_redis

test: elvis dialyzer
	$(MAKE) start-redis
	rebar3 ct -v --config ./config/test.config --dir test --suite esnowflake_SUITE --group test
	$(MAKE) stop-redis
