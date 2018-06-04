.PHONY: elvis test bench redis

elvis:
	elvis --config elvis.config

bench:
	rebar3 ct -v --dir test --suite esnowflake_SUITE --group bench

dialyzer:
	rebar3 dialyzer

start-redis:
	docker run -d --rm --name esnowflake_redis -p 26379:6379 redis:4

stop-redis:
	docker stop esnowflake_redis

test: elvis dialyzer
	rebar3 ct -v --config ./config/test.config --dir test --suite esnowflake_SUITE --group test
