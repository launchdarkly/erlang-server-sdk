REBAR3 = rebar3

all:
	@$(REBAR3) do clean, compile, ct, dialyzer

compile:
	@$(REBAR3) compile

dialyze:
	@$(REBAR3) dialyzer

deps:
	@$(REBAR3) get-deps

rel: all
	@$(REBAR3) release

run:
	@$(REBAR3) shell

doc:
	@$(REBAR3) edoc

tests:
	docker run --name ld-test-redis -p 6379:6379 -d redis
	@$(REBAR3) ct --logdir logs/ct
	docker kill ld-test-redis
	docker rm ld-test-redis

#This is used on CircleCI because the Redis Docker container is already started unlike the local tests command
circle-tests:
	@$(REBAR3) ct --logdir logs/ct

#This is for local debugging if your tests fail and the Redis Docker container is not torn down properly
clean-redis:
	docker kill ld-test-redis
	docker rm ld-test-redis

.PHONY: all compile dialyze deps rel run doc tests clean-redis circle-tests
