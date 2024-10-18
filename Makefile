REBAR3 = rebar3
ERL_VERSION = `erl -eval 'io:fwrite("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell`

all:
	@$(REBAR3) do clean, compile, ct, dialyzer

compile:
	@$(REBAR3) compile

dialyze:
	@$(REBAR3) as usage dialyzer

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
	@$(REBAR3) ct --dir="test,test-redis" --logdir logs/ct
	docker rm --force ld-test-redis

#This is used in running releaser. In this environment we do not want to run the redis tests.
release-tests:
	@$(REBAR3) ct --dir="test" --logdir logs/ct

#This is used on CircleCI because the Redis Docker container is already started unlike the local tests command
ci-tests:
	@if [ "$(ERL_VERSION)" -ge "25" ]; then\
		echo Tests for OTP 25+;\
		$(REBAR3) ct as test_otp_25 --dir="test,test-redis" --logdir logs/ct;\
	else\
		echo Tests for OTP 21;\
		@$(REBAR3) ct as test_otp_21 --dir="test,test-redis" --logdir logs/ct;\
	fi


tls-tests:
	@$(REBAR3) ct --dir="test-tls" --logdir logs/ct

#This is for local debugging if your tests fail and the Redis Docker container is not torn down properly
clean-redis:
	docker rm --force ld-test-redis

colon := :
build-contract-tests:
	@mkdir -p test-service/_checkouts
	@rm -f $(CURDIR)/test-service/_checkouts/ldclient
	@ln -sf $(CURDIR)/ $(CURDIR)/test-service/_checkouts/ldclient
	@if [ "$(ERL_VERSION)" -ge "26" ]; then\
		echo Dialyze for OTP 26+;\
		cd test-service && $(REBAR3) as otp26 dialyzer;\
	else\
		cd test-service && $(REBAR3) dialyzer;\
	fi
	@cd test-service && $(REBAR3) as prod release

start-contract-test-service:
	@$(CURDIR)/test-service/_build/prod/rel/ts/bin/ts foreground

start-contract-test-service-bg:
	@$(CURDIR)/test-service/_build/prod/rel/ts/bin/ts daemon

run-contract-tests:
	@curl -s https://raw.githubusercontent.com/launchdarkly/sdk-test-harness/main/downloader/run.sh \
      | VERSION=v2 PARAMS="-url http://localhost:8000 -debug -stop-service-at-end $(TEST_HARNESS_PARAMS)" sh

contract-tests: build-contract-tests start-contract-test-service-bg run-contract-tests

.PHONY: all compile dialyze deps rel run doc tests clean-redis circle-tests start-contract-test-service start-contract-test-service-bg run-contract-tests contract-tests
