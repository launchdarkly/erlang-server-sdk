PROJECT = eld
PROJECT_DESCRIPTION = Erlang LaunchDarkly SDK Client
PROJECT_VERSION = 1.0.0-alpha1

# Dependencies

LOCAL_DEPS = inets crypto asn1 public_key ssl

DEPS = shotgun jsx verl lru backoff
dep_shotgun = git https://github.com/inaka/shotgun master
dep_jsx = git https://github.com/talentdeficit/jsx v2.9.0
dep_verl = git https://github.com/jelly-beam/verl.git v1.0.1
dep_lru = git https://github.com/barrel-db/erlang-lru 1.3.1
dep_backoff = git https://github.com/ferd/backoff 1.1.6

CT_OPTS = -ct_hooks cth_surefire -logdir logs

DOC_DEPS = edown
EDOC_OPTS += '{doclet,edown_doclet}'

# Standard targets

include erlang.mk
