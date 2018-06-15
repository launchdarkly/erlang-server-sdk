PROJECT = eld
PROJECT_DESCRIPTION = Erlang LaunchDarkly SDK Client
PROJECT_VERSION = 0.0.1

# Whitespace to be used when creating files from templates.
SP = 4

DEPS = shotgun jsx
dep_shotgun = git https://github.com/inaka/shotgun master
dep_jsx = git https://github.com/talentdeficit/jsx v2.9.0

DOC_DEPS = edown
EDOC_OPTS += '{doclet,edown_doclet}'

include erlang.mk
