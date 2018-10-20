# eld

[![Build Status](https://travis-ci.org/unix1/eld.svg?branch=master)](https://travis-ci.org/unix1/eld)

Erlang/OTP SDK client for LaunchDarkly.

#### Status
Incomplete

Implemented:
- Flag and segment storage
- Evaluations (most, see #7)

Not yet implemented:
- Events
- Fancy configuration options
- Edge cases: reloads, reconnections with backoff, etc.

#### Prerequisites

- Erlang/OTP >= 20
- make

#### Play

Run in console
```dtd
make run
```

Connect and evaluate
```erlang
eld:start("[sdk-key]").
eld_eval:flag_key_for_user(<<"my-flag">>, #{key => <<"user-12345">>}).
```

Run tests
```dtd
make tests
```
