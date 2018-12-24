# eld

[![Build Status](https://travis-ci.org/unix1/eld.svg?branch=master)](https://travis-ci.org/unix1/eld)

Erlang/OTP SDK client for LaunchDarkly.

#### Status
Incomplete

Implemented:
- Flag and segment storage
- Evaluations (most, see [#7](https://github.com/unix1/eld/issues/7))

Not yet implemented:
- Events
- Fancy configuration options
- Edge cases: reloads, reconnections with backoff, etc.

#### Prerequisites

- Erlang/OTP >= 20
- make

#### Play

Run in console
```bash
make run
```

Connect and evaluate
```erlang
eld:start_instance("sdk-key").
eld:evaluate(<<"my-flag">>, #{key => <<"user-12345">>}, "default-value").
```

Run tests
```dtd
make tests
```
