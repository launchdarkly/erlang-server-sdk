# Change log

All notable changes to the LaunchDarkly Erlang/Elixir SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

## [1.0.0-beta4] - 2020-08-24

### Changed

- Updaters now initialize asynchronously.
- Updated `shotgun` from 0.4.0 to 0.5.0.

### Fixed

- Fixed `GenServer` crash on socket close in `handle_cast` in `ldclient_event_dispatch_httpc.erl`.
- Fixed network timeout on initial connection.
- Fixed restarting updates after a crash.
- Fixed updater initialization state bugs.

## [1.0.0-beta3] - 2020-05-18

### Added

- Added Erlang/OTP 23 image to CircleCI tests

### Fixed

- Fixed a bug preventing user first name and last name attibutes from properly being set
- Fixed dates in tests to be RFC3339 compliant
- Fixed dialyzer warning when application was started with `offline` option
- Fixed SSE parsing bug in shotgun library with a workaround

## [1.0.0-beta2] - 2020-02-20

### Changed

- Renamed `eld` to `ldclient` to better adhere to LaunchDarkly SDK naming conventions.
- Changed the `pkg_name` to `launchdarkly_server_sdk`

## [1.0.0-beta1] - 2020-02-12

### Added

- Added offline mode which stops the SDK making remote calls to LaunchDarkly and variation calls will then fall back to default values for your feature flags. You can do this by setting offline mode in the config map with the `offline` key.

- Added ETag polling cache for If-None-Match on update requests.

- The SDK now specifies a uniquely identifiable request header when sending events to LaunchDarkly to ensure that events are only processed once, even if the SDK sends them two times due to a failed initial attempt.

- Added an initialized function which indicates whether the SDK is in offline mode or if the update processor has been initialized.

### Changed

- Return last variation when user bucket exceeds variation weight sum.

- Client now checks initialization status when evaluating a variation or all flags.

## [1.0.0-alpha4] - 2019-12-19

### Added

- Support for experimentation features. See `eld:track_with_metric/4-5`.

### Fixed

- Custom URI configuration is now consistent with other SDKs
- Bucketing logic for custom non-string attributes is brought in line with the other SDKs

## [1.0.0-alpha3] - 2019-10-29

### Fixed

- Dependencies specify tagged versions and use hex (thank you @hez)
- `eld:all_flags_state/2` now uses correct non-default instance (thank you @hez)
- Streaming connection will now retry after initial request timeout

### Removed
- `eld:evaluate/3-4` which were deprecated in the previous version
- `erlang.mk`, `Makefile` now uses `rebar3`

## [1.0.0-alpha2] - 2019-08-07

### Added

- Support for all server side LaunchDarkly events
- `eld:variation/3-4` and `eld:variation_detail/3-4` functions to replace `eld:evaluate/3-4` to reflect the naming convention
- Feature to inline users inside events
- Feature to define global private user attributes, including making all user attributes private

### Fixed

- Events POST URI, it no longer gets HTTP 405 Method Not Allowed error

### Deprecated
- `eld:evaluate/3-4` will be removed in a future release

### Missing

- Polling support
- Known issues for some edge case error conditions, and other minor missing features

## [1.0.0-alpha1] - 2019-07-24

### Added

- Initial public release
- Support for streaming and evaluations

### Missing

- Events don't pass integration tests
- Polling support
- Other known issues and minor missing features
