# Change log

All notable changes to the LaunchDarkly Erlang/Elixir SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

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
