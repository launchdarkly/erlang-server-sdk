# Change log

All notable changes to the LaunchDarkly Erlang/Elixir SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

## [3.1.0](https://github.com/launchdarkly/erlang-server-sdk/compare/v3.0.4...v3.1.0) (2024-01-04)


### Features

* For otp 25+ use public_key:cacerts_get for the default certificate list. ([#114](https://github.com/launchdarkly/erlang-server-sdk/issues/114)) ([b7065aa](https://github.com/launchdarkly/erlang-server-sdk/commit/b7065aacb2bdfab1a395eb8bdd5a0754b19bee1a))


### Bug Fixes

* Make gun open errors explicitly into temporary failures ([#118](https://github.com/launchdarkly/erlang-server-sdk/issues/118)) ([fc70445](https://github.com/launchdarkly/erlang-server-sdk/commit/fc70445daf179e3d24a5dc5685f5db6ea0ee91cc))

## [3.0.4](https://github.com/launchdarkly/erlang-server-sdk/compare/v3.0.3...v3.0.4) (2023-12-04)


### Bug Fixes

* handle `already_started` when starting instance ([#111](https://github.com/launchdarkly/erlang-server-sdk/issues/111)) ([5e181d4](https://github.com/launchdarkly/erlang-server-sdk/commit/5e181d44c371dc1021255090bceab8151d7dfb76))

## [3.0.3](https://github.com/launchdarkly/erlang-server-sdk/compare/v3.0.2...v3.0.3) (2023-11-17)


### Bug Fixes

* Allow Elixir projects to read flags from a local file ([#106](https://github.com/launchdarkly/erlang-server-sdk/issues/106)) ([c290599](https://github.com/launchdarkly/erlang-server-sdk/commit/c290599223c9115115b36628bc16bc529bb87afd))

## [3.0.2](https://github.com/launchdarkly/erlang-server-sdk/compare/v3.0.1...v3.0.2) (2023-11-16)


### Bug Fixes

* Update certifi dependency to 2.12.0 ([b0c5e56](https://github.com/launchdarkly/erlang-server-sdk/commit/b0c5e56066fc432705a822adbecef723ce77a86c))

## [3.0.1](https://github.com/launchdarkly/erlang-server-sdk/compare/v3.0.0...v3.0.1) (2023-09-26)


### Bug Fixes

* Fix arithmetic error in stream re-connection logic. ([#104](https://github.com/launchdarkly/erlang-server-sdk/issues/104)) ([416af68](https://github.com/launchdarkly/erlang-server-sdk/commit/416af68f61975cbfbab1b5e38982c4b489043a8e))

## [3.0.0](https://github.com/launchdarkly/erlang-server-sdk/compare/2.1.2...v3.0.0) (2023-09-14)

**Version 3.0 requires Gun 2.x**. There are no changes to the Erlang SDK API or functionality. However, version 3.0 now requires [Gun 2.x](https://hex.pm/packages/gun).

If your application depends directly on Gun, or if you have other dependencies that use Gun, then you must ensure that they work with Gun 2.x.

### ⚠ BREAKING CHANGES

* Upgrade to support Gun 2.x ([#99](https://github.com/launchdarkly/erlang-server-sdk/issues/99))

### Features

* Upgrade to support Gun 2.x ([#99](https://github.com/launchdarkly/erlang-server-sdk/issues/99)) ([f15bf4d](https://github.com/launchdarkly/erlang-server-sdk/commit/f15bf4d8011c3d83e127b6aefb6e55c67c359649))

## [2.1.2](https://github.com/launchdarkly/erlang-server-sdk/compare/2.1.1...v2.1.2) (2023-08-24)


### Bug Fixes

* Changes for OTP 26. ([#93](https://github.com/launchdarkly/erlang-server-sdk/issues/93)) ([5f40282](https://github.com/launchdarkly/erlang-server-sdk/commit/5f40282f56a825138af9ca869e9a2c99bdeecd08))

### Changed
* Update to `eredis` version `1.7.1`.

## [2.1.1] - 2023-08-02
### Fixed:
- Fixed `ldclient_instance:options()` to include new `redis_tls` option. Only specs affected.

## [2.1.0] - 2023-08-01
### Added:
- A new configuration option, `redis_tls`, for configuring TLS settings for redis. When this option is omitted Redis will not use TLS. When specified it should contain a list of `ssl:tls_option()`. These options will be forwarded to `eredis`.

## [2.0.5] - 2023-06-12
### Changed:
- Upgraded `yamerl` to version `0.10.0`.

## [2.0.4] - 2023-06-05
### Fixed:
- Remove error log message associated with debugging redis initialization state.

## [2.0.3] - 2023-05-30
### Fixed:
- Add missing `boolean()` type to the `ldclient_context:attribute_value()` definition.

## [2.0.2] - 2023-05-26
### Fixed:
- Fixed an issue that would prevent using values from redis for `variation` or `all_flags_state` calls before client initialization was complete. After this change if the erlang SDK is using an initialized redis prefix, then it will be able to evaluate against that store before initialization is complete. Note that the SDK did not previously store the `$inited` key used when detecting that a store is initialized, so the store will need updated by this SDK version (or newer) at least once before it would be considered initialized.

## [2.0.1] - 2023-05-04
### Fixed:
- Fixed an issue where invalid contexts would generate events resulting in a crash in the event server.

## [2.0.0] - 2023-02-22
The latest version of this SDK supports LaunchDarkly's new custom contexts feature. Contexts are an evolution of a previously-existing concept, "users." Contexts let you create targeting rules for feature flags based on a variety of different information, including attributes pertaining to users, organizations, devices, and more. You can even combine contexts to create "multi-contexts." 

For detailed information about this version, please refer to the list below. For information on how to upgrade from the previous version, please read the [migration guide](https://docs.launchdarkly.com/sdk/server-side/erlang/migration-1-to-2).

### Added:
- Added a new module, `ldclient_context`, which defines the new "context" model.
- All SDK methods that took an `ldclient_user:user()` now accept an `ldclient_context:context()` as well.
- Added `ldclient_flagbuilder:if_match/4`, `ldclient_flagbuilder:if_not_match/4`, `ldclient_flagbuilder:and_match/4`, and `ldclient_flagbuilder:and_not_match/4` which allow creating rules targeting specific context kinds using `ldclient_testdata`.

### Changed _(breaking changes from 1.x)_:
- The secondary meta-attribute, which previously affected percentage rollouts, no longer exists. If you set an attribute with that name in `ldclient_context:context()`, it will simply be a custom attribute like any other.
- Evaluations now treat the anonymous attribute as a simple boolean, with no distinction between a false state and an undefined state.
- The `binary()` values in `private_attributes` in the configuration map now represent attribute references. If an attribute name had started with a `/`, then that will need to be escaped. For instance `/myAttribute` would now reference an attribute named `myAttribute` not an attribute named `/myAttribute`. The attribute reference would need to be updated to the escaped form `/~1myAttribute`.
- `ldclient_flagbuilder:variation_for_all_users` has been replaced with `ldclient_flagbuilder:variation_for_all` and now applies to contexts.
- `ldclient_flagbuilder:variation_for_user` has been replaced with `ldclient_flagbuilder:variation_for_context` and requires a context kind to be provided.
- `user_keys_capacity` has been replaced with `context_keys_capacity` in the option configuration map.

### Changed (behavioral changes):
- The SDK can now evaluate segments that have rules referencing other segments.
- Analytics event data now uses a new JSON schema due to differences between the context model and the old user model.

### Removed:
- Removed the `secondary` meta-attribute in `ldclient_user:user()`.
- The `ldclient:alias` method no longer exists because alias events are not needed in the new context model.
- The `inline_users_in_events` option no longer exists because it is not relevant in the new context model.

## [1.6.0] - 2023-01-30
### Added:
- `application` option, for configuration of application metadata that may be used in LaunchDarkly analytics or other product features. This does not affect feature flag evaluations.
```
ldclient:start_instance("sdk-key", #{
  application => #{
    id => <<"my-app-id">>,
    version => <<"my-app-version">>
  }
})
```
- Added support for using server time, from response headers, when determining if debug events should be sent.

### Changed:
- Updated jitter/backoff implementation to be consistent with other SDK implementations.
- Upgraded `lru` to version `2.4.0`.
- Upgrade `certifi` to version `2.10.0`.
- Removed dependency on `backoff`.

### Fixed:
- Fixed an issue where the SDK did not handle deleted flags/segments correctly in combination with evaluation of all flags.
- Allow for track data to be `null`.

## [1.5.0] - 2022-06-23
### Changed:
- Updated `certifi` to 2.9.0.
- Updated `shotgun` to 0.5.3. This should obviate the need to override `shotgun` in `mix.exs` for Elixir projects.

### Fixed:
- Updated URI parsing to work with OTP 25. Removing the usage of `http_uri:parse` and replacing it with `uri_string:parse`.

## [1.4.0] - 2022-04-20
### Added:
- `ldclient:all_flags_state/3` to be used instead of `ldclient:all_flags_state/1 or /2` if you are passing flag data to the front end for use with the JavaScript SDK. It preserves some flag metadata that the front end requires in order to send analytics events correctly. It does NOT yet support selecting client-side-enabled flags.
- `ldclient_testdata` is a new way to inject feature flag data programmatically into the SDK for testing—either with fixed values for each flag, or with targets and/or rules that can return different values for different users. Unlike the file data source, this mechanism does not use any external resources, only the data that your test code has provided.
- Support for the [SDK test harness](https://github.com/launchdarkly/sdk-test-harness).

### Fixed:
- An issue with handling `firstName` and `lastName` in rules. The SDK uses `first_name` and `last_name` atoms for these fields and there was an issue with when the attribute names were converted. Now name conversion for attributes are done after evaluation and processing of private attributes.
- `ldclient:track` and `ldclient:tack_metric` will not allow for types other than `map()` to be used for `data`.
- The SDK was sending identify events for users with an empty key when it should not.
- The SDK would send duplicate events for a user which had been identified and then was noticed by a feature evaluation. Now the event will be de-duplicated correctly within the LRU cache timeout.
- Rule matches against dates in the user object, which could not be parsed, would cause evaluation to fail and return the default value. Now they will correctly result in the rule not matching instead.
- The configuration was not trimming trailing `/` from URLs.

### Changed:
- Event fields which contained false or null will not generally be omitted from events for compactness.
- Clarifications to documentation.

## [1.3.2] - 2021-12-30
### Fixed:
- The file `file_auto_update` feature was not working correctly. If the flag files did not contain any changes between polling intervals, then the file system watcher would crash. (Thanks, [matt-glover](https://github.com/launchdarkly/erlang-server-sdk/pull/52)!)

## [1.3.1] - 2021-11-09
### Fixed:
- Updated the `options()` typings for `ldclient_instance:start` to include missing configuration options. These included redis configuration, ld relay configuration, cache ttl, file data source configuration, and the http options. This is not a functionality change, but will correct issues reported by the dialyzer.

## [1.3.0] - 2021-11-03
### Added:
- Support for configuring HTTP options. Including:
     - The ability to specify custom headers.

     ```
      ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [
                {"Basic-String-Header", "String"},
                {"Binary-String-Header", "Binary"}
            ]}
        }),
    ```
    - The ability to specify a custom connect timeout.
 
    ```
    ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            connect_timeout => 2000
        }
    }),
    ```
    - The ability to specify a custom TLS configuration.
    The default behavior for OTP is to use `verify_none` for TLS verification. If no configuration is specified, then the SDK will inherit this behavior.

    ```
    ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            %% This is a list of tls_client_options()
            %% https://www.erlang.org/doc/man/ssl.html#type-tls_client_option
            tls_options => ldclient_config:tls_basic_options()
        }
    }),
    ```
    - A number of TLS configuration helpers. A full TLS configuration can be specified, but these methods provide a number of basic configurations.
        - `ldclient_config:tls_basic_options() -> [ssl:tls_client_option()]`  
        Create a basic TLS configuration which will try to use a default location for certificate authority store.
        If that store is not present, then instead the configuration will use a certifi store which is a Rebar3 dependency.
        - `ldclient_config:tls_basic_linux_options() -> [ssl:tls_client_option()]`
        Create a basic TLS configuration which will use a default location for a certificate store. If the store is not present, then it will produce an error.
        - `ldclient_config:tls_ca_certfile_options(CaStorePath :: string()) -> [ssl:tls_client_option()]`
        Create a basic TLS configuration with the specified certificate store. If the store is not present, then it will produce an error.
        - `ldclient_config:with_tls_revocation(Options :: [ssl:tls_client_option()]) -> [ssl:tls_client_option()]`
        Decorate a configuration with revocation checks. Currently revocation checks are not cached in OTP, so this will result in additional requests for each HTTP request.
        - `ldclient_config:tls_basic_certifi_options() -> [ssl:tls_client_option()]`
        Create a basic TLS configuration which will use the certifi store specified in the Rebar3 dependencies.

## [1.2.0] - 2021-10-18
### Added:
- The SDK now supports the ability to control the proportion of traffic allocation to an experiment. This works in conjunction with a new platform feature now available to early access customers.
- The SDK now supports the ability to read flags from a file.

## [1.1.3] - 2021-09-27
### Fixed:
- When a rule was missing both the rollout and the variation, and that rule was matched, then variation 0 would be returned instead of the default value.
- When a fall-through was missing both a rollout and a variation, then null would be returned instead of the default value.

## [1.1.2] - 2021-09-13
### Fixed:
- The SDK now forces connections to use HTTP/1.1 instead of the newer HTTP/2 protocol as an underlying dependency does not yet support HTTP/2. This change does not impact current behavior as LaunchDarkly’s servers do not yet accept requests with HTTP/2; however, this SDK change ensures operational continuity when LaunchDarkly’s services enable HTTP/2 compatibility.
- The SDK is more resilient when opening an SSE connection, processing events from the stream, and evaluating feature flags.

## [1.1.1] - 2021-08-06
### Changed:
- Updated the `eredis` dependency to version `1.4.0`. (Thanks, [jeffgrunewald](https://github.com/launchdarkly/erlang-server-sdk/pull/32)!)
- Updated the `jsx` dependency to version `3.1.0`. (Thanks, [bitwalker](https://github.com/launchdarkly/erlang-server-sdk/pull/35)!)
- Updated the `uuid` dependency to version `2.0.2`. (Thanks, [jeffgrunewald](https://github.com/launchdarkly/erlang-server-sdk/pull/32)!)
- Expanded test coverage to include OTP 24.

## [1.1.0] - 2021-05-06
### Added:
- `ldclient:alias/2` and `ldclient:alias/3` functions. These can be used to associate two user objects for analytics purposes with an alias event.


## [1.0.1] - 2021-02-23
### Changed:
- Removed internal storage files from generated documentation.




## [1.0.0] - 2021-02-22
Initial supported release of the SDK.

### Added:
- Added Redis feature store.
- Added LDD mode.
- Reload in-memory flag data when storage server fails.

### Changed:
- Parse flags and segments before in-memory storage.
- Updated the default base URL to `sdk.launchdarkly.com`.
- Events URL path from `api/events/bulk` to `/bulk`.
- Renamed `ldclient_settings` to `ldclient_config`.
- Renamed `ldclient:track_with_metric` to `ldclient:track_metric`.
- Improved generated documentation.

### Fixed:
- Ignore path in streaming put events.

### Removed:
- Removed `ldclient_settings`.
- Removed `ldclient:track_with_metric`.



## [1.0.0-beta4] - 2020-08-27

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
