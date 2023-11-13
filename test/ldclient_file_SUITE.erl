-module(ldclient_file_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    check_file_json/1,
    check_file_no_segments_json/1,
    check_file_no_flags_json/1,
    check_file_simple_flags_json/1,
    check_file_simple_flags_yaml/1,
    check_file_all_properties_yaml/1,
    check_multiple_data_files/1,
    check_binary_file_path/1,
    check_file_watcher/1,
    check_with_missing_file/1,
    check_with_only_missing_file/1,
    check_with_duplicate_flags/1,
    check_with_duplicate_flags_allowed/1,
    load_non_modified_file/1
]).

all() ->
    [
        check_file_json,
        check_file_no_segments_json,
        check_file_no_flags_json,
        check_file_simple_flags_json,
        check_file_simple_flags_yaml,
        check_file_all_properties_yaml,
        check_multiple_data_files,
        check_binary_file_path,
        check_file_watcher,
        check_with_missing_file,
        check_with_only_missing_file,
        check_with_duplicate_flags,
        check_with_duplicate_flags_allowed,
        load_non_modified_file
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),

    DataFileFlagsAndSegments = code:priv_dir(ldclient) ++ "/flags-from-file.json",
    OptionsFlagsAndSegments = #{
        file_datasource => true, %% use legacy file_datasource boolean flag
        send_events => false,
        file_paths => [DataFileFlagsAndSegments],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", OptionsFlagsAndSegments),

    DataFileNoSegments = code:priv_dir(ldclient) ++ "/flags-from-file-no-segments.json",
    OptionsNoSegments = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileNoSegments],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", no_segments, OptionsNoSegments),

    DataFileNoFlags = code:priv_dir(ldclient) ++ "/flags-from-file-no-flags.json",
    OptionsNoFlags = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileNoFlags],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", no_flags, OptionsNoFlags),

    DataFileSimpleFlags = code:priv_dir(ldclient) ++ "/flags-from-file-simple.json",
    OptionsSimpleFlags = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileSimpleFlags],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", simple_flags, OptionsSimpleFlags),

    DataFileSimpleFlagsYaml = code:priv_dir(ldclient) ++ "/flags-from-file-simple.yaml",
    OptionsSimpleFlagsYaml = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", simple_flags_yaml, OptionsSimpleFlagsYaml),

    OptionsBinaryPathYaml = #{
        datasource => file,
        send_events => false,
        file_paths => [list_to_binary(DataFileSimpleFlagsYaml)],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", binary_path_yaml, OptionsBinaryPathYaml),

    DataFileAllPropertiesYaml = code:priv_dir(ldclient) ++ "/flags-all-properties.yaml",
    OptionsAllPropertiesYaml = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileAllPropertiesYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", all_properties_yaml, OptionsAllPropertiesYaml),

    OptionsMultipleDataFiles = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileFlagsAndSegments, DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", multiple_data_files, OptionsMultipleDataFiles),

    _ = file:delete("tmpfile.json"),
    _ = file:write_file("tmpfile.json", ["{\"flagValues\": {\"test-flag\": true}}"]),

    OptionsWatchFiles = #{
        datasource => file,
        send_events => false,
        file_paths => ["tmpfile.json"],
        feature_store => ldclient_storage_map,
        file_auto_update => true,
        file_poll_interval => 50
    },
    ldclient:start_instance("", watch_files, OptionsWatchFiles),

    OptionsMissingAndValidFile = #{
        datasource => file,
        send_events => false,
        file_paths => ["thisisnotafile.json", DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", missing_and_valid_files, OptionsMissingAndValidFile),

    OptionsOnlyMissingFile = #{
        datasource => file,
        send_events => false,
        file_paths => ["thisisnotafile.json", DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", missing_file, OptionsOnlyMissingFile),

    OptionsFilesWithDuplicateFlags = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileSimpleFlags, DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map
    },
    ldclient:start_instance("", duplicate_flags, OptionsFilesWithDuplicateFlags),

    OptionsFilesWithDuplicateFlagsAllowed = #{
        datasource => file,
        send_events => false,
        file_paths => [DataFileSimpleFlags, DataFileSimpleFlagsYaml],
        feature_store => ldclient_storage_map,
        file_allow_duplicate_keys => true
    },
    ldclient:start_instance("", duplicate_flags_allowed, OptionsFilesWithDuplicateFlagsAllowed),

    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%====================================================================
%% Tests
%%====================================================================

check_file_json(_) ->
    {{1, false, off}, _} = ldclient_eval:flag_key_for_context(default, <<"keep-it-off">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    %% This rule wouldn't match if the segment could not be read.
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, _} =
        ldclient_eval:flag_key_for_context(default, <<"segment-me">>, #{key => <<"context-12345">>, kind => <<"user">>}, "foo").

check_file_no_segments_json(_) ->
    {{0, true, fallthrough}, _Events} = ldclient_eval:flag_key_for_context(no_segments, <<"keep-it-on">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    %% The segment isn't there, so the user isn't included.
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, _} =
        ldclient_eval:flag_key_for_context(no_segments, <<"segment-me">>, #{key => <<"context-12345">>, kind => <<"user">>}, "foo").

check_file_no_flags_json(_) ->
    {{null, "foo", {error, flag_not_found}}, _} = ldclient_eval:flag_key_for_context(no_flags, <<"keep-it-on">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_file_simple_flags_json(_) ->
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags, <<"my-boolean-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, 3, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags, <<"my-integer-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_file_simple_flags_yaml(_) ->
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags_yaml, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags_yaml, <<"my-boolean-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, 3, fallthrough}, _} = ldclient_eval:flag_key_for_context(simple_flags_yaml, <<"my-integer-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_file_all_properties_yaml(_) ->
    {{2, <<"on">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"flag1">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-boolean-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, 3, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-integer-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_multiple_data_files(_) ->
    {{1, false, off}, _} = ldclient_eval:flag_key_for_context(default, <<"keep-it-off">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-boolean-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, 3, fallthrough}, _} = ldclient_eval:flag_key_for_context(all_properties_yaml, <<"my-integer-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_binary_file_path(_) ->
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(binary_path_yaml, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(binary_path_yaml, <<"my-boolean-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    {{0, 3, fallthrough}, _} = ldclient_eval:flag_key_for_context(binary_path_yaml, <<"my-integer-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_file_watcher(_) ->
    {{0, true, fallthrough}, _} = ldclient_eval:flag_key_for_context(watch_files, <<"test-flag">>, #{key => <<"user123">>, kind => <<"user">>}, "foo"),
    %The timestamp for the modified time is in seconds. So we wait a moment to get a new timestamp compared to creation.
    timer:sleep(1200),
    file:write_file("tmpfile.json", ["{\"flagValues\": {\"test-flag\": false}}"]),
    timer:sleep(1000),
    {{0, false, fallthrough}, _} = ldclient_eval:flag_key_for_context(watch_files, <<"test-flag">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_with_missing_file(_) ->
    %% If a file is missing, then we do not want to use data from any file.
    {{null, "foo", {error, client_not_ready}}, _} = ldclient_eval:flag_key_for_context(missing_and_valid_files, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").
check_with_only_missing_file(_) ->
    {{null, "foo", {error, client_not_ready}}, _} = ldclient_eval:flag_key_for_context(missing_file, <<"keep-it-on">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_with_duplicate_flags(_) ->
    % With duplicate flags, and not allowing duplicate flags, the store will not be initialized.
    {{null, "foo", {error, client_not_ready}}, _} = ldclient_eval:flag_key_for_context(duplicate_flags, <<"keep-it-on">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

check_with_duplicate_flags_allowed(_) ->
    % With duplicate flags, and allowing duplicate flags, the store should work.
    {{0, <<"value-1">>, fallthrough}, _} = ldclient_eval:flag_key_for_context(duplicate_flags_allowed, <<"my-string-flag-key">>, #{key => <<"user123">>, kind => <<"user">>}, "foo").

load_non_modified_file(_) ->
    {ok, State} = ldclient_update_file_server:init([default]),
    {ok, NewState} = ldclient_update_file_server:load_files_if_modified([code:priv_dir(ldclient) ++ "/flags-from-file.json"], State),
    {ok, NewState} = ldclient_update_file_server:load_files_if_modified([code:priv_dir(ldclient) ++ "/flags-from-file.json"], NewState).
