%%-------------------------------------------------------------------
%% @doc `eld_storage_engine' module
%%
%% This is a behavior that all storage engines must implement. It works with
%% the concept of buckets and keys.
%% @end
%%-------------------------------------------------------------------

-module(eld_storage_engine).

%% Types
-type event_operation() :: put | patch | delete.
%% Operation for processing events.

-export_type([event_operation/0]).

%% `init' gets called during startup. Typically storage engine would initialize
%% here. E.g. start application, supervisor, workers, establish connections, or
%% any other initialization resources as needed.
%% It is expected that `flags' and `segments' buckets will be reachable after
%% initialization, which should also be accounted for here.
-callback init(SupRef :: atom(), Tag :: atom(), Options :: list()) ->
    ok.

%% `create' must create a named bucket with a given atom. It must return
%% `already_exists' error if the bucket by that name was previously created.
-callback create(Tag :: atom(), Bucket :: atom()) ->
    ok
    | {error, already_exists, string()}.

%% `empty' must delete all records from the specified bucket.
-callback empty(Tag :: atom(), Bucket :: atom()) ->
    ok
    | {error, bucket_not_found, string()}.

%% `get' must look up the given key in the bucket and return the result as a
%% list of matching key-value pairs as tuples. If the bucket doesn't exist it
%% must return `bucket_not_found' error.
-callback get(Tag :: atom(), Bucket :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: any()}]
    | {error, bucket_not_found, string()}.

%% `list' must return all key-value pairs for the specified bucket as tuples.
%% If the bucket doesn't exist, it must return `bucket_not_found' error.
-callback list(Tag :: atom(), Bucket :: atom()) ->
    [{Key :: binary(), Value :: any()}]
    | {error, bucket_not_found, string()}.

%% `put' must create or update key-value pair records in the given bucket.
%% If the bucket doesn't exist, it must return `bucket_not_found' error.
-callback put(Tag :: atom(), Bucket :: atom(), Item :: #{Key :: binary() => Value :: any()}) ->
    ok
    | {error, bucket_not_found, string()}.

%% `delete` removes a specific record from a specified bucket.
-callback delete(Tag :: atom(), Bucket :: atom(), Key :: binary()) ->
    ok
    | {error, bucket_not_found, string()}.

%% `terminate' is the opposite of `init'. It is expected to clean up any
%% resources and fully shut down the storage backend.
-callback terminate(Tag :: atom()) -> ok.
