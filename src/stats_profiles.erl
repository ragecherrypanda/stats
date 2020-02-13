%%%-----------------------------------------------------------------------------
%%% @doc
%%% Profiles are the "saved setup of stat configuration", basically, if the
%%% current statuses of the stats are in a state you would like to repeat either
%%% for testing or default preferences they can be saved into the metadata and
%%% gossiped to all nodes. Thus allowing a profile for one node to be mimic'ed
%%% on all nodes in the cluster.
%%%
%%% This means - unfortunately, trying to save the nodes setup (if the state of
%%% the stats status is different on all or some nodes) at once with the same
%%% name, the LWW and the nodes individual setup could be overwritten, all
%%% setups are treated as a globally registered setup that can be enabled on one
%%% or all nodes in a cluster. therefore saving a clusters setup should be done
%%% on a per node basis.
%%%
%%% save-profile <entry> ->
%%%     Saves the current stats and their status as a value to the
%%%     key: <entry>, in the metadata
%%%
%%% load-profile <entry> ->
%%%     Loads a profile that is saved in the metadata with the key <entry>,
%%%     pulls the stats and their status out of the profiles metadata value
%%%
%%% load-profile-all <entry> ->
%%%     Same as the load profile but will load that profile on all
%%%     the nodes in the cluster.
%%%
%%% delete-profile <entry> ->
%%%     Delete a profile <entry> from the metadata, does not effect the
%%%     current configuration if it is the currently loaded profile
%%%
%%% reset-profile ->
%%%     unloads the current profile and changes all the stats back to
%%%     enabled, no entry needed. Does not delete any profiles
%%%
%%% pull_profiles() ->
%%%     Pulls the list of all the profiles saved in the metadata and
%%%     their [{Stat, {status, Status}}] list
%%%
%%% NOTE :
%%%   When creating a profile, you can make changes and then overwrite the
%%%   profile with the new stat configuration. However if the profile is saved
%%%   and then loaded, upon re-write the new profile is different but still
%%%   enabled as loaded, therefore it can not be loaded again until it is
%%%   deleted and created again, or the profile is reset
%%%
%%%     for example:
%%% Node1: stat save-profile test-profile
%%% -- saves the profile and the current setup in metadata --
%%%
%%% Node2: stat load-profile test-profile
%%% -- loads the profile that is saved, is recognised as the
%%%    current profile --
%%%
%%% Node1: stat disable stat.name.**
%%% Node1: stat save-profile test-profile
%%% -- changes the setup of the stats and rewrites the current
%%%     profile saved --
%%%
%%% Node2: stat load-profile test-profile
%%%     > "profile already loaded"
%%% -- even though the setup has changed this profile is currently
%%%    loaded in the old configuration, it will not change the stats
%%%    to the new save unless it is unloaded and reloaded again --
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stats_profiles).

%% API
-export([save_profile/1, load_profile/2, delete_profile/1, reset_profile/1]).

-define(PROFILE_PREFIX,           {profiles, list}).
-define(PROFILE_KEY(Profile),     ?PROFILE_PREFIX, Profile}).

-define(LOADED_PREFIX,            {profiles, loaded}).
-define(LOADED_PKEY,              {?LOADED_PREFIX, node()}).
-define(LOADED_PKEY(Key),         {?LOADED_PREFIX, Key}).

-type profilename() :: [string()].

-export_type([profilename/0]).

%% -----------------------------------------------------------------------------
%% @doc
%% Saves the profile name in the metadata with all the current stats and their
%% status as the value; multiple saves of the same name overwrites the profile
%% @end
%% -----------------------------------------------------------------------------
-spec(save_profile(profilename()) -> {(ok | error()), profilename()}).
save_profile(ProfileName) ->
    Stats = stats_persist:get_all_stats(),
    {stats_persist:put(?PROFILE_PREFIX,ProfileName,Stats), ProfileName}.

%%%-----------------------------------------------------------------------------
%% @doc
%% Find the profile in the metadata and pull out stats to change them. It will
%% compare the current stats with the profile stats and will change the ones
%% that need changing to prevent errors/less expense
%% @end
%%%-----------------------------------------------------------------------------
-spec(load_profile(profilename(),[node()]) -> {(ok | error()), profilename()}).
load_profile(ProfileName,Nodes) ->
    ProfileKey = ?PROFILE_KEY(ProfileName),
    Response =
        case stats_persist:check_meta(ProfileKey) of
            [] -> {error, not_found};
            unregistered -> {error, deleted};
            SavedStats ->
                CurrentStats = stats_persist:get_all_stats(),
                ToChange = SavedStats -- CurrentStats,
                %% delete stats that are already enabled/disabled, any
                %% duplicates with different statuses will be replaced
                %% with the profile one
                stats_manager:change_status(ToChange),
                lists:foreach(fun(Node) ->
                    stats_persist:put(?LOADED_PREFIX, Node, ProfileName)
                              end, Nodes)
                %% the reason a profile is not checked to see if it is already
                %% loaded is because it is easier to "reload" an already loaded
                %% profile in the case the stats configuration is changed,
                %% rather than "unloading" the profile and reloading it to
                %% change many stats statuses unnecessarily

                %% consequentially, save -> load -> change -> load again
                %% would mean no stats would change if the profile is already
                %% loaded
        end,
    {Response, ProfileName}.

%%%-----------------------------------------------------------------------------
%% @doc
%% deletes the profile from the metadata and all its values but it does not
%% affect the status of the stats.
%% @end
%%%-----------------------------------------------------------------------------
-spec(delete_profile(profilename()) -> {(ok | error()), profilename()}).
delete_profile(ProfileName) ->
    Response =
        case stats_persist:check_meta(?PROFILE_KEY(ProfileName)) of
            []           -> {error, doesnt_exist};
            unregistered -> {error, deleted};
            _  ->
                OnAllNodes = [node()|nodes()],
                %% unload the profile on all nodes if it is loaded.
                lists:foreach(fun(Node) ->
                    unload_profile(ProfileName, Node)
                              end, OnAllNodes),
                %% delete the Profile from the metadata
                stats_persist:delete(?PROFILE_PREFIX, ProfileName),
                ok
        end,
    {Response, ProfileName}.

unload_profile(ProfileName, Node) ->
    case stats_persist:check_meta(?LOADED_PKEY(Node)) of
        ProfileName ->
            stats_persist:put(?LOADED_PREFIX, Node, ["none"]);
        _ ->  %% profile is not loaded on that node.
            ok %% do nothing
    end.

%%%-----------------------------------------------------------------------------
%% @doc resets the profile and enable all the disabled stats. @end
%%%-----------------------------------------------------------------------------
-spec(reset_profile([node()]) -> {(ok | error()), profilename()}).
reset_profile(Nodes) ->
    %% get list of profiles unloaded and the nodes.
    Profiles =
        lists:foldl(fun(Node, ProfileAcc) ->
                        case stats_persist:check_meta(?LOADED_PKEY(Node)) of
                            ["none"]     -> ProfileAcc;
                            []           -> ProfileAcc;
                            unregistered -> ProfileAcc;
                            Profile ->
                                stats_persist:put(?LOADED_PREFIX,Node,["none"]),
                                [{Profile,Node} | ProfileAcc]
                        end
                    end, [], Nodes),

    %% get all the stats that are disabled and re-enable them
    CurrentStats = stats_persist:get_all_stats(),
    stats_manager:change_status(
        [{Stat,enabled} || {Stat,Status} <- CurrentStats, Status =/= enabled]),
    {ok, Profiles}.

