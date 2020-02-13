
-type listofstats()     :: [metricname()] | [atom()] | any().

-type datapoints()      :: [datapoint()] | [].
-type datapoint()       :: mean | max | min | mean | mode | 99 | 95 | 90
                         | 100 | 75 | 50 | value | default | other().

-type consolearg()      :: [string()] | [atom()] | list() | [].
-type profilename()     :: list() | [string()].
-type profile_prefix()  :: {atom(), atom()}.


-type options()         :: [exometer:options()|[statusopts()|[cacheopts()]]]
                            | [] | list() | any().
-type aliases()         :: exometer_alias:alias().

-type statusopts()      :: [{status,status()}].
-type status()          :: enabled | disabled | unregistered | '_'.

-type cacheopts()       :: [{cache,cache()}].
-type cache()           :: non_neg_integer().

% function specific
-type pattern()         :: ets:match_spec().


%%% Return arguments



-type print()           :: list() | string() | [] | ok.
-type error()           :: {error, reason()} | error.
-type reason()          :: generalerror() | exometererror() | profileerror() | metaerror().
-type exometererror()   :: no_template | exists | not_found.
-type profileerror()    :: profile_exists_already | no_stats | no_data | no_profile.
-type metaerror()       :: unregistered | no_stat | no_status.
-type generalerror()    :: badarg | econnrefused | other().
-type other()           :: any().
-type acc()             :: any().

-type timestamp()       :: erlang:timestamp().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Stat Macros %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



