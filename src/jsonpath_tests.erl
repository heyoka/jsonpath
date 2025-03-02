%% @doc Unit Tests for jsonpath.erl
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
-module(jsonpath_tests).
-include_lib("eunit/include/eunit.hrl").
-export([bench/4]).

-define(JSON, <<"{\"menu\": {
  \"id\": \"file\",
  \"value\": \"File\",
  \"popup\": {
    \"menuitem\": [
      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},
      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},
      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}
    ]
  }
}}">>).

search_single_node_test() ->
	?assertEqual(<<"file">>, jsonpath:search(<<"menu.id">>, ?JSON)).

search_single_node_undefined_test() ->
	?assertEqual(undefined, jsonpath:search(<<"menu.id.noexist">>, ?JSON)).

search_array_result_test() ->
	?assertMatch([_H|_T], jsonpath:search(<<"menu.popup.menuitem">>, ?JSON)).

search_array_index_test() ->
	?assertEqual({[{<<"value">>,<<"Open">>},
			{<<"onclick">>,<<"OpenDoc()">>}]},
		jsonpath:search(<<"menu.popup.menuitem[1]">>, ?JSON)).

update_single_node_test() ->
	?assertEqual({[{<<"menu">>,
		{[{<<"id">>,<<"file">>},
			{<<"value">>,<<"File">>},
			{<<"popup">>,
				{[{<<"menuitem">>,
					[{[{<<"value">>,<<"New">>},
						{<<"onclick">>,<<"CreateNewDoc()">>}]},
						{[{<<"value">>,<<"Open">>},{<<"onclick">>,<<"OpenDoc()">>}]},
						{[{<<"value">>,<<"Close">>},
							{<<"onclick">>,<<"CloseDoc()">>},
							{<<"ondbclick">>, <<"print()">>}
						]}]}]}}]}}]},
		jsonpath:update(<<"menu.popup.menuitem[2].ondbclick">>, <<"print()">>, ?JSON)).

update_node_test() ->
	?assertEqual({[{<<"menu">>,
		{[{<<"id">>,<<"file">>},
			{<<"value">>,<<"File">>},
			{<<"popup">>,
				{[{<<"menuitem">>,
					[{[{<<"value">>,<<"New">>},
						{<<"onclick">>,<<"CreateNewDoc()">>}]},
						{[{<<"value">>,<<"Open">>},{<<"onclick">>,<<"OpenDoc()">>}]},
						{[{<<"value">>,<<"Close">>},
							{<<"onclick">>,<<"CloseDoc()">>}
						]}]},
				{<<"menuitem2">>, {[{<<"value">>,43}]}}]}
			}]} }
		]},
		jsonpath:update(<<"menu.popup.menuitem2">>, {[{<<"value">>, 43}]}, ?JSON)).

%%update_array_index_test() ->
%%	?assertEqual(
%%		{[{<<"menu">>,
%%			{[{<<"id">>,<<"file">>},
%%				{<<"value">>,<<"File">>},
%%				{<<"popup">>,
%%					{[{<<"menuitem">>,
%%						[{[{<<"value">>,<<"New">>},
%%							{<<"onclick">>,<<"CreateNewDoc()">>}]},
%%							{[{<<"value">>,<<"foo">>},
%%								{<<"onclick">>,<<"OpenDoc()">>}]},
%%							{[{<<"value">>,<<"Close">>},
%%								{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
%%		jsonpath:update(<<"menu.popup.menuitem[1].value">>, <<"foo">>, ?JSON)).


delete_single_node_test() ->
	?assertEqual({[{<<"menu">>,
		{[{<<"id">>,<<"file">>},
			{<<"value">>,<<"File">>},
			{<<"popup">>,
				{[{<<"menuitem">>,
					[{[{<<"value">>,<<"New">>},
						{<<"onclick">>,<<"CreateNewDoc()">>}]},
						{[{<<"value">>,<<"Open">>}]},
						{[{<<"value">>,<<"Close">>},
							{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
		jsonpath:delete(<<"menu.popup.menuitem[1].onclick">>, ?JSON)).

delete_array_index_test() ->
	?assertEqual({[{<<"menu">>,
		{[{<<"id">>,<<"file">>},
			{<<"value">>,<<"File">>},
			{<<"popup">>,
				{[{<<"menuitem">>,
					[{[{<<"value">>,<<"New">>},
						{<<"onclick">>,<<"CreateNewDoc()">>}]},
						{[{<<"value">>,<<"Close">>},
							{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
		jsonpath:delete(<<"menu.popup.menuitem[1]">>, ?JSON)).

replace_single_node_test() ->
	?assertEqual(
		{[{<<"menu">>,
		   {[{<<"id">>,<<"foo">>},
			 {<<"value">>,<<"File">>},
			 {<<"popup">>,
			  {[{<<"menuitem">>,
				 [{[{<<"value">>,<<"New">>},
					{<<"onclick">>,<<"CreateNewDoc()">>}]},
				  {[{<<"value">>,<<"Open">>},{<<"onclick">>,<<"OpenDoc()">>}]},
				  {[{<<"value">>,<<"Close">>},
					{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
		jsonpath:replace(<<"menu.id">>, <<"foo">>, ?JSON)).

replace_array_index_test() ->
	?assertEqual(
		{[{<<"menu">>,
		   {[{<<"id">>,<<"file">>},
			 {<<"value">>,<<"File">>},
			 {<<"popup">>,
			  {[{<<"menuitem">>,
				 [{[{<<"value">>,<<"New">>},
					{<<"onclick">>,<<"CreateNewDoc()">>}]},
				  {[{<<"value">>,<<"foo">>},
					{<<"onclick">>,<<"OpenDoc()">>}]},
				  {[{<<"value">>,<<"Close">>},
					{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
	 	jsonpath:replace(<<"menu.popup.menuitem[1].value">>, <<"foo">>, ?JSON)).

bench(M, F, A, N) when N > 0 ->
        L = bench_loop(M, F, A, N, []),
        Length = length(L),
        Min = lists:min(L),
        Max = lists:max(L),
        Med = lists:nth(round((Length / 2)), lists:sort(L)),
        Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
        io:format("Range: ~b - ~b mics~n"
                  "Median: ~b mics~n"
                  "Average: ~b mics~n",
                  [Min, Max, Med, Avg]),
        Med.

bench_loop(_M, _F, _A, 0, List) ->
        List;
bench_loop(M, F, A, N, List) ->
        {T, _Result} = timer:tc(M, F, A),
        bench_loop(M, F, A, N - 1, [T|List]).
