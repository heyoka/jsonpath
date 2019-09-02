%%
%% jsonpath - json data retrieval and updates via
%%            javascript-like notation
%%
%% Copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc Fast javascript-like "path" notation for querying and updating JSON
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
-module(jsonpath).
-export([search/2, replace/3, delete/2, update/3]).
-export([parse_path/1]).

-include("jsonpath.hrl").

search(Path, Data) when is_binary(Data) ->
	search(Path, jiffy:decode(Data));
search(Path, Data) ->
	search_data(parse_path(Path), Data).

replace(Path, Replace, Data) when is_binary(Data) ->
	replace(Path, Replace, jiffy:decode(Data));
replace(Path, Replace, Data) ->
	replace_data(parse_path(Path), Replace, Data).

replace_data([SearchHead|SearchTail], Replace, Structure) ->
	case Structure of 
		{TupleList} ->
%%			?DEBUG("tuple list: ~p", [TupleList]),
			{ replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) };
		List ->
%%			?DEBUG("looks like a list: ~p", [List]),
			replace_list([SearchHead|SearchTail], Replace, List)
	end.

replace_list([SearchHead|SearchTail], Replace, List) ->
	try 
		Index = list_to_integer(binary_to_list(SearchHead)) + 1,
		case (Index > length(List)) of
			true ->
%%				?DEBUG("Index out of range ~p for list size ~p", [Index, length(List)]),
				undefined;
			false ->
				replace_list([Index|SearchTail], Replace, List, 1, [])
		end
	catch
		_:_ ->
%%			?DEBUG("This is not an integer: ~p", [SearchHead]),
			undefined
	end.
replace_list([_SearchHead|_SearchTail], _Replace, [], _Count, Accum) ->
	%?DEBUG("at the end of this list with accum: ~p", [Accum]),
	lists:reverse(Accum);
replace_list([SearchHead|SearchTail], Replace, [Head|Tail], Count, Accum) ->
	%?DEBUG("list: ~p", [Head|Tail]),
	Data = case SearchHead of 
		Count ->
%%			?DEBUG("Found index ~p", [Count]),
			case SearchTail of
				[] ->
					Replace;
				_SearchTail ->
					%?DEBUG("Not last, so no replacement, but replaceing into: ~p", [Head]),
					replace_data(SearchTail, Replace, Head)
			end;
		_SearchHead ->
			%?DEBUG("Not index ~p", [Count]),
			Head
	end,
	replace_list([SearchHead|SearchTail], Replace, Tail, Count+1, [Data|Accum]).


replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) ->
	replace_tuple_list([SearchHead|SearchTail], Replace, TupleList, []).
replace_tuple_list([_SearchHead|_SearchTail], _Replace, [], Accum) ->
%%	?DEBUG("at the end of this tuple list with accum: ~p", [Accum]),
	lists:reverse(Accum);
replace_tuple_list([SearchHead|SearchTail], Replace, [Head|Tail], Accum) ->
	%?DEBUG("tuple: ~p", [Head]),
	Data = case Head of
		{SearchHead, Value} ->
%%			?DEBUG("Found match for ~p: ~p", [SearchHead, {SearchHead, Value}]),
			case SearchTail of 
				[] ->
					{SearchHead, Replace};
				_SearchTail ->
					%?DEBUG("Not last, so no replacement, but replaceing into : ~p",[Head]),
					{SearchHead, replace_data(SearchTail, Replace, Value) }
			end;
		_Other ->
			%?DEBUG("No match for ~p: ~p", [SearchHead, Other]),
			Head
	end,
%%	?DEBUG("continue processing tail: ~p", [Tail]),
	replace_tuple_list([SearchHead|SearchTail], Replace, Tail, [Data|Accum]).

search_data([], Data) ->
	Data;
search_data([Head|Tail], Data) ->
	%?DEBUG("Searching for ~p in ~p", [Head,Data]),
	case Head of
		<<>> ->
			search_data(Tail, Data);
		_Other ->
			case Data of 
				{_Tuple} ->
					%?DEBUG("found tuple: ~p", [Tuple]),
					search_tuple([Head|Tail], Data);
				_List ->
					%?DEBUG("found list: ~p", [List]),
					search_list([Head|Tail], Data)
			end
	end.

search_list([Head|Tail], List) ->
	%?DEBUG("list search for ~p in ~p",[Head, List]),
	try 
		Index = list_to_integer(binary_to_list(Head)) + 1,
		case (Index > length(List)) of
			true ->
				undefined;
			false ->
				search_data(Tail, lists:nth(Index, List))
		end
	catch
		_:_ -> 
			%?DEBUG("that wasn't an integer",[]),
			undefined
	end.

search_tuple([Head|Tail], Tuple) ->
	{TuplePayload} = Tuple,
	case lists:keyfind(Head, 1, TuplePayload) of
		false ->
			%?DEBUG("did not find tuple value for ~p. done.", [Head]),
			undefined;
		{Head,Value} ->
			%?DEBUG("found tuple value for ~p: ~p", [Head,Value]),
			search_data(Tail, Value)
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Setting data.
update(Path, Replace, Data) when is_binary(Data) ->
	update(Path, Replace, jiffy:decode(Data));
update(Path, Replace, Data) when is_list(Path) ->
	update_data(Path, Replace, Data);
update(Path, Replace, Data) when is_binary(Path) ->
	update_data(parse_path(Path), Replace, Data).

update_data([SearchHead|SearchTail], Replace, Structure) ->
	case Structure of
		{TupleList} ->
			{ update_tuple_list([SearchHead|SearchTail], Replace, TupleList) };
		List ->
			update_list([SearchHead|SearchTail], Replace, List)
	end.

update_list([SearchHead|SearchTail], Replace, List) ->
	try
		Index = list_to_integer(binary_to_list(SearchHead)) + 1,
		case (Index > length(List)) of
			true ->
				undefined;
			false ->
				update_list([Index|SearchTail], Replace, List, 1, [])
		end
	catch
		_:_ ->
			undefined
	end.
update_list([_SearchHead|_SearchTail], _Replace, [], _Count, Accum) ->
	lists:reverse(Accum);
update_list([SearchHead|SearchTail], Replace, [Head|Tail], Count, Accum) ->
	Data = case SearchHead of
				 Count ->
					 case SearchTail of
						 [] ->
							 Replace;
						 _SearchTail ->
							 update_data(SearchTail, Replace, Head)
					 end;
				 _SearchHead ->
					 Head
			 end,
	update_list([SearchHead|SearchTail], Replace, Tail, Count+1, [Data|Accum]).


update_tuple_list([SearchHead|SearchTail], Replace, TupleList) ->
	update_tuple_list([SearchHead|SearchTail], Replace, TupleList, []).

update_tuple_list([SearchHead|[]], Replacement, [], Accum) ->
	%% Here we are on the final search element and have reached the end of the json structure,
	%% so we need to "create" a new tuple proplist pair for our Replacement value.
	lists:reverse([{SearchHead,Replacement}|Accum]);
update_tuple_list([_SearchHead|_SearchTail], _Replace, [], Accum) ->
	%% In this case we have reached the end of the json structure without
	%% finding anything, we need to now create structure so we can add our
	%% thing...
	lists:reverse(Accum);
update_tuple_list([SearchHead|SearchTail], Replace, [Head|Tail], Accum) ->
	Data = case Head of
				 {SearchHead, Value} ->
					 case SearchTail of
						 [] ->
							 {SearchHead, Replace};
						 _SearchTail ->
							 {SearchHead, update_data(SearchTail, Replace, Value) }
					 end;
				 _Other ->
					 Head
			 end,
	update_tuple_list([SearchHead|SearchTail], Replace, Tail, [Data|Accum]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Deleting data.
delete(Path, Data) when is_binary(Data) ->
	delete(Path, jiffy:decode(Data));
delete(Path, Data) when is_list(Path) ->
	delete_data(Path, Data);
delete(Path, Data) when is_binary(Path) ->
	delete_data(parse_path(Path), Data).

delete_data([SearchHead|SearchTail], Structure) ->
	case Structure of
		{TupleList} ->
			{ delete_tuple_list([SearchHead|SearchTail], TupleList) };
		List ->
			delete_list([SearchHead|SearchTail], List)
	end.

delete_list([SearchHead|SearchTail], List) ->
	Index = list_to_integer(binary_to_list(SearchHead)) + 1,
	case (Index > length(List)) of
		true ->
			throw(index_too_big);
		false ->
			delete_list([Index|SearchTail], List, 1, [])
	end.

delete_list([_SearchHead|_SearchTail], [], _Count, Accum) ->
	lists:reverse(Accum);
delete_list([SearchHead|SearchTail], [Head|Tail], Count, Accum) ->
	Data = case SearchHead of
				 Count ->
					 case SearchTail of
						 [] ->
							 skip;
						 _SearchTail ->
							 {skip, delete_data(SearchTail, Head)}
					 end;
				 _SearchHead ->
					 {continue, Head}
			 end,
	case Data of
		skip ->
			delete_list([SearchHead|SearchTail], Tail, Count+1, Accum);
		{skip, More} ->
			delete_list([SearchHead|SearchTail], Tail, Count+1, [More|Accum]);
		{continue, Keep} ->
			delete_list([SearchHead|SearchTail], Tail, Count+1, [Keep|Accum])
	end.


delete_tuple_list([SearchHead|SearchTail], TupleList) ->
	delete_tuple_list([SearchHead|SearchTail], TupleList, []).


%% Done when we are at the end of search and structure.
delete_tuple_list([], [], Accum) ->
	lists:reverse(Accum);

%% Error if we have more search but no more structure.
delete_tuple_list(_Any, [], _Accum) ->
	throw(search_list_too_long);

%% Gobble up rest of the list
delete_tuple_list([], [Head|Tail], Accum) ->
	delete_tuple_list([], Tail, [Head|Accum]);

%%    lists:reverse(Accum);
%%
%% Actually delete only when we are on the leaf, otherwise we are
%% just traversing...
%%
delete_tuple_list([SearchHead|SearchTail], [Head|Tail], Accum) ->
	%% io:format("match: ~w, ~w~n", [SearchHead, Head]),
	case Head of
		{SearchHead, Value} ->
			case SearchTail of
				[] ->
					delete_tuple_list([], Tail, Accum);
				_Any ->
					delete_tuple_list([], Tail, [{SearchHead, delete_data(SearchTail, Value)} | Accum])
			end;
		_NonMatch ->
			%% No? Try again with the rest of the struct.
			delete_tuple_list([SearchHead|SearchTail], Tail, [Head|Accum])
	end.













parse_path(Path) ->
	Split = binary:split(Path, [<<".">>,<<"[">>,<<"]">>], [global]),
	lists:filter(fun(X) -> X =/= <<>> end, Split).

