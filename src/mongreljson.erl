% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2012 CA Meijer
%% @doc This module provides functions for converting between the MongoDB import/export
%%      JSON format used by MongoDB and the tuple encapsulation of a document required
%%      by the official Erlang MongoDB driver.
%%      
-module(mongreljson).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([tuple_to_json/1,
		 json_to_tuple/1]).

%%
%% API Functions
%%

%% @doc Converts a JSON document to a tuple for the MongoDB Erlang driver.
%%
%% @spec json_to_tuple(string()) -> tuple()
-spec(json_to_tuple(string()) -> tuple()).

json_to_tuple(JsonData) ->
	{struct, KeyValueList} = mochijson2:decode(JsonData),
	erlang:list_to_tuple(parse(KeyValueList)).

%% @doc Converts a MongoDB Erlang driver tuple encapsulating a MongoDB
%%      document to a JSON document.
%%
%% @spec tuple_to_json(tuple()) -> string()
-spec(tuple_to_json(tuple()) -> string()).

tuple_to_json(DriverData) ->
	DriverDataList = erlang:tuple_to_list(DriverData),
	"{" ++ parse_driver_data(DriverDataList) ++ "}".



%%
%% Local Functions
%%
parse([]) ->
	[];
parse([{Key, Value} | Tail]) ->
	[erlang:binary_to_atom(Key, unicode), construct_value(Value)] ++ parse(Tail).

construct_value({struct, [{<<"$oid">>, ObjectId}]}) ->
	Oid = erlang:list_to_integer(erlang:binary_to_list(ObjectId), 16),
	{ pad_oid(binary:encode_unsigned(Oid)) };
construct_value({struct, [{<<"$date">>, MilliSecondsInEpoch}]}) ->
	MegaSeconds = MilliSecondsInEpoch div 1000000000,
	Seconds = (MilliSecondsInEpoch - MegaSeconds * 1000000000) div 1000,
	MicroSeconds = (MilliSecondsInEpoch rem 1000) * 1000,
	{MegaSeconds, Seconds, MicroSeconds};
construct_value({struct, [{<<"$binary">>, Base64Value}, {<<"$type">>, <<"00">>}]}) ->
	{bin, bin, base64:decode(Base64Value)};
construct_value({struct, [{<<"$binary">>, Base64Value}, {<<"$type">>, <<"01">>}]}) ->
	{bin, function, base64:decode(Base64Value)};
construct_value({struct, [{<<"$binary">>, Base64Value}, {<<"$type">>, <<"03">>}]}) ->
	{bin, uuid, base64:decode(Base64Value)};
construct_value({struct, [{<<"$binary">>, Base64Value}, {<<"$type">>, <<"05">>}]}) ->
	{bin, md5, base64:decode(Base64Value)};
construct_value({struct, [{<<"$binary">>, Base64Value}, {<<"$type">>, <<"ffffff80">>}]}) ->
	{bin, userdefined, base64:decode(Base64Value)};
construct_value({struct, [{<<"$minKey">>, 1}]}) ->
	'MIN_KEY';
construct_value({struct, [{<<"$maxKey">>, 1}]}) ->
	'MAX_KEY';
construct_value({struct, [{<<"t">>, Time}, {<<"i">>, Inc}]}) ->
	{mongostamp, Inc, Time div 1000};
construct_value({struct, [{<<"$regex">>, Regex}, {<<"$options">>, Options}]}) ->
	{regex, Regex, Options};
construct_value({struct, KeyValueList}) ->
	erlang:list_to_tuple(parse(KeyValueList));
construct_value([Head | Tail]) ->
	[construct_value(Head)] ++ construct_value(Tail);
construct_value(Value) ->
	Value.

parse_driver_data([]) ->
	[];
parse_driver_data([Key, Value]) ->
	"\"" ++ erlang:atom_to_list(Key) ++ "\" : " ++ construct_json_value(Value);
parse_driver_data([Key, Value | Tail]) ->
	"\"" ++ erlang:atom_to_list(Key) ++ "\" : " ++ construct_json_value(Value) ++ ", " ++ parse_driver_data(Tail).

construct_json_value({BinaryId}) when is_binary(BinaryId) ->
	HexId = erlang:integer_to_list(binary:decode_unsigned(BinaryId), 16),
	"{\"$oid\" : \"" ++ to_lower(pad(HexId), []) ++ "\"}";
construct_json_value({mongostamp, Increment, Unixtime}) ->
	"{\"t\" : " ++ erlang:integer_to_list(Unixtime * 1000) ++ ", \"i\" : " ++ 
		erlang:integer_to_list(Increment) ++ "}";
construct_json_value(Value) when is_tuple(Value) andalso size(Value) rem 2 =:= 0 ->
	DriverDataList = erlang:tuple_to_list(Value),
	"{" ++ parse_driver_data(DriverDataList) ++ "}";
construct_json_value(Value) when is_binary(Value) ->
	"\"" ++ erlang:binary_to_list(Value) ++ "\"";
construct_json_value(null) ->
	"null";
construct_json_value(true) ->
	"true";
construct_json_value(false) ->
	"false";
construct_json_value('MIN_KEY') ->
	"{\"$minKey\" : 1}";
construct_json_value('MAX_KEY') ->
	"{\"$maxKey\" : 1}";
construct_json_value({bin, bin, BinaryValue}) ->
	"{\"$binary\" : \"" ++ erlang:binary_to_list(base64:encode(BinaryValue)) ++ "\", \"$type\" : \"00\"}";
construct_json_value({bin, function, BinaryValue}) ->
	"{\"$binary\" : \"" ++ erlang:binary_to_list(base64:encode(BinaryValue)) ++ "\", \"$type\" : \"01\"}";
construct_json_value({bin, uuid, BinaryValue}) ->
	"{\"$binary\" : \"" ++ erlang:binary_to_list(base64:encode(BinaryValue)) ++ "\", \"$type\" : \"03\"}";
construct_json_value({bin, md5, BinaryValue}) ->
	"{\"$binary\" : \"" ++ erlang:binary_to_list(base64:encode(BinaryValue)) ++ "\", \"$type\" : \"05\"}";
construct_json_value({bin, userdefined, BinaryValue}) ->
	"{\"$binary\" : \"" ++ erlang:binary_to_list(base64:encode(BinaryValue)) ++ "\", \"$type\" : \"ffffff80\"}";
construct_json_value({MegaSecs, Secs, Microsecs}) when is_integer(MegaSecs) ->
	"{\"$date\" : " ++ erlang:integer_to_list(MegaSecs * 1000000000 + Secs * 1000 + (Microsecs div 1000)) ++ "}";
construct_json_value({regex, Regex, Options}) ->
	"{\"$regex\" : \"" ++ erlang:binary_to_list(Regex) ++ "\", \"$options\" : \"" ++ 
		erlang:binary_to_list(Options) ++ "\"}";
construct_json_value(Value) when is_integer(Value) ->
	erlang:integer_to_list(Value);
construct_json_value(Value) when is_float(Value) ->
	erlang:float_to_list(Value);
construct_json_value(Value) when is_list(Value) ->
	"[" ++ parse_list(Value) ++ "]".

pad_oid(Oid) when size(Oid) =:= 12 ->
	Oid;
pad_oid(Oid) when size(Oid) < 12 ->
	pad_oid(<<0, Oid/binary>>).

pad(Value) when length(Value) < 24 ->
	pad("0" ++ Value);
pad(Value) when length(Value) =:= 24 ->
	Value.

to_lower([], Result) ->
	lists:reverse(Result);
to_lower([H|T], Result) when H >= $A andalso H =< $F ->
	to_lower(T, [H+32|Result]);
to_lower([H|T], Result) ->
	to_lower(T, [H|Result]).

parse_list([]) ->
	[];
parse_list([H]) ->
	construct_json_value(H);
parse_list([H|T]) ->
	construct_json_value(H) ++ "," ++ parse_list(T).
