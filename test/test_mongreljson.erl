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
%% @doc This module has unit tests for the mongrel module.
%%      
-module(test_mongreljson).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
simple_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efc60658afa960953000001\" }, \"simple1\" : 1, \"simple2\" : \"hello\" }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id', _, simple1, 1, simple2, <<"hello">>} = DriverData.

nested_document_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efc62b18afa960953000002\" }, \"simple1\" : 1, \"complex1\" : { \"simple1\" : 1, \"simple2\" : 2 } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id', _, simple1, 1, complex1, {simple1, 1, simple2, 2}} = DriverData.
	
oid_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efa9a0eb5f04b86f9f132bd\" }}",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id', {<<78,250,154,14,181,240,75,134,249,241,50,189>>}} = DriverData.
	
oid_to_driver_format2_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"00000000000000000000000d\" }}",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id', {<<0,0,0,0,0,0,0,0,0,0,0,13>>}} = DriverData.
	
date_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efa9a0eb5f04b86f9f132bd\" }, \"my_date\" : { \"$date\" : 1325046271870 } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id', _, my_date, {1325,46271,870000}} = DriverData.

null_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efc6b0d848a5a3bf487407e\" }, \"x\" : null }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_, x, null} = DriverData.

boolean_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efc6e2691203aa570371b80\" }, \"b1\" : true, \"b2\" : false }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,b1,true,b2,false} = DriverData.

int32_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efd4652e443c4aadf65b9d0\" }, \"int32a\" : 2147483647, \"int32b\" : -2147483648 }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,int32a,2147483647,int32b,-2147483648} = DriverData.

int64_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efd4652e443c4aadf65b9d0\" }, \"int64a\" : 562949953421312, \"int64b\" : -562949953421312 }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,int64a,562949953421312,int64b,-562949953421312} = DriverData.

float_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4efd667f80a2b347be0d40c8\" }, \"pi\" : 3.141592653589793, \"big_value\" : 1.2345678901e+123 }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,pi,3.141592653589793,big_value,1.2345678901e123} = DriverData.
	
array_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4eff06f1322bacf439751b99\" }, \"my_arr\" : [ \"element1\", 2.3, { \"foo\" : \"bar\" } ] }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,my_arr,[<<"element1">>,2.3,{foo,<<"bar">>}]} = DriverData.

utf8_string_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4eff1d7c073b62e34a65b526\" }, \"s1\" : \"1\", \"s2\" : \"ሰማይ አይታረስ ንጉሥ አይከሰስ።\", \"s3\" : \"three\" }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<78,255,29,124,7,59,98,227,74,101,181,38>>},
        s1,<<"1">>,s2,
        <<225,136,176,225,136,155,225,139,173,32,225,138,160,225,139,173,225,
          137,179,225,136,168,225,136,181,32,225,138,149,225,140,137,225,136,
          165,32,225,138,160,225,139,173,225,138,168,225,136,176,225,136,181,
          225,141,162>>,
        s3,<<"three">>} = DriverData.

bin_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4eff3a5b8afa960850000001\" }, \"bin_data1\" : { \"$binary\" : \"AQL/\", \"$type\" : \"00\" }, \"bin_data2\" : { \"$binary\" : \"AAAAAAAAAAAAAAAAAQ==\", \"$type\" : \"00\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<78,255,58,91,138,250,150,8,80,0,0,1>>},
        bin_data1,
        {bin,bin,<<1,2,255>>},
        bin_data2,
        {bin,bin,<<0,0,0,0,0,0,0,0,0,0,0,0,1>>}} = DriverData.

bin_func_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4effffae8afa960b81000001\" }, \"bin_func\" : { \"$binary\" : \"ZnVuY3Rpb24gZih4KXtyZXR1cm4geDt9\", \"$type\" : \"01\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<78,255,255,174,138,250,150,11,129,0,0,1>>},
        bin_func,
        {bin,function,<<"function f(x){return x;}">>}} = DriverData.
	
uuid_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f00026a8afa960b81000002\" }, \"uuid\" : { \"$binary\" : \"AAF/gP7/\", \"$type\" : \"03\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,0,2,106,138,250,150,11,129,0,0,2>>},
        uuid,
        {bin,uuid,<<0,1,127,128,254,255>>}} = DriverData.
	
md5_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f0003c38afa960b81000003\" }, \"hash\" : { \"$binary\" : \"AAECAwQFBgcICQoLDA0ODw==\", \"$type\" : \"05\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,0,3,195,138,250,150,11,129,0,0,3>>},
        hash,
        {bin,md5,<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>}} = DriverData.

user_defined_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f0005178afa960b81000004\" }, \"user_data\" : { \"$binary\" : \"AAECAwQFBgcICQoLDA0ODw==\", \"$type\" : \"ffffff80\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,0,5,23,138,250,150,11,129,0,0,4>>},
        user_data,
        {bin,userdefined,<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>}} = DriverData.

min_max_keys_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f0053668afa96089f000007\" }, \"min\" : { \"$minKey\" : 1 }, \"max\" : { \"$maxKey\" : 1 } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,0,83,102,138,250,150,8,159,0,0,7>>},
        min,'MIN_KEY',max,'MAX_KEY'} = DriverData.

timestamp_to_driver_format_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"000000000000000000000001\" }, \"ts\" : { \"t\" : 1000000000000 , \"i\" : 1234567890 } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',_,ts,{mongostamp,1234567890,1000000000}} = DriverData.
	
regex_to_tuple_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f0821aa8afa961c5d000004\" }, \"a_regex\" : { \"$regex\" : \"^\\d{5}$\", \"$options\" : \"\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,8,33,170,138,250,150,28,93,0,0,4>>}, a_regex, {regex,<<"^\\d{5}$">>,<<>>}} = DriverData.

regex_with_option_to_tuple_test() ->
	JsonData = "{ \"_id\" : { \"$oid\" : \"4f0821aa8afa961c5d000004\" }, \"a_regex\" : { \"$regex\" : \"best|worst\", \"$options\" : \"i\" } }",
	DriverData = mongreljson:json_to_tuple(JsonData),
	{'_id',{<<79,8,33,170,138,250,150,28,93,0,0,4>>}, a_regex, {regex,<<"best|worst">>,<<"i">>}} = DriverData.

%% The tests below rely on the JSON to driver function working correctly to test the drver to JSON function.
oid_to_json_test() ->
	DriverData = {'_id', {<<78,250,154,14,181,240,75,134,249,241,50,189>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).
	
oid_to_json_check_encoding_test() ->
	DriverData = {'_id', {<<0,0,0,0,0,0,0,0,0,0,0,10>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	{struct, [{<<"_id">>, Oid1}]} = mochijson2:decode(JsonData),
	{struct, [{<<"$oid">>, Oid2}]} = Oid1,
	<<"00000000000000000000000a">> = Oid2.

simple_to_json_test() ->
	DriverData = {simple1, 1, simple2, <<"hello">>},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).
	
nested_document_to_json_test() ->
	DriverData = {simple1, 1, complex1, {simple1, 1, simple2, 2}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

null_to_json_test() ->
	DriverData = {x, null},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

boolean_to_json_test() ->
	DriverData = {b1,true,b2,false},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

float_to_json_test() ->
	DriverData = {pi,3.1415926,big_value,1.23e123},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

utf8_string_to_json_test() ->
	DriverData = {'_id',{<<78,255,29,124,7,59,98,227,74,101,181,38>>},
        s1,<<"1">>,s2,
        <<225,136,176,225,136,155,225,139,173,32,225,138,160,225,139,173,225,
          137,179,225,136,168,225,136,181,32,225,138,149,225,140,137,225,136,
          165,32,225,138,160,225,139,173,225,138,168,225,136,176,225,136,181,
          225,141,162>>,
        s3,<<"three">>},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

date_to_json_test() ->
	DriverData = {my_date, {1325,46271,870000}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

min_max_keys_to_json_test() ->
	DriverData = {'_id',{<<79,0,83,102,138,250,150,8,159,0,0,7>>},
        min,'MIN_KEY',max,'MAX_KEY'},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

bin_to_json_test() ->
	DriverData = {'_id',{<<78,255,58,91,138,250,150,8,80,0,0,1>>},
        bin_data1,
        {bin,bin,<<1,2,255>>},
        bin_data2,
        {bin,bin,<<0,0,0,0,0,0,0,0,0,0,0,0,1>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

bin_func_to_json_test() ->
	DriverData = {'_id',{<<78,255,255,174,138,250,150,11,129,0,0,1>>},
        bin_func,
        {bin,function,<<"function f(x){return x;}">>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

uuid_to_json_test() ->
	DriverData = {'_id',{<<79,0,2,106,138,250,150,11,129,0,0,2>>},
        uuid,
        {bin,uuid,<<0,1,127,128,254,255>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

md5_to_json_test() ->
	DriverData = {'_id',{<<79,0,3,195,138,250,150,11,129,0,0,3>>},
        hash,
        {bin,md5,<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

user_defined_to_json_test() ->
	DriverData = {'_id',{<<79,0,5,23,138,250,150,11,129,0,0,4>>},
        user_data,
        {bin,userdefined,<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

array_to_json_test() ->
	DriverData = {arr1,[<<"element1">>,2.3,{foo,<<"bar">>}], arr2, [], arr3, [1, [1], []]},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

timestamp_to_json_test() ->
	DriverData = 	{ts,{mongostamp,1234567890,1000000000}}, 
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

regex_to_json_test() ->
	DriverData = {'_id',{<<79,8,33,170,138,250,150,28,93,0,0,4>>}, a_regex, {regex,<<"^\\d{5}$">>,<<>>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).

regex_with_option_to_json_test() ->
	DriverData = {'_id',{<<79,8,33,170,138,250,150,28,93,0,0,4>>}, a_regex, {regex,<<"best|worst">>,<<"i">>}},
	JsonData = mongreljson:tuple_to_json(DriverData),
	DriverData = mongreljson:json_to_tuple(JsonData).
