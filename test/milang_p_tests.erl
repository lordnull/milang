-module(milang_p_tests).

-include_lib("eunit/include/eunit.hrl").

valid_parse_test_() ->
	StringsToParse =
	% module declarations.
	[ <<"-module SomeMod exposing [ , main] .">>
	, <<"-module Path.To.Some.Mode exposing [, yo ] .">>
	, <<"-module SomeMod exposing [, Type] .">>
	, <<"-module Some.Other.Mod exposing[,State,Msg,main,frob].">>
	% import declarations
	, <<"-import Mod.">>
	, <<"-import Mod as Mood.">>
	, <<"-import Path.To.Mod exposing [, Pants, slacks].">>
	, <<"-import Path.To.Mod as Mod.">>
	, <<"-import Pants as Evil exposing[,Pants,slacks].">>
	, <<"-import Path.To.Paths as Evil exposing [, Pants , slacks , jeans ].">>
	% alias declartions
	, <<"-alias AList = List Some.Other.Type.">>
	, <<"-alias Goober a = List a .">>
	, <<"-alias Goober = { , pants = Slacks } .">>
	, <<"-alias Goober a = {, pants = Slacks , shirts = List a } .">>
	, <<"-alias Goober = Slacks -> Remote.Shoes.">>
	, <<"-alias when {, a = Eq } Goober a = Map Int a.">>
%	% type declarations
	, <<"-type Goober [, Pants].">>
	, <<"-type Goober\n [ , Pants\n  , Slacks\n ] .">>
	, <<"-type Maybe a [ , Nothing , Just a ].">>
	, <<"-type Goober [ , Pants {, id = String , threadcount = Int } ] .">>
	, <<"-type CollectionName [ , FirstName List String , SecondName SomeOtherType , ThirdName {, id = Yet.Another.Type } , FourthName (List String) ] .">>
	, <<"-type CollectionNameWithNL [\n    , FirstName List String\n    , SecondName SomeOtherType\n    , ThirdName Yet.Another.Type \n    , FourthName (List String) ].">>
	, <<"-type when {, q = Order } Flegle [ , DahList (List q) ] .">>
%	% type class declarations
	, <<"-class SomeClass a [ , do_a_thing : a -> Bool ] .">>
	, <<"-class WordyEq a [ , equal : a -> a -> Bool , not_equal : a -> a -> Bool ] .">>
	, <<"-class SomeDefault a [ , useless x = not_here x. ] .">>
	, <<"-class WordyEq a [, equal : a -> a -> Bool , not_equal : a -> a -> Bool , not_equal x y = not (equal x y) .] .">>
	, <<"-class Eq a [, '==' : a -> a -> Bool , '!=' : a -> a -> Bool , '==' x y = not ('!=' x y) . , '!=' x y = not ('==' x y). ].">>
	, <<"-class ClassName anArg\n  [  , funcName : anArg -> CompareResult ].">>
	, <<"-class when {, a = Eq } Classname a [, yeah_a_thing : a -> Woot ].">>
%	% funciton specifications
	, <<"specThisFunc:String.">>
	, <<"specThisFunc : List String -> String.">>
	, <<"main : List String -> Actor State Msg Exit .">>
	, <<"frobNob : Goober a.">>
	, <<"higherOrder : (a -> b) -> (b -> c) -> (a -> c) .">>
%	% function bodies
	, <<"literal5 -> 5.">>
	, <<"literalString ->	\n\"Harry\".">>
	, <<"literalFloat -> -7.3.">>
	, <<"callsAnother->a.">>
	, <<"nicelySpaced -> anotherLocal .">>
	, <<"callsRemote -> SomeModule.hi.">>
	, <<"addHaHa a b ->\n    add a b\n.">>
	, <<"makeFrob n -> Frob n.">>
	, <<"defaultList -> [, 1 , 2 , 3 ] .">>
	, <<"defaultRecord -> {, f1 = 5 , f2 = \"hi\" } .">>
	, <<"nestSomeThings a ->
		[, a
		, [, 1
		  , 2
		  , 3
		  ]
		, {, f1 = 1
		  , f2 = \"hi\"
		  }
		]
		.">>
	, <<"withABinding q -> varp = Goober, frob q varp.">>
	, <<"withAndInfifx e ->\n    Frob a »pipe quux\n    ."/utf8>>
	% formats that are used for headers. Essentially for headers we allow fully
	% qualified names for basically anything.
	, <<"-type SomeModule.T [, SomeModule.First , SomeModule.Second SupportModule.Cool ].">>
	, <<"-alias SomeModule.A = List Integer.">>
	, <<"-class SomeModule.SomeClass a [, do_a_thing : a -> Bool ].">>
	, <<"SomeModule.specThisFunc : SomeModule.T.">>
	, <<"SomeModule.specThisFunc : SomeModule.T -> a -> Support.Cool.">>
	],
	lists:map(fun(Str) ->
		{Str, fun() -> {ok, _, <<>>} = parse:it(Str, milang_p:module()) end}
	end, StringsToParse).
