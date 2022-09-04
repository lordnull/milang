-module(milang_expression).

-record(literal_integer, { value :: integer() }).
-record(literal_float, {value :: float() }).
-record(literal_string, {value :: unicode:chardata()}).
-record(literal_list, {values = [] }).
-record(literal_record, {key_value_pairs = []}).

-type literal()
	:: #literal_integer{}
	|  #literal_float{}
	|  #literal_string{}
	|  #literal_list{ values :: [ literal() ]}
	|  #literal_record{ key_value_pairs :: [ {milang_name:name(), literal()}]}
	.

-record(construct,
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, args = []
	}).
-type construct() :: #construct{}.

-record(infix_symbol,
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, weight = 1 :: non_neg_integer()
	, associativity = none :: left | none | right
	}).
-type infix_symbol() :: #infix_symbol{}.

-record(apply,
	{ name :: milang_name:name()
	, args = []
	}).
-type apply() :: #apply{}.

-record(infix_sequence,
	{ head
	, tail = []
	}).
-type infix_sequence() :: #infix_sequence{}.

-record(function,
	{ arg_names = [] :: [ milang_name:name() ]
	, expression
	}).
-type function() :: #function{}.

-record(record_access,
	{ field_name :: milang_name:name()
	, record
	}).
-type record_access() :: #record_access{}.

-record(record_update,
	{ record
	, updates = []
	}).
-type record_update() :: #record_update{}.

-record(match,
	{ match_what
	, bind_and_expression = []
	}).
-type match() :: #match{}.

-type expression()
	:: literal()
	|  construct()
	|  apply()
	|  infix_sequence()
	|  function()
	|  record_access()
	|  record_update()
	|  match()
	.

-export_type(
	[ expression/0
	, literal/0
	, construct/0
	, infix_symbol/0
	, infix_sequence/0
	, function/0
	, record_access/0
	, record_update/0
	, match/0
	, ]).


