%%% @doc Things which return a value.
-module(milang_expression).

%% @doc we make no diference at this level between constructions and function
%% calls. Also, a parameter is either 0 or 1 due to curry.
-record(call,
	{ expression :: any()
	, parameter :: maybe:maybe(any())
	}).
-type call(ParamType) :: #call{ expression :: ParamType, parameter :: maybe:maybe(ParamType)}.

%% @doc aka constants
-record(literal_float,
	{ value :: number()
	}).
-type literal_float() :: #literal_float{}.

-record(literal_integer,
	{ value :: integer()
	}).
-type literal_integer() :: #literal_integer{}.

-record(literal_string,
	{ value :: unicode:chardata()
	}).
-type literal_string() :: #literal_string{}.

%% @doc not exactly constants, but still pretty literal.
-record(literal_list,
	{ value = [] :: [ any() ]
	}).
-type literal_list(ExpressionType) :: #literal_list{ value :: [ ExpressionType ]}.

-record(literal_record_entry,
	{ name :: milang_type:variable_name()
	, expression
	}).
-type literal_record_entry(ExpressionType) :: #literal_record_entry{ expression :: ExpressionType }.

-record(literal_record,
	{ value = [] :: [ literal_record_entry(any()) ]
	}).
-type literal_record(ExpressionType) :: #literal_record{ value :: [ literal_record_entry( ExpressionType )]}.

%% @doc an entier match is an expression as it does return a value, but we need
%% to break it down a bit for human sanity.
-record(match_clause,
	{ binding :: milang_declaration:binding()
	, guard
	, body
	}).
-type match_clause(ExpressionType) :: #match_clause{ guard :: ExpressionType, body :: ExpressionType}.

-record(match_expression,
	{ expression
	, matches = [] :: [ match_clause(any()) ]
	}).
-type match(ExpressionType) :: #match_expression{ expression :: ExpressionType, matches :: [ match_clause(ExpressionType) ]}.


-type expression()
	:: call(expression())
	|  literal_float()
	|  literal_integer()
	|  literal_string()
	|  literal_list(expression())
	|  literal_record(expression())
	|  match(expression())
	.

-export_type(
	[ expression/0
	]).

