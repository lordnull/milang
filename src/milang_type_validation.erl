-module(milang_type_validation).

-export(
	[ new/0
	, new_scope/1
	, pop_scope/1
	, add_entry/3
	, lookup/2
	, resolve_function_name/2
	, validate/2
	, refine/3
	]).
-export(
	[ constructor/3
	, data/3
	, function/2
	, concrete/2
	, alias/3
	]).

% okay, so given the following:
% -type Goober | Pants | Slacks Integer .
%
% -spec inseam : Goober -> Integer.
% inseam goober =
%   case goober of
%       Pants -> 1,
%       Slacks n -> n.
%
% We need to:
%   Load up Goober as a type with no constraints and no args
%   Load up Pants as a type with no constraints, no args, and Goober as parent.
%   Load up Slacks as a type with no constraints, and 1 arg:
%       Integer type (base type? primitive? something like that.)
%   Load up inseam as a type with noconstrats, function with 1 arg, and 1 return value
%       arg is a Goober
%       return is an Integer
%   Check the implemetation of inseam matches the spec, or do some inferencing.

-record(constructor, {
	name,
	constructor_of,
	args = []
}).

-record(type, {
	name,
	constraints = #{},
	arg_names = []
}).

-record(function, {
	name,
	modue_name,
	local_name,
	types = []
}).

-record(concrete, {
	name,
	args = []
}).

-record(alias, {
	name, alias_type, truname
	}).

-type type_entry() :: #constructor{} | #type{} | #function{} | #concrete{} | #alias{}.

-type lookup_table() :: [ #{ atom() => type_entry()} ].

-export_type([type_entry/0, lookup_table/0]).

new() ->
	new_scope([]).

new_scope(OldTable) ->
	[#{} | OldTable].

pop_scope([]) ->
	error(somehow_no_table_at_all);
pop_scope([_]) ->
	error(cannot_pop_last_scope);
pop_scope([_ | Tail]) ->
	Tail.

resolve_function_name(LocalName, Table) ->
	case lookup(LocalName, Table) of
		{error, _} = E ->
			E;
		{ok, #function{}} = Ok ->
			Ok;
		{ok, #alias{alias_type = function_name_remote} = A} ->
			resolve_function_name(A#alias.truname, Table);
		{ok, _} ->
			{error, not_function}
	end.

lookup(_Name, []) ->
	{error, notfound};
lookup(Name, [Table | Tail]) ->
	case maps:find(Name, Table) of
		error ->
			lookup(Name, Tail);
		{ok, _} = Ok ->
			Ok
	end.

add_entry(Name, Entry, Table) ->
	case lookup(Name, Table) of
		{error, notfound} ->
			[OldHead | Tail] = Table,
			NewHead = maps:put(Name, Entry, OldHead),
			{ok, [NewHead | Tail]};
		{ok, Entry} ->
			{error, {shadowing, Name, Entry}}
	end.

% the Just a | Nothing from Maybe a for example.
constructor(Name, Parent, Args) ->
	{constructor, Name, Parent, Args}.

% the Maybe a itself.
data(Name, Constraints, ArgNames) ->
	{type, Name, Constraints, ArgNames}.

% function specs, or an argument to a a construtor or data.
function(Name, Types) ->
	{function, Name, Types}.

% Maybe Int, the variable for Maybe a defined.
concrete(Name, Args) ->
	{concrete, Name, Args}.

% a name that points to another name.
-type alias_type() :: function_name_remote | type | type_name_remote.
-spec alias(atom(), alias_type(), atom()) -> #alias{}.
alias(Name, AliasType, TrueName) ->
	{alias, Name, AliasType, TrueName}.

validate(_Name, _LookupTable) ->
	% TODO actually implement type checking.
	ok.

refine(_Name, _Refinement, LookupTable) ->
	% TODO actuall implement refinements.
	{ok, LookupTable}.

