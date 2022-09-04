-module(milang_match).

% a 'match' is a destructure of data, and optional binding of elements of that
% data to values.
%
% There are 2 areas where a match is used: let declarations, and match
% expressions.
%
% let declarations _must not_ have multiple possible match structures. For
% example:
%
% ```milang
% {- valid -}
% let maybe_a = Just 5.
% {- invalid, we don't know that the data returned from the expression is a 'Just'
% constructor. -}
% let maybe_b = a | return_some_other_maybe.
%
% {- valid -}
% let some_list = [1,2,3].
% {- invalid, we don't know that special filter will always return a list of
% length 1. -}
% let [ head ] = some_list | special_filter.
%
% {- valid -}
% let { spouse = maybe_spouse } = create_person_record("George", Nothing).
% {- invalid, we don't know for certain the spouse field will be the 'Nothing'
% constructor. -}
% let { spouse = Nothing, name = who } = create_person_record("Jeff", maybe_spouse).
% ```
%
% match expressions _must_ be exhaustive. In otherwords, every possible structure
% of the data must be accounted for. Each branch of a match expression _must_ be
% the same type as well. This is realatively easy to enforce since an expression
% can only return 1 type anyway.
%
% ```milang
% {- valid as it has a catch-all clause. -}
% match "hi" with
% 	"bye" ->
% 		"bye bye".
% 	_ ->
% 		"hello".
% .
%
% {- valid as the final clause is a named binding. -}
% match "greet" with
% 	"bye" ->
% 		"okay bye".
% 	greet ->
% 		"yes, " ++ greet.
% .
%
% {- invalid as the empty list possiblity is missing. -}
% match List.filterId some_list with
% 	[ head ] ->
% 		Ok head.
% 	[ _ | _ ] ->
% 		Err "overflow".
% .
%
% {- invalid as some clauses differ in type. -}
% match var_name with
% 	"hello" ->
% 		"string".
% 	5 ->
% 		"integer".
% 	_ ->
% 		"other".
% .
% ```


-type bindable_match()
	:: {match_record, [{unicode:chardata(), match()}]}
	|  match_ignore
	|  {match_named_ignore, unicode:chardata()}
	|  {match_bind, bindable_match() | undefined, unicode:chardata()}
	.

-type match()
	:: bindable_match()
	|  {match_list, [ match() ]}
	|  {match_float, float()}
	|  {match_integer, integer()}
	|  {match_string, unicode:chardata()}
	|  {match_type, unicode:chardata(), [ match() ]}
	.
