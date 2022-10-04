-module('Core').

-export(
	[ '|>'/2
	, '|>'/0
	, '<|'/2
	, '<|'/0
	, always/1
	, always/0
	, identity/0
	, identity/1
	, 'True'/0
	, 'False'/0
	, 'not'/0
	, 'not'/1
	, 'and'/0
	, 'and'/2
	, 'or'/0
	, 'or'/2
	, 'xor'/0
	, 'xor'/2

	, 'LessThan'/0
	, 'GreaterThan'/0
	, 'EqualTo'/0

	, 'compare'/2
	, 'compare'/0
	, '>'/2
	, '>'/0
	, '>='/2
	, '>='/0
	, '<'/2
	, '<'/0
	, '<='/2
	, '<='/0
	, '=='/2
	, '=='/0
	, '<>'/2
	, '<>'/0

	, 'integer-=='/0
	, 'integer-=='/2
	, 'integer-compare'/0
	, 'integer-compare'/2

	, 'float-=='/0
	, 'float-=='/2
	, 'float-compare'/0
	, 'float-compare'/2


	, 'boolean-=='/0
	, 'boolean-=='/2

	, 'string-=='/0
	, 'string-=='/2

	, 'integer+'/0
	, 'integer+'/2
	, 'integer*'/0
	, 'integer/'/0
	, 'integer/'/2
	, 'integer^'/0
	, 'integer^'/2
	, 'integer-abs'/0
	, 'integer-abs'/1
	, 'integer-sign_of'/0
	, 'integer-sign_of'/1
	, 'integer-additive_inverse'/0
	, 'integer-additive_inverse'/1

	, 'float+'/0
	, 'float+'/2
	, 'float*'/0
	, 'float/'/0
	, 'float/'/2
	, 'float^'/0
	, 'float^'/2
	, 'float-abs'/0
	, 'float-abs'/1
	, 'float-sign_of'/0
	, 'float-sign_of'/1
	, 'float-from_integer'/0
	, 'float-from_integer'/1
	, 'float-additive_inverse'/0
	, 'float-additive_inverse'/1
	]).

-define(curried(FunctionName, Arity), FunctionName() -> milang_curry:stack(fun FunctionName/Arity)).
-define(curried(FunctionName), ?curry(FunctionName, 0)).

identity(E) -> E.

identity() ->
	milang_curry:stack(fun identity/1).

always(A) ->
	fun(_) -> A end.

always() ->
	milang_curry:stack(fun always/1).

'|>'(Input, Func) ->
	Func(Input).

'|>'() ->
	milang_curry:stack(fun '|>'/2).

'<|'(Func, Input) ->
	Func(Input).

'<|'() ->
	milang_curry:stack(fun '<|'/2).

'True'() -> true.

'False'() -> false.

'not'() ->
	milang_curry:stack(fun 'not'/1).

'not'(true) -> false;
'not'(false) -> true.

'and'() ->
	milang_curry:stack(fun 'and'/2).

'and'(A, B) ->
	A andalso B.

'or'() ->
	milang_curry:stack(fun 'or'/2).

'or'(A, B) ->
	A orelse B.

'xor'() ->
	milang_curry:stack(fun 'xor'/2).

'xor'(A, B) ->
	A xor B.

'integer-=='() ->
	milang_curry:stack(fun 'integer-=='/2).

'integer-=='(A, B) ->
	A == B.

'float-=='() ->
	milang_curry:stack(fun 'float-=='/2).

'float-=='(A, B) ->
	A == B.

'boolean-=='() ->
	milang_curry:stack(fun 'boolean-=='/2).

'boolean-=='(A, B) ->
	A == B.

'string-=='() ->
	milang_curry:stack(fun 'string-=='/2).

'string-=='(A, B) ->
	A == B.

'integer-compare'() ->
	milang_curry:stack(fun 'integer-compare'/2).

'integer-compare'(A, B) ->
	compare(A, B).

'float-compare'() ->
	milang_curry:stack(fun 'float-compare'/2).

'float-compare'(A, B) ->
	compare(A, B).

compare(A, B) ->
	if
		A == B ->
			'EqualTo'();
		A > B ->
			'GreaterThan'();
		A < B ->
			'LessThan'()
	end.

?curried(compare, 2).

'LessThan'() -> less_than.

'GreaterThan'() -> greater_than.

'EqualTo'() -> equal_to.

'integer+'() ->
	milang_curry:stack(fun 'integer+'/2).

'integer+'(A, B) ->
	A + B.

'integer*'() ->
	milang_curry:stack(fun 'integer*'/2).

'integer*'(A, B) ->
	A * B.

'integer/'() ->
	milang_curry:stack(fun 'integer/'/2).

'integer/'(A, B) ->
	try A / B of
		N ->
			'Maybe':'Some'(floor(N))
	catch
		error:badarith ->
			'Maybe':'Nothing'()
	end.

'integer^'() ->
	milang_curry:stack(fun 'integer^'/2).

'integer^'(A, _B) when A == 0 ->
	'Maybe':'Nothing'();
'integer^'(A, B) when A == 0 ->
	'Maybe':'Some'(math:pow(A, B)).

'integer-abs'() ->
	milang_curry:stack(fun 'integer-abs'/1).

'integer-abs'(A) ->
	abs(A).

'integer-sign_of'() ->
	milang_curry:stack(fun 'integer-sign_of'/1).

'integer-sign_of'(A) when A == 0 ->
	0;
'integer-sign_of'(A) when A > 0 ->
	1;
'integer-sign_of'(A) when A < 0 ->
	-1.

'integer-additive_inverse'() ->
	milang_curry:stack(fun 'integer-additive_inverse'/1).

'integer-additive_inverse'(A) ->
	A * -1.

'float+'() ->
	milang_curry:stack(fun 'float+'/2).

'float+'(A, B) ->
	A + B.

'float*'() ->
	milang_curry:stack( fun 'float*'/2).

'float*'(A, B) ->
	A * B.

'float/'() ->
	milang_curry:stack( fun 'float/'/2).

'float/'(A, B) ->
	try A / B of
		N ->
			'Maybe':'Some'(N)
	catch
		error:badarith ->
			'Maybe':'Nothing'()
	end.

'float^'() ->
	milang_curry:stack(fun 'float^'/2).

'float^'(A, _B) when A == 0.0 ->
	'Maybe':'Nothing'();
'float^'(A, B) ->
	'Maybe':'Some'(math:pow(A, B)).

'float-abs'() ->
	milang_curry:stack(fun 'float-abs'/1).

'float-abs'(A) ->
	abs(A).

'float-sign_of'() ->
	milang_curry:stack(fun 'float-sign_of'/1).

'float-sign_of'(A) when A == 0.0 ->
	0.0;
'float-sign_of'(A) when A > 0.0 ->
	1.0;
'float-sign_of'(A) when A < 0.0 ->
	-1.0.

'float-from_integer'() ->
	milang_curry:stack(fun 'float-from_integer'/1).

'float-from_integer'(A) ->
	A * 1.0.

'float-additive_inverse'() ->
	milang_curry:stack(fun 'float-additive_inverse'/1).

'float-additive_inverse'(A) ->
	A * -1.0.

'>'(A, B) ->
	Compared = compare(A, B),
	'GreaterThan'() =:= Compared.

?curried('>', 2).

'>='(A, B) ->
	Compared = compare(A, B),
	'GreaterThan'() =:= Compared
		orelse 'EqualTo'() =:= Compared.

?curried('>=', 2).

'<'(A, B) ->
	Compared = compare(A, B),
	'LessThan'() =:= Compared.

?curried('<', 2).

'<='(A, B) ->
	Compared = compare(A, B),
	'LessThan'() =:= Compared
		orelse 'EqualTo'() =:= Compared.

?curried('<=', 2).

'=='(A, B) ->
	Compared = compare(A, B),
	'EqualTo'() =:= Compared.

?curried('==', 2).

'<>'(A, B) ->
	Compared = compare(A, B),
	'EqualTo'() =/= Compared.

?curried('<>', 2).
