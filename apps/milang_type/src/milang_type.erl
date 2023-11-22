-module(milang_type).

-type name() :: unicode:chardata().
-type data_type(ArgTypes) :: {data, name(), [ ArgTypes ], [ name() ]}.
-type function_type(ArgTypes) :: {function, ArgTypes, ArgTypes}.
-type record_type(ArgTypes) :: {record, boolean(), #{name() => ArgTypes}}.
-type variable() :: {constrained, name(), [ name() ]}.
-type ignored() :: {ignored, name()}.
-type arg_type()
	:: data_type(arg_type())
	|  function_type(arg_type())
	|  record_type(arg_type())
	|  variable()
	|  ignored()
	.

-export_type(
	[ arg_type/0
	, data_type/1
	, function_type/1
	, record_type/1
	, variable/0
	, ignored/0
	]).

-export(
	[ compare_types/2
	, data_type/3
	, function_type/2
	, record_type/2
	, constrained/2
	, ignored/1
	]).

data_type(Name, Args, ClassesLearned) ->
	{data, Name, Args, ordsets:from_list(ClassesLearned)}.

function_type(Arg, Return) ->
	{function, Arg, Return}.

record_type(AllowAdditionalFields, Fields) ->
	{record, AllowAdditionalFields, Fields}.

constrained(VaraibleName, Classes) ->
	{constrained, VaraibleName, ordsets:from_list(Classes)}.

ignored(Name) ->
	{ignored, Name}.

compare_types(Type, Type) ->
	{ok, Type};

compare_types({ignored, _} = Ignored, _) ->
	{ok, Ignored};

compare_types(_, {ignored, _} = Ignored) ->
	{ok, Ignored};

compare_types({constrained, Name, Constraints1}, {constrained, Name, Constraints2}) ->
	{ok, {variable, Name, ordsets:union(Constraints1, Constraints2)}};

compare_types({constrained, _Name, Constraints}, {data, TypeName, _Args, Classes} = Ok) ->
	case ordsets:is_subset(Constraints, Classes) of
		false ->
			{error, #{ reason => data_type_missing_classes, missing_classes => ordsets:subtract(Constraints, Classes), data_type => TypeName}};
		true ->
			{ok, Ok}
	end;

compare_types({data, _, _, _} = D, {constrained, _, _} = C) ->
	compare_types(C, D);

compare_types({record, true, Fields1}, {record, true, Fields2}) ->
	AllKeys = ordsets:from_list(maps:keys(Fields1) ++ maps:keys(Fields2)),
	FieldsOkay = lists:foldl(fun(Key, ResultAcc) ->
		result:and_then(fun(Acc) ->
			Type1 = maps:find(Key, Fields1),
			Type2 = maps:find(Key, Fields2),
			case {Type1, Type2} of
				{error, _} ->
					{ok, maps:put(Key, Type2, Acc)};
				{_, error} ->
					{ok, maps:put(Key, Type2, Acc)};
				_ ->
					result:map(fun(ClosedType) ->
						maps:put(Key, ClosedType, Acc)
					end, compare_types(Type1, Type2))
			end
		end, ResultAcc)
	end, {ok, #{}}, AllKeys),
	result:map(fun(Fields) ->
		{record, true, Fields}
	end, FieldsOkay);

compare_types({function, Arg1, Return1}, {function, Arg2, Return2}) ->
	ArgResult = lazy:func(fun compare_types/2, [Arg1, Arg2]),
	ReturnResult = lazy:func(fun compare_types/2, [Return1, Return2]),
	result:map_n(fun(Arg, Return) ->
		{function, Arg, Return}
	end, [ ArgResult, ReturnResult ]);

compare_types(Type1, Type2) ->
	{error, #{ reason => unresolvable_types, type1 => Type1, type2 => Type2}}.
