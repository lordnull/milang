-ifndef(MILANG_AST).

% IMPORTANT
% note how the records below are underspecified. The actual types live in
% milang_ast.erl. These are here so other modules (like milang_compile) can
% pattern match on the data values easily. 90% of the record manipulation should
% be done via the milang_ast module.
-record(milang_ast, {
	location = {1,1},
	doc = <<>>,
	data
}).

-record(expression_call,
	{ function
	, args = []
	}).

-record(expression_construct,
	{ type
	, args = []
	}).

-record(expression_record,
	{ base_reference
	, fields = []
	}).

-record(expression_record_field,
	{ name
	, expression
	}).

-record(record_field_access, { name }).

-record(infix_notation,
	{ name
	, weight = 1 :: pos_integer()
	, assoc = left :: left | right
	}).

-record(infix_operation,
	{ operator
	, expression
	}).

-record(expression_infix,
	{ head
	, infix_ops = []
	}).

-record(declaration_module,
	{ name :: atom()
	, exposing = []
	}).

-record(declaration_import,
	{ name :: atom()
	, alias :: undefined | {ok, atom()}
	, exposing = []
	}).

-record(type_variable, { name }).

-record(function_variable, {name }).

-record(type_function, { args = [] }).

-record(type_concrete,
	{ name
	, args = []
	}).

-record(type_record_field, { name, type }).

-record(type_record, { fields = [] }).

-record(constraint,
	{ name
	, constraint
	}).

-record(constructor,
	{ name
	, args = []
	}).


-record(declaration_type,
	{ name
	, args
	, constraints = []
	, constructors = []
	}).


-record(declaration_alias,
	{ name
	, args = []
	, constraints = []
	, alias_of
	}).

-record(class_member_definition,
	{ name
	, definition
	}).

-record(class_member_default,
	{ name
	, args = []
	, expression
	}).

-record(declaration_class,
	{ name
	, constraints = []
	, args = []
	, members = []
	}).

-record(declaration_spec,
	{ name
	, type
	}).

-record(binding,
	{ name
	, expression
	}).

-record(declaration_function,
	{ name
	, args
	, bindings = []
	, expression
	}).





-define(MILANG_AST, true).
-endif.
