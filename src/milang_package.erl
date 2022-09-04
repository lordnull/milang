-module(milang_package).

-record(type,
	{ name
	, docs = []
	, arg_specs = []
	}).

-record(function,
	{ name
	, docs = []
	, spec
	}).

-record(infix,
	{ name
	, docs = []
	, spec
	}).

-record(module,
	{ name
	, docs = []
	, types
	, functions
	, infix_ops
	}).

-record(package,
	{ name
	, docs = []
	, packages = []
	, modules = []
	}).

-export(
	[ new_package/4
	, new_module/5
	, new_infix/3
	, new_function/3
	, new_type/3
	, modules/1
	, functions/1
	, types/1
	, infix_ops/1
	, name/1
	, docs/1
	, spec/1
	, arg_specs/1
	, packages/1
	]).
-export(
	[ load_all_packages/0
	]).

load_all_packages() ->
	% TODO this should search through the milang_env:search_dirs/0 for all
	% *.milang-header files, make a hierachry of them for packages, and then
	% expose that. However, in the spirit of "just get something working asap",
	% we're going with hard coded goodness.
	PrintLn = new_function("print_line", "Outputs a given string with a \\n appended for you.", "String -> Unit"),

	Functions = [ PrintLn ],

	String = new_type("String", "A UTF8 encoded binary.", []),
	Unit = new_type("Unit", "Mainly just a placeholder value.", []),

	Types = [ String, Unit ],

	Module = new_module("Core", "The basics.", Types, Functions, []),

	Modules = [ Module ],

	CorePackage = new_package("Core", "A hard coded core of doom.", [], Modules),

	[ CorePackage ].


new_package(Name, Docs, Packages, Modules) ->
	#package{ name = Name, docs = Docs, packages = Packages, modules = Modules}.

new_module(Name, Docs, Types, Functions, InfixOps) ->
	#module{ name = Name, docs = Docs, types = Types, functions = Functions, infix_ops = InfixOps }.

new_infix(Name, Docs, Spec) ->
	#infix{ name = Name, docs = Docs, spec = Spec }.

new_function(Name, Docs, Spec) ->
	#function{ name = Name, docs = Docs, spec = Spec }.

new_type(Name, Docs, Spec) ->
	#type{ name = Name, docs = Docs, arg_specs = Spec }.

modules(Rec) ->
	Rec#package.modules.

functions(Rec) ->
	Rec#module.functions.

types(Rec) ->
	Rec#module.types.

infix_ops(Rec) ->
	Rec#module.infix_ops.

name(#package{ name = Name}) ->
	Name;
name(#module{ name = Name}) ->
	Name;
name(#function{name = Name}) ->
	Name;
name(#type{name = Name}) ->
	Name;
name(#infix{ name = Name}) ->
	Name.


docs(#package{ docs = Docs}) ->
	Docs;
docs(#module{ docs = Docs}) ->
	Docs;
docs(#function{docs = Docs}) ->
	Docs;
docs(#type{docs = Docs}) ->
	Docs;
docs(#infix{ docs = Docs}) ->
	Docs.

spec(#function{spec = Spec}) ->
	Spec;
spec(#infix{ spec = Spec}) ->
	Spec.

arg_specs(Rec) ->
	Rec#type.arg_specs.

packages(Rec) ->
	Rec#package.packages.

