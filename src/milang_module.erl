-module(milang_module).

-export(
	[ new/1
	, name/1, name/2
	, declarations/2, declarations/1
	, imports/1, imports/2
	]).

-record(module,
	{ name :: unicode:chardata()
	, docs
	, imports = []
	, declarations = []
	}).

new(Name) ->
	#module{ name = Name, docs = [], declarations = [] }.

name(Rec) ->
	Rec#module.name.

name(Name, Rec) ->
	Rec#module{ name = Name }.

declarations(Rec) ->
	Rec#module.declarations.

declarations(Decs, Rec) ->
	Rec#module{ declarations = Decs }.

imports(Rec) ->
	Rec#module.imports.

imports(Imports, Rec) ->
	Rec#module{ imports = Imports}.
