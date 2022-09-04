-module(milang_module).

-record(milang_module,
	{ name :: unicode:chardata()
	, docs = [] :: unicode:chardata()
	, declarations = [] :: [ milang_declaration:declaration() ]}).
-type milang_module() :: #milang_module{}.

-export(
	[ new/1
	, name/1, name/2
	, docs/1, docs/2
	, declarations/1, declarations/2
	]).

-export_type([ milang_module/1 ]).

new(Name) ->
	#milang_module{ name = Name, docs = <<>> }.

name(Module) ->
	Module#milang_module.name.

name(NewName, Module) ->
	Module#milang_module{ name = NewName }.

docs(Module) ->
	Module#milang_module.docs.

docs(NewDocs, Module) ->
	Module#milang_module{ docs = NewDocs }.

declarations(Module) ->
	Module#milang_module.declarations.

declarations(NewDeclares, Module) ->
	Module#milang_module{ declarations = NewDeclares }.
