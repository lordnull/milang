%%% @doc depricated in favor of milang_ast_concrete_function.
-module(milang_ast_signature).
-record(signature,
	{ args
	}).
-type signature() :: #signature{}.
-export_type([signature/0]).
-export(
	[ new/1
	, 'args'/1, 'args'/2
	]).

new(V0) -> #'signature'{'args'=V0}.

'args'(R) -> R#'signature'.'args'.
'args'(V,R) -> R#'signature'{ 'args' = V }.

