-ifndef(DEFAULT_IMPORTS).
-define(DEFAULT_IMPORTS,
"import Maybe.\n"
"alias Maybe a = Maybe.Maybe a.\n"
"import Core.\n"
"alias Integer = Core.Integer.\n"
"alias String = Core.String.\n"
"alias Unit = Core.Unit.\n"
"alias Never = Core.Never.\n"
"alias Boolean = Core.Boolean.\n"
"let |> = Core.|>.\n"
"let <| = Core.<|.\n"
"let always = Core.always.\n"
"import List.\n"
"alias List a = List.List a.\n"
"\n").
-endif.
