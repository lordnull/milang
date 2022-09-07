-ifndef(DEFAULT_IMPORTS).
-define(DEFAULT_IMPORTS,
"import Core.\n"
"alias Integer = Core.Integer.\n"
"alias String = Core.String.\n"
"alias Unit = Core.Unit.\n"
"alias Never = Core.Never.\n"
"alias List a = Core.List a.\n"
"let | = Core.|.\n"
"let always = Core.always.\n"
"\n").
-endif.
