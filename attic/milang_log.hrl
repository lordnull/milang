-ifndef(MILANG_LOG).
-define(log_info, #{ line => ?LINE, module => ?MODULE, function => ?FUNCTION_NAME, arity => ?FUNCTION_ARITY }).
-define(MILANG_LOG, true).
-endif.