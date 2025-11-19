#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t FIXNUM_P(habu_value_t);
habu_value_t CONS_P(habu_value_t);
habu_value_t SYMBOL_P(habu_value_t);
habu_value_t STRING_P(habu_value_t);
habu_value_t NIL_P(habu_value_t);
habu_value_t STR_CMP_LOOP(habu_value_t, habu_value_t, habu_value_t, habu_value_t);
habu_value_t STRING_EQ_P(habu_value_t, habu_value_t);
habu_value_t SYMBOL_EQ_P(habu_value_t, habu_value_t);
habu_value_t IS_DIGIT_P(habu_value_t);
habu_value_t IS_ALPHA_P(habu_value_t);
habu_value_t IS_SYMBOL_START_P(habu_value_t);
habu_value_t IS_SYMBOL_CHAR_P(habu_value_t);
habu_value_t IS_WHITESPACE_P(habu_value_t);
habu_value_t SKIP_WS(habu_value_t, habu_value_t);
habu_value_t COLLECT_CHARS(habu_value_t, habu_value_t, habu_value_t);
habu_value_t REVERSE_LIST(habu_value_t);
habu_value_t REVERSE_HELPER(habu_value_t, habu_value_t);
habu_value_t MAKE_SYM_FROM_CHARS(habu_value_t);
habu_value_t LIST_LENGTH(habu_value_t, habu_value_t);
habu_value_t FILL_VEC(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_NUM(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_SYM(habu_value_t, habu_value_t);
habu_value_t PARSE_LIST(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_ONE(habu_value_t, habu_value_t);
habu_value_t READ_STR(habu_value_t);
habu_value_t EVAL_EXPR(habu_value_t, habu_value_t);
habu_value_t EVAL_IF(habu_value_t, habu_value_t);
habu_value_t EVAL_LET(habu_value_t, habu_value_t);
habu_value_t EVAL_BINDINGS(habu_value_t, habu_value_t);
habu_value_t EVAL_LIST(habu_value_t, habu_value_t);
habu_value_t EVAL_APPLY(habu_value_t, habu_value_t, habu_value_t);
habu_value_t APPLY_LAMBDA(habu_value_t, habu_value_t);
habu_value_t ENV_LOOKUP(habu_value_t, habu_value_t);
habu_value_t ENV_EXTEND(habu_value_t, habu_value_t, habu_value_t);
habu_value_t ENV_EXTEND_LIST(habu_value_t, habu_value_t, habu_value_t);
habu_value_t REPL_START();
habu_value_t REPL_LOOP(habu_value_t);

habu_value_t FIXNUM_P(habu_value_t X) {
    return (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL);
}

habu_value_t CONS_P(habu_value_t X) {
    return (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(1)) ? fixnum_to_value(1) : NIL);
}

habu_value_t SYMBOL_P(habu_value_t X) {
    return (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(2)) ? fixnum_to_value(1) : NIL);
}

habu_value_t STRING_P(habu_value_t X) {
    return (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(4)) ? fixnum_to_value(1) : NIL);
}

habu_value_t NIL_P(habu_value_t X) {
    return (value_to_fixnum(X) == value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL);
}

habu_value_t STR_CMP_LOOP(habu_value_t S1, habu_value_t S2, habu_value_t I, habu_value_t LEN) {
    return (is_nil((value_to_fixnum(I) >= value_to_fixnum(LEN) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(habu_string_ref(S1, value_to_fixnum(I))) == value_to_fixnum(habu_string_ref(S2, value_to_fixnum(I))) ? fixnum_to_value(1) : NIL)) ? NIL : STR_CMP_LOOP(S1, S2, fixnum_to_value(value_to_fixnum(I) + value_to_fixnum(fixnum_to_value(1))), LEN)) : fixnum_to_value(1));
}

habu_value_t STRING_EQ_P(habu_value_t S1, habu_value_t S2) {
    return ({
    habu_value_t LEN1 = fixnum_to_value(habu_string_length_raw(S1));
    ({
      habu_value_t LEN2 = fixnum_to_value(habu_string_length_raw(S2));
      (is_nil((value_to_fixnum(LEN1) == value_to_fixnum(LEN2) ? fixnum_to_value(1) : NIL)) ? NIL : STR_CMP_LOOP(S1, S2, fixnum_to_value(0), LEN1));
      });
    });
}

habu_value_t SYMBOL_EQ_P(habu_value_t S1, habu_value_t S2) {
    return STRING_EQ_P(habu_symbol_name(S1), habu_symbol_name(S2));
}

habu_value_t IS_DIGIT_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(48)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(57)) ? fixnum_to_value(1) : NIL));
}

habu_value_t IS_ALPHA_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(65)) ? fixnum_to_value(1) : NIL)) ? NIL : (is_nil((value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(90)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(97)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(122)) ? fixnum_to_value(1) : NIL)) : fixnum_to_value(1)));
}

habu_value_t IS_SYMBOL_START_P(habu_value_t CH) {
    return (is_nil(IS_ALPHA_P(CH)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(43)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(45)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(42)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(47)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(61)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(60)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(62)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(63)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(33)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t IS_SYMBOL_CHAR_P(habu_value_t CH) {
    return (is_nil(IS_SYMBOL_START_P(CH)) ? (is_nil(IS_DIGIT_P(CH)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t IS_WHITESPACE_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(32)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(10)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(9)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t SKIP_WS(habu_value_t STR, habu_value_t IDX) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_WHITESPACE_P(CH)) ? IDX : SKIP_WS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1)))));
    }) : IDX);
}

habu_value_t COLLECT_CHARS(habu_value_t STR, habu_value_t IDX, habu_value_t CHARS) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_SYMBOL_CHAR_P(CH)) ? habu_cons(CHARS, IDX) : COLLECT_CHARS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), habu_cons(CH, CHARS)));
    }) : habu_cons(CHARS, IDX));
}

habu_value_t REVERSE_LIST(habu_value_t LST) {
    return REVERSE_HELPER(LST, NIL);
}

habu_value_t REVERSE_HELPER(habu_value_t LST, habu_value_t ACC) {
    return (is_nil(NIL_P(LST)) ? REVERSE_HELPER(habu_cdr(LST), habu_cons(habu_car(LST), ACC)) : ACC);
}

habu_value_t MAKE_SYM_FROM_CHARS(habu_value_t CHARS) {
    return ({
    habu_value_t LEN = LIST_LENGTH(CHARS, fixnum_to_value(0));
    ({
      habu_value_t VEC = habu_make_vector(LEN);
      ({
  FILL_VEC(CHARS, VEC, fixnum_to_value(0));
  habu_make_symbol_from_string(habu_make_string_from_vector(VEC));
});
      });
    });
}

habu_value_t LIST_LENGTH(habu_value_t LST, habu_value_t ACC) {
    return (is_nil(NIL_P(LST)) ? LIST_LENGTH(habu_cdr(LST), fixnum_to_value(value_to_fixnum(ACC) + value_to_fixnum(fixnum_to_value(1)))) : ACC);
}

habu_value_t FILL_VEC(habu_value_t CHARS, habu_value_t VEC, habu_value_t IDX) {
    return (is_nil(NIL_P(CHARS)) ? ({
  ({habu_vector_set(VEC, value_to_fixnum(IDX), habu_car(CHARS)); NIL;});
  FILL_VEC(habu_cdr(CHARS), VEC, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))));
}) : VEC);
}

habu_value_t PARSE_NUM(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_DIGIT_P(CH)) ? habu_cons(ACC, IDX) : PARSE_NUM(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), fixnum_to_value(value_to_fixnum(fixnum_to_value(value_to_fixnum(ACC) * value_to_fixnum(fixnum_to_value(10)))) + value_to_fixnum(fixnum_to_value(value_to_fixnum(CH) - value_to_fixnum(fixnum_to_value(48)))))));
    }) : habu_cons(ACC, IDX));
}

habu_value_t PARSE_SYM(habu_value_t STR, habu_value_t IDX) {
    return ({
    habu_value_t RESULT = COLLECT_CHARS(STR, IDX, NIL);
    ({
      habu_value_t CHARS = REVERSE_LIST(habu_car(RESULT));
      habu_cons(MAKE_SYM_FROM_CHARS(CHARS), habu_cdr(RESULT));
      });
    });
}

habu_value_t PARSE_LIST(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    return ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(41)) ? fixnum_to_value(1) : NIL)) ? ({
        habu_value_t ELEM_RESULT = PARSE_ONE(STR, IDX2);
        PARSE_LIST(STR, habu_cdr(ELEM_RESULT), habu_cons(habu_car(ELEM_RESULT), ACC));
        }) : habu_cons(REVERSE_LIST(ACC), fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1)))));
      }) : habu_cons(REVERSE_LIST(ACC), IDX2));
    });
}

habu_value_t PARSE_ONE(habu_value_t STR, habu_value_t IDX) {
    return ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(40)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(39)) ? fixnum_to_value(1) : NIL)) ? (is_nil(IS_DIGIT_P(CH)) ? (is_nil(IS_SYMBOL_START_P(CH)) ? habu_cons(NIL, IDX2) : PARSE_SYM(STR, IDX2)) : PARSE_NUM(STR, IDX2, fixnum_to_value(0))) : ({
        habu_value_t QUOTED_RESULT = PARSE_ONE(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))));
        habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("quote", 5)), habu_cons(habu_car(QUOTED_RESULT), NIL)), habu_cdr(QUOTED_RESULT));
        })) : PARSE_LIST(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))), NIL));
      }) : habu_cons(NIL, IDX2));
    });
}

habu_value_t READ_STR(habu_value_t STR) {
    return habu_car(PARSE_ONE(STR, fixnum_to_value(0)));
}

habu_value_t EVAL_EXPR(habu_value_t EXPR, habu_value_t ENV) {
    return (is_nil(FIXNUM_P(EXPR)) ? (is_nil(NIL_P(EXPR)) ? (is_nil(SYMBOL_P(EXPR)) ? (is_nil(CONS_P(EXPR)) ? EXPR : ({
    habu_value_t FIRST = habu_car(EXPR);
    (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("quote", 5)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("if", 2)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("let", 3)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("lambda", 6)))) ? EVAL_APPLY(FIRST, habu_cdr(EXPR), ENV) : habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(ENV, habu_cdr(EXPR)))) : EVAL_LET(habu_cdr(EXPR), ENV)) : EVAL_IF(habu_cdr(EXPR), ENV)) : habu_car(habu_cdr(EXPR)));
    })) : ENV_LOOKUP(EXPR, ENV)) : NIL) : EXPR);
}

habu_value_t EVAL_IF(habu_value_t ARGS, habu_value_t ENV) {
    return ({
    habu_value_t TEST = EVAL_EXPR(habu_car(ARGS), ENV);
    (is_nil(NIL_P(TEST)) ? EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV) : EVAL_EXPR(habu_car(habu_cdr(habu_cdr(ARGS))), ENV));
    });
}

habu_value_t EVAL_LET(habu_value_t ARGS, habu_value_t ENV) {
    return ({
    habu_value_t BINDINGS = habu_car(ARGS);
    ({
      habu_value_t BODY = habu_car(habu_cdr(ARGS));
      ({
        habu_value_t NEW_ENV = EVAL_BINDINGS(BINDINGS, ENV);
        EVAL_EXPR(BODY, NEW_ENV);
        });
      });
    });
}

habu_value_t EVAL_BINDINGS(habu_value_t BINDINGS, habu_value_t ENV) {
    return (is_nil(NIL_P(BINDINGS)) ? ({
    habu_value_t BINDING = habu_car(BINDINGS);
    ({
      habu_value_t SYM = habu_car(BINDING);
      ({
        habu_value_t VAL_EXPR = habu_car(habu_cdr(BINDING));
        ({
          habu_value_t VAL = EVAL_EXPR(VAL_EXPR, ENV);
          EVAL_BINDINGS(habu_cdr(BINDINGS), ENV_EXTEND(SYM, VAL, ENV));
          });
        });
      });
    }) : ENV);
}

habu_value_t EVAL_LIST(habu_value_t EXPRS, habu_value_t ENV) {
    return (is_nil(NIL_P(EXPRS)) ? habu_cons(EVAL_EXPR(habu_car(EXPRS), ENV), EVAL_LIST(habu_cdr(EXPRS), ENV)) : NIL);
}

habu_value_t EVAL_APPLY(habu_value_t OP, habu_value_t ARGS, habu_value_t ENV) {
    return (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("+", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("-", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("*", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("/", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("cons", 4)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("car", 3)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("cdr", 3)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("list", 4)))) ? ({
    habu_value_t FN = EVAL_EXPR(OP, ENV);
    (is_nil(CONS_P(FN)) ? NIL : (is_nil(SYMBOL_EQ_P(habu_car(FN), habu_make_symbol_from_string(habu_make_string("closure", 7)))) ? NIL : APPLY_LAMBDA(FN, EVAL_LIST(ARGS, ENV))));
    }) : EVAL_LIST(ARGS, ENV)) : habu_cdr(EVAL_EXPR(habu_car(ARGS), ENV))) : habu_car(EVAL_EXPR(habu_car(ARGS), ENV))) : habu_cons(EVAL_EXPR(habu_car(ARGS), ENV), EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) / value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) * value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) - value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) + value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV))));
}

habu_value_t APPLY_LAMBDA(habu_value_t CLOSURE, habu_value_t ARG_VALS) {
    return ({
    habu_value_t CLOSURE_ENV = habu_car(habu_cdr(CLOSURE));
    ({
      habu_value_t PARAMS = habu_car(habu_cdr(habu_cdr(CLOSURE)));
      ({
        habu_value_t BODY = habu_car(habu_cdr(habu_cdr(habu_cdr(CLOSURE))));
        ({
          habu_value_t NEW_ENV = ENV_EXTEND_LIST(PARAMS, ARG_VALS, CLOSURE_ENV);
          EVAL_EXPR(BODY, NEW_ENV);
          });
        });
      });
    });
}

habu_value_t ENV_LOOKUP(habu_value_t SYM, habu_value_t ENV) {
    return (is_nil(NIL_P(ENV)) ? ({
    habu_value_t BINDING = habu_car(ENV);
    (is_nil(SYMBOL_EQ_P(SYM, habu_car(BINDING))) ? ENV_LOOKUP(SYM, habu_cdr(ENV)) : habu_cdr(BINDING));
    }) : NIL);
}

habu_value_t ENV_EXTEND(habu_value_t SYM, habu_value_t VAL, habu_value_t ENV) {
    return habu_cons(habu_cons(SYM, VAL), ENV);
}

habu_value_t ENV_EXTEND_LIST(habu_value_t SYMS, habu_value_t VALS, habu_value_t ENV) {
    return (is_nil(NIL_P(SYMS)) ? ENV_EXTEND_LIST(habu_cdr(SYMS), habu_cdr(VALS), ENV_EXTEND(habu_car(SYMS), habu_car(VALS), ENV)) : ENV);
}

habu_value_t REPL_START() {
    return ({
  habu_print_value(habu_make_string("Habu REPL - Programmable", 24));
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Features: let, lambda", 21));
  ({printf("\n"); NIL;});
  REPL_LOOP(NIL);
});
}

habu_value_t REPL_LOOP(habu_value_t ENV) {
    return ({
    habu_value_t LINE = (habu_value_t)lineedit_readline("habu> ");
    (is_nil(LINE) ? ({
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Goodbye!", 8));
  ({printf("\n"); NIL;});
}) : ({
  ({
        habu_value_t STR = ({char* s = (char*)LINE; s ? habu_make_string(s, strlen(s)) : NIL;});
        ({
          habu_value_t EXPR = READ_STR(STR);
          ({
            habu_value_t RESULT = EVAL_EXPR(EXPR, ENV);
            ({
  habu_print_value(RESULT);
  ({printf("\n"); NIL;});
});
            });
          });
        });
  REPL_LOOP(ENV);
}));
    });
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = REPL_START();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
