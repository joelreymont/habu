#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t IS_WHITESPACE_(habu_value_t);
habu_value_t IS_DIGIT_(habu_value_t);
habu_value_t IS_ALPHA_(habu_value_t);
habu_value_t IS_SPECIAL_CHAR_(habu_value_t);
habu_value_t IS_SYMBOL_CHAR_(habu_value_t);
habu_value_t DIGIT_TO_INT(habu_value_t);
habu_value_t READER_AT_END_(habu_value_t);
habu_value_t READER_PEEK(habu_value_t);
habu_value_t READER_ADVANCE(habu_value_t);
habu_value_t SKIP_WS(habu_value_t);
habu_value_t PARSE_NUM(habu_value_t, habu_value_t);
habu_value_t PARSE_LIST_ELEMS(habu_value_t);
habu_value_t PARSE_LIST(habu_value_t);
habu_value_t READ_ONE(habu_value_t);
habu_value_t READ_FROM_STRING(habu_value_t);
habu_value_t ENV_LOOKUP(habu_value_t, habu_value_t);
habu_value_t SYM_EQ_(habu_value_t, habu_value_t);
habu_value_t EVAL_LIST(habu_value_t, habu_value_t);
habu_value_t APPLY_PLUS(habu_value_t);
habu_value_t APPLY_MULT(habu_value_t);
habu_value_t APPLY_BUILTIN(habu_value_t, habu_value_t);
habu_value_t EVAL(habu_value_t, habu_value_t);
habu_value_t EVAL_PROGN(habu_value_t, habu_value_t);
habu_value_t REPL_LOOP();
habu_value_t REPL_BODY(habu_value_t);

habu_value_t IS_WHITESPACE_(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(32)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(10)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(9)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(13)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t IS_DIGIT_(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(48)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(57)) ? fixnum_to_value(1) : NIL));
}

habu_value_t IS_ALPHA_(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(65)) ? fixnum_to_value(1) : NIL)) ? NIL : (is_nil((value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(90)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(97)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(122)) ? fixnum_to_value(1) : NIL)) : fixnum_to_value(1)));
}

habu_value_t IS_SPECIAL_CHAR_(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(43)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(45)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(42)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(47)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(61)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(60)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(62)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(63)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t IS_SYMBOL_CHAR_(habu_value_t CH) {
    return (is_nil(IS_ALPHA_(CH)) ? (is_nil(IS_DIGIT_(CH)) ? IS_SPECIAL_CHAR_(CH) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t DIGIT_TO_INT(habu_value_t CH) {
    return fixnum_to_value(value_to_fixnum(CH) - value_to_fixnum(fixnum_to_value(48)));
}

habu_value_t READER_AT_END_(habu_value_t STATE) {
    return (value_to_fixnum(habu_cdr(STATE)) >= value_to_fixnum(fixnum_to_value((char*)habu_car(STATE) ? strlen((char*)habu_car(STATE)) : 0)) ? fixnum_to_value(1) : NIL);
}

habu_value_t READER_PEEK(habu_value_t STATE) {
    return (is_nil(READER_AT_END_(STATE)) ? habu_string_ref(habu_car(STATE), value_to_fixnum(habu_cdr(STATE))) : NIL);
}

habu_value_t READER_ADVANCE(habu_value_t STATE) {
    return habu_cons(habu_car(STATE), fixnum_to_value(value_to_fixnum(habu_cdr(STATE)) + value_to_fixnum(fixnum_to_value(1))));
}

habu_value_t SKIP_WS(habu_value_t STATE) {
    return (is_nil(READER_AT_END_(STATE)) ? ({
    habu_value_t CH = READER_PEEK(STATE);
    (is_nil(IS_WHITESPACE_(CH)) ? STATE : SKIP_WS(READER_ADVANCE(STATE)));
    }) : STATE);
}

habu_value_t PARSE_NUM(habu_value_t STATE, habu_value_t ACC) {
    return (is_nil(READER_AT_END_(STATE)) ? ({
    habu_value_t CH = READER_PEEK(STATE);
    (is_nil(IS_DIGIT_(CH)) ? habu_cons(ACC, STATE) : PARSE_NUM(READER_ADVANCE(STATE), fixnum_to_value(value_to_fixnum(fixnum_to_value(value_to_fixnum(ACC) * value_to_fixnum(fixnum_to_value(10)))) + value_to_fixnum(DIGIT_TO_INT(CH)))));
    }) : habu_cons(ACC, STATE));
}

habu_value_t PARSE_LIST_ELEMS(habu_value_t STATE) {
    return ({
    habu_value_t S2 = SKIP_WS(STATE);
    (is_nil(READER_AT_END_(S2)) ? ({
      habu_value_t CH = READER_PEEK(S2);
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(41)) ? fixnum_to_value(1) : NIL)) ? ({
        habu_value_t ELEM_RESULT = READ_ONE(S2);
        ({
          habu_value_t REST_RESULT = PARSE_LIST_ELEMS(habu_cdr(ELEM_RESULT));
          habu_cons(habu_cons(habu_car(ELEM_RESULT), habu_car(REST_RESULT)), habu_cdr(REST_RESULT));
          });
        }) : habu_cons(NIL, READER_ADVANCE(S2)));
      }) : habu_cons(NIL, S2));
    });
}

habu_value_t PARSE_LIST(habu_value_t STATE) {
    return PARSE_LIST_ELEMS(READER_ADVANCE(STATE));
}

habu_value_t READ_ONE(habu_value_t STATE) {
    return ({
    habu_value_t S2 = SKIP_WS(STATE);
    (is_nil(READER_AT_END_(S2)) ? ({
      habu_value_t CH = READER_PEEK(S2);
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(40)) ? fixnum_to_value(1) : NIL)) ? (is_nil(IS_DIGIT_(CH)) ? habu_cons(NIL, S2) : PARSE_NUM(S2, fixnum_to_value(0))) : PARSE_LIST(S2));
      }) : habu_cons(NIL, S2));
    });
}

habu_value_t READ_FROM_STRING(habu_value_t STR) {
    return habu_car(READ_ONE(habu_cons(STR, fixnum_to_value(0))));
}

habu_value_t ENV_LOOKUP(habu_value_t SYM, habu_value_t ENV) {
    return (is_nil(habu_nil_p(ENV)) ? ({
    habu_value_t BINDING = habu_car(ENV);
    (is_nil(habu_symbol_eq(habu_car(BINDING), SYM)) ? ENV_LOOKUP(SYM, habu_cdr(ENV)) : habu_cdr(BINDING));
    }) : NIL);
}

habu_value_t SYM_EQ_(habu_value_t SYM, habu_value_t NAME) {
    return (is_nil(habu_symbol_p(SYM)) ? NIL : habu_string_eq(habu_symbol_name(SYM), NAME));
}

habu_value_t EVAL_LIST(habu_value_t EXPRS, habu_value_t ENV) {
    return (is_nil(habu_nil_p(EXPRS)) ? habu_cons(EVAL(habu_car(EXPRS), ENV), EVAL_LIST(habu_cdr(EXPRS), ENV)) : NIL);
}

habu_value_t APPLY_PLUS(habu_value_t ARGS) {
    return (is_nil(habu_nil_p(ARGS)) ? fixnum_to_value(value_to_fixnum(habu_car(ARGS)) + value_to_fixnum(APPLY_PLUS(habu_cdr(ARGS)))) : fixnum_to_value(0));
}

habu_value_t APPLY_MULT(habu_value_t ARGS) {
    return (is_nil(habu_nil_p(ARGS)) ? fixnum_to_value(value_to_fixnum(habu_car(ARGS)) * value_to_fixnum(APPLY_MULT(habu_cdr(ARGS)))) : fixnum_to_value(1));
}

habu_value_t APPLY_BUILTIN(habu_value_t FN, habu_value_t ARGS) {
    return (is_nil(SYM_EQ_(FN, habu_make_string("+", 1))) ? (is_nil(SYM_EQ_(FN, habu_make_string("*", 1))) ? (is_nil(SYM_EQ_(FN, habu_make_string("-", 1))) ? (is_nil(SYM_EQ_(FN, habu_make_string("cons", 4))) ? (is_nil(SYM_EQ_(FN, habu_make_string("car", 3))) ? (is_nil(SYM_EQ_(FN, habu_make_string("cdr", 3))) ? (is_nil(SYM_EQ_(FN, habu_make_string("list", 4))) ? NIL : ARGS) : habu_cdr(habu_car(ARGS))) : habu_car(habu_car(ARGS))) : habu_cons(habu_car(ARGS), habu_car(habu_cdr(ARGS)))) : (is_nil(habu_nil_p(habu_cdr(ARGS))) ? fixnum_to_value(value_to_fixnum(habu_car(ARGS)) - value_to_fixnum(APPLY_PLUS(habu_cdr(ARGS)))) : fixnum_to_value(value_to_fixnum(fixnum_to_value(0)) - value_to_fixnum(habu_car(ARGS))))) : APPLY_MULT(ARGS)) : APPLY_PLUS(ARGS));
}

habu_value_t EVAL(habu_value_t EXPR, habu_value_t ENV) {
    return (is_nil(habu_fixnum_p(EXPR)) ? (is_nil(habu_nil_p(EXPR)) ? (is_nil(habu_cons_p(EXPR)) ? EXPR : ({
    habu_value_t FIRST = habu_car(EXPR);
    ({
      habu_value_t ARGS = habu_cdr(EXPR);
      (is_nil(SYM_EQ_(FIRST, habu_make_string("quote", 5))) ? (is_nil(SYM_EQ_(FIRST, habu_make_string("if", 2))) ? (is_nil(SYM_EQ_(FIRST, habu_make_string("progn", 5))) ? APPLY_BUILTIN(FIRST, EVAL_LIST(ARGS, ENV)) : EVAL_PROGN(ARGS, ENV)) : (is_nil(habu_nil_p(EVAL(habu_car(ARGS), ENV))) ? EVAL(habu_car(habu_cdr(ARGS)), ENV) : EVAL(habu_car(habu_cdr(habu_cdr(ARGS))), ENV))) : habu_car(ARGS));
      });
    })) : NIL) : EXPR);
}

habu_value_t EVAL_PROGN(habu_value_t EXPRS, habu_value_t ENV) {
    return (is_nil(habu_nil_p(habu_cdr(EXPRS))) ? ({
  EVAL(habu_car(EXPRS), ENV);
  EVAL_PROGN(habu_cdr(EXPRS), ENV);
}) : EVAL(habu_car(EXPRS), ENV));
}

habu_value_t REPL_LOOP() {
    return ({
  habu_print_value(habu_make_string("Habu REPL - Written in Lisp!", 28));
  ({printf("\n"); NIL;});
  ({printf("\n"); NIL;});
  REPL_BODY(NIL);
});
}

habu_value_t REPL_BODY(habu_value_t ENV) {
    return ({
  habu_print_value(habu_make_string("habu> ", 6));
  ({
      habu_value_t LINE = (habu_value_t)habu_fgets_line();
      (is_nil(LINE) ? ({
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Bye!", 4));
  ({printf("\n"); NIL;});
}) : ({
  (is_nil((value_to_fixnum(fixnum_to_value((char*)LINE ? strlen((char*)LINE) : 0)) > value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL)) ? NIL : ({
          habu_value_t STR = ({char* s = (char*)LINE; s ? habu_make_string(s, strlen(s)) : NIL;});
          ({
            habu_value_t EXPR = READ_FROM_STRING(STR);
            ({
              habu_value_t RESULT = EVAL(EXPR, ENV);
              ({
  habu_print_value(RESULT);
  ({printf("\n"); NIL;});
});
              });
            });
          }));
  REPL_BODY(ENV);
}));
      });
});
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = REPL_LOOP();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
