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
habu_value_t IS_WHITESPACE_P(habu_value_t);
habu_value_t IS_DIGIT_P(habu_value_t);
habu_value_t IS_ALPHA_P(habu_value_t);
habu_value_t IS_SYMBOL_CHAR_P(habu_value_t);
habu_value_t SKIP_WS(habu_value_t, habu_value_t);
habu_value_t AT_END_P(habu_value_t, habu_value_t);
habu_value_t PARSE_NUM(habu_value_t, habu_value_t, habu_value_t);
habu_value_t COLLECT_SYM_CHARS(habu_value_t, habu_value_t, habu_value_t);
habu_value_t REVERSE_LIST(habu_value_t);
habu_value_t REVERSE_HELPER(habu_value_t, habu_value_t);
habu_value_t CHARS_TO_STRING(habu_value_t);
habu_value_t LIST_LEN(habu_value_t, habu_value_t);
habu_value_t CHARS_TO_VEC(habu_value_t, habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_SYM(habu_value_t, habu_value_t);
habu_value_t PARSE_LIST(habu_value_t, habu_value_t);
habu_value_t PARSE_LIST_ELEMS(habu_value_t, habu_value_t, habu_value_t);
habu_value_t READ_ONE(habu_value_t, habu_value_t);
habu_value_t READ_STR(habu_value_t);
habu_value_t EVAL_EXPR(habu_value_t, habu_value_t);
habu_value_t EVAL_IF(habu_value_t, habu_value_t);
habu_value_t EVAL_APPLY(habu_value_t, habu_value_t, habu_value_t);
habu_value_t ENV_LOOKUP(habu_value_t, habu_value_t);
habu_value_t REPL_START();
habu_value_t REPL_LOOP();

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

habu_value_t IS_WHITESPACE_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(32)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(10)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(9)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(13)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t IS_DIGIT_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(48)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(57)) ? fixnum_to_value(1) : NIL));
}

habu_value_t IS_ALPHA_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(65)) ? fixnum_to_value(1) : NIL)) ? NIL : (is_nil((value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(90)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(97)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(122)) ? fixnum_to_value(1) : NIL)) : fixnum_to_value(1)));
}

habu_value_t IS_SYMBOL_CHAR_P(habu_value_t CH) {
    return (is_nil(IS_ALPHA_P(CH)) ? (is_nil(IS_DIGIT_P(CH)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(43)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(45)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(42)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(47)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(61)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(60)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(62)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(63)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(33)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
}

habu_value_t SKIP_WS(habu_value_t STR, habu_value_t IDX) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_WHITESPACE_P(CH)) ? IDX : SKIP_WS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1)))));
    }) : IDX);
}

habu_value_t AT_END_P(habu_value_t STR, habu_value_t IDX) {
    return (value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL);
}

habu_value_t PARSE_NUM(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    return (is_nil(AT_END_P(STR, IDX)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_DIGIT_P(CH)) ? habu_cons(ACC, IDX) : PARSE_NUM(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), fixnum_to_value(value_to_fixnum(fixnum_to_value(value_to_fixnum(ACC) * value_to_fixnum(fixnum_to_value(10)))) + value_to_fixnum(fixnum_to_value(value_to_fixnum(CH) - value_to_fixnum(fixnum_to_value(48)))))));
    }) : habu_cons(ACC, IDX));
}

habu_value_t COLLECT_SYM_CHARS(habu_value_t STR, habu_value_t IDX, habu_value_t CHARS) {
    return (is_nil(AT_END_P(STR, IDX)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_SYMBOL_CHAR_P(CH)) ? habu_cons(CHARS, IDX) : COLLECT_SYM_CHARS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), habu_cons(CH, CHARS)));
    }) : habu_cons(CHARS, IDX));
}

habu_value_t REVERSE_LIST(habu_value_t LST) {
    return REVERSE_HELPER(LST, NIL);
}

habu_value_t REVERSE_HELPER(habu_value_t LST, habu_value_t ACC) {
    return (is_nil(NIL_P(LST)) ? REVERSE_HELPER(habu_cdr(LST), habu_cons(habu_car(LST), ACC)) : ACC);
}

habu_value_t CHARS_TO_STRING(habu_value_t CHARS) {
    return ({
    habu_value_t LEN = LIST_LEN(CHARS, fixnum_to_value(0));
    CHARS_TO_VEC(CHARS, LEN, habu_make_vector(LEN), fixnum_to_value(0));
    });
}

habu_value_t LIST_LEN(habu_value_t LST, habu_value_t ACC) {
    return (is_nil(NIL_P(LST)) ? LIST_LEN(habu_cdr(LST), fixnum_to_value(value_to_fixnum(ACC) + value_to_fixnum(fixnum_to_value(1)))) : ACC);
}

habu_value_t CHARS_TO_VEC(habu_value_t CHARS, habu_value_t LEN, habu_value_t VEC, habu_value_t IDX) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(LEN) ? fixnum_to_value(1) : NIL)) ? ({
  ({habu_vector_set(VEC, value_to_fixnum(IDX), habu_car(CHARS)); NIL;});
  CHARS_TO_VEC(habu_cdr(CHARS), LEN, VEC, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))));
}) : VEC);
}

habu_value_t PARSE_SYM(habu_value_t STR, habu_value_t IDX) {
    return ({
    habu_value_t RESULT = COLLECT_SYM_CHARS(STR, IDX, NIL);
    ({
      habu_value_t CHARS = REVERSE_LIST(habu_car(RESULT));
      ({
        habu_value_t NEW_IDX = habu_cdr(RESULT);
        habu_cons(habu_make_symbol_from_string(habu_make_string("temp", 4)), NEW_IDX);
        });
      });
    });
}

habu_value_t PARSE_LIST(habu_value_t STR, habu_value_t IDX) {
    return ({
    habu_value_t IDX2 = SKIP_WS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))));
    PARSE_LIST_ELEMS(STR, IDX2, NIL);
    });
}

habu_value_t PARSE_LIST_ELEMS(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    return ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    (is_nil(AT_END_P(STR, IDX2)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(41)) ? fixnum_to_value(1) : NIL)) ? ({
        habu_value_t ELEM_RESULT = READ_ONE(STR, IDX2);
        ({
          habu_value_t ELEM = habu_car(ELEM_RESULT);
          ({
            habu_value_t NEW_IDX = habu_cdr(ELEM_RESULT);
            PARSE_LIST_ELEMS(STR, NEW_IDX, habu_cons(ELEM, ACC));
            });
          });
        }) : habu_cons(REVERSE_LIST(ACC), fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1)))));
      }) : habu_cons(REVERSE_LIST(ACC), IDX2));
    });
}

habu_value_t READ_ONE(habu_value_t STR, habu_value_t IDX) {
    return ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    (is_nil(AT_END_P(STR, IDX2)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(40)) ? fixnum_to_value(1) : NIL)) ? (is_nil(IS_DIGIT_P(CH)) ? (is_nil(IS_SYMBOL_CHAR_P(CH)) ? habu_cons(NIL, IDX2) : PARSE_SYM(STR, IDX2)) : PARSE_NUM(STR, IDX2, fixnum_to_value(0))) : PARSE_LIST(STR, IDX2));
      }) : habu_cons(NIL, IDX2));
    });
}

habu_value_t READ_STR(habu_value_t STR) {
    return habu_car(READ_ONE(STR, fixnum_to_value(0)));
}

habu_value_t EVAL_EXPR(habu_value_t EXPR, habu_value_t ENV) {
    return (is_nil(FIXNUM_P(EXPR)) ? (is_nil(NIL_P(EXPR)) ? (is_nil(SYMBOL_P(EXPR)) ? (is_nil(CONS_P(EXPR)) ? EXPR : ({
    habu_value_t FIRST = habu_car(EXPR);
    (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("quote", 5)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("if", 2)))) ? EVAL_APPLY(FIRST, habu_cdr(EXPR), ENV) : EVAL_IF(habu_cdr(EXPR), ENV)) : habu_car(habu_cdr(EXPR)));
    })) : ENV_LOOKUP(EXPR, ENV)) : NIL) : EXPR);
}

habu_value_t EVAL_IF(habu_value_t ARGS, habu_value_t ENV) {
    return ({
    habu_value_t TEST = EVAL_EXPR(habu_car(ARGS), ENV);
    (is_nil(NIL_P(TEST)) ? EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV) : EVAL_EXPR(habu_car(habu_cdr(habu_cdr(ARGS))), ENV));
    });
}

habu_value_t EVAL_APPLY(habu_value_t OP, habu_value_t ARGS, habu_value_t ENV) {
    return (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("+", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("-", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("*", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("/", 1)))) ? NIL : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) / value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) * value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) - value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) + value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV))));
}

habu_value_t ENV_LOOKUP(habu_value_t SYM, habu_value_t ENV) {
    return fixnum_to_value(0);
}

habu_value_t REPL_START() {
    return ({
  habu_print_value(habu_make_string("Habu REPL - Full S-expression Support", 37));
  ({printf("\n"); NIL;});
  REPL_LOOP();
});
}

habu_value_t REPL_LOOP() {
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
            habu_value_t RESULT = EVAL_EXPR(EXPR, NIL);
            ({
  habu_print_value(RESULT);
  ({printf("\n"); NIL;});
});
            });
          });
        });
  REPL_LOOP();
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
