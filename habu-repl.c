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
habu_value_t EVAL_EXPR(habu_value_t);
habu_value_t IS_DIGIT_P(habu_value_t);
habu_value_t PARSE_NUMBER(habu_value_t, habu_value_t, habu_value_t);
habu_value_t READ_STR(habu_value_t);
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

habu_value_t EVAL_EXPR(habu_value_t EXPR) {
    return (is_nil(FIXNUM_P(EXPR)) ? (is_nil(NIL_P(EXPR)) ? (is_nil(CONS_P(EXPR)) ? EXPR : ({
    habu_value_t OP = habu_car(EXPR);
    ({
      habu_value_t ARGS = habu_cdr(EXPR);
      (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("+", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("-", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("*", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("/", 1)))) ? EXPR : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS))) / value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)))))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS))) * value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)))))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS))) - value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)))))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS))) + value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS))))));
      });
    })) : NIL) : EXPR);
}

habu_value_t IS_DIGIT_P(habu_value_t CH) {
    return (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(48)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(57)) ? fixnum_to_value(1) : NIL));
}

habu_value_t PARSE_NUMBER(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    return (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    (is_nil(IS_DIGIT_P(CH)) ? ACC : PARSE_NUMBER(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), fixnum_to_value(value_to_fixnum(fixnum_to_value(value_to_fixnum(ACC) * value_to_fixnum(fixnum_to_value(10)))) + value_to_fixnum(fixnum_to_value(value_to_fixnum(CH) - value_to_fixnum(fixnum_to_value(48)))))));
    }) : ACC);
}

habu_value_t READ_STR(habu_value_t STR) {
    return PARSE_NUMBER(STR, fixnum_to_value(0), fixnum_to_value(0));
}

habu_value_t REPL_START() {
    return ({
  habu_print_value(habu_make_string("Habu REPL with Line Editing", 27));
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
          habu_value_t NUM = READ_STR(STR);
          ({
            habu_value_t RESULT = EVAL_EXPR(NUM);
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
