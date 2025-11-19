#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t FIXNUM_P(habu_value_t);
habu_value_t CONS_P(habu_value_t);
habu_value_t SYMBOL_P(habu_value_t);
habu_value_t NIL_P(habu_value_t);
habu_value_t STRING_CMP_LOOP(habu_value_t, habu_value_t, habu_value_t, habu_value_t);
habu_value_t STRING_EQ_P(habu_value_t, habu_value_t);
habu_value_t SYMBOL_EQ_P(habu_value_t, habu_value_t);
habu_value_t EVAL(habu_value_t);
habu_value_t REPL();
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

habu_value_t NIL_P(habu_value_t X) {
    return (value_to_fixnum(X) == value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL);
}

habu_value_t STRING_CMP_LOOP(habu_value_t S1, habu_value_t S2, habu_value_t I, habu_value_t LEN) {
    return (is_nil((value_to_fixnum(I) >= value_to_fixnum(LEN) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(habu_string_ref(S1, value_to_fixnum(I))) == value_to_fixnum(habu_string_ref(S2, value_to_fixnum(I))) ? fixnum_to_value(1) : NIL)) ? NIL : STRING_CMP_LOOP(S1, S2, fixnum_to_value(value_to_fixnum(I) + value_to_fixnum(fixnum_to_value(1))), LEN)) : fixnum_to_value(1));
}

habu_value_t STRING_EQ_P(habu_value_t S1, habu_value_t S2) {
    return ({
    habu_value_t LEN1 = fixnum_to_value(habu_string_length_raw(S1));
    ({
      habu_value_t LEN2 = fixnum_to_value(habu_string_length_raw(S2));
      (is_nil((value_to_fixnum(LEN1) == value_to_fixnum(LEN2) ? fixnum_to_value(1) : NIL)) ? NIL : STRING_CMP_LOOP(S1, S2, fixnum_to_value(0), LEN1));
      });
    });
}

habu_value_t SYMBOL_EQ_P(habu_value_t S1, habu_value_t S2) {
    return STRING_EQ_P(habu_symbol_name(S1), habu_symbol_name(S2));
}

habu_value_t EVAL(habu_value_t EXPR) {
    return (is_nil(FIXNUM_P(EXPR)) ? (is_nil(NIL_P(EXPR)) ? (is_nil(CONS_P(EXPR)) ? EXPR : ({
    habu_value_t OP = habu_car(EXPR);
    ({
      habu_value_t ARGS = habu_cdr(EXPR);
      (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("+", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("*", 1)))) ? EXPR : fixnum_to_value(42)) : fixnum_to_value(value_to_fixnum(EVAL(habu_car(ARGS))) + value_to_fixnum(EVAL(habu_car(habu_cdr(ARGS))))));
      });
    })) : NIL) : EXPR);
}

habu_value_t REPL() {
    return ({
  habu_print_value(habu_make_string("Simple REPL", 11));
  ({printf("\n"); NIL;});
  REPL_LOOP();
});
}

habu_value_t REPL_LOOP() {
    return ({
  habu_print_value(habu_make_string("> ", 2));
  ({
      habu_value_t LINE = (habu_value_t)habu_fgets_line();
      (is_nil(LINE) ? ({
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Bye", 3));
  ({printf("\n"); NIL;});
}) : ({
  ({
          habu_value_t NUM = fixnum_to_value(42);
          ({
  habu_print_value(NUM);
  ({printf("\n"); NIL;});
});
          });
  REPL_LOOP();
}));
      });
});
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = REPL();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
