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
habu_value_t APPEND_ENV(habu_value_t, habu_value_t);
habu_value_t APPLY_LAMBDA(habu_value_t, habu_value_t, habu_value_t);
habu_value_t ENV_LOOKUP(habu_value_t, habu_value_t);
habu_value_t ENV_EXTEND(habu_value_t, habu_value_t, habu_value_t);
habu_value_t ENV_EXTEND_LIST(habu_value_t, habu_value_t, habu_value_t);
habu_value_t IS_DEFUN_P(habu_value_t);
habu_value_t EVAL_TOPLEVEL(habu_value_t, habu_value_t);
habu_value_t MAKE_INITIAL_ENV();
habu_value_t REPL_START();
habu_value_t REPL_LOOP(habu_value_t);

habu_value_t FIXNUM_P(habu_value_t X) {
    habu_gc_add_root(&X);
    habu_value_t __result = (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL);
    habu_gc_remove_root(&X);
    return __result;
}

habu_value_t CONS_P(habu_value_t X) {
    habu_gc_add_root(&X);
    habu_value_t __result = (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(1)) ? fixnum_to_value(1) : NIL);
    habu_gc_remove_root(&X);
    return __result;
}

habu_value_t SYMBOL_P(habu_value_t X) {
    habu_gc_add_root(&X);
    habu_value_t __result = (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(2)) ? fixnum_to_value(1) : NIL);
    habu_gc_remove_root(&X);
    return __result;
}

habu_value_t STRING_P(habu_value_t X) {
    habu_gc_add_root(&X);
    habu_value_t __result = (value_to_fixnum(habu_get_tag(X)) == value_to_fixnum(fixnum_to_value(4)) ? fixnum_to_value(1) : NIL);
    habu_gc_remove_root(&X);
    return __result;
}

habu_value_t NIL_P(habu_value_t X) {
    habu_gc_add_root(&X);
    habu_value_t __result = (value_to_fixnum(X) == value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL);
    habu_gc_remove_root(&X);
    return __result;
}

habu_value_t STR_CMP_LOOP(habu_value_t S1, habu_value_t S2, habu_value_t I, habu_value_t LEN) {
    habu_gc_add_root(&S1);
    habu_gc_add_root(&S2);
    habu_gc_add_root(&I);
    habu_gc_add_root(&LEN);
    habu_value_t __result = (is_nil((value_to_fixnum(I) >= value_to_fixnum(LEN) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(habu_string_ref(S1, value_to_fixnum(I))) == value_to_fixnum(habu_string_ref(S2, value_to_fixnum(I))) ? fixnum_to_value(1) : NIL)) ? NIL : STR_CMP_LOOP(S1, S2, fixnum_to_value(value_to_fixnum(I) + value_to_fixnum(fixnum_to_value(1))), LEN)) : fixnum_to_value(1));
    habu_gc_remove_root(&LEN);
    habu_gc_remove_root(&I);
    habu_gc_remove_root(&S2);
    habu_gc_remove_root(&S1);
    return __result;
}

habu_value_t STRING_EQ_P(habu_value_t S1, habu_value_t S2) {
    habu_gc_add_root(&S1);
    habu_gc_add_root(&S2);
    habu_value_t __result = ({
    habu_value_t LEN1 = fixnum_to_value(habu_string_length_raw(S1));
    habu_gc_add_root(&LEN1);
    habu_value_t __let_result_40694 = ({
      habu_value_t LEN2 = fixnum_to_value(habu_string_length_raw(S2));
      habu_gc_add_root(&LEN2);
      habu_value_t __let_result_73593 = (is_nil((value_to_fixnum(LEN1) == value_to_fixnum(LEN2) ? fixnum_to_value(1) : NIL)) ? NIL : STR_CMP_LOOP(S1, S2, fixnum_to_value(0), LEN1));
      habu_gc_remove_root(&LEN2);
      __let_result_73593;
      });
    habu_gc_remove_root(&LEN1);
    __let_result_40694;
    });
    habu_gc_remove_root(&S2);
    habu_gc_remove_root(&S1);
    return __result;
}

habu_value_t SYMBOL_EQ_P(habu_value_t S1, habu_value_t S2) {
    habu_gc_add_root(&S1);
    habu_gc_add_root(&S2);
    habu_value_t __result = STRING_EQ_P(habu_symbol_name(S1), habu_symbol_name(S2));
    habu_gc_remove_root(&S2);
    habu_gc_remove_root(&S1);
    return __result;
}

habu_value_t IS_DIGIT_P(habu_value_t CH) {
    habu_gc_add_root(&CH);
    habu_value_t __result = (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(48)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(57)) ? fixnum_to_value(1) : NIL));
    habu_gc_remove_root(&CH);
    return __result;
}

habu_value_t IS_ALPHA_P(habu_value_t CH) {
    habu_gc_add_root(&CH);
    habu_value_t __result = (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(65)) ? fixnum_to_value(1) : NIL)) ? NIL : (is_nil((value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(90)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) >= value_to_fixnum(fixnum_to_value(97)) ? fixnum_to_value(1) : NIL)) ? NIL : (value_to_fixnum(CH) <= value_to_fixnum(fixnum_to_value(122)) ? fixnum_to_value(1) : NIL)) : fixnum_to_value(1)));
    habu_gc_remove_root(&CH);
    return __result;
}

habu_value_t IS_SYMBOL_START_P(habu_value_t CH) {
    habu_gc_add_root(&CH);
    habu_value_t __result = (is_nil(IS_ALPHA_P(CH)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(43)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(45)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(42)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(47)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(61)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(60)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(62)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(63)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(33)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
    habu_gc_remove_root(&CH);
    return __result;
}

habu_value_t IS_SYMBOL_CHAR_P(habu_value_t CH) {
    habu_gc_add_root(&CH);
    habu_value_t __result = (is_nil(IS_SYMBOL_START_P(CH)) ? (is_nil(IS_DIGIT_P(CH)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1));
    habu_gc_remove_root(&CH);
    return __result;
}

habu_value_t IS_WHITESPACE_P(habu_value_t CH) {
    habu_gc_add_root(&CH);
    habu_value_t __result = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(32)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(10)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(9)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1)) : fixnum_to_value(1)) : fixnum_to_value(1));
    habu_gc_remove_root(&CH);
    return __result;
}

habu_value_t SKIP_WS(habu_value_t STR, habu_value_t IDX) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_value_t __result = (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    habu_gc_add_root(&CH);
    habu_value_t __let_result_13612 = (is_nil(IS_WHITESPACE_P(CH)) ? IDX : SKIP_WS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1)))));
    habu_gc_remove_root(&CH);
    __let_result_13612;
    }) : IDX);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t COLLECT_CHARS(habu_value_t STR, habu_value_t IDX, habu_value_t CHARS) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_gc_add_root(&CHARS);
    habu_value_t __result = (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    habu_gc_add_root(&CH);
    habu_value_t __let_result_65541 = (is_nil(IS_SYMBOL_CHAR_P(CH)) ? habu_cons(CHARS, IDX) : COLLECT_CHARS(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), habu_cons(CH, CHARS)));
    habu_gc_remove_root(&CH);
    __let_result_65541;
    }) : habu_cons(CHARS, IDX));
    habu_gc_remove_root(&CHARS);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t REVERSE_LIST(habu_value_t LST) {
    habu_gc_add_root(&LST);
    habu_value_t __result = REVERSE_HELPER(LST, NIL);
    habu_gc_remove_root(&LST);
    return __result;
}

habu_value_t REVERSE_HELPER(habu_value_t LST, habu_value_t ACC) {
    habu_gc_add_root(&LST);
    habu_gc_add_root(&ACC);
    habu_value_t __result = (is_nil(NIL_P(LST)) ? REVERSE_HELPER(habu_cdr(LST), habu_cons(habu_car(LST), ACC)) : ACC);
    habu_gc_remove_root(&ACC);
    habu_gc_remove_root(&LST);
    return __result;
}

habu_value_t MAKE_SYM_FROM_CHARS(habu_value_t CHARS) {
    habu_gc_add_root(&CHARS);
    habu_value_t __result = ({
    habu_value_t LEN = LIST_LENGTH(CHARS, fixnum_to_value(0));
    habu_gc_add_root(&LEN);
    habu_value_t __let_result_19386 = ({
      habu_value_t VEC = habu_make_vector(LEN);
      habu_gc_add_root(&VEC);
      habu_value_t __let_result_2347 = ({
  FILL_VEC(CHARS, VEC, fixnum_to_value(0));
  habu_make_symbol_from_string(habu_make_string_from_vector(VEC));
});
      habu_gc_remove_root(&VEC);
      __let_result_2347;
      });
    habu_gc_remove_root(&LEN);
    __let_result_19386;
    });
    habu_gc_remove_root(&CHARS);
    return __result;
}

habu_value_t LIST_LENGTH(habu_value_t LST, habu_value_t ACC) {
    habu_gc_add_root(&LST);
    habu_gc_add_root(&ACC);
    habu_value_t __result = (is_nil(NIL_P(LST)) ? LIST_LENGTH(habu_cdr(LST), fixnum_to_value(value_to_fixnum(ACC) + value_to_fixnum(fixnum_to_value(1)))) : ACC);
    habu_gc_remove_root(&ACC);
    habu_gc_remove_root(&LST);
    return __result;
}

habu_value_t FILL_VEC(habu_value_t CHARS, habu_value_t VEC, habu_value_t IDX) {
    habu_gc_add_root(&CHARS);
    habu_gc_add_root(&VEC);
    habu_gc_add_root(&IDX);
    habu_value_t __result = (is_nil(NIL_P(CHARS)) ? ({
  ({habu_vector_set(VEC, value_to_fixnum(IDX), habu_car(CHARS)); NIL;});
  FILL_VEC(habu_cdr(CHARS), VEC, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))));
}) : VEC);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&VEC);
    habu_gc_remove_root(&CHARS);
    return __result;
}

habu_value_t PARSE_NUM(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_gc_add_root(&ACC);
    habu_value_t __result = (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    habu_gc_add_root(&CH);
    habu_value_t __let_result_26723 = (is_nil(IS_DIGIT_P(CH)) ? habu_cons(ACC, IDX) : PARSE_NUM(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), fixnum_to_value(value_to_fixnum(fixnum_to_value(value_to_fixnum(ACC) * value_to_fixnum(fixnum_to_value(10)))) + value_to_fixnum(fixnum_to_value(value_to_fixnum(CH) - value_to_fixnum(fixnum_to_value(48)))))));
    habu_gc_remove_root(&CH);
    __let_result_26723;
    }) : habu_cons(ACC, IDX));
    habu_gc_remove_root(&ACC);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t PARSE_SYM(habu_value_t STR, habu_value_t IDX) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_value_t __result = ({
    habu_value_t RESULT = COLLECT_CHARS(STR, IDX, NIL);
    habu_gc_add_root(&RESULT);
    habu_value_t __let_result_42533 = ({
      habu_value_t CHARS = REVERSE_LIST(habu_car(RESULT));
      habu_gc_add_root(&CHARS);
      habu_value_t __let_result_27999 = habu_cons(MAKE_SYM_FROM_CHARS(CHARS), habu_cdr(RESULT));
      habu_gc_remove_root(&CHARS);
      __let_result_27999;
      });
    habu_gc_remove_root(&RESULT);
    __let_result_42533;
    });
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t PARSE_LIST(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_gc_add_root(&ACC);
    habu_value_t __result = ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    habu_gc_add_root(&IDX2);
    habu_value_t __let_result_96272 = (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      habu_gc_add_root(&CH);
      habu_value_t __let_result_76519 = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(41)) ? fixnum_to_value(1) : NIL)) ? ({
        habu_value_t ELEM_RESULT = PARSE_ONE(STR, IDX2);
        habu_gc_add_root(&ELEM_RESULT);
        habu_value_t __let_result_5860 = PARSE_LIST(STR, habu_cdr(ELEM_RESULT), habu_cons(habu_car(ELEM_RESULT), ACC));
        habu_gc_remove_root(&ELEM_RESULT);
        __let_result_5860;
        }) : habu_cons(REVERSE_LIST(ACC), fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1)))));
      habu_gc_remove_root(&CH);
      __let_result_76519;
      }) : habu_cons(REVERSE_LIST(ACC), IDX2));
    habu_gc_remove_root(&IDX2);
    __let_result_96272;
    });
    habu_gc_remove_root(&ACC);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t PARSE_ONE(habu_value_t STR, habu_value_t IDX) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_value_t __result = ({
    habu_value_t IDX2 = SKIP_WS(STR, IDX);
    habu_gc_add_root(&IDX2);
    habu_value_t __let_result_61273 = (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      habu_gc_add_root(&CH);
      habu_value_t __let_result_56058 = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(40)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(39)) ? fixnum_to_value(1) : NIL)) ? (is_nil(IS_DIGIT_P(CH)) ? (is_nil(IS_SYMBOL_START_P(CH)) ? habu_cons(NIL, IDX2) : PARSE_SYM(STR, IDX2)) : PARSE_NUM(STR, IDX2, fixnum_to_value(0))) : ({
        habu_value_t QUOTED_RESULT = PARSE_ONE(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))));
        habu_gc_add_root(&QUOTED_RESULT);
        habu_value_t __let_result_87201 = habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("quote", 5)), habu_cons(habu_car(QUOTED_RESULT), NIL)), habu_cdr(QUOTED_RESULT));
        habu_gc_remove_root(&QUOTED_RESULT);
        __let_result_87201;
        })) : PARSE_LIST(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))), NIL));
      habu_gc_remove_root(&CH);
      __let_result_56058;
      }) : habu_cons(NIL, IDX2));
    habu_gc_remove_root(&IDX2);
    __let_result_61273;
    });
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t READ_STR(habu_value_t STR) {
    habu_gc_add_root(&STR);
    habu_value_t __result = habu_car(PARSE_ONE(STR, fixnum_to_value(0)));
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t EVAL_EXPR(habu_value_t EXPR, habu_value_t ENV) {
    habu_gc_add_root(&EXPR);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(FIXNUM_P(EXPR)) ? (is_nil(NIL_P(EXPR)) ? (is_nil(SYMBOL_P(EXPR)) ? (is_nil(CONS_P(EXPR)) ? EXPR : ({
    habu_value_t FIRST = habu_car(EXPR);
    habu_gc_add_root(&FIRST);
    habu_value_t __let_result_38161 = (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("quote", 5)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("if", 2)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("let", 3)))) ? (is_nil(SYMBOL_EQ_P(FIRST, habu_make_symbol_from_string(habu_make_string("lambda", 6)))) ? EVAL_APPLY(FIRST, habu_cdr(EXPR), ENV) : habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(ENV, habu_cdr(EXPR)))) : EVAL_LET(habu_cdr(EXPR), ENV)) : EVAL_IF(habu_cdr(EXPR), ENV)) : habu_car(habu_cdr(EXPR)));
    habu_gc_remove_root(&FIRST);
    __let_result_38161;
    })) : ENV_LOOKUP(EXPR, ENV)) : NIL) : EXPR);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&EXPR);
    return __result;
}

habu_value_t EVAL_IF(habu_value_t ARGS, habu_value_t ENV) {
    habu_gc_add_root(&ARGS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = ({
    habu_value_t TEST = EVAL_EXPR(habu_car(ARGS), ENV);
    habu_gc_add_root(&TEST);
    habu_value_t __let_result_50355 = (is_nil(NIL_P(TEST)) ? EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV) : EVAL_EXPR(habu_car(habu_cdr(habu_cdr(ARGS))), ENV));
    habu_gc_remove_root(&TEST);
    __let_result_50355;
    });
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&ARGS);
    return __result;
}

habu_value_t EVAL_LET(habu_value_t ARGS, habu_value_t ENV) {
    habu_gc_add_root(&ARGS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = ({
    habu_value_t BINDINGS = habu_car(ARGS);
    habu_gc_add_root(&BINDINGS);
    habu_value_t __let_result_311 = ({
      habu_value_t BODY = habu_car(habu_cdr(ARGS));
      habu_gc_add_root(&BODY);
      habu_value_t __let_result_58538 = ({
        habu_value_t NEW_ENV = EVAL_BINDINGS(BINDINGS, ENV);
        habu_gc_add_root(&NEW_ENV);
        habu_value_t __let_result_43609 = EVAL_EXPR(BODY, NEW_ENV);
        habu_gc_remove_root(&NEW_ENV);
        __let_result_43609;
        });
      habu_gc_remove_root(&BODY);
      __let_result_58538;
      });
    habu_gc_remove_root(&BINDINGS);
    __let_result_311;
    });
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&ARGS);
    return __result;
}

habu_value_t EVAL_BINDINGS(habu_value_t BINDINGS, habu_value_t ENV) {
    habu_gc_add_root(&BINDINGS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(NIL_P(BINDINGS)) ? ({
    habu_value_t BINDING = habu_car(BINDINGS);
    habu_gc_add_root(&BINDING);
    habu_value_t __let_result_77404 = ({
      habu_value_t SYM = habu_car(BINDING);
      habu_gc_add_root(&SYM);
      habu_value_t __let_result_7995 = ({
        habu_value_t VAL_EXPR = habu_car(habu_cdr(BINDING));
        habu_gc_add_root(&VAL_EXPR);
        habu_value_t __let_result_43484 = ({
          habu_value_t VAL = EVAL_EXPR(VAL_EXPR, ENV);
          habu_gc_add_root(&VAL);
          habu_value_t __let_result_97677 = EVAL_BINDINGS(habu_cdr(BINDINGS), ENV_EXTEND(SYM, VAL, ENV));
          habu_gc_remove_root(&VAL);
          __let_result_97677;
          });
        habu_gc_remove_root(&VAL_EXPR);
        __let_result_43484;
        });
      habu_gc_remove_root(&SYM);
      __let_result_7995;
      });
    habu_gc_remove_root(&BINDING);
    __let_result_77404;
    }) : ENV);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&BINDINGS);
    return __result;
}

habu_value_t EVAL_LIST(habu_value_t EXPRS, habu_value_t ENV) {
    habu_gc_add_root(&EXPRS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(NIL_P(EXPRS)) ? habu_cons(EVAL_EXPR(habu_car(EXPRS), ENV), EVAL_LIST(habu_cdr(EXPRS), ENV)) : NIL);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&EXPRS);
    return __result;
}

habu_value_t EVAL_APPLY(habu_value_t OP, habu_value_t ARGS, habu_value_t ENV) {
    habu_gc_add_root(&OP);
    habu_gc_add_root(&ARGS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("+", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("-", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("*", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("/", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("=", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("<", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string(">", 1)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("cons", 4)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("car", 3)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("cdr", 3)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("list", 4)))) ? (is_nil(SYMBOL_EQ_P(OP, habu_make_symbol_from_string(habu_make_string("get-tag", 7)))) ? ({
    habu_value_t FN = EVAL_EXPR(OP, ENV);
    habu_gc_add_root(&FN);
    habu_value_t __let_result_35889 = (is_nil(CONS_P(FN)) ? NIL : (is_nil(SYMBOL_EQ_P(habu_car(FN), habu_make_symbol_from_string(habu_make_string("closure", 7)))) ? NIL : APPLY_LAMBDA(FN, EVAL_LIST(ARGS, ENV), ENV)));
    habu_gc_remove_root(&FN);
    __let_result_35889;
    }) : habu_get_tag(EVAL_EXPR(habu_car(ARGS), ENV))) : EVAL_LIST(ARGS, ENV)) : habu_cdr(EVAL_EXPR(habu_car(ARGS), ENV))) : habu_car(EVAL_EXPR(habu_car(ARGS), ENV))) : habu_cons(EVAL_EXPR(habu_car(ARGS), ENV), EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV))) : (is_nil((value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) > value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1))) : (is_nil((value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) < value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1))) : (is_nil((value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) == value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)) ? fixnum_to_value(1) : NIL)) ? NIL : fixnum_to_value(1))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) / value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) * value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) - value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV)))) : fixnum_to_value(value_to_fixnum(EVAL_EXPR(habu_car(ARGS), ENV)) + value_to_fixnum(EVAL_EXPR(habu_car(habu_cdr(ARGS)), ENV))));
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&ARGS);
    habu_gc_remove_root(&OP);
    return __result;
}

habu_value_t APPEND_ENV(habu_value_t ENV1, habu_value_t ENV2) {
    habu_gc_add_root(&ENV1);
    habu_gc_add_root(&ENV2);
    habu_value_t __result = (is_nil(NIL_P(ENV1)) ? habu_cons(habu_car(ENV1), APPEND_ENV(habu_cdr(ENV1), ENV2)) : ENV2);
    habu_gc_remove_root(&ENV2);
    habu_gc_remove_root(&ENV1);
    return __result;
}

habu_value_t APPLY_LAMBDA(habu_value_t CLOSURE, habu_value_t ARG_VALS, habu_value_t CURRENT_ENV) {
    habu_gc_add_root(&CLOSURE);
    habu_gc_add_root(&ARG_VALS);
    habu_gc_add_root(&CURRENT_ENV);
    habu_value_t __result = ({
    habu_value_t CLOSURE_ENV = habu_car(habu_cdr(CLOSURE));
    habu_gc_add_root(&CLOSURE_ENV);
    habu_value_t __let_result_31098 = ({
      habu_value_t PARAMS = habu_car(habu_cdr(habu_cdr(CLOSURE)));
      habu_gc_add_root(&PARAMS);
      habu_value_t __let_result_6505 = ({
        habu_value_t BODY = habu_car(habu_cdr(habu_cdr(habu_cdr(CLOSURE))));
        habu_gc_add_root(&BODY);
        habu_value_t __let_result_54445 = ({
          habu_value_t COMBINED_ENV = APPEND_ENV(CURRENT_ENV, CLOSURE_ENV);
          habu_gc_add_root(&COMBINED_ENV);
          habu_value_t __let_result_84907 = ({
            habu_value_t NEW_ENV = ENV_EXTEND_LIST(PARAMS, ARG_VALS, COMBINED_ENV);
            habu_gc_add_root(&NEW_ENV);
            habu_value_t __let_result_2440 = EVAL_EXPR(BODY, NEW_ENV);
            habu_gc_remove_root(&NEW_ENV);
            __let_result_2440;
            });
          habu_gc_remove_root(&COMBINED_ENV);
          __let_result_84907;
          });
        habu_gc_remove_root(&BODY);
        __let_result_54445;
        });
      habu_gc_remove_root(&PARAMS);
      __let_result_6505;
      });
    habu_gc_remove_root(&CLOSURE_ENV);
    __let_result_31098;
    });
    habu_gc_remove_root(&CURRENT_ENV);
    habu_gc_remove_root(&ARG_VALS);
    habu_gc_remove_root(&CLOSURE);
    return __result;
}

habu_value_t ENV_LOOKUP(habu_value_t SYM, habu_value_t ENV) {
    habu_gc_add_root(&SYM);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(NIL_P(ENV)) ? ({
    habu_value_t BINDING = habu_car(ENV);
    habu_gc_add_root(&BINDING);
    habu_value_t __let_result_40780 = (is_nil(SYMBOL_EQ_P(SYM, habu_car(BINDING))) ? ENV_LOOKUP(SYM, habu_cdr(ENV)) : habu_cdr(BINDING));
    habu_gc_remove_root(&BINDING);
    __let_result_40780;
    }) : NIL);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&SYM);
    return __result;
}

habu_value_t ENV_EXTEND(habu_value_t SYM, habu_value_t VAL, habu_value_t ENV) {
    habu_gc_add_root(&SYM);
    habu_gc_add_root(&VAL);
    habu_gc_add_root(&ENV);
    habu_value_t __result = habu_cons(habu_cons(SYM, VAL), ENV);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&VAL);
    habu_gc_remove_root(&SYM);
    return __result;
}

habu_value_t ENV_EXTEND_LIST(habu_value_t SYMS, habu_value_t VALS, habu_value_t ENV) {
    habu_gc_add_root(&SYMS);
    habu_gc_add_root(&VALS);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(NIL_P(SYMS)) ? ENV_EXTEND_LIST(habu_cdr(SYMS), habu_cdr(VALS), ENV_EXTEND(habu_car(SYMS), habu_car(VALS), ENV)) : ENV);
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&VALS);
    habu_gc_remove_root(&SYMS);
    return __result;
}

habu_value_t IS_DEFUN_P(habu_value_t EXPR) {
    habu_gc_add_root(&EXPR);
    habu_value_t __result = (is_nil(CONS_P(EXPR)) ? NIL : SYMBOL_EQ_P(habu_car(EXPR), habu_make_symbol_from_string(habu_make_string("defun", 5))));
    habu_gc_remove_root(&EXPR);
    return __result;
}

habu_value_t EVAL_TOPLEVEL(habu_value_t EXPR, habu_value_t ENV) {
    habu_gc_add_root(&EXPR);
    habu_gc_add_root(&ENV);
    habu_value_t __result = (is_nil(IS_DEFUN_P(EXPR)) ? habu_cons(EVAL_EXPR(EXPR, ENV), ENV) : ({
    habu_value_t NAME = habu_car(habu_cdr(EXPR));
    habu_gc_add_root(&NAME);
    habu_value_t __let_result_68794 = ({
      habu_value_t PARAMS = habu_car(habu_cdr(habu_cdr(EXPR)));
      habu_gc_add_root(&PARAMS);
      habu_value_t __let_result_26255 = ({
        habu_value_t BODY = habu_car(habu_cdr(habu_cdr(habu_cdr(EXPR))));
        habu_gc_add_root(&BODY);
        habu_value_t __let_result_35269 = ({
          habu_value_t CLOSURE = habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(ENV, habu_cons(PARAMS, habu_cons(BODY, NIL))));
          habu_gc_add_root(&CLOSURE);
          habu_value_t __let_result_44108 = habu_cons(NAME, ENV_EXTEND(NAME, CLOSURE, ENV));
          habu_gc_remove_root(&CLOSURE);
          __let_result_44108;
          });
        habu_gc_remove_root(&BODY);
        __let_result_35269;
        });
      habu_gc_remove_root(&PARAMS);
      __let_result_26255;
      });
    habu_gc_remove_root(&NAME);
    __let_result_68794;
    }));
    habu_gc_remove_root(&ENV);
    habu_gc_remove_root(&EXPR);
    return __result;
}

habu_value_t MAKE_INITIAL_ENV() {
    habu_value_t __result = ({
    habu_value_t ENV = NIL;
    habu_gc_add_root(&ENV);
    habu_value_t __let_result_11885 = ({
      habu_value_t ENV2 = ENV_EXTEND(habu_make_symbol_from_string(habu_make_string("fixnum?", 7)), habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(NIL, habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("=", 1)), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("get-tag", 7)), habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL)), habu_cons(fixnum_to_value(0), NIL))), NIL)))), ENV);
      habu_gc_add_root(&ENV2);
      habu_value_t __let_result_82370 = ({
        habu_value_t ENV3 = ENV_EXTEND(habu_make_symbol_from_string(habu_make_string("cons?", 5)), habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(NIL, habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("=", 1)), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("get-tag", 7)), habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL)), habu_cons(fixnum_to_value(1), NIL))), NIL)))), ENV2);
        habu_gc_add_root(&ENV3);
        habu_value_t __let_result_69325 = ({
          habu_value_t ENV4 = ENV_EXTEND(habu_make_symbol_from_string(habu_make_string("symbol?", 7)), habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(NIL, habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("=", 1)), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("get-tag", 7)), habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL)), habu_cons(fixnum_to_value(2), NIL))), NIL)))), ENV3);
          habu_gc_add_root(&ENV4);
          habu_value_t __let_result_64878 = ({
            habu_value_t ENV5 = ENV_EXTEND(habu_make_symbol_from_string(habu_make_string("nil?", 4)), habu_cons(habu_make_symbol_from_string(habu_make_string("closure", 7)), habu_cons(NIL, habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), NIL), habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("=", 1)), habu_cons(habu_make_symbol_from_string(habu_make_string("x", 1)), habu_cons(fixnum_to_value(0), NIL))), NIL)))), ENV4);
            habu_gc_add_root(&ENV5);
            habu_value_t __let_result_91700 = ENV5;
            habu_gc_remove_root(&ENV5);
            __let_result_91700;
            });
          habu_gc_remove_root(&ENV4);
          __let_result_64878;
          });
        habu_gc_remove_root(&ENV3);
        __let_result_69325;
        });
      habu_gc_remove_root(&ENV2);
      __let_result_82370;
      });
    habu_gc_remove_root(&ENV);
    __let_result_11885;
    });
    return __result;
}

habu_value_t REPL_START() {
    habu_value_t __result = ({
  habu_print_value(habu_make_string("Habu REPL - Recursive (Self-Hosting)", 36));
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Features: let, lambda, defun, fixnum?, cons?, symbol?, nil?", 59));
  ({printf("\n"); NIL;});
  REPL_LOOP(MAKE_INITIAL_ENV());
});
    return __result;
}

habu_value_t REPL_LOOP(habu_value_t ENV) {
    habu_gc_add_root(&ENV);
    habu_value_t __result = ({
    habu_value_t LINE = (habu_value_t)lineedit_readline("habu> ");
    habu_gc_add_root(&LINE);
    habu_value_t __let_result_28043 = (is_nil(LINE) ? ({
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Goodbye!", 8));
  ({printf("\n"); NIL;});
}) : ({
  ({
        habu_value_t STR = ({char* s = (char*)LINE; s ? habu_make_string(s, strlen(s)) : NIL;});
        habu_gc_add_root(&STR);
        habu_value_t __let_result_58386 = ({
          habu_value_t EXPR = READ_STR(STR);
          habu_gc_add_root(&EXPR);
          habu_value_t __let_result_11710 = ({
            habu_value_t RESULT_ENV = EVAL_TOPLEVEL(EXPR, ENV);
            habu_gc_add_root(&RESULT_ENV);
            habu_value_t __let_result_94119 = ({
              habu_value_t RESULT = habu_car(RESULT_ENV);
              habu_gc_add_root(&RESULT);
              habu_value_t __let_result_35161 = ({
                habu_value_t NEW_ENV = habu_cdr(RESULT_ENV);
                habu_gc_add_root(&NEW_ENV);
                habu_value_t __let_result_12930 = ({
  habu_print_value(RESULT);
  ({printf("\n"); NIL;});
  REPL_LOOP(NEW_ENV);
});
                habu_gc_remove_root(&NEW_ENV);
                __let_result_12930;
                });
              habu_gc_remove_root(&RESULT);
              __let_result_35161;
              });
            habu_gc_remove_root(&RESULT_ENV);
            __let_result_94119;
            });
          habu_gc_remove_root(&EXPR);
          __let_result_11710;
          });
        habu_gc_remove_root(&STR);
        __let_result_58386;
        });
  NIL;
}));
    habu_gc_remove_root(&LINE);
    __let_result_28043;
    });
    habu_gc_remove_root(&ENV);
    return __result;
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
