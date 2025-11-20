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
habu_value_t MAKE_STRING_FROM_CHARS(habu_value_t);
habu_value_t LIST_LENGTH(habu_value_t, habu_value_t);
habu_value_t FILL_VEC(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_NUM(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_SYM(habu_value_t, habu_value_t);
habu_value_t PARSE_STRING_HELPER(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_STRING(habu_value_t, habu_value_t);
habu_value_t PARSE_LIST(habu_value_t, habu_value_t, habu_value_t);
habu_value_t PARSE_ONE(habu_value_t, habu_value_t);

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

habu_value_t MAKE_STRING_FROM_CHARS(habu_value_t CHARS) {
    habu_gc_add_root(&CHARS);
    habu_value_t __result = habu_make_string("Create Lisp string from list of character codes", 47);
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

habu_value_t PARSE_STRING_HELPER(habu_value_t STR, habu_value_t IDX, habu_value_t ACC) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_gc_add_root(&ACC);
    habu_value_t __result = (is_nil((value_to_fixnum(IDX) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
    habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX));
    habu_gc_add_root(&CH);
    habu_value_t __let_result_96272 = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(34)) ? fixnum_to_value(1) : NIL)) ? PARSE_STRING_HELPER(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), habu_cons(CH, ACC)) : habu_cons(MAKE_STRING_FROM_CHARS(REVERSE_LIST(ACC)), fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1)))));
    habu_gc_remove_root(&CH);
    __let_result_96272;
    }) : habu_cons(NIL, IDX));
    habu_gc_remove_root(&ACC);
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

habu_value_t PARSE_STRING(habu_value_t STR, habu_value_t IDX) {
    habu_gc_add_root(&STR);
    habu_gc_add_root(&IDX);
    habu_value_t __result = PARSE_STRING_HELPER(STR, fixnum_to_value(value_to_fixnum(IDX) + value_to_fixnum(fixnum_to_value(1))), NIL);
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
    habu_value_t __let_result_76519 = (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      habu_gc_add_root(&CH);
      habu_value_t __let_result_5860 = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(41)) ? fixnum_to_value(1) : NIL)) ? ({
        habu_value_t ELEM_RESULT = PARSE_ONE(STR, IDX2);
        habu_gc_add_root(&ELEM_RESULT);
        habu_value_t __let_result_61273 = PARSE_LIST(STR, habu_cdr(ELEM_RESULT), habu_cons(habu_car(ELEM_RESULT), ACC));
        habu_gc_remove_root(&ELEM_RESULT);
        __let_result_61273;
        }) : habu_cons(REVERSE_LIST(ACC), fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1)))));
      habu_gc_remove_root(&CH);
      __let_result_5860;
      }) : habu_cons(REVERSE_LIST(ACC), IDX2));
    habu_gc_remove_root(&IDX2);
    __let_result_76519;
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
    habu_value_t __let_result_56058 = (is_nil((value_to_fixnum(IDX2) >= value_to_fixnum(fixnum_to_value(habu_string_length_raw(STR))) ? fixnum_to_value(1) : NIL)) ? ({
      habu_value_t CH = habu_string_ref(STR, value_to_fixnum(IDX2));
      habu_gc_add_root(&CH);
      habu_value_t __let_result_87201 = (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(40)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(39)) ? fixnum_to_value(1) : NIL)) ? (is_nil((value_to_fixnum(CH) == value_to_fixnum(fixnum_to_value(34)) ? fixnum_to_value(1) : NIL)) ? (is_nil(IS_DIGIT_P(CH)) ? (is_nil(IS_SYMBOL_START_P(CH)) ? habu_cons(NIL, IDX2) : PARSE_SYM(STR, IDX2)) : PARSE_NUM(STR, IDX2, fixnum_to_value(0))) : PARSE_STRING(STR, IDX2)) : ({
        habu_value_t QUOTED_RESULT = PARSE_ONE(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))));
        habu_gc_add_root(&QUOTED_RESULT);
        habu_value_t __let_result_38161 = habu_cons(habu_cons(habu_make_symbol_from_string(habu_make_string("quote", 5)), habu_cons(habu_car(QUOTED_RESULT), NIL)), habu_cdr(QUOTED_RESULT));
        habu_gc_remove_root(&QUOTED_RESULT);
        __let_result_38161;
        })) : PARSE_LIST(STR, fixnum_to_value(value_to_fixnum(IDX2) + value_to_fixnum(fixnum_to_value(1))), NIL));
      habu_gc_remove_root(&CH);
      __let_result_87201;
      }) : habu_cons(NIL, IDX2));
    habu_gc_remove_root(&IDX2);
    __let_result_56058;
    });
    habu_gc_remove_root(&IDX);
    habu_gc_remove_root(&STR);
    return __result;
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = NIL;
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
