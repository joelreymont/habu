/* Root system stress tests */

#include "../runtime/habu.h"
#include <stdio.h>
#include <assert.h>

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    static void test_##name(void); \
    static void run_test_##name(void) { \
        tests_run++; \
        printf("  %s... ", #name); \
        fflush(stdout); \
        test_##name(); \
        tests_passed++; \
        printf("ok\n"); \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

/* Test that rooted values survive heavy allocation */
TEST(root_survives_heavy_allocation) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT(obj, cons(fixnum_to_value(42), fixnum_to_value(43)));

    /* Allocate enough to trigger multiple GCs */
    for (int i = 0; i < 20000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* Verify obj is still valid */
    assert(get_tag(obj) == TAG_CONS);
    assert(value_to_fixnum(car(obj)) == 42);
    assert(value_to_fixnum(cdr(obj)) == 43);

    HABU_UNROOT(obj);
    shutdown();
}

/* Test multiple roots survive together */
TEST(multiple_roots_survive) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT2(obj1, cons(fixnum_to_value(1), NIL),
               obj2, cons(fixnum_to_value(2), NIL));

    /* Trigger GC */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* Both should survive */
    assert(get_tag(obj1) == TAG_CONS);
    assert(get_tag(obj2) == TAG_CONS);
    assert(value_to_fixnum(car(obj1)) == 1);
    assert(value_to_fixnum(car(obj2)) == 2);

    HABU_UNROOT2(obj1, obj2);
    shutdown();
}

/* Test building list with roots */
TEST(build_list_with_roots) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT(list, NIL);

    for (int i = 0; i < 100; i++) {
        HABU_ROOT(item, cons(fixnum_to_value(i), NIL));
        list = cons(item, list);
        HABU_UNROOT(item);
    }

    /* Verify list has 100 elements */
    int count = 0;
    habu_value_t curr = list;
    while (!is_nil(curr)) {
        count++;
        curr = cdr(curr);
    }
    assert(count == 100);

    HABU_UNROOT(list);
    shutdown();
}

/* Test nested function calls with roots */
TEST(nested_calls_with_roots) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT(str1, make_string("test1", 5));
    HABU_ROOT(str2, make_string("test2", 5));

    /* Make symbols (which internally allocate strings) */
    HABU_ROOT(sym1, make_symbol("symbol1"));
    HABU_ROOT(sym2, make_symbol("symbol2"));

    /* All should still be valid */
    assert(get_tag(str1) == TAG_STRING);
    assert(get_tag(str2) == TAG_STRING);
    assert(get_tag(sym1) == TAG_SYMBOL);
    assert(get_tag(sym2) == TAG_SYMBOL);

    HABU_UNROOT(sym2);
    HABU_UNROOT(sym1);
    HABU_UNROOT(str2);
    HABU_UNROOT(str1);
    shutdown();
}

/* Test root updates during GC */
TEST(roots_update_during_gc) {
    init(1024 * 1024);  /* 1MB heap */

    /* Create object in young gen */
    habu_value_t obj = cons(fixnum_to_value(99), NIL);

    gc_add_root(&obj);

    /* Trigger GC - obj might move */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* obj should still be valid (possibly with new address) */
    assert(get_tag(obj) == TAG_CONS);
    assert(value_to_fixnum(car(obj)) == 99);

    gc_remove_root(&obj);
    shutdown();
}

/* Test rooting parameters that get passed around */
TEST(root_parameters) {
    init(1024 * 1024);  /* 1MB heap */

    habu_value_t arg1 = cons(fixnum_to_value(1), NIL);
    habu_value_t arg2 = cons(fixnum_to_value(2), NIL);

    gc_add_root(&arg1);
    gc_add_root(&arg2);

    /* Use args while triggering GC */
    HABU_ROOT(result, NIL);
    for (int i = 0; i < 100; i++) {
        result = cons(arg1, result);
        result = cons(arg2, result);

        /* Trigger allocations */
        cons(fixnum_to_value(i), NIL);
    }

    /* Verify args still valid */
    assert(get_tag(arg1) == TAG_CONS);
    assert(get_tag(arg2) == TAG_CONS);
    assert(value_to_fixnum(car(arg1)) == 1);
    assert(value_to_fixnum(car(arg2)) == 2);

    HABU_UNROOT(result);
    gc_remove_root(&arg2);
    gc_remove_root(&arg1);
    shutdown();
}

/* Test that symbols with nested string allocation work */
TEST(symbol_nested_allocation) {
    init(1024 * 1024);  /* 1MB heap */

    /* This internally does: alloc symbol, then alloc string */
    HABU_ROOT(sym, make_symbol("test_symbol"));

    /* Trigger GC */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* Symbol should still be valid */
    assert(get_tag(sym) == TAG_SYMBOL);

    habu_value_t name = symbol_name(sym);
    assert(get_tag(name) == TAG_STRING);

    HABU_UNROOT(sym);
    shutdown();
}

/* Test vector with rooted elements */
TEST(vector_with_roots) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT(vec, make_vector(10));

    /* Fill with rooted objects */
    for (size_t i = 0; i < 10; i++) {
        HABU_ROOT(elem, cons(fixnum_to_value(i), NIL));
        vector_set(vec, i, elem);
        HABU_UNROOT(elem);
    }

    /* Trigger GC */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* Verify vector and elements still valid */
    assert(get_tag(vec) == TAG_VECTOR);
    for (size_t i = 0; i < 10; i++) {
        habu_value_t elem = vector_ref(vec, i);
        assert(get_tag(elem) == TAG_CONS);
        assert(value_to_fixnum(car(elem)) == (int64_t)i);
    }

    HABU_UNROOT(vec);
    shutdown();
}

/* Test that fixnums don't need rooting */
TEST(fixnums_no_root_needed) {
    init(1024 * 1024);  /* 1MB heap */

    habu_value_t num = fixnum_to_value(12345);

    /* Trigger heavy GC without rooting */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    /* Fixnum should still be valid (it's immediate) */
    assert(is_fixnum(num));
    assert(value_to_fixnum(num) == 12345);

    shutdown();
}

/* Test root churning performance */
TEST(root_churning_stress) {
    init(1024 * 1024);  /* 1MB heap */

    /* Churn roots many times */
    for (int i = 0; i < 1000; i++) {
        HABU_ROOT(obj, cons(fixnum_to_value(i), NIL));
        /* Do something with obj */
        car(obj);
        HABU_UNROOT(obj);
    }

    shutdown();
}

/* Test reusing root variable */
TEST(reuse_root_variable) {
    init(1024 * 1024);  /* 1MB heap */

    HABU_ROOT(item, NIL);

    for (int i = 0; i < 100; i++) {
        /* Reuse same root location */
        item = cons(fixnum_to_value(i), NIL);

        /* Trigger GC */
        for (int j = 0; j < 100; j++) {
            cons(fixnum_to_value(j), NIL);
        }

        /* item should still be valid */
        assert(get_tag(item) == TAG_CONS);
        assert(value_to_fixnum(car(item)) == i);
    }

    HABU_UNROOT(item);
    shutdown();
}

int main(void) {
    printf("Root system tests:\n");

    RUN_TEST(root_survives_heavy_allocation);
    RUN_TEST(multiple_roots_survive);
    RUN_TEST(build_list_with_roots);
    RUN_TEST(nested_calls_with_roots);
    RUN_TEST(roots_update_during_gc);
    RUN_TEST(root_parameters);
    RUN_TEST(symbol_nested_allocation);
    RUN_TEST(vector_with_roots);
    RUN_TEST(fixnums_no_root_needed);
    RUN_TEST(root_churning_stress);
    RUN_TEST(reuse_root_variable);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
