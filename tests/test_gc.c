/* Garbage collector tests */

#include "../runtime/habu.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    static void test_##name(void); \
    static void run_test_##name(void) { \
        tests_run++; \
        printf("  %s... ", #name); \
        fflush(stdout); \
        habu_init(4 * 1024 * 1024); \
        test_##name(); \
        habu_shutdown(); \
        tests_passed++; \
        printf("ok\n"); \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

TEST(gc_init_shutdown) {
    size_t heap_size = habu_gc_heap_size();
    assert(heap_size > 0);
}

TEST(gc_cons_alloc) {
    habu_value_t car = fixnum_to_value(42);
    habu_value_t cdr = fixnum_to_value(43);
    habu_value_t cons = habu_cons(car, cdr);

    assert(!is_nil(cons));
    assert(get_tag(cons) == TAG_CONS);
    assert(habu_car(cons) == car);
    assert(habu_cdr(cons) == cdr);
}

TEST(gc_vector_alloc) {
    habu_value_t vec = habu_make_vector(5);
    assert(!is_nil(vec));
    assert(get_tag(vec) == TAG_VECTOR);

    habu_vector_t *v = value_to_vector(vec);
    assert(v->length == 5);
    assert(is_nil(v->data[0]));
}

TEST(gc_string_alloc) {
    const char *text = "hello world";
    habu_value_t str = habu_make_string(text, 11);
    assert(!is_nil(str));
    assert(get_tag(str) == TAG_STRING);

    habu_string_t *s = value_to_string(str);
    assert(s->length == 11);
    assert(strcmp(s->data, "hello world") == 0);
}

TEST(gc_symbol_alloc) {
    habu_value_t sym = habu_make_symbol("test");
    assert(!is_nil(sym));
    assert(get_tag(sym) == TAG_SYMBOL);

    habu_symbol_t *s = value_to_symbol(sym);
    assert(!is_nil(s->name));
    assert(is_nil(s->value));
    assert(is_nil(s->plist));
}

TEST(gc_multiple_allocs) {
    for (int i = 0; i < 100; i++) {
        habu_value_t cons = habu_cons(fixnum_to_value(i), NIL);
        assert(!is_nil(cons));
    }
}

TEST(gc_nested_cons) {
    habu_value_t c1 = habu_cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t c2 = habu_cons(c1, fixnum_to_value(3));
    habu_value_t c3 = habu_cons(c2, c1);

    assert(!is_nil(c3));
    assert(habu_car(c3) == c2);
    assert(habu_cdr(c3) == c1);
}

TEST(gc_list_creation) {
    habu_value_t list = NIL;
    for (int i = 0; i < 10; i++) {
        list = habu_cons(fixnum_to_value(i), list);
    }
    assert(!is_nil(list));

    int count = 0;
    while (!is_nil(list)) {
        count++;
        list = habu_cdr(list);
    }
    assert(count == 10);
}

TEST(gc_vector_operations) {
    habu_value_t vec = habu_make_vector(10);

    for (size_t i = 0; i < 10; i++) {
        habu_vector_set(vec, i, fixnum_to_value(i * 2));
    }

    for (size_t i = 0; i < 10; i++) {
        habu_value_t val = habu_vector_ref(vec, i);
        assert(is_fixnum(val));
        assert(value_to_fixnum(val) == (habu_fixnum_t)(i * 2));
    }
}

TEST(gc_collect_empty) {
    habu_gc_collect();

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    assert(stats.last_pause_ns > 0);
}

TEST(gc_collect_with_objects) {
    for (int i = 0; i < 100; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    habu_gc_collect();

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    assert(stats.last_pause_ns > 0);
}

TEST(gc_disabled_mode) {
    habu_disable_gc();

    for (int i = 0; i < 100; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    assert(stats.young_collections == 0);

    habu_enable_gc();
}

TEST(gc_mixed_types) {
    habu_value_t cons = habu_cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t vec = habu_make_vector(3);
    habu_value_t str = habu_make_string("test", 4);
    habu_value_t sym = habu_make_symbol("x");

    habu_vector_set(vec, 0, cons);
    habu_vector_set(vec, 1, str);
    habu_vector_set(vec, 2, sym);

    assert(!is_nil(habu_vector_ref(vec, 0)));
    assert(!is_nil(habu_vector_ref(vec, 1)));
    assert(!is_nil(habu_vector_ref(vec, 2)));
}

TEST(gc_heap_usage) {
    size_t before = habu_gc_heap_used();

    for (int i = 0; i < 100; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    size_t after = habu_gc_heap_used();
    assert(after > before);
}

TEST(gc_stats_tracking) {
    habu_gc_reset_stats();

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    assert(stats.young_collections == 0);
    assert(stats.total_allocated == 0);

    for (int i = 0; i < 100; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    habu_gc_get_stats(&stats);
    assert(stats.total_allocated > 0);
}

int main(void) {
    printf("Garbage collector tests:\n");

    RUN_TEST(gc_init_shutdown);
    RUN_TEST(gc_cons_alloc);
    RUN_TEST(gc_vector_alloc);
    RUN_TEST(gc_string_alloc);
    RUN_TEST(gc_symbol_alloc);
    RUN_TEST(gc_multiple_allocs);
    RUN_TEST(gc_nested_cons);
    RUN_TEST(gc_list_creation);
    RUN_TEST(gc_vector_operations);
    RUN_TEST(gc_collect_empty);
    RUN_TEST(gc_collect_with_objects);
    RUN_TEST(gc_disabled_mode);
    RUN_TEST(gc_mixed_types);
    RUN_TEST(gc_heap_usage);
    RUN_TEST(gc_stats_tracking);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
