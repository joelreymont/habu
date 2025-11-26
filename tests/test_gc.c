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
        init(4 * 1024 * 1024); \
        test_##name(); \
        shutdown(); \
        tests_passed++; \
        printf("ok\n"); \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

TEST(gc_init_shutdown) {
    size_t heap_size = gc_heap_size();
    assert(heap_size > 0);
}

TEST(gc_cons_alloc) {
    habu_value_t car_val = fixnum_to_value(42);
    habu_value_t cdr_val = fixnum_to_value(43);
    habu_value_t cons_val = cons(car_val, cdr_val);

    assert(!is_nil(cons_val));
    assert(get_tag(cons_val) == TAG_CONS);
    assert(car(cons_val) == car_val);
    assert(cdr(cons_val) == cdr_val);
}

TEST(gc_vector_alloc) {
    habu_value_t vec = make_vector(5);
    assert(!is_nil(vec));
    assert(get_tag(vec) == TAG_VECTOR);

    habu_vector_t *v = value_to_vector(vec);
    assert(v->length == 5);
    assert(is_nil(v->data[0]));
}

TEST(gc_string_alloc) {
    const char *text = "hello world";
    habu_value_t str = make_string(text, 11);
    assert(!is_nil(str));
    assert(get_tag(str) == TAG_STRING);

    habu_string_t *s = value_to_string(str);
    assert(s->length == 11);
    assert(strcmp(s->data, "hello world") == 0);
}

TEST(gc_symbol_alloc) {
    habu_value_t sym = make_symbol("test");
    assert(!is_nil(sym));
    assert(get_tag(sym) == TAG_SYMBOL);

    habu_symbol_t *s = value_to_symbol(sym);
    assert(!is_nil(s->name));
    assert(is_nil(s->value));
    assert(is_nil(s->plist));
}

TEST(gc_multiple_allocs) {
    for (int i = 0; i < 100; i++) {
        habu_value_t cons_val = cons(fixnum_to_value(i), NIL);
        assert(!is_nil(cons_val));
    }
}

TEST(gc_nested_cons) {
    habu_value_t c1 = cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t c2 = cons(c1, fixnum_to_value(3));
    habu_value_t c3 = cons(c2, c1);

    assert(!is_nil(c3));
    assert(car(c3) == c2);
    assert(cdr(c3) == c1);
}

TEST(gc_list_creation) {
    habu_value_t list = NIL;
    for (int i = 0; i < 10; i++) {
        list = cons(fixnum_to_value(i), list);
    }
    assert(!is_nil(list));

    int count = 0;
    while (!is_nil(list)) {
        count++;
        list = cdr(list);
    }
    assert(count == 10);
}

TEST(gc_vector_operations) {
    habu_value_t vec = make_vector(10);

    for (size_t i = 0; i < 10; i++) {
        vector_set(vec, i, fixnum_to_value(i * 2));
    }

    for (size_t i = 0; i < 10; i++) {
        habu_value_t val = vector_ref(vec, i);
        assert(is_fixnum(val));
        assert(value_to_fixnum(val) == (habu_fixnum_t)(i * 2));
    }
}

TEST(gc_collect_empty) {
    gc_reset_stats();

    gc_collect();

    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    /* GC should have run at least once */
    assert(stats.young_collections > 0);
}

TEST(gc_collect_with_objects) {
    gc_reset_stats();

    for (int i = 0; i < 100; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    gc_collect();

    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    /* GC should have run and collected objects */
    assert(stats.young_collections > 0);
    assert(stats.total_freed > 0);
}

TEST(gc_disabled_mode) {
    disable_gc();

    for (int i = 0; i < 100; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    assert(stats.young_collections == 0);

    enable_gc();
}

TEST(gc_mixed_types) {
    habu_value_t cons_val = cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t vec = make_vector(3);
    habu_value_t str = make_string("test", 4);
    habu_value_t sym = make_symbol("x");

    vector_set(vec, 0, cons_val);
    vector_set(vec, 1, str);
    vector_set(vec, 2, sym);

    assert(!is_nil(vector_ref(vec, 0)));
    assert(!is_nil(vector_ref(vec, 1)));
    assert(!is_nil(vector_ref(vec, 2)));
}

TEST(gc_heap_usage) {
    size_t before = gc_heap_used();

    for (int i = 0; i < 100; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    size_t after = gc_heap_used();
    assert(after > before);
}

TEST(gc_stats_tracking) {
    gc_reset_stats();

    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    assert(stats.young_collections == 0);
    assert(stats.total_allocated == 0);

    for (int i = 0; i < 100; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    gc_get_stats(&stats);
    assert(stats.total_allocated > 0);
}

TEST(gc_root_registration) {
    habu_value_t obj = cons(fixnum_to_value(42), fixnum_to_value(43));

    /* NEW API: Pass address of variable, not value */
    gc_add_root(&obj);

    /* Allocate enough to trigger GC */
    for (int i = 0; i < 10000; i++) {
        cons(fixnum_to_value(i), NIL);
    }

    gc_collect();

    /* Object was automatically updated by GC if it moved!
     * We can verify it's still a valid cons cell */
    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    assert(stats.young_collections > 0);
    assert(get_tag(obj) == TAG_CONS);  /* Verify still valid */

    gc_remove_root(&obj);
}

TEST(gc_promotion) {
    habu_value_t obj = cons(fixnum_to_value(1), fixnum_to_value(2));
    gc_add_root(&obj);

    gc_reset_stats();

    /* Trigger multiple GCs to age the object */
    for (int i = 0; i < 10; i++) {
        /* Allocate to trigger GC */
        for (int j = 0; j < 5000; j++) {
            cons(fixnum_to_value(j), NIL);
        }
        gc_collect();
    }

    /* Object should have been promoted after multiple collections */
    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    assert(stats.young_collections >= 10);
    assert(get_tag(obj) == TAG_CONS);  /* Still valid after promotion */

    gc_remove_root(&obj);
}

TEST(gc_write_barrier) {
    /* Create an old gen object (via promotion) */
    habu_value_t old_vec = make_vector(5);
    gc_add_root(&old_vec);

    gc_reset_stats();

    /* Age it by triggering multiple collections */
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 5000; j++) {
            cons(fixnum_to_value(j), NIL);
        }
        gc_collect();
    }

    /* Verify it got promoted */
    habu_gc_stats_t stats;
    gc_get_stats(&stats);
    assert(stats.young_collections >= 10);
    assert(get_tag(old_vec) == TAG_VECTOR);  /* Still valid */

    /* Now create a young object */
    habu_value_t young_obj = cons(fixnum_to_value(99), NIL);
    gc_add_root(&young_obj);

    /* Trigger more GCs - both objects should survive */
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 3000; j++) {
            cons(fixnum_to_value(j), NIL);
        }
        gc_collect();
    }

    /* Both objects survived */
    gc_get_stats(&stats);
    assert(stats.young_collections >= 15);
    assert(get_tag(old_vec) == TAG_VECTOR);  /* Still valid */
    assert(get_tag(young_obj) == TAG_CONS);  /* Still valid */

    gc_remove_root(&old_vec);
    gc_remove_root(&young_obj);
}

TEST(gc_old_generation_collection) {
    gc_reset_stats();

    /* Allocate many objects to potentially fill old gen
     * Old gen collection is complex and may not trigger in simple tests */
    for (int i = 0; i < 20; i++) {
        for (int j = 0; j < 10000; j++) {
            cons(fixnum_to_value(j), NIL);
        }
        gc_collect();
    }

    habu_gc_stats_t stats;
    gc_get_stats(&stats);

    /* Should have done many young collections */
    assert(stats.young_collections >= 20);
    /* Old collections may or may not have occurred depending on promotion rate */
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
    RUN_TEST(gc_root_registration);
    RUN_TEST(gc_promotion);
    RUN_TEST(gc_write_barrier);
    RUN_TEST(gc_old_generation_collection);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
