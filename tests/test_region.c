/* Region allocator tests */

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
        test_##name(); \
        tests_passed++; \
        printf("ok\n"); \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

TEST(region_create_destroy) {
    habu_region_t *region = region_create(1024);
    assert(region != NULL);
    assert(region_used(region) == 0);
    region_destroy(region);
}

TEST(region_alloc_basic) {
    habu_region_t *region = region_create(1024);
    void *p1 = region_alloc(region, 16);
    assert(p1 != NULL);
    assert(region_used(region) == 16);

    void *p2 = region_alloc(region, 32);
    assert(p2 != NULL);
    assert(region_used(region) == 48);

    region_destroy(region);
}

TEST(region_alloc_aligned) {
    habu_region_t *region = region_create(1024);

    void *p1 = region_alloc(region, 1);
    assert(((uintptr_t)p1 & 15) == 0);
    assert(region_used(region) == 16);

    void *p2 = region_alloc(region, 17);
    assert(((uintptr_t)p2 & 15) == 0);
    assert(region_used(region) == 48);

    region_destroy(region);
}

TEST(region_exhaustion) {
    habu_region_t *region = region_create(64);

    void *p1 = region_alloc(region, 32);
    assert(p1 != NULL);

    void *p2 = region_alloc(region, 32);
    assert(p2 != NULL);

    void *p3 = region_alloc(region, 1);
    assert(p3 == NULL);

    region_destroy(region);
}

TEST(region_reset) {
    habu_region_t *region = region_create(1024);

    region_alloc(region, 100);
    assert(region_used(region) > 0);

    region_reset(region);
    assert(region_used(region) == 0);

    void *p = region_alloc(region, 50);
    assert(p != NULL);
    assert(region_used(region) == 64);

    region_destroy(region);
}

TEST(region_cons_basic) {
    habu_region_t *region = region_create(1024);

    habu_value_t car_val = fixnum_to_value(42);
    habu_value_t cdr_val = fixnum_to_value(43);
    habu_value_t cons_val = region_cons(region, car_val, cdr_val);

    assert(!is_nil(cons_val));
    assert(get_tag(cons_val) == TAG_CONS);
    assert(car(cons_val) == car_val);
    assert(cdr(cons_val) == cdr_val);

    region_destroy(region);
}

TEST(region_cons_nested) {
    habu_region_t *region = region_create(1024);

    habu_value_t a = fixnum_to_value(1);
    habu_value_t b = fixnum_to_value(2);
    habu_value_t c = fixnum_to_value(3);

    habu_value_t cons1 = region_cons(region, a, b);
    habu_value_t cons2 = region_cons(region, cons1, c);

    assert(!is_nil(cons2));
    assert(car(cons2) == cons1);
    assert(cdr(cons2) == c);
    assert(car(car(cons2)) == a);

    region_destroy(region);
}

TEST(region_vector_basic) {
    habu_region_t *region = region_create(1024);

    habu_value_t vec = region_make_vector(region, 3);
    assert(!is_nil(vec));
    assert(get_tag(vec) == TAG_VECTOR);

    habu_vector_t *v = value_to_vector(vec);
    assert(v->length == 3);
    assert(is_nil(v->data[0]));
    assert(is_nil(v->data[1]));
    assert(is_nil(v->data[2]));

    region_destroy(region);
}

TEST(region_vector_set_ref) {
    habu_region_t *region = region_create(1024);

    habu_value_t vec = region_make_vector(region, 5);
    habu_value_t val = fixnum_to_value(99);

    vector_set(vec, 2, val);
    assert(vector_ref(vec, 2) == val);
    assert(is_nil(vector_ref(vec, 0)));

    region_destroy(region);
}

TEST(region_string_basic) {
    habu_region_t *region = region_create(1024);

    const char *text = "hello";
    habu_value_t str = region_make_string(region, text, strlen(text));
    assert(!is_nil(str));
    assert(get_tag(str) == TAG_STRING);

    habu_string_t *s = value_to_string(str);
    assert(s->length == 5);
    assert(strcmp(s->data, "hello") == 0);

    region_destroy(region);
}

TEST(region_string_empty) {
    habu_region_t *region = region_create(1024);

    habu_value_t str = region_make_string(region, "", 0);
    assert(!is_nil(str));

    habu_string_t *s = value_to_string(str);
    assert(s->length == 0);
    assert(s->data[0] == '\0');

    region_destroy(region);
}

TEST(region_mixed_allocation) {
    habu_region_t *region = region_create(4096);

    habu_value_t cons_val = region_cons(region, fixnum_to_value(1), NIL);
    habu_value_t vec = region_make_vector(region, 10);
    habu_value_t str = region_make_string(region, "test", 4);

    assert(!is_nil(cons_val));
    assert(!is_nil(vec));
    assert(!is_nil(str));

    assert(get_tag(cons_val) == TAG_CONS);
    assert(get_tag(vec) == TAG_VECTOR);
    assert(get_tag(str) == TAG_STRING);

    region_destroy(region);
}

int main(void) {
    printf("Region allocator tests:\n");

    RUN_TEST(region_create_destroy);
    RUN_TEST(region_alloc_basic);
    RUN_TEST(region_alloc_aligned);
    RUN_TEST(region_exhaustion);
    RUN_TEST(region_reset);
    RUN_TEST(region_cons_basic);
    RUN_TEST(region_cons_nested);
    RUN_TEST(region_vector_basic);
    RUN_TEST(region_vector_set_ref);
    RUN_TEST(region_string_basic);
    RUN_TEST(region_string_empty);
    RUN_TEST(region_mixed_allocation);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
