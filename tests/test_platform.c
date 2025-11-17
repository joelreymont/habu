/* Platform verification tests */

#include "../runtime/habu.h"
#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <limits.h>

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

TEST(platform_info) {
    printf("\n");
    printf("    Platform: ");
#if defined(__x86_64__) || defined(_M_X64)
    printf("x86_64");
#elif defined(__aarch64__) || defined(_M_ARM64)
    printf("ARM64");
#elif defined(__i386__) || defined(_M_IX86)
    printf("x86");
#elif defined(__arm__) || defined(_M_ARM)
    printf("ARM32");
#else
    printf("unknown");
#endif
    printf("\n");

    printf("    OS: ");
#if defined(__linux__)
    printf("Linux");
#elif defined(__APPLE__)
    printf("macOS");
#elif defined(_WIN32)
    printf("Windows");
#else
    printf("unknown");
#endif
    printf("\n");

    printf("    Compiler: ");
#if defined(__clang__)
    printf("Clang %d.%d.%d", __clang_major__, __clang_minor__, __clang_patchlevel__);
#elif defined(__GNUC__)
    printf("GCC %d.%d.%d", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
#else
    printf("unknown");
#endif
    printf("\n");

    printf("    Pointer size: %zu bytes\n", sizeof(void*));
    printf("    ");
}

TEST(type_sizes) {
    assert(sizeof(habu_value_t) == 8);
    assert(sizeof(habu_fixnum_t) == 8);
    assert(sizeof(habu_header_t) == 16);
    assert(sizeof(void*) >= 4);
}

TEST(alignment) {
    assert(sizeof(habu_header_t) == 16);
    assert((sizeof(habu_header_t) & 15) == 0);
}

TEST(pointer_tagging) {
    void *ptr = (void*)0x1000;
    habu_value_t tagged = tag_pointer(ptr, TAG_CONS);

    assert(get_tag(tagged) == TAG_CONS);
    assert(untag_pointer(tagged) == ptr);
}

TEST(fixnum_range) {
    habu_value_t v1 = fixnum_to_value(0);
    assert(is_fixnum(v1));
    assert(value_to_fixnum(v1) == 0);

    habu_value_t v2 = fixnum_to_value(42);
    assert(is_fixnum(v2));
    assert(value_to_fixnum(v2) == 42);

    habu_value_t v3 = fixnum_to_value(-42);
    assert(is_fixnum(v3));
    assert(value_to_fixnum(v3) == -42);

    habu_fixnum_t max = (1LL << 59) - 1;
    habu_value_t v4 = fixnum_to_value(max);
    assert(is_fixnum(v4));
    assert(value_to_fixnum(v4) == max);

    habu_fixnum_t min = -(1LL << 59);
    habu_value_t v5 = fixnum_to_value(min);
    assert(is_fixnum(v5));
    assert(value_to_fixnum(v5) == min);
}

TEST(nil_representation) {
    assert(is_nil(NIL));
    assert(NIL == fixnum_to_value(0));
}

TEST(endianness) {
    union {
        uint32_t i;
        uint8_t c[4];
    } test = { 0x01020304 };

    int is_little_endian = (test.c[0] == 0x04);

    printf("\n    Endianness: %s\n    ",
           is_little_endian ? "little" : "big");
}

TEST(clock_resolution) {
    uint64_t t1 = habu_time_ns();
    uint64_t t2 = habu_time_ns();
    uint64_t t3 = habu_time_ns();

    assert(t2 >= t1);
    assert(t3 >= t2);

    printf("\n    Clock resolution: ~%lu ns\n    ", (unsigned long)(t3 - t1));
}

TEST(memory_allocation) {
    habu_region_t *region = habu_region_create(4096);
    assert(region != NULL);

    void *p1 = habu_region_alloc(region, 16);
    assert(p1 != NULL);
    assert(((uintptr_t)p1 & 15) == 0);

    void *p2 = habu_region_alloc(region, 1);
    assert(p2 != NULL);
    assert(((uintptr_t)p2 & 15) == 0);

    habu_region_destroy(region);
}

TEST(gc_initialization) {
    habu_init(1024 * 1024);

    size_t heap_size = habu_gc_heap_size();
    assert(heap_size > 0);

    habu_shutdown();
}

int main(void) {
    printf("Platform verification tests:\n");

    RUN_TEST(platform_info);
    RUN_TEST(type_sizes);
    RUN_TEST(alignment);
    RUN_TEST(pointer_tagging);
    RUN_TEST(fixnum_range);
    RUN_TEST(nil_representation);
    RUN_TEST(endianness);
    RUN_TEST(clock_resolution);
    RUN_TEST(memory_allocation);
    RUN_TEST(gc_initialization);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
