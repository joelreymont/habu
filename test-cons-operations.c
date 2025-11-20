/* Test cons/car/cdr operations with C runtime integration
 * These operations need to call the C runtime for heap allocation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include "runtime/habu.h"

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Initialize runtime */
extern void habu_init(size_t heap_size);

/* Test helper to execute machine code that can call C runtime */
typedef habu_value_t (*cons_fn_t)(void);

habu_value_t execute_cons_code(unsigned char *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    cons_fn_t fn = (cons_fn_t)mem;
    habu_value_t result = fn();

    munmap(mem, page_size);
    return result;
}

void test_cons_basic() {
    /* Test: (cons 1 2) → (1 . 2)
     * This requires:
     * 1. Tag fixnums 1 and 2 (1<<4=16, 2<<4=32)
     * 2. Call habu_cons(16, 32)
     * 3. Return the pointer (already tagged by habu_cons)
     */
    printf("Test 1: (cons 1 2)\n");

    /* We need to get the address of habu_cons at runtime */
    void *cons_addr = (void*)habu_cons;
    uint64_t cons_ptr = (uint64_t)cons_addr;

    /* For now, this test documents what code we NEED to generate
     * TODO: Actually generate this code from the compiler
     */
    printf("  habu_cons address: %p\n", cons_addr);

    /* Manually call to verify runtime works */
    habu_value_t result = habu_cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t car_result = habu_car(result);
    habu_value_t cdr_result = habu_cdr(result);

    printf("  cons result: %lld\n", result);
    printf("  car: %lld (expected: %lld)\n", value_to_fixnum(car_result), 1LL);
    printf("  cdr: %lld (expected: %lld)\n", value_to_fixnum(cdr_result), 2LL);

    if (value_to_fixnum(car_result) == 1 && value_to_fixnum(cdr_result) == 2) {
        printf("  ✅ PASS (runtime verified)\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_car_operation() {
    printf("Test 2: (car (cons 42 99)) → 42\n");

    habu_value_t cons_cell = habu_cons(fixnum_to_value(42), fixnum_to_value(99));
    habu_value_t result = habu_car(cons_cell);
    int64_t untagged = value_to_fixnum(result);

    printf("  Result: %lld (expected: 42)\n", untagged);
    if (untagged == 42) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_cdr_operation() {
    printf("Test 3: (cdr (cons 42 99)) → 99\n");

    habu_value_t cons_cell = habu_cons(fixnum_to_value(42), fixnum_to_value(99));
    habu_value_t result = habu_cdr(cons_cell);
    int64_t untagged = value_to_fixnum(result);

    printf("  Result: %lld (expected: 99)\n", untagged);
    if (untagged == 99) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_nested_cons() {
    printf("Test 4: (car (cons (cons 1 2) 3)) → (cons 1 2)\n");

    habu_value_t inner = habu_cons(fixnum_to_value(1), fixnum_to_value(2));
    habu_value_t outer = habu_cons(inner, fixnum_to_value(3));
    habu_value_t result = habu_car(outer);

    habu_value_t car_inner = habu_car(result);
    habu_value_t cdr_inner = habu_cdr(result);

    printf("  car of inner: %lld (expected: 1)\n", value_to_fixnum(car_inner));
    printf("  cdr of inner: %lld (expected: 2)\n", value_to_fixnum(cdr_inner));

    if (value_to_fixnum(car_inner) == 1 && value_to_fixnum(cdr_inner) == 2) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_list_construction() {
    printf("Test 5: (list 1 2 3) as (cons 1 (cons 2 (cons 3 nil)))\n");

    habu_value_t nil = 0;  /* nil is fixnum 0 */
    habu_value_t list = habu_cons(fixnum_to_value(1),
                          habu_cons(fixnum_to_value(2),
                            habu_cons(fixnum_to_value(3), nil)));

    habu_value_t first = habu_car(list);
    habu_value_t rest = habu_cdr(list);
    habu_value_t second = habu_car(rest);
    habu_value_t rest2 = habu_cdr(rest);
    habu_value_t third = habu_car(rest2);
    habu_value_t rest3 = habu_cdr(rest2);

    printf("  first: %lld (expected: 1)\n", value_to_fixnum(first));
    printf("  second: %lld (expected: 2)\n", value_to_fixnum(second));
    printf("  third: %lld (expected: 3)\n", value_to_fixnum(third));
    printf("  rest3 is nil: %s (expected: yes)\n", rest3 == 0 ? "yes" : "no");

    if (value_to_fixnum(first) == 1 &&
        value_to_fixnum(second) == 2 &&
        value_to_fixnum(third) == 3 &&
        rest3 == 0) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

int main() {
    printf("=== Cons/Car/Cdr Operations Tests ===\n\n");
    printf("NOTE: These tests verify C runtime works.\n");
    printf("TODO: Generate machine code that calls these functions.\n\n");

    /* Initialize runtime with 1MB heap */
    habu_init(1024 * 1024);

    test_cons_basic();
    test_car_operation();
    test_cdr_operation();
    test_nested_cons();
    test_list_construction();

    printf("All cons/car/cdr runtime tests passed! ✅\n");
    printf("\nNext step: Add code generation for cons/car/cdr to compiler.\n");
    return 0;
}
