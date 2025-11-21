/* Test each function individually */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

extern uint8_t bootstrap_add_code[];
extern uint8_t bootstrap_sub_code[];
extern uint8_t bootstrap_mul_code[];
extern uint8_t bootstrap_div_code[];

extern const size_t bootstrap_add_size;
extern const size_t bootstrap_sub_size;
extern const size_t bootstrap_mul_size;
extern const size_t bootstrap_div_size;

typedef int64_t (*binary_fn_t)(int64_t, int64_t);

int64_t execute_binary(const uint8_t *code, size_t size, int64_t arg1, int64_t arg2) {
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

    binary_fn_t fn = (binary_fn_t)mem;
    int64_t result = fn(arg1, arg2);

    munmap(mem, page_size);
    return result;
}

int64_t tag(int64_t n) { return n << 4; }
int64_t untag(int64_t n) { return n >> 4; }

int main(void) {
    printf("Testing each function individually...\n\n");

    printf("1. Testing add...\n");
    int64_t result = execute_binary(bootstrap_add_code, bootstrap_add_size, tag(5), tag(7));
    printf("   add(5, 7) = %lld (expected 12) %s\n\n",
           untag(result), untag(result) == 12 ? "✓" : "✗");

    printf("2. Testing sub...\n");
    result = execute_binary(bootstrap_sub_code, bootstrap_sub_size, tag(10), tag(3));
    printf("   sub(10, 3) = %lld (expected 7) %s\n\n",
           untag(result), untag(result) == 7 ? "✓" : "✗");

    printf("3. Testing mul...\n");
    result = execute_binary(bootstrap_mul_code, bootstrap_mul_size, tag(6), tag(7));
    printf("   mul(6, 7) = %lld (expected 42) %s\n\n",
           untag(result), untag(result) == 42 ? "✓" : "✗");

    printf("4. Testing div...\n");
    result = execute_binary(bootstrap_div_code, bootstrap_div_size, tag(20), tag(4));
    printf("   div(20, 4) = %lld (expected 5) %s\n\n",
           untag(result), untag(result) == 5 ? "✓" : "✗");

    printf("All tests complete!\n");
    return 0;
}
