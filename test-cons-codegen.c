/* Test cons code generation with actual BL to C runtime
 * This demonstrates what code the compiler needs to generate
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

typedef habu_value_t (*test_fn_t)(void);

habu_value_t execute_code_with_runtime(unsigned char *code, size_t size) {
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

    test_fn_t fn = (test_fn_t)mem;
    habu_value_t result = fn();

    munmap(mem, page_size);
    return result;
}

void test_cons_with_bl() {
    /* Test: (cons 1 2) using BL to habu_cons
     *
     * This is what the compiler needs to generate:
     * 1. Tag arguments: 1 << 4 = 16, 2 << 4 = 32
     * 2. Put args in x0, x1
     * 3. BL to habu_cons
     * 4. Result already in x0 (cons cell pointer)
     * 5. Return
     */
    printf("Test 1: (cons 1 2) with BL to habu_cons\n");

    /* Calculate BL offset to habu_cons
     * BL encoding: offset is signed 26-bit, in units of 4 bytes
     * offset = (target_addr - current_pc) / 4
     */
    void *cons_addr = (void*)habu_cons;
    printf("  habu_cons at: %p\n", cons_addr);

    /* We'll place code at arbitrary address, so calculate offset at runtime
     * For now, document what NEEDS to be generated
     */

    /* Expected code structure:
     *
     * prologue:
     *   stp x29, x30, [sp, #-16]!   ; Save frame pointer and link register
     *   mov x29, sp                  ; Set up frame pointer
     *
     * setup args:
     *   movz x0, #16                 ; First arg: 1 << 4
     *   movz x1, #32                 ; Second arg: 2 << 4
     *
     * call:
     *   bl <habu_cons_offset>        ; Call habu_cons, result in x0
     *
     * epilogue:
     *   mov sp, x29                  ; Restore stack pointer
     *   ldp x29, x30, [sp], #16      ; Restore frame pointer and link register
     *   ret                          ; Return (result already in x0)
     */

    printf("  This test documents the required code structure.\n");
    printf("  Actual BL offset calculation requires:\n");
    printf("    1. Knowing code placement address\n");
    printf("    2. Computing: (cons_addr - pc) / 4\n");
    printf("    3. Encoding as signed 26-bit value\n");
    printf("  \n");
    printf("  For self-hosting, we need to either:\n");
    printf("    a) Generate relocatable code with a linker, OR\n");
    printf("    b) Use JIT compilation with runtime address resolution\n");
    printf("  ✅ ARCHITECTURE UNDERSTOOD\n\n");
}

void test_cons_via_function_pointer() {
    /* Alternative: Use function pointers instead of BL
     * This is easier for JIT compilation
     *
     * 1. Load habu_cons address into register
     * 2. BLR (branch to register) instead of BL
     */
    printf("Test 2: (cons 1 2) with BLR via function pointer\n");

    /* Code structure:
     *
     * prologue:
     *   stp x29, x30, [sp, #-16]!
     *   mov x29, sp
     *
     * load function pointer:
     *   movz x2, #low16(habu_cons)
     *   movk x2, #mid16(habu_cons), lsl #16
     *   movk x2, #mid32(habu_cons), lsl #32
     *   movk x2, #high16(habu_cons), lsl #48
     *
     * setup args:
     *   movz x0, #16
     *   movz x1, #32
     *
     * call:
     *   blr x2                      ; Branch to address in x2
     *
     * epilogue:
     *   mov sp, x29
     *   ldp x29, x30, [sp], #16
     *   ret
     */

    uint64_t cons_ptr = (uint64_t)habu_cons;
    printf("  habu_cons address: 0x%llx\n", cons_ptr);
    printf("  \n");
    printf("  Using BLR approach:\n");
    printf("    - Load 64-bit address with movz/movk sequence\n");
    printf("    - Use BLR (branch to register)\n");
    printf("    - More flexible for JIT compilation\n");
    printf("    - No offset calculation needed\n");
    printf("  ✅ BLR APPROACH PREFERRED FOR JIT\n\n");
}

void test_car_cdr_similar() {
    printf("Test 3: car and cdr follow same pattern\n");
    printf("  \n");
    printf("  (car x):\n");
    printf("    1. x is already in x0 (tagged pointer)\n");
    printf("    2. BLR to habu_car\n");
    printf("    3. Result in x0\n");
    printf("  \n");
    printf("  (cdr x):\n");
    printf("    1. x is already in x0 (tagged pointer)\n");
    printf("    2. BLR to habu_cdr\n");
    printf("    3. Result in x0\n");
    printf("  \n");
    printf("  ✅ SIMPLE PATTERN ESTABLISHED\n\n");
}

int main() {
    printf("=== Cons Code Generation Strategy ===\n\n");

    /* Initialize runtime */
    habu_init(1024 * 1024);

    test_cons_with_bl();
    test_cons_via_function_pointer();
    test_car_cdr_similar();

    printf("=== Implementation Plan ===\n\n");
    printf("For the compiler (habu-arm64-codegen.lisp):\n\n");
    printf("1. Add ARM64 encoding functions:\n");
    printf("   - arm64-movk (move with keep for address loading)\n");
    printf("   - arm64-blr (branch to register)\n\n");
    printf("2. Add runtime function address table:\n");
    printf("   - Map function names to addresses\n");
    printf("   - Generated at compile time\n\n");
    printf("3. Extend codegen-expr to recognize cons/car/cdr:\n");
    printf("   - (cons a b): compile args, load addr, BLR\n");
    printf("   - (car x): compile x, load addr, BLR\n");
    printf("   - (cdr x): compile x, load addr, BLR\n\n");
    printf("4. Test with JIT execution:\n");
    printf("   - Generate code\n");
    printf("   - Execute with mmap/mprotect\n");
    printf("   - Verify results\n\n");
    printf("Estimated implementation time: 2-3 hours\n\n");

    return 0;
}
