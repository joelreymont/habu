/* Test compiler pipeline: Habu -> IR -> ARM64 machine code */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

/* Execute machine code bytes */
int64_t execute_bytes(unsigned char *code, size_t len) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, len);
    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);
    return result;
}

/* Convert Habu list to byte array */
int list_to_bytes(FILE *pipe, unsigned char *buffer, size_t max_size) {
    char line[4096];
    int count = 0;
    int in_list = 0;

    while (fgets(line, sizeof(line), pipe)) {
        /* Look for list output like (123 45 67 ...) */
        char *p = line;
        while (*p) {
            if (*p == '(') {
                in_list = 1;
                p++;
                continue;
            }
            if (*p == ')') {
                in_list = 0;
                break;
            }
            if (in_list && (*p >= '0' && *p <= '9')) {
                int value = atoi(p);
                if (count < max_size) {
                    buffer[count++] = (unsigned char)value;
                }
                /* Skip to next space or closing paren */
                while (*p && *p != ' ' && *p != ')') p++;
                continue;
            }
            p++;
        }
        if (!in_list && count > 0) break;
    }

    return count;
}

/* Run Habu compiler and get machine code */
int compile_and_get_bytes(const char *expr, unsigned char *buffer, size_t max_size) {
    char cmd[1024];
    snprintf(cmd, sizeof(cmd),
             "./habu <<'EOF'\n"
             "(load \"habu-arm64-codegen.lisp\")\n"
             "(print (compile-to-arm64 %s))\n"
             "EOF\n", expr);

    FILE *pipe = popen(cmd, "r");
    if (!pipe) {
        perror("popen");
        return -1;
    }

    int count = list_to_bytes(pipe, buffer, max_size);
    int status = pclose(pipe);

    if (status != 0) {
        fprintf(stderr, "Compiler failed with status %d\n", status);
        return -1;
    }

    return count;
}

int main() {
    unsigned char code_buffer[4096];
    int pass = 0, fail = 0;

    printf("========================================\n");
    printf("Compiler Pipeline Tests\n");
    printf("========================================\n");

    /* Test 1: Simple literal */
    printf("\n=== Test 1: 42 ===\n");
    {
        int len = compile_and_get_bytes("42", code_buffer, sizeof(code_buffer));
        if (len > 0) {
            printf("Generated %d bytes\n", len);
            int64_t result = execute_bytes(code_buffer, len);
            printf("Result: %lld ", result);
            if (result == 42) {
                printf("✓ PASS\n");
                pass++;
            } else {
                printf("✗ FAIL\n");
                fail++;
            }
        } else {
            printf("✗ FAIL - compilation failed\n");
            fail++;
        }
    }

    /* Test 2: Addition */
    printf("\n=== Test 2: (+ 2 3) ===\n");
    {
        int len = compile_and_get_bytes("'(+ 2 3)", code_buffer, sizeof(code_buffer));
        if (len > 0) {
            printf("Generated %d bytes\n", len);
            int64_t result = execute_bytes(code_buffer, len);
            printf("Result: %lld ", result);
            if (result == 5) {
                printf("✓ PASS\n");
                pass++;
            } else {
                printf("✗ FAIL\n");
                fail++;
            }
        } else {
            printf("✗ FAIL - compilation failed\n");
            fail++;
        }
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
