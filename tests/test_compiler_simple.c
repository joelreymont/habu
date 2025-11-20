/* Simple compiler tests - just verify code generation works */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

static int tests_run = 0;
static int tests_passed = 0;

/* Get temporary directory (portable) */
static const char* get_temp_dir(void) {
    const char *tmpdir = getenv("TMPDIR");
    if (!tmpdir) tmpdir = getenv("TEMP");
    if (!tmpdir) tmpdir = getenv("TMP");
    if (!tmpdir) tmpdir = "/tmp";
    return tmpdir;
}

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

TEST(compiler_generates_fixnum_code) {
    /* Tests run from project root, so bootstrap/ is relative to root */
    int ret = system("cd bootstrap && sbcl --noinform --disable-debugger --load test-compiler.lisp --quit > /dev/null 2>&1");
    assert(ret == 0);
}

TEST(compiled_binaries_exist) {
    char path[512];
    const char *tmpdir = get_temp_dir();

    snprintf(path, sizeof(path), "%s/test-x86_64.bin", tmpdir);
    assert(access(path, F_OK) == 0);

    snprintf(path, sizeof(path), "%s/test-arm64.bin", tmpdir);
    assert(access(path, F_OK) == 0);
}

TEST(x86_64_code_size_reasonable) {
    char path[512];
    const char *tmpdir = get_temp_dir();

    snprintf(path, sizeof(path), "%s/test-x86_64.bin", tmpdir);
    FILE *f = fopen(path, "rb");
    assert(f != NULL);

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fclose(f);

    assert(size > 0 && size < 1024);
}

TEST(arm64_code_size_reasonable) {
    char path[512];
    const char *tmpdir = get_temp_dir();

    snprintf(path, sizeof(path), "%s/test-arm64.bin", tmpdir);
    FILE *f = fopen(path, "rb");
    assert(f != NULL);

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fclose(f);

    assert(size > 0 && size < 1024);
}

int main(void) {
    printf("Compiler tests:\n");

    RUN_TEST(compiler_generates_fixnum_code);
    RUN_TEST(compiled_binaries_exist);
    RUN_TEST(x86_64_code_size_reasonable);
    RUN_TEST(arm64_code_size_reasonable);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
