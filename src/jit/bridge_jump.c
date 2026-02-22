#include <setjmp.h>
#include <stddef.h>
#include <stdint.h>

#ifndef HABU_JIT_BRIDGE_MAX_DEPTH
#define HABU_JIT_BRIDGE_MAX_DEPTH 64
#endif

typedef struct {
    jmp_buf env;
} HabuJitBridgeFrame;

typedef struct {
    HabuJitBridgeFrame frames[HABU_JIT_BRIDGE_MAX_DEPTH];
    int sp;
} HabuJitBridgeStack;

static _Thread_local HabuJitBridgeStack g_habu_jit_bridge_stack = {0};

typedef uint64_t (*habu_jit_bridge_run_fn)(void *);

int habu_jit_bridge_run(habu_jit_bridge_run_fn func, void *ctx, uint64_t *out_raw) {
    if (g_habu_jit_bridge_stack.sp >= HABU_JIT_BRIDGE_MAX_DEPTH) {
        return -1;
    }
    const int idx = g_habu_jit_bridge_stack.sp++;
    const int jump_rc = setjmp(g_habu_jit_bridge_stack.frames[idx].env);
    if (jump_rc == 0) {
        const uint64_t result_raw = func(ctx);
        if (out_raw != NULL) {
            *out_raw = result_raw;
        }
        g_habu_jit_bridge_stack.sp = idx;
        return 0;
    }

    g_habu_jit_bridge_stack.sp = idx;
    return jump_rc;
}

void habu_jit_bridge_throw(void) {
    if (g_habu_jit_bridge_stack.sp <= 0) {
        return;
    }
    longjmp(g_habu_jit_bridge_stack.frames[g_habu_jit_bridge_stack.sp - 1].env, 1);
}

int habu_jit_bridge_depth(void) {
    return g_habu_jit_bridge_stack.sp;
}
