#include <stdio.h>
#include <stdint.h>

int main() {
    /* Simulate (not (= 5 3)) step by step */
    
    /* Step 1: = 5 3 returns 0 (false) */
    int64_t x0 = 0;  /* (= 5 3) result */
    int64_t x0_tagged = x0 << 4;  /* Tag it: 0 << 4 = 0 */
    
    printf("After =: x0=%lld, tagged=%lld\n", x0, x0_tagged);
    
    /* Step 2: NOT */
    /* cmp x0, xzr - compares x0_tagged (0) with 0 */
    int is_zero = (x0_tagged == 0);  /* ZF=1 if equal */
    printf("After cmp: is_zero=%d\n", is_zero);
    
    /* cset x0, EQ - sets x0=1 if ZF=1 */
    x0 = is_zero ? 1 : 0;
    printf("After cset EQ: x0=%lld\n", x0);
    
    /* lsl x0, x0, #4 */
    x0_tagged = x0 << 4;
    printf("After lsl: x0_tagged=%lld\n", x0_tagged);
    
    /* Untag */
    int64_t result = x0_tagged >> 4;
    printf("Final result: %lld (expected 1)\n", result);
    
    return 0;
}
