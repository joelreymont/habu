.globl _main
.align 4

_main:
    /* Test CSET encodings */
    cset x0, EQ        /* Equal */
    cset x1, NE        /* Not equal */
    cset x2, LT        /* Less than */
    cset x3, LE        /* Less or equal */
    cset x4, GT        /* Greater than */
    cset x5, GE        /* Greater or equal */

    /* CMP encoding */
    cmp x0, x1

    ret
