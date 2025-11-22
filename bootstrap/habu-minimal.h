/* Minimal Habu runtime interface for bootstrap compiler
 * This provides just enough functionality to compile and test
 * the bootstrap compiler without the full runtime.
 */

#ifndef HABU_MINIMAL_H
#define HABU_MINIMAL_H

#include <stdint.h>

/* Value representation */
typedef int64_t habu_value_t;

/* Tagged value constants and macros */
#define HABU_NIL 0

/* Tagging scheme: lower 4 bits are tag
 * Tag 0: Fixnum (60-bit signed integer, value >> 4)
 * Tag 1: Cons cell (pointer with low bits cleared)
 * Tag 2: Symbol (pointer with low bits cleared)
 */
#define HABU_TAG_FIXNUM(n) ((habu_value_t)(n) << 4)
#define HABU_UNTAG_FIXNUM(v) ((int64_t)(v) >> 4)
#define HABU_IS_FIXNUM(v) (((v) & 0xF) == 0)
#define HABU_IS_CONS(v) (((v) & 0xF) == 1)
#define HABU_IS_SYMBOL(v) (((v) & 0xF) == 2)

/* Runtime functions */
habu_value_t habu_cons(habu_value_t car, habu_value_t cdr);
habu_value_t habu_car(habu_value_t cons);
habu_value_t habu_cdr(habu_value_t cons);
habu_value_t habu_intern(const char *name);

/* Reader */
habu_value_t habu_read(const char *input);

#endif /* HABU_MINIMAL_H */
