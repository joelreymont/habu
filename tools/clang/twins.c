/* twins.c - the C twin of every pinned corpus row, for the clang reference
 * column of the code generator comparison.
 * One concern: the reference programs themselves.
 *
 * WHAT A TWIN IS. For each row of tools/codegen-compare-corpus*.f there is one
 * function here that computes the SAME function of the SAME arguments. The
 * harness calls it on the row's pinned inputs and records its answers with the
 * same VECTOR discipline the other two columns use, so a twin that computes
 * something else is a finding rather than a faster number.
 *
 * WHY THE TWINS OWN THEIR DATA. A row whose point is a side effect - the cell
 * bump, the vector copy, the weight step, the store-load - needs storage. The
 * other two columns share the corpus's own memory because they are two
 * compilations of one program; clang is compiling a different program, so it
 * gets its own storage, filled with the same pinned values, and the harness
 * reads it back through the accessors below. Every buffer here is initialised
 * from the same constants tools/codegen-compare-corpus*.f initialise theirs
 * from, and the recorded answers are compared against the same pins.
 *
 * WHY EVERY PARAMETER AND EVERY RETURN IS A LONG OR A DOUBLE. The harness
 * reaches these through lib/ffi-abi.f, which marshals integer and floating
 * cells. A twin that wanted a pointer takes the address as a long and the
 * harness asks for it once, at setup, through one of the *_ptr accessors. That
 * keeps the number of distinct call shapes - and therefore the number of entry
 * floors that have to be measured - to the fourteen the harness declares.
 *
 * THE FLOORS. hf_* are empty functions, one per call shape. They are what the
 * harness measures its FFI entry cost with: a row's cost minus the cost of the
 * SAME shape with an empty body is the cost of the emitted code. An FFI call
 * costs far more than a bl, so a single zero-argument floor would not have made
 * the comparison honest; each shape carries its own.
 *
 * WHAT IS NOT DONE TO CLANG. No twin is hand-tuned, no inlining is forced or
 * forbidden, and no arithmetic is reassociated. The build passes -O2 and
 * -fno-math-errno and nothing else that changes code generation: -fno-math-errno
 * so that a square root is the fsqrt instruction the habu word is rather than a
 * libm call with an errno test around it, and NOT -ffast-math, because the
 * recorded float answers are compared bit for bit and reassociating a sum would
 * change them. Where clang recognises a closed form for a loop, or folds a dead
 * store the engine keeps, it is allowed to: that difference IS the measurement.
 *
 * AND WHERE THE LINE IS, because the sentence above was read too widely and two
 * rows measured nothing for it. A row is a comparison of two compilations of ONE
 * program. Clang finding a cheaper way to run that program is the measurement;
 * clang running a DIFFERENT program is not, and that is what happened to the two
 * rows whose whole subject is memory traffic. Recorded on this host at -O2,
 * before the volatile below:
 *
 *   hc4_store_load  compiled to one ldr, one str and `x0 + 3 * len`. The row is
 *                   named for a load and a store to one address every turn of a
 *                   loop - a loop-carried dependence through memory - and the
 *                   reference had no loop and no dependence at all.
 *   hc1_cell_bump   compiled to one str. The first of the word's two writes was
 *                   dead in C and folded, so the row named "a store, a load and
 *                   a store again" measured a store.
 *
 * A qualifier is the C for "this access really happens", so the storage those
 * two rows step is volatile and each one now emits the accesses its name claims.
 * This CHANGES those rows' reference code, deliberately: their byte counts and
 * their times both move, and they move because the row is finally measuring what
 * it says. Nothing else in this file is volatile - a row that is not about
 * memory traffic has no business forbidding clang an optimisation.
 *
 * AND NOTHING CHECKS THAT THOSE TWO QUALIFIERS ARE STILL HERE. Both rows answer
 * the same values either way, and the reference column is measured rather than
 * pinned, so a volatile deleted by accident would put both rows quietly back to
 * measuring a different program. Dot habu-check-the-mem-7cf9ab6c carries the
 * check; until it lands, the disassembly above is the evidence and this
 * paragraph is the warning.
 *
 * WHAT VOLATILE CANNOT REPAIR, and hc4_pressure_loop is the case, so it is
 * written down rather than papered over. Its loop body loads fourteen cells and
 * holds them live at once; clang hoists all fourteen loads out of the loop,
 * vectorises the sum and multiplies by the trip count, so the reference never
 * holds fourteen values and never spills. Volatile would force the fourteen
 * loads back into the loop but would NOT force the liveness: clang would fold
 * each value into the accumulator as it arrived and hold one. There is no honest
 * C that makes the reference face the allocator question the row asks, so the
 * row's reference column is a lower bound on a different program, and the report
 * says so beside it rather than this file pretending otherwise.
 */

#include <stdint.h>

typedef int64_t i64;
typedef uint64_t u64;

/* ==========================================================================
 * The entry floors: one empty function per call shape the harness uses.
 * ========================================================================== */

i64 hf_i0(void) { return 0; }
i64 hf_i1(i64 a) { (void)a; return 0; }
i64 hf_i2(i64 a, i64 b) { (void)a; (void)b; return 0; }
i64 hf_i3(i64 a, i64 b, i64 c) { (void)a; (void)b; (void)c; return 0; }
i64 hf_i5(i64 a, i64 b, i64 c, i64 d, i64 e) {
   (void)a; (void)b; (void)c; (void)d; (void)e; return 0;
}
i64 hf_i6(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f) {
   (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; return 0;
}
i64 hf_i9(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, i64 h, i64 k) {
   (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h;
   (void)k; return 0;
}

double hf_id(i64 a) { (void)a; return 0.0; }
double hf_iid(i64 a, i64 b) { (void)a; (void)b; return 0.0; }
double hf_iiid(i64 a, i64 b, i64 c) { (void)a; (void)b; (void)c; return 0.0; }
double hf_dd(double a) { (void)a; return 0.0; }
double hf_ddd(double a, double b) { (void)a; (void)b; return 0.0; }
double hf_dddd(double a, double b, double c) {
   (void)a; (void)b; (void)c; return 0.0;
}
i64 hf_di(double a) { (void)a; return 0; }
i64 hf_diii(double a, i64 b, i64 c, i64 d) {
   (void)a; (void)b; (void)c; (void)d; return 0;
}

/* ==========================================================================
 * Corpus 1 - tools/codegen-compare-corpus.f
 * ========================================================================== */

/* CELL-BUMP's cell. The habu word owns one cell of its own and materialises
 * its address; this is the same program with C's storage. It is volatile
 * because the row is named for the traffic through it - see the head of this
 * file - and without that the first of the two writes is dead in C. */
static volatile i64 c1_bump_cell;

/* BYTE-SUM and BYTE-FIND are measured over this span and over an empty one. */
static const unsigned char c1_subject[] = "habu codegen baseline";

i64 hc1_subject_ptr(void) { return (i64)(intptr_t)c1_subject; }
i64 hc1_bump_get(void) { return c1_bump_cell; }

i64 hc1_add3(i64 a, i64 b, i64 c) { return a + b + c; }

i64 hc1_square_sum(i64 a, i64 b) { return a * a + b * b; }

i64 hc1_max2(i64 a, i64 b) { return a < b ? b : a; }

i64 hc1_lerp(i64 a, i64 b, i64 t) { return (b - a) * t / 100 + a; }

i64 hc1_sum_to(i64 n) {
   i64 s = 0;
   for (i64 i = 0; i < n; i++) s += i;
   return s;
}

i64 hc1_count_down(i64 n) {
   do { n = n - 1; } while (!(n <= 0));
   return n;
}

i64 hc1_fact(i64 n) {
   if (n <= 1) return 1;
   return n * hc1_fact(n - 1);
}

/* Store, load, add one, store - and the answer is the value that was stored,
 * not a third read of the cell. CODEGEN-CORPUS:CELL-BUMP keeps the incremented
 * value on the stack and returns THAT, so a twin that read the cell again would
 * make one more access than the word it is the twin of. That did not matter
 * while the cell was ordinary, because the extra read folded away; now that the
 * cell is volatile it would be a real load, so the value is named here. */
i64 hc1_cell_bump(i64 n) {
   c1_bump_cell = n;
   i64 v = c1_bump_cell + 1;
   c1_bump_cell = v;
   return v;
}

i64 hc1_byte_sum(i64 ap, i64 u) {
   const unsigned char *a = (const unsigned char *)(intptr_t)ap;
   i64 s = 0;
   for (i64 i = 0; i < u; i++) s += a[i];
   return s;
}

i64 hc1_byte_find(i64 ap, i64 u, i64 c) {
   const unsigned char *a = (const unsigned char *)(intptr_t)ap;
   for (i64 i = 0; i < u; i++) {
      if ((i64)a[i] == c) return i;
   }
   return -1;
}

/* ==========================================================================
 * Corpus 2 - tools/codegen-compare-corpus2.f
 * ========================================================================== */

#define C2_TV_SLOTS 32
#define C2_COPY_CELLS 8

static i64 c2_tv_table[C2_TV_SLOTS];
static i64 c2_copy_src[C2_COPY_CELLS];
static i64 c2_copy_dst[C2_COPY_CELLS];

/* The binding chain the walk is measured on, laid down exactly as
 * CODEGEN-CORPUS2:BIND-CHAIN lays down the habu one. */
i64 hc2_setup(void) {
   for (i64 i = 0; i < C2_TV_SLOTS; i++) c2_tv_table[i] = -1;
   c2_tv_table[1] = (2 << 3) | 1;
   c2_tv_table[2] = (3 << 3) | 0;
   for (i64 i = 0; i < C2_COPY_CELLS; i++) {
      c2_copy_src[i] = i * 100 + 7;
      c2_copy_dst[i] = 0;
   }
   return 0;
}

i64 hc2_copy_src_ptr(void) { return (i64)(intptr_t)c2_copy_src; }
i64 hc2_copy_dst_ptr(void) { return (i64)(intptr_t)c2_copy_dst; }
i64 hc2_copy_dst_get(i64 k) { return c2_copy_dst[k]; }

static const unsigned char c2_subject[] = "aha aha aha";

i64 hc2_subject_ptr(void) { return (i64)(intptr_t)c2_subject; }

i64 hc2_tag(i64 n) { return n & 7; }

i64 hc2_ws(i64 c) { return (c == 32) | (c == 9) | (c == 10) | (c == 13); }

i64 hc2_sym_fold_c(i64 c) {
   if (c < 0x41) return c;
   if (c > 0x5A) return c;
   return c | 0x20;
}

i64 hc2_max_dim(i64 a, i64 b) { return a > b ? a : b; }

i64 hc2_count_char(i64 ap, i64 u, i64 c) {
   const unsigned char *a = (const unsigned char *)(intptr_t)ap;
   i64 n = 0;
   for (i64 i = 0; i < u; i++) {
      if ((i64)a[i] == c) n++;
   }
   return n;
}

/* TV-NEXT?'s three ways out, in the order the habu word has them. */
static int c2_tv_next(i64 *t) {
   if ((*t & 7) != 1) return 0;
   i64 v = c2_tv_table[(i64)((u64)*t >> 3)];
   if (v == -1) return 0;
   *t = v;
   return 1;
}

i64 hc2_t_res_walk(i64 t) {
   while (c2_tv_next(&t)) { }
   return t;
}

static i64 *c2_cell_field(i64 *base, i64 off) { return base + off; }

i64 hc2_vec_copy_cells(i64 srcp, i64 dstp, i64 len) {
   i64 *src = (i64 *)(intptr_t)srcp;
   i64 *dst = (i64 *)(intptr_t)dstp;
   for (i64 i = 0; i < len; i++) {
      *c2_cell_field(dst, i) = *c2_cell_field(src, i);
   }
   return 0;
}

/* ==========================================================================
 * Corpus 3 - tools/codegen-compare-corpus3.f
 * ========================================================================== */

#define C3_VEC_LEN 4
#define C3_SUM_LEN 3
#define C3_W_CELLS 5

static double c3_a[C3_VEC_LEN];
static double c3_b[C3_VEC_LEN];
static double c3_z[C3_VEC_LEN];
static double c3_sum[C3_SUM_LEN];
static double c3_w[C3_W_CELLS];
static double c3_g[C3_VEC_LEN];

i64 hc3_w_reset(void) {
   c3_w[0] = 1.0;
   c3_w[1] = -1.0;
   c3_w[2] = 0.5;
   c3_w[3] = 0.0;
   c3_w[4] = 2.5;
   return 0;
}

i64 hc3_setup(void) {
   c3_a[0] = -2.5; c3_a[1] = 0.0; c3_a[2] = 1.5; c3_a[3] = 0.25;
   c3_b[0] = 1.0; c3_b[1] = -1.0; c3_b[2] = 1.5; c3_b[3] = 0.0;
   c3_z[0] = 0.0; c3_z[1] = 0.0; c3_z[2] = 0.0; c3_z[3] = 0.0;
   c3_sum[0] = 9007199254740992.0; c3_sum[1] = 1.0; c3_sum[2] = 1.0;
   c3_g[0] = 0.5; c3_g[1] = 0.5; c3_g[2] = -2.0; c3_g[3] = 3.0;
   return hc3_w_reset();
}

i64 hc3_a_ptr(void) { return (i64)(intptr_t)c3_a; }
i64 hc3_b_ptr(void) { return (i64)(intptr_t)c3_b; }
i64 hc3_z_ptr(void) { return (i64)(intptr_t)c3_z; }
i64 hc3_s_ptr(void) { return (i64)(intptr_t)c3_sum; }
i64 hc3_w_ptr(void) { return (i64)(intptr_t)c3_w; }
i64 hc3_g_ptr(void) { return (i64)(intptr_t)c3_g; }

double hc3_w_get(i64 k) { return c3_w[k]; }

double hc3_t_sum(i64 basep, i64 len) {
   const double *base = (const double *)(intptr_t)basep;
   double acc = 0.0;
   for (i64 i = 0; i < len; i++) acc = acc + base[i];
   return acc;
}

i64 hc3_t_sgd(double lr, i64 wp, i64 gp, i64 len) {
   double *w = (double *)(intptr_t)wp;
   const double *g = (const double *)(intptr_t)gp;
   for (i64 i = 0; i < len; i++) w[i] = w[i] - lr * g[i];
   return 0;
}

double hc3_t_dist2(i64 ap, i64 bp, i64 len) {
   const double *a = (const double *)(intptr_t)ap;
   const double *b = (const double *)(intptr_t)bp;
   double acc = 0.0;
   for (i64 i = 0; i < len; i++) {
      double d = a[i] - b[i];
      acc = acc + d * d;
   }
   return acc;
}

double hc3_t_norm2(i64 bp, i64 len) {
   const double *b = (const double *)(intptr_t)bp;
   double acc = 0.0;
   for (i64 i = 0; i < len; i++) acc = acc + b[i] * b[i];
   return acc;
}

double hc3_t_rel_l2(i64 ap, i64 bp, i64 len) {
   return __builtin_sqrt(hc3_t_dist2(ap, bp, len))
        / __builtin_sqrt(hc3_t_norm2(bp, len));
}

double hc3_relu_f(double x) { return x < 0.0 ? 0.0 : x; }

double hc3_max_f(double x, double y) { return x < y ? y : x; }

double hc3_sgd(double w, double g, double lr) { return w - lr * g; }

double hc3_seg_rsqrt(i64 d) { return 1.0 / __builtin_sqrt((double)d); }

i64 hc3_fround(double x) {
   return x < 0.0 ? (i64)(x - 0.5) : (i64)(x + 0.5);
}

/* ==========================================================================
 * Corpus 4 - tools/codegen-compare-corpus4.f
 * ========================================================================== */

#define C4_ROW_CELLS 14

static i64 c4_rec[C4_ROW_CELLS];
static i64 c4_step[2];

i64 hc4_step_reset(void) {
   c4_step[0] = 100;
   c4_step[1] = -7;
   return 0;
}

i64 hc4_setup(void) {
   for (i64 i = 0; i < C4_ROW_CELLS; i++) c4_rec[i] = i * 7 + 3;
   c4_rec[5] = -11;
   c4_rec[9] = 0;
   return hc4_step_reset();
}

i64 hc4_rec_ptr(void) { return (i64)(intptr_t)c4_rec; }
i64 hc4_step_ptr(void) { return (i64)(intptr_t)c4_step; }
i64 hc4_step_get(i64 k) { return c4_step[k]; }

static i64 c4_add1(i64 n) { return n + 1; }
static i64 c4_mul2(i64 n) { return n * 2; }
static i64 c4_and7(i64 n) { return n & 7; }
static i64 c4_xor5(i64 n) { return n ^ 5; }
static i64 c4_mad(i64 n) { return n * 3 + 5; }

static i64 c4_long(i64 n) {
   i64 a = n * 3 + (n ^ 5) + (n & 7);
   return (a + a * 11) ^ 13;
}

i64 hc4_call_fan(i64 n) {
   return c4_add1(c4_xor5(c4_and7(c4_mul2(c4_add1(n)))));
}

i64 hc4_call_fan_big(i64 n) {
   return c4_mad(c4_mad(c4_mad(c4_mad(c4_mad(n)))));
}

i64 hc4_call_loop_3(i64 a, i64 b, i64 c, i64 seed, i64 len) {
   i64 x = seed;
   for (i64 i = 0; i < len; i++) x = c4_xor5(c4_mul2(c4_add1(x)));
   return x + a + b + c;
}

i64 hc4_wide_arity(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f) {
   return (a - b) + (c - d) + (e - f);
}

i64 hc4_ladder(i64 x) {
   if (x < 1) return 0;
   if (x < 2) return 1;
   if (x < 4) return 2;
   if (x < 8) return 3;
   if (x < 16) return 4;
   if (x < 32) return 5;
   if (x < 64) return 6;
   if (x < 128) return 7;
   return 8;
}

/* The row volatile cannot repair: clang hoists all fourteen loads, vectorises
 * the sum and multiplies by the trip count, so the reference never holds the
 * fourteen live values the row is named for. Deliberately left alone - the head
 * of this file carries the argument, and the report marks the row. */
i64 hc4_pressure_loop(i64 basep, i64 len) {
   const i64 *base = (const i64 *)(intptr_t)basep;
   i64 acc = 0;
   for (i64 i = 0; i < len; i++) {
      i64 v0 = base[0], v1 = base[1], v2 = base[2], v3 = base[3];
      i64 v4 = base[4], v5 = base[5], v6 = base[6], v7 = base[7];
      i64 v8 = base[8], v9 = base[9], v10 = base[10], v11 = base[11];
      i64 v12 = base[12], v13 = base[13];
      acc = acc + v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11
          + v12 + v13;
   }
   return acc;
}

i64 hc4_call_pressure(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g,
                      i64 seed, i64 len) {
   i64 x = seed;
   for (i64 i = 0; i < len; i++) x = c4_long(x);
   return x + a + b + c + d + e + f + g + len;
}

i64 hc4_big_consts(i64 len) {
   i64 acc = 0;
   for (i64 i = 0; i < len; i++) {
      acc = acc + (i ^ 1234605616436508552LL);
      acc = acc + (i ^ 7378697629483820646LL);
      acc = acc + (i ^ -6148914691236517206LL);
      acc = acc + (i ^ 1311768467294899695LL);
   }
   return acc;
}

i64 hc4_many_locals(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, i64 h,
                    i64 len) {
   i64 acc = 0;
   for (i64 i = 0; i < len; i++) acc = acc + a + b + c + d + e + f + g + h;
   return acc;
}

i64 hc4_tiny_callee(i64 seed, i64 len) {
   i64 x = seed;
   for (i64 i = 0; i < len; i++) x = c4_add1(c4_add1(c4_add1(c4_add1(x))));
   return x;
}

i64 hc4_float_mix(i64 seed, i64 len) {
   i64 acc = seed;
   for (i64 i = 0; i < len; i++) acc = acc + (i64)((double)i * 0.5);
   return acc;
}

/* One load and one store to the same address every turn, which is the row's
 * whole subject. The cell is volatile so the dependence survives -O2; see the
 * head of this file for what it compiled to before. The trailing read is the
 * habu word's own `cell @` after its loop and not an extra access. */
i64 hc4_store_load(i64 cellp, i64 len) {
   volatile i64 *cell = (volatile i64 *)(intptr_t)cellp;
   for (i64 i = 0; i < len; i++) *cell = *cell + 3;
   return *cell;
}

/* ==========================================================================
 * Corpus 5 - tools/codegen-compare-corpus5.f
 *
 * The tail-call corpus. Every row is one call, or two, placed where a compiler
 * may or may not turn the last of them into a branch, so the reference column
 * here says what clang -O2 does with the same six placements.
 *
 * THE TWO CALLEES ARE static AND THAT IS ALL THAT IS DONE TO THEM. Nothing
 * forces or forbids inlining: whether clang copies c5_long into a row, calls
 * it, or tail-branches to it IS the measurement, exactly as the head of this
 * file says. They are the same arithmetic as the habu corpus's C5-LONG and
 * C5-PAIR, on the same arguments.
 *
 * WHY C5-PAIR'S TWIN NEEDS AN ACCESSOR. The habu word leaves TWO values, and a
 * C function returns one. The pair is computed once, the first of the two
 * results is left in c5_pair_deep and the second is returned, and the harness
 * reads the first back through hc5_pair_deep - the same discipline
 * hc4_store_load and hc4_step_get already use for a row whose answer does not
 * fit in a return value. Both columns record the same two values in the same
 * order: the top of the habu stack first.
 * ========================================================================== */

/* The deeper of C5-PAIR's two results, left where the harness can read it. */
static i64 c5_pair_deep;

static i64 c5_long(i64 n) {
   i64 a = n * 3 + (n ^ 5) + (n & 7);
   return (a + a * 11) ^ 13;
}

static i64 c5_pair(i64 a, i64 b) {
   i64 s = a * 3 + (b ^ 5);
   c5_pair_deep = s;
   return (s & 7) + ((s * 11) ^ 13);
}

i64 hc5_pair_deep(void) { return c5_pair_deep; }

i64 hc5_tail_big(i64 n) { return c5_long(n); }

i64 hc5_tail_work(i64 n) { return c5_long(n + 1); }

i64 hc5_nontail(i64 n) { return c5_long(n) + 1; }

i64 hc5_tail_mid(i64 n) { return c5_long(n); }

i64 hc5_tail_chain(i64 n) { return hc5_tail_mid(n); }

i64 hc5_tail_pair(i64 a, i64 b) { return c5_pair(a, b); }

i64 hc5_tail_after(i64 n) { return c5_long(c5_long(n)); }
