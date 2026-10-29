\ ffi-struct-fixture.f - the C library the struct-by-value FFI cases call.
\ test/ffi-struct-arg.f and the full-image restore case build it with the
\ system compiler: no system library takes integer records of every
\ AAPCS64 argument class by value. The layouts are asserted at compile time,
\ and every checksum weighs each member by its position (MIX), so a member
\ read from the wrong offset, register or stack slot changes the answer.
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/codegen.f
require src/lib/test/assert.f          \ T-EX-FAIL

package STRUCT-FIXTURE

$1800 CODEGEN:BUFFER SRC
create SRC-PATH PATH-CAP allot
create LIB-PATH PATH-CAP allot
variable SRC-U
variable LIB-U

: L+ ( ptr u8 n -- )
   SRC CODEGEN:APPEND-STRING
   10 SRC CODEGEN:APPEND-BYTE ;

: TYPES ( -- )
   s" #include <stddef.h>" L+
   s" #include <stdint.h>" L+
   s" typedef int64_t i64;" L+
   s" typedef uint64_t u64;" L+
   s" #define SA(c) _Static_assert(c, #c)" L+
   s" #define MIX(h, x) ((u64)(h) * 31u + (u64)(x))" L+
   s" typedef struct { int32_t a, b; } s8;" L+
   s" typedef struct { int32_t a, b, c; } s12;" L+
   s" typedef struct { i64 a, b; } s16;" L+
   s" typedef struct { i64 a; int32_t b; uint32_t c; i64 d; } s24;" L+
   s" typedef struct { i64 a, b, c, d; } s32;" L+
   s" typedef struct { i64 v[16]; } s128;" L+
   s" SA(sizeof(s8) == 8); SA(offsetof(s8, b) == 4);" L+
   s" SA(sizeof(s12) == 12); SA(offsetof(s12, c) == 8);" L+
   s" SA(sizeof(s16) == 16); SA(offsetof(s16, b) == 8);" L+
   s" SA(sizeof(s24) == 24); SA(offsetof(s24, b) == 8);" L+
   s" SA(offsetof(s24, c) == 12); SA(offsetof(s24, d) == 16);" L+
   s" SA(sizeof(s32) == 32); SA(offsetof(s32, d) == 24);" L+
   s" SA(sizeof(s128) == 128);" L+ ;

: SUMS ( -- )
   s" i64 hb_sum8(s8 s) { return (i64)MIX(MIX(1, s.a), s.b); }" L+
   s" int hb_sum12(s12 s) { return s.a * 100 + s.b * 10 + s.c; }" L+
   s" i64 hb_sum16(s16 s) { return (i64)MIX(MIX(1, s.a), s.b); }" L+
   s" i64 hb_sum24(s24 s) { return (i64)MIX(MIX(MIX(MIX(1, s.a), s.b), s.c), s.d); }" L+
   s" i64 hb_sum32(s32 s) { return (i64)MIX(MIX(MIX(MIX(1, s.a), s.b), s.c), s.d); }" L+
   s" i64 hb_sum128(s128 s) {" L+
   s"    u64 h = 1; for (int i = 0; i < 16; i++) h = MIX(h, s.v[i]); return (i64)h; }" L+ ;

\ Seven longs leave x7 alone for a 16-byte record, which then goes wholly to
\ the stack with the long after it; eight longs push a large record's
\ pointer onto the stack.
: PLACEMENTS ( -- )
   s" u64 hb_fold8(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, u64 k) {" L+
   s"    return MIX(MIX(MIX(MIX(MIX(MIX(MIX(MIX(1, a), b), c), d), e), f), g), k); }" L+
   s" i64 hb_spill16(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, s16 s, i64 t) {" L+
   s"    u64 h = MIX(hb_fold8(a, b, c, d, e, f, g, 0), hb_sum16(s)); return (i64)MIX(h, t); }" L+
   s" i64 hb_big2(i64 a, s24 x, i64 b, s32 y, i64 c) {" L+
   s"    return (i64)MIX(MIX(MIX(MIX(MIX(1, a), hb_sum24(x)), b), hb_sum32(y)), c); }" L+
   s" i64 hb_bigstack(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, i64 k, s32 y, i64 t) {" L+
   s"    u64 h = MIX(hb_fold8(a, b, c, d, e, f, g, k), hb_sum32(y)); return (i64)MIX(h, t); }" L+
   s" i64 hb_fmix(double d, s16 s, float f) {" L+
   s"    return (i64)MIX(MIX(hb_sum16(s), (i64)(d * 4)), (i64)(f * 4)); }" L+ ;

\ A callee owns its by-value parameter: it may write it, or keep its address
\ across a callback that makes another struct call.
: OWNERSHIP ( -- )
   s" i64 hb_mutate(s32 v) {" L+
   s"    s32 *volatile p = &v; p->a += 1000; p->d = -p->d; return hb_sum32(*p); }" L+
   s" i64 hb_hold(s32 v, i64 (*cb)(void)) {" L+
   s"    s32 *volatile held = &v; i64 r = cb(); return (i64)MIX(hb_sum32(*held), r); }" L+
   s" static s24 kept;" L+
   s" void hb_keep24(s24 s) { kept = s; }" L+
   s" i64 hb_kept(void) { return hb_sum24(kept); }" L+
   s" static s16 parked;" L+
   s" s16 *hb_park16(s16 s) { parked = s; return &parked; }" L+ ;

: RESULTS ( -- )
   s" s16 hb_swap16(s16 s) { s16 r = { s.b, s.a }; return r; }" L+
   s" s32 hb_echo32(i64 k, s32 s) {" L+
   s"    s32 r = { s.a + k, s.b + k, s.c + k, s.d + k }; return r; }" L+
   s" s32 hb_ret9(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g, i64 k, i64 i) {" L+
   s"    s32 r = { (i64)hb_fold8(a, b, c, d, e, f, g, k), i, a, k }; return r; }" L+ ;

: CC ( -- )
   PROC-ARGV-RESET
   s" -shared" >LEN PROC-ARGV+
   s" -O2" >LEN PROC-ARGV+
   s" -std=c11" >LEN PROC-ARGV+
   s" -Wall" >LEN PROC-ARGV+
   s" -Werror" >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   LIB-PATH LIB-U @ >LEN PROC-ARGV+
   SRC-PATH SRC-U @ >LEN PROC-ARGV+
   s" /usr/bin/cc" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC MATCH result
      ok OF drop ENDOF
      err OF drop s" struct-fixture: cc failed" T-EX-FAIL die ENDOF
   ;MATCH ;

public

\ Write the fixture source into dir, compile it there, and answer the
\ library's path, which stays valid until the next BUILD.
: BUILD ( ptr u8 n -- ptr u8 n ) {: dir:ptr diru :}
   SRC CODEGEN:RESET
   TYPES SUMS PLACEMENTS OWNERSHIP RESULTS
   dir diru s" hb-struct.c" SRC-PATH FS:JOIN-PATH SRC-U !
   dir diru s" libhbstruct.dylib" LIB-PATH FS:JOIN-PATH LIB-U !
   SRC-PATH SRC-U @ SRC CODEGEN:CONTENTS FS:WRITE-ALL
   CC
   LIB-PATH LIB-U @ ;

;package
