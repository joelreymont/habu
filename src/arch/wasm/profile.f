\ profile.f - WPROF, the WebAssembly profile the Wasm backend generates code for.
\
\ docs/wasm-backend.md section 3 fixes the browser baseline as scalar core plus
\ multi-value and lists every further instruction feature individually, because
\ the version number of an evolving specification is not a feature set; section
\ 4.3 binds code generation to a closed profile of permitted features, arities,
\ locals, module bytes and resource limits, and forbids semantic settings that
\ hide in mutable globals. The target contract cannot say any of this: CTARGET
\ carries BASE and SCALAR-FP for wasm and nothing else (src/compiler/target.f
\ MASK-WASM), so nothing there could admit or refuse a feature-gated opcode.
\
\ ONE CLOSED RECORD. Every number the first slice pins is a field of `profile`:
\ the admitted features, the 16/16 lane arity past which a call takes the
\ aligned frame (section 7.3), the memory layout of section 17.5, the offsets of
\ the context fields, and the ceilings a browser engine enforces. Two profiles
\ that differ in any one field are different values (`WPROF-PROFILE:EQ`), so a
\ profile is its own identity. P6 publishes no digest of it and writes no
\ custom section for it; the release identity that would read one is P7's.
\
\ INSTALLED ONCE. The backend's INSTALL hands its profile to `CURRENT!`, and the
\ selector, encoder, linker and harness read `CURRENT` and the readers below. A
\ second install is refused rather than replacing the profile code was already
\ generated under, and a read before the install is refused rather than
\ answered with a default nobody chose.

require lib/prelude.f
require lib/errors.f

\ WPROF's codes, -9810..-9814, in the Wasm backend's block -9800..-9829.
-9810 constant E-WPROF-FIRST
-9814 constant E-WPROF-LAST
-9810 constant E-WPROF-INSTALLED   \ a profile is already installed: CURRENT is set once
-9811 constant E-WPROF-ABSENT      \ the profile was read before the backend installed one

package WPROF
public

\ The instruction features beyond scalar core, each named as wasm-tools'
\ --features flag spells it, which is how the harness validates a module with
\ exactly the profile's set.
ENUM feature
   multi-value
   saturating-float-to-int
   sign-extension
   bulk-memory
   mutable-global
   reference-types
   tail-call
   threads
   memory64
   simd
   gc
   exceptions
;ENUM

\ The fields, in order: the admitted feature bits (BIT below); the most input
\ and output lanes a direct call passes in registers; the starts of the context,
\ output, data-stack and static-data regions, the null reservation being
\ [0, ctx-base); the byte offset inside the context of each of its seven
\ fields; and the most locals one function declares, functions one module
\ defines, bytes one function body holds and bytes one module holds.
STRUCTURE profile 0 DERIVE eq addr
   FIELD features n
   FIELD params-max n
   FIELD results-max n
   FIELD ctx-base n
   FIELD out-base n
   FIELD stack-base n
   FIELD data-base n
   FIELD ctx-stack-base n
   FIELD ctx-stack-top n
   FIELD ctx-out-len n
   FIELD ctx-throw-code n
   FIELD ctx-fault-kind n
   FIELD ctx-fault-addr n
   FIELD ctx-epoch n
   FIELD locals-max n
   FIELD functions-max n
   FIELD body-bytes-max n
   FIELD module-bytes-max n
;STRUCTURE

\ A feature's bit in the `features` field, stated here and never derived from
\ the declaration order, so reordering the enum cannot move a feature.
: BIT ( WPROF:feature -- n )
   MATCH feature
      multi-value             OF $1 ENDOF
      saturating-float-to-int OF $2 ENDOF
      sign-extension          OF $4 ENDOF
      bulk-memory             OF $8 ENDOF
      mutable-global          OF $10 ENDOF
      reference-types         OF $20 ENDOF
      tail-call               OF $40 ENDOF
      threads                 OF $80 ENDOF
      memory64                OF $100 ENDOF
      simd                    OF $200 ENDOF
      gc                      OF $400 ENDOF
      exceptions              OF $800 ENDOF
   ;MATCH ;

\ The first slice's profile. It admits exactly multi-value, which the call
\ signature (status, outputs) needs, and saturating-float-to-int, which makes
\ `f>s` one instruction: i64.trunc_sat_f64_s truncates, saturates and answers 0
\ for a NaN exactly as src/compiler/native/hir-word.f defines realint.
\
\ The layout is section 17.5's: null [0,$10000), ctx [$10000,$11000), output
\ [$11000,$21000), data stack [$21000,$31000) - the native boot stack's 8192
\ cells - and static data from $31000. The slice has no allocator, so the
\ memory never grows: its maximum equals its minimum by rule, not by a field.
\ Each context field has its own eight-byte slot, so the i64 throw code and
\ fault address and the i32 fields beside them are all naturally aligned.
\
\ The ceilings are the WebAssembly JavaScript API's implementation limits,
\ which every conforming browser engine enforces when it compiles a module:
\ 50000 locals in a function, 1000000 functions, 7654321 bytes in a function
\ body and 1073741824 bytes in a module.
: V1 ( -- WPROF:profile )
   WPROF-FEATURE:MULTI-VALUE BIT
   WPROF-FEATURE:SATURATING-FLOAT-TO-INT BIT or
   16 16
   $10000 $11000 $21000 $31000
   0 8 16 24 32 40 48
   50000 1000000 7654321 1073741824
   WPROF-PROFILE:MAKE ;

private

TYPED-VARIABLE CUR WPROF:profile
variable CUR-SET
0 CUR-SET !

: INSTALLED ( -- ptr WPROF:profile )
   CUR-SET @ 0= if E-WPROF-ABSENT throw then
   CUR ;

public

\ The backend's INSTALL is the one caller.
: CURRENT! ( WPROF:profile -- )
   {: p:WPROF:profile :}
   CUR-SET @ if E-WPROF-INSTALLED throw then
   p CUR !
   1 CUR-SET ! ;

: CURRENT ( -- WPROF:profile )
   INSTALLED @ ;

: ADMITS? ( WPROF:feature -- bool )
   BIT INSTALLED WPROF-PROFILE:FEATURES @ and 0<> ;

\ ---- the installed profile's numbers -----------------------------------------
: PARAMS-MAX ( -- n )        INSTALLED WPROF-PROFILE:PARAMS-MAX @ ;
: RESULTS-MAX ( -- n )       INSTALLED WPROF-PROFILE:RESULTS-MAX @ ;
: CTX-BASE ( -- n )          INSTALLED WPROF-PROFILE:CTX-BASE @ ;
: OUT-BASE ( -- n )          INSTALLED WPROF-PROFILE:OUT-BASE @ ;
: STACK-BASE ( -- n )        INSTALLED WPROF-PROFILE:STACK-BASE @ ;
: DATA-BASE ( -- n )         INSTALLED WPROF-PROFILE:DATA-BASE @ ;
: CTX-STACK-BASE ( -- n )    INSTALLED WPROF-PROFILE:CTX-STACK-BASE @ ;
: CTX-STACK-TOP ( -- n )     INSTALLED WPROF-PROFILE:CTX-STACK-TOP @ ;
: CTX-OUT-LEN ( -- n )       INSTALLED WPROF-PROFILE:CTX-OUT-LEN @ ;
: CTX-THROW-CODE ( -- n )    INSTALLED WPROF-PROFILE:CTX-THROW-CODE @ ;
: CTX-FAULT-KIND ( -- n )    INSTALLED WPROF-PROFILE:CTX-FAULT-KIND @ ;
: CTX-FAULT-ADDR ( -- n )    INSTALLED WPROF-PROFILE:CTX-FAULT-ADDR @ ;
: CTX-EPOCH ( -- n )         INSTALLED WPROF-PROFILE:CTX-EPOCH @ ;
: LOCALS-MAX ( -- n )        INSTALLED WPROF-PROFILE:LOCALS-MAX @ ;
: FUNCTIONS-MAX ( -- n )     INSTALLED WPROF-PROFILE:FUNCTIONS-MAX @ ;
: BODY-BYTES-MAX ( -- n )    INSTALLED WPROF-PROFILE:BODY-BYTES-MAX @ ;
: MODULE-BYTES-MAX ( -- n )  INSTALLED WPROF-PROFILE:MODULE-BYTES-MAX @ ;

;package
