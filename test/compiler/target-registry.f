\ target-registry.f - complete backend registration through the real load path.
require lib/test.f
require lib/task.f
require test/checker-assert.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/ir/context.f
require src/compiler/native/backend.f
require src/arch/arm64/passes.f
require src/arch/x86-64/passes.f

package CTREG-TEST
private

: MK ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width -- CTARGET:contract )
   CTARGET:F-BASE CTARGET:CONTRACT ;
: ARM ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;
: X64 ( -- CTARGET:contract )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;
: BIG ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:BIG
   CTARGET-PTR--WIDTH:BITS64 MK ;
: GPU ( -- CTARGET:contract )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;
: NO-GPU ( -- ) GPU NBACK:LOWERS? drop ;
: YES ( CTARGET:contract -- bool ) drop true ;
: NO ( CTARGET:contract -- bool ) drop false ;
: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;
: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;
: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) E-CTGT-UNLOADED throw ;
: NO-STAGE ( -- ) ;
: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ) 2drop 2drop ;
variable SAW
: DECLARE ( n n NBACK:linkage -- ) 2drop drop 1 SAW +! ;
: GPU-DECL ( n n NBACK:linkage -- ) 2drop drop 100 SAW +! ;

: FAKE-PASS ( [ n n NBACK:linkage -- ] -- NBACK:pass )
   {: decl :}
   NBACK-MODE:EXCLUSIVE-SESSION
   decl [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-REWRITE ;]
   [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;]
   [: NO-PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE ;
: BACK ( n CTARGET:arch [ CTARGET:contract -- bool ] [ CTARGET:contract -- bool ] -- CTARGET:backend )
   {: id:n a:CTARGET:arch lo em :}
   id CTARGET:ID a lo em CTARGET-BACKEND:MAKE ;
: INSTALL ( n CTARGET:arch -- )
   {: id:n a:CTARGET:arch :}
   id a [: YES ;] [: NO ;] BACK [: DECLARE ;] FAKE-PASS NBACK:REGISTER ;
: DUP-ID ( -- )
   10 CTARGET-ARCH:PTX [: YES ;] [: NO ;] BACK [: DECLARE ;] FAKE-PASS NBACK:REGISTER ;
: DUP-ARCH ( -- )
   99 CTARGET-ARCH:AARCH64 [: NO ;] [: NO ;] BACK [: DECLARE ;] FAKE-PASS NBACK:REGISTER ;
: INVALID-ID ( -- )
   0 CTARGET-BACKEND--ID:MAKE CTARGET-ARCH:PTX [: YES ;] [: NO ;]
      CTARGET-BACKEND:MAKE [: DECLARE ;] FAKE-PASS NBACK:REGISTER ;
: BUSY-INSTALL ( -- )
   30 CTARGET-ARCH:PTX [: YES ;] [: NO ;] BACK [: GPU-DECL ;] FAKE-PASS NBACK:REGISTER ;
: BUSY-ROOT ( NLEASE:lease -- )
   drop [: BUSY-INSTALL ;] NLEASE:E-BUSY TTHROWSQ ;
TASK:MIN-STACK TASK:TASK WORKER
: TASK-ENTRY ( -- )
   [: BUSY-INSTALL ;] catch TASK:RETURN ;
: JOIN-UNWRAP ( result<n,n> -- n n )
   MATCH result ok OF 0 ENDOF err OF 1 ENDOF ;MATCH ;
: TASK-REFUSAL ( -- )
   ['] TASK-ENTRY WORKER TASK:ACTIVATE
   WORKER TASK:JOIN JOIN-UNWRAP
   0 T= NLEASE:E-TASK T= ;

: POLICY ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;
TYPED-VARIABLE W-SESSION NSESSION:session

: DECLARE-STAGES ( -- )
   W-SESSION @ 2 3 NBACK:L-CALLED NBACK:DECLARE ;

: DECLARE-CLEAN ( -- )
   W-SESSION @ NBACK:RELEASE
   W-SESSION @ NBACK:RETIRE ;

: DECLARE-WORK ( NSESSION:session -- )
   W-SESSION !
   [: DECLARE-STAGES ;] [: DECLARE-CLEAN ;] finally ;

: DECLARE-CONTEXT ( NLEASE:lease IR-CTX:ctx -- )
   {: l:NLEASE:lease c:IR-CTX:ctx :}
   c l NSESSION:NEW [: DECLARE-WORK ;] NSESSION:WITH-WORK ;

: DECLARE-LEASE ( NLEASE:lease -- )
   0 SAW !
   GPU POLICY CBIND:BIND [: DECLARE-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

: GPU-DECLARE ( -- )
   [: DECLARE-LEASE ;] NLEASE:WITH ;

: ID-AT ( n -- n )
   NBACK:BACKEND@ CTARGET-BACKEND:UNMAKE 2drop drop CTARGET:ID-CODE ;

: EXPECT-SORTED ( -- )
   NBACK:COUNT 0 ?do
      i 0 > if
         i 1- ID-AT
         i ID-AT < TTRUE
      then
   loop ;

public
: RUN ( -- )
   T-RESET
   s" complete ARM64 and x64 providers load from passes.f" T-LABEL
   NBACK:COUNT 2 T=
   ARM NBACK:LOWERS? TTRUE
   ARM NBACK:EMITS? TTRUE
   X64 NBACK:LOWERS? TTRUE
   X64 NBACK:EMITS? TTRUE
   BIG NBACK:LOWERS? TFALSE
   10 CTARGET:ID NBACK:ID-ROW ID-AT 10 T=
   20 CTARGET:ID NBACK:ID-ROW ID-AT 20 T=
   CTARGET-ARCH:AARCH64 NBACK:MODE@
      NBACK-MODE:EXCLUSIVE-SESSION NBACK-MODE:EQ TTRUE
   [: NO-GPU ;] E-CTGT-UNLOADED TTHROWSQ
   s" no installer callback or writable registry pointer is public" T-LABEL
   s" BAD-INSTALL ( CTARGET:backend -- ) [: drop ;] CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 1 T=
   s" BAD-ROW ( n -- ptr NBACK:pass ) NBACK:PASS-ROW"
      CHECK-QUIET-CANDIDATE! 1 T=
   s" incomplete or wrongly typed registration cannot publish" T-LABEL
   s" BAD-REG ( CTARGET:backend -- ) [: drop ;] NBACK:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   NBACK:COUNT 2 T=
   ARM NBACK:LOWERS? TTRUE
   X64 NBACK:EMITS? TTRUE
   [: NO-GPU ;] E-CTGT-UNLOADED TTHROWSQ
   s" typed backend and pass records reject wrong callbacks" T-LABEL
   s" BAD-BACK ( n CTARGET:arch n [ CTARGET:contract -- bool ] -- CTARGET:backend ) CTARGET-BACKEND:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-PASS ( n -- NBACK:pass ) NBACK-PASS:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" providers insert by stable id while row lookup tracks moved passes" T-LABEL
   90 CTARGET-ARCH:PTX [: YES ;] [: NO ;] BACK
      [: GPU-DECL ;] FAKE-PASS NBACK:REGISTER
   40 CTARGET-ARCH:A32 INSTALL
   70 CTARGET-ARCH:THUMB2 INSTALL
   25 CTARGET-ARCH:C66X INSTALL
   60 CTARGET-ARCH:WASM INSTALL
   NBACK:COUNT 7 T=
   EXPECT-SORTED
   10 CTARGET:ID NBACK:ID-ROW ID-AT 10 T=
   20 CTARGET:ID NBACK:ID-ROW ID-AT 20 T=
   CTARGET-ARCH:AARCH64 NBACK:ROW CTARGET-ARCH:PTX NBACK:ROW <> TTRUE
   GPU NBACK:LOWERS? TTRUE
   GPU NBACK:EMITS? TFALSE
   CTARGET-ARCH:PTX NBACK:MODE@
      NBACK-MODE:EXCLUSIVE-SESSION NBACK-MODE:EQ TTRUE
   GPU-DECLARE SAW @ 100 T=
   s" rejected duplicate id and architecture leave existing providers usable" T-LABEL
   [: DUP-ID ;] E-CTGT-REGISTERED TTHROWSQ
   [: DUP-ARCH ;] E-CTGT-REGISTERED TTHROWSQ
   [: INVALID-ID ;] E-CTGT-ID TTHROWSQ
   NBACK:COUNT 7 T=
   ARM NBACK:LOWERS? TTRUE
   X64 NBACK:EMITS? TTRUE
   GPU NBACK:LOWERS? TTRUE
   GPU-DECLARE SAW @ 100 T=
   s" active lease and task refuse publication without disturbing providers" T-LABEL
   [: BUSY-ROOT ;] NLEASE:WITH
   TASK-REFUSAL
   NLEASE:IDLE-CK
   NBACK:COUNT 7 T=
   GPU-DECLARE SAW @ 100 T=
   T-REPORT ;
;package
CTREG-TEST:RUN
