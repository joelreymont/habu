\ stage2.f - the FIXPOINT driver: the running standalone (stage1) compiles the
\ compiler's source from <root>/stage2-src with ENGINE-EMIT:FORTH, wraps it with
\ BUILD-IMAGE and writes the unsigned stage2 binary to <root>/stage2-got; root is
\ the script argument, else TMPDIR, else /tmp. build-fixpoint asserts stage2 is
\ byte-identical to the previous native stage. A package, as stdin.f: the AOT seed
\ runs at every boot's prefix end, so a global SLEN would collide with repl.f's.
package STAGE2
private

\ Fixed path scratch; the build-fixpoint driver owns the artifacts.
create PATH-BUF PATH-CAP allot
s" PATH-BUF" s" -- ptr u8" TRUST

: PATH-CHECK ( n -- )
   PATH-CAP > IF s" stage2: path exceeds buffer" 74 die THEN ;

: ROOT ( -- ptr u8 n )
   SCRIPT-ARGC 0 > IF 0 SCRIPT-ARGV$ EXIT THEN
   s" TMPDIR" GETENV dup 0 > IF EXIT THEN
   drop drop
   s" /tmp" ;

: PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT {: root:ptr rootu:n :}
   rootu 1 + u + PATH-CHECK
   rootu 0 ?do  root i + c@  PATH-BUF i + c!  loop
   47 PATH-BUF rootu + c!
   u 0 ?do  a i + c@  PATH-BUF rootu + 1 + i + c!  loop
   PATH-BUF rootu 1 + u + ;

: SRC-PATH$ ( -- ptr u8 n )
   s" stage2-src" PATH ;

: OUT-PATH$ ( -- ptr u8 n )
   s" stage2-got" PATH ;
variable LEN  variable CAP  variable FD  variable GOT
DYNAMIC-BUFFER SRC u8   \ READ-SRC doubles it as the source needs
$10000 constant SOURCE-START

\ The size is learnt by reading, not from stat: the Gforth-built hb-stage0 runs
\ this driver first and its primitives (bootstrap/cg/forth.fs EMIT-PRIMS) have no
\ stat64.
: READ-SRC ( -- )
   SRC-PATH$ PATH0 0 0 open FD !
   FD @ 0 < IF s" stage2: cannot open source" 74 die THEN
   SOURCE-START SRC-RESERVE  SOURCE-START CAP !  0 LEN !
   BEGIN                                                 \ loop: read() may return short
     LEN @ CAP @ = IF CAP @ 2 * dup SRC-RESERVE CAP ! THEN
     FD @  LEN @ SRC  CAP @ LEN @ -  read GOT !
     GOT @ 0 >
   WHILE  LEN @ GOT @ + LEN !  REPEAT
   GOT @ 0 < IF FD @ close s" stage2: read failed" 74 die THEN
   FD @ close
   LEN @ 0 > 0= IF s" stage2: empty source" 74 die THEN ;

: DRIVE ( -- )
   READ-SRC
   DRV-RETIRE-RELOADS
   0 SRC LEN @ ENGINE-BUILD:BUILD
   SIGN-ID:ENGINE$ OUT-PATH$ DRV-EMIT-IMAGE ;

public

\ Process boundary: report uncaught throws instead of exiting silently
\ (driver-io.f DRV-FAIL; exit code stays the throw code when representable,
\ else die maps it to UNCAUGHT-RC).
: RUN ( -- )
   [: DRIVE ;] catch
   dup 0 = IF drop DRV-EXIT-OK THEN
   DRV-FAIL ;

;package

STAGE2:RUN
