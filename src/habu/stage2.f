\ stage2.fs — the FIXPOINT driver: the running standalone (stage1) reads the
\ compiler's own source from HB_TMP/stage2-src, compiles it with the ported engine
\ builder (ENGINE-EMIT:FORTH), wraps it in the target executable (BUILD-IMAGE), and writes the
\ unsigned stage2 binary to /tmp/stage2-got. The native build-fixpoint driver
\ asserts stage2 is byte-identical to the previous native stage for the same source.
\
\ The driver is a package like its sibling src/habu/stdin.f, and it has to be:
\ its source buffer cells used to be the global names SBUF/SLEN, and `SLEN` is
\ also the line length the engine's baked line editor (src/habu/repl.f)
\ publishes. Since the AOT seed runs at the end of the engine prefix on every
\ boot (dot habu-decide-arm-the-5234727b), both names would land in one
\ dictionary and the second would die `duplicate definition` before this driver
\ read a byte.
package STAGE2
private

\ fixpoint I/O paths — the single knobs; the build-fixpoint driver owns artifacts
\ These rows expose the fixed path scratch and raw source-buffer cell.
\ Retirement: habu-builder-trust-rows-c5d41af6.
256 constant PATH-CAP
s" PATH-CAP" s" -- n" TRUST
create PATH-BUF PATH-CAP allot
s" PATH-BUF" s" -- ptr u8" TRUST

: PATH-CHECK ( n -- )
   PATH-CAP > IF s" stage2: path exceeds buffer" 74 die THEN ;

: ROOT ( -- ptr u8 n )
   s" HB_TMP" GETENV dup 0 > IF EXIT THEN
   drop drop
   SCRIPT-ARGC 0 > IF 0 SCRIPT-ARGV$ EXIT THEN
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
variable BUF  variable LEN  variable FD  variable GOT
SOURCE-ARENA-CAP constant SOURCE-CAP     \ mmap'd generated compiler source;
                                         \ the 2026-07-13 baseline was 1,036,134
                                         \ bytes. The fixpoint test enforces the
                                         \ named policy from the live source size.
$1002 constant MAP-PRIVATE-ANON
: BUF@ BUF @ ;
s" BUF@" s" -- ptr u8" TRUST

: ALLOC-SOURCE ( -- )
   0 SOURCE-CAP 3 MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" stage2: source mmap failed" 74 die then
   BUF ! ;

: READ-SRC ( -- )
   SRC-PATH$ PATH0 0 0 open FD !
   FD @ 0 < IF s" stage2: cannot open source" 74 die THEN
   ALLOC-SOURCE  0 LEN !
   BEGIN                                                 \ loop: read() may return short
     FD @  BUF@ LEN @ +  SOURCE-CAP LEN @ -  read GOT !
     GOT @ 0 >
   WHILE  LEN @ GOT @ + LEN !  REPEAT
   GOT @ 0 < IF FD @ close s" stage2: read failed" 74 die THEN
   FD @ close
   LEN @ 0 > 0= IF s" stage2: empty source" 74 die THEN
   LEN @ SOURCE-CAP = IF s" stage2: source exceeds buffer" 74 die THEN ;

: DRIVE ( -- )
   READ-SRC
   DRV-RETIRE-RELOADS
   BUF@ LEN @ ENGINE-BUILD:BUILD
   s" hb" OUT-PATH$ DRV-EMIT-IMAGE ;

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
