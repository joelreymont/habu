\ stage2.fs — the FIXPOINT driver: the running standalone (stage1) reads the
\ compiler's own source from /tmp/stage2-src, compiles it with the ported engine
\ builder (EMIT-FORTH), wraps it in the target executable (BUILD-IMAGE), and writes the
\ unsigned stage2 binary to /tmp/stage2-got. The native build-fixpoint driver
\ asserts stage2 is byte-identical to the previous native stage for the same source.
\ fixpoint I/O paths — the single knobs; the build-fixpoint driver owns artifacts
256 constant S2-PATH-CAP
s" S2-PATH-CAP" s" -- n" TRUST
create S2-PATH-BUF S2-PATH-CAP allot
s" S2-PATH-BUF" s" -- ptr u8" TRUST

: S2-PATH-CHECK ( n -- )
   S2-PATH-CAP > IF s" stage2: path exceeds buffer" 74 die THEN ;

: S2-ROOT ( -- ptr u8 n )
   SCRIPT-ARGC 0 > IF 0 SCRIPT-ARGV$ EXIT THEN
   s" HB_TMP" GETENV dup 0= IF drop drop s" /tmp" THEN ;

: S2-PATH ( ptr u8 n -- ptr u8 n ) {: a u :}
   S2-ROOT {: root:ptr rootu :}
   rootu 1 + u + S2-PATH-CHECK
   rootu 0 ?do  root i + c@  S2-PATH-BUF i + c!  loop
   47 S2-PATH-BUF rootu + c!
   u 0 ?do  a i + c@  S2-PATH-BUF rootu + 1 + i + c!  loop
   S2-PATH-BUF rootu 1 + u + ;

: S2-IN ( -- ptr u8 n )
   s" stage2-src" S2-PATH ;

: S2-OUT ( -- ptr u8 n )
   s" stage2-got" S2-PATH ;
variable SBUF  variable SLEN  variable SFD  variable SRD
$C0000 constant S2-SOURCE-CAP   \ mmap'd source cap; prefix grew past $A0000 with sumtype.f
$1002 constant S2-MAP-PRIVATE-ANON
: SBUF@ SBUF @ ;
s" SBUF@" s" -- ptr u8" TRUST

: S2-ALLOC-SOURCE ( -- )
   0 S2-SOURCE-CAP 3 S2-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" stage2: source mmap failed" 74 die then
   SBUF ! ;

: READ-SRC ( -- )
   S2-IN PATH0 0 0 open SFD !
   SFD @ 0 < IF s" stage2: cannot open source" 74 die THEN
   S2-ALLOC-SOURCE  0 SLEN !
   BEGIN                                                 \ loop: read() may return short
     SFD @  SBUF@ SLEN @ +  S2-SOURCE-CAP SLEN @ -  read SRD !
     SRD @ 0 >
   WHILE  SLEN @ SRD @ + SLEN !  REPEAT
   SRD @ 0 < IF SFD @ close s" stage2: read failed" 74 die THEN
   SFD @ close
   SLEN @ 0 > 0= IF s" stage2: empty source" 74 die THEN
   SLEN @ S2-SOURCE-CAP = IF s" stage2: source exceeds buffer" 74 die THEN ;

: GO ( -- )
   READ-SRC
   DRV-RETIRE-RELOADS
   SBUF@ SLEN @ EMIT-FORTH
   s" hb" S2-OUT DRV-EMIT-IMAGE ;

\ Process boundary: report uncaught throws instead of exiting silently
\ (driver-io.f DRV-FAIL; exit code stays the throw code when representable,
\ else die maps it to UNCAUGHT-RC).
: S2-RUN ( -- )
   [: GO ;] catch
   dup 0 = IF drop DRV-EXIT-OK THEN
   DRV-FAIL ;

S2-RUN
