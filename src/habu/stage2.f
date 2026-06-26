\ stage2.fs — the FIXPOINT driver: the running standalone (stage1) reads the
\ compiler's own source from /tmp/stage2-src, compiles it with the ported engine
\ builder (EMIT-FORTH), wraps it in the target executable (BUILD-IMAGE), and writes the
\ unsigned stage2 binary to /tmp/stage2-got. The native build-fixpoint driver
\ asserts stage2 is byte-identical to the previous native stage for the same source.
\ fixpoint I/O paths — the single knobs; the build-fixpoint driver owns artifacts
: S2-IN  s" stage2-src" TMP-PATH ;
: S2-OUT s" stage2-got" TMP-PATH ;
variable SBUF  variable SLEN  variable SFD  variable SRD
$80000 constant S2-SOURCE-CAP
$1002 constant S2-MAP-PRIVATE-ANON
: SBUF@ SBUF @ ;
s" SBUF@" s" -- ptr u8" TRUST

: S2-ALLOC-SOURCE ( -- )
   0 S2-SOURCE-CAP 3 S2-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" stage2: source mmap failed" 74 die then
   SBUF ! ;

: READ-SRC
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

: GO
   READ-SRC
   SBUF@ SLEN @ EMIT-FORTH
   BUILD-IMAGE
   s" hb" SET-SIGID  CODESIG2
   S2-OUT DRV-WRITE-IMAGE
   DRV-EXIT-OK ;
GO
