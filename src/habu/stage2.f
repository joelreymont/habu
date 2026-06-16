\ stage2.fs — the FIXPOINT driver: the running standalone (stage1) reads the
\ compiler's own source from /tmp/stage2-src, compiles it with the ported engine
\ builder (EMIT-FORTH), wraps it in the full Mach-O (BUILD-IMAGE), and writes the
\ unsigned stage2 binary to /tmp/stage2-got. tools/build.sh asserts stage2 is
\ byte-identical to the previous native stage for the same source.
\ fixpoint I/O paths — the single knobs; tools/build.sh owns the artifacts
: S2-IN  s" stage2-src" TMP-PATH ;
: S2-OUT s" stage2-got" TMP-PATH ;
variable SBUF  variable SLEN  variable SFD  variable SRD
$40000 constant SMAX
: SBUF@ SBUF @ ;
s" SBUF@" s" -- ptr u8" TRUST

: READ-SRC
   S2-IN PATH0 0 0 open SFD !
   here SBUF !  SMAX allot  0 SLEN !
   BEGIN                                                 \ loop: read() may return short
     SFD @  SBUF@ SLEN @ +  SMAX SLEN @ -  read SRD !
     SRD @ 0 >
   WHILE  SLEN @ SRD @ + SLEN !  REPEAT
   SFD @ close
   SLEN @ 0 > 0= IF s" stage2: empty source" 74 die THEN
   SLEN @ SMAX = IF s" stage2: source exceeds buffer" 74 die THEN ;

: GO
   READ-SRC
   SBUF@ SLEN @ EMIT-FORTH
   BUILD-IMAGE
   s" hb" SET-SIGID  CODESIG2
   S2-OUT PATH0 1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
