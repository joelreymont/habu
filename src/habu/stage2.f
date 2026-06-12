\ stage2.fs — the FIXPOINT driver: the running standalone (stage1) reads the
\ compiler's own source from /tmp/stage2-src, compiles it with the ported engine
\ builder (EMIT-FORTH), wraps it in the full Mach-O (BUILD-MACHO), and writes the
\ unsigned stage2 binary to /tmp/stage2-got. t-sh-stage2.fs asserts stage2 is
\ byte-identical to the gforth-built engine for the same source.
create S2P 32 allot   create O2P 32 allot

: PATHZ {: a u d :}
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
variable SBUF  variable SLEN  variable SFD  variable SRD
$40000 constant SMAX

: READ-SRC
   s" /tmp/stage2-src" S2P PATHZ
   S2P 0 0 open SFD !
   here SBUF !  SMAX allot  0 SLEN !
   BEGIN                                                 \ loop: read() may return short
     SFD @  SBUF @ SLEN @ +  SMAX SLEN @ -  read SRD !
     SRD @ 0 >
   WHILE  SLEN @ SRD @ + SLEN !  REPEAT
   SFD @ close
   SLEN @ 0 > 0= IF s" stage2: empty source" 74 die THEN
   SLEN @ SMAX = IF s" stage2: source exceeds buffer" 74 die THEN ;

: GO
   READ-SRC
   SBUF @ SLEN @ EMIT-FORTH
   BUILD-MACHO
   s" hb" SET-SIGID  CODESIG2
   s" /tmp/stage2-got" O2P PATHZ
   O2P 1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
