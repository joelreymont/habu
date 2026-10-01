\ bootstrap-begin-nest.fs - Gforth-hosted stage0 BEGIN-frame regression.

s" HABU_TARGET" getenv nip 0= [IF]
   .( bootstrap-begin-nest: HABU_TARGET is required ) cr
   64 (bye)
[THEN]

require nf.fs

: BN-STAGE0-TEST ( -- )
   s" test/bootstrap-begin-nest-src.f" slurp-file NF-RUN
   $? WSTAT>RC 0<> abort" stage0 BEGIN nest process failed"
   s\" ok\n" NF= 0= abort" stage0 BEGIN nest output mismatch" ;

\ NF-RUN drops fd 2; the refusal is judged by its exit status and its line.
: BN-OVER-RUN ( src-a src-u -- rc )
   NF-BIN$ FORTH-EXE
   0 NF-CMD-U !  NF-BIN$ NF-ARG,  s"  > " NF-CMD,  NF-OUT$ NF-ARG,  s"  2>&1" NF-CMD,
   NF-CMD NF-CMD-U @ system $? WSTAT>RC
   NF-OUT$ slurp-file NFOUT 2! ;

: BN-OVER-TEST ( -- )
   s" test/bootstrap-begin-nest-over-src.f" slurp-file BN-OVER-RUN
   75 <> abort" stage0 BEGIN nest past the frame limit: exit status is not 75"
   s\" hb: BEGIN nesting full at 28 frames: BN-OVER needs 29\n" NF= 0=
      abort" stage0 BEGIN nest past the frame limit: refusal does not name the limit and depth" ;

BN-STAGE0-TEST
BN-OVER-TEST
bye
