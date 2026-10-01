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

BN-STAGE0-TEST
bye
