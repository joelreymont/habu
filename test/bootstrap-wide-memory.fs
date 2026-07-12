\ bootstrap-wide-memory.fs - Gforth-hosted stage0 execution regression.

s" HABU_TARGET" getenv nip 0= [IF]
   .( bootstrap-wide-memory: HABU_TARGET is required ) cr
   64 (bye)
[THEN]

require nf.fs

: BWM-STAGE0-TEST ( -- )
   s" test/bootstrap-wide-memory-src.f" slurp-file NF-RUN
   s\" ok\n" NF= 0= abort" bootstrap wide memory mismatch" ;

BWM-STAGE0-TEST
bye
