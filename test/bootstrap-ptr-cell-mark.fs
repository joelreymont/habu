\ bootstrap-ptr-cell-mark.fs - Gforth-hosted stage0 ptr-cell-mark regression.

s" HABU_TARGET" getenv nip 0= [IF]
   .( bootstrap-ptr-cell-mark: HABU_TARGET is required ) cr
   64 (bye)
[THEN]

require nf.fs

: PCM-STAGE0-TEST ( -- )
   s" test/bootstrap-ptr-cell-mark-src.f" slurp-file NF-RUN
   s\" ok\n" NF= 0= abort" stage0 ptr-cell-mark missing or wrong: bootstrap/cg/forth.fs must register ptr-cell-mark for PERSISTED-PTR-VARIABLE" ;

PCM-STAGE0-TEST
bye
