\ bootstrap-wide-memory.fs - Gforth-hosted stage0 execution regression.

s" HABU_TARGET" getenv nip 0= [IF]
   .( bootstrap-wide-memory: HABU_TARGET is required ) cr
   64 (bye)
[THEN]

require nf.fs
require bootstrap-primitive-registry.fs

: BWM-ATOMIC-REFUSAL ( src-a src-u -- )
   NF-RUN $? WSTAT>RC ENGINE-ERROR:SEAL-VIOLATION <>
   abort" bootstrap atomic write bypassed the sealed friend span" ;

: BWM-STAGE0-TEST ( -- )
   s" 0 data-base $20 + atomic!" BWM-ATOMIC-REFUSAL
   s" 1 data-base $20 + atomic-add drop" BWM-ATOMIC-REFUSAL
   \ Both matching and nonmatching CAS must check the protected write span.
   s" $90 0 data-base $20 + atomic-cas drop" BWM-ATOMIC-REFUSAL
   s" 0 0 data-base $20 + atomic-cas drop" BWM-ATOMIC-REFUSAL
   s" test/bootstrap-wide-memory-src.f" slurp-file NF-RUN
   $? WSTAT>RC 0<> abort" bootstrap wide memory process failed"
   s\" ok\n" NF= 0= abort" bootstrap wide memory mismatch" ;

BWM-STAGE0-TEST
bye
