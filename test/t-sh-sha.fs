\ t-sh-sha.fs — the standalone's SHA-256 (selfhost/sha256.fs) runs natively and
\ matches FIPS-180 vectors. The standalone will use this to self-sign its Mach-O
\ (ad-hoc CodeDirectory) with zero gforth and zero external codesign.
\ Run: gforth test/t-sh-sha.fs -e bye
require nf.fs
require tester.fs
create CBUF 32768 allot   variable CL
: +B {: a u -- }  a  CBUF CL @ +  u move  u CL +! ;
: SHA-OUT ( -- a u )
   0 CL !
   s" selfhost/sha256.fs"  slurp-file +B   s"  " +B
   s" selfhost/sha-check.fs" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ '0' = zero digest mismatches across all three vectors (standalone '.' adds \n)
T{ SHA-OUT s\" 0\n" compare 0= -> true }T
