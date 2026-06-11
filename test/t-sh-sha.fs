\ t-sh-sha.fs — the standalone's SHA-256 (src/core/sha256.f) runs natively and
\ matches FIPS-180 vectors. The standalone will use this to self-sign its Mach-O
\ (ad-hoc CodeDirectory) with zero gforth and zero external codesign.
\ Run: gforth test/t-sh-sha.fs -e bye
require sh-driver.fs
: SHA-OUT ( -- a u )
   0 CL !
   s" src/core/sha256.f"  slurp-file +B   s"  " +B
   s" src/core/sha-check.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ '0' = zero digest mismatches across all three vectors (standalone '.' adds \n)
T{ SHA-OUT s\" 0\n" compare 0= -> true }T
