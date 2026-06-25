\ signature-scan-emitter-test.f - source-shape regression for signature scanners.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/signature-scan-emitter-test.f

$20000 constant SSET-CAP

create SSET-BUF SSET-CAP allot
variable SSET-LEN

: SSET-SOURCE ( -- ptr u8 n )
   SSET-BUF SSET-LEN @ ;

: SSET-LOAD ( ptr u8 n -- )
   SSET-BUF SSET-CAP READ-ALL SSET-LEN ! ;

: SSET-HAS? ( ptr u8 n -- bool )
   SSET-SOURCE 2swap CONTAINS? ;

: SSET-MUST-HAVE ( ptr u8 n -- )
   SSET-HAS? TTRUE ;

: SSET-MUST-LACK ( ptr u8 n -- )
   SSET-HAS? 0= TTRUE ;

: SSET-COUNT ( ptr u8 n -- n ) {: needle:ptr needleu :}
   needleu 0= if 0 exit then
   SSET-LEN @ needleu < if 0 exit then
   0 0 begin dup SSET-LEN @ needleu - <= while
      SSET-BUF over + needleu needle needleu STR= if swap 1+ swap then
      1+
   repeat drop ;

: SSET-COUNT= ( ptr u8 n n -- ) {: needle:ptr needleu want :}
   needle needleu SSET-COUNT want T= ;

: SSET-TEST-COMMON-HELPERS ( -- )
   s" : C-SIG-START ( n -- )" SSET-MUST-HAVE
   s" : C-SIG-END ( n -- )" SSET-MUST-HAVE
   s" : C-SIG-INNER$ ( -- )" SSET-MUST-HAVE
   s" : C-SIG-FULL$ ( -- )" SSET-MUST-HAVE
   s" : C-SIG-CAPTURE-TSIG ( -- )" SSET-MUST-HAVE
   s" : C-SIG-BAD ( -- )" SSET-MUST-HAVE
   s" : C-COLON-MAYBE-SIG ( -- )" SSET-MUST-HAVE ;

: SSET-TEST-NATIVE ( -- )
   s" src/habu/habu2.f" SSET-LOAD
   SSET-TEST-COMMON-HELPERS
   s" : C-PARSE-REQUIRED-SIG ( -- )" SSET-MUST-HAVE
   s" : C-PARSE-TRUST-SIG ( -- )" SSET-MUST-HAVE
   s" C-PARSE-REQUIRED-SIG ;" SSET-MUST-HAVE
   s" C-COLON-MAYBE-SIG" 2 SSET-COUNT=
   s" C-SIG-START" 4 SSET-COUNT=
   s" C-SIG-CAPTURE-TSIG" 3 SSET-COUNT=
   s" LBL LBL LBL LBL LBL {: ws got scan done bad :}" SSET-MUST-LACK
   s" LBL {: nsig :}  LBL {: sigq :}  LBL {: sp1 :}" SSET-MUST-LACK
   s" 16 14 1 ADDI,  10 15 14 SUB" SSET-MUST-LACK ;

: SSET-TEST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" SSET-LOAD
   SSET-TEST-COMMON-HELPERS
   s" C-COLON-MAYBE-SIG" 2 SSET-COUNT=
   s" C-SIG-START" 3 SSET-COUNT=
   s" C-SIG-CAPTURE-TSIG" 2 SSET-COUNT=
   s" LBL {: nsig :}  LBL {: sigq :}  LBL {: sp1 :}" SSET-MUST-LACK
   s" 16 14 1 ADDI,  10 15 14 SUB" SSET-MUST-LACK ;

: SSET-MAIN ( -- )
   T-RESET
   SSET-TEST-NATIVE
   SSET-TEST-BOOTSTRAP
   T-REPORT
   s" signature-scan-emitter-test: ok" type cr ;

SSET-MAIN
