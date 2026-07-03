\ signature-scan-emitter-test.f - source-shape regression for signature scanners.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/test/src-shape.f tools/signature-scan-emitter-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/test/src-shape.f

: SSET-TEST-COMMON-HELPERS ( -- )
   s" : C-SIG-INNER$ ( -- )" SHAPE:MUST-HAVE
   s" : C-SIG-FULL$ ( -- )" SHAPE:MUST-HAVE
   s" : C-SIG-CAPTURE-TSIG ( -- )" SHAPE:MUST-HAVE
   s" : C-SIG-BAD ( -- )" SHAPE:MUST-HAVE
   s" : C-COLON-MAYBE-SIG ( -- )" SHAPE:MUST-HAVE ;

: SSET-TEST-NATIVE ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD
   SSET-TEST-COMMON-HELPERS
   s" : C-SIG-START ( label -- )" SHAPE:MUST-HAVE
   s" : C-SIG-END ( label -- )" SHAPE:MUST-HAVE
   s" : C-PARSE-REQUIRED-SIG ( -- )" SHAPE:MUST-HAVE
   s" : C-PARSE-TRUST-SIG ( -- )" SHAPE:MUST-HAVE
   s" C-PARSE-REQUIRED-SIG ;" SHAPE:MUST-HAVE
   s" C-COLON-MAYBE-SIG" 2 SHAPE:COUNT=
   s" C-SIG-START" 4 SHAPE:COUNT=
   s" C-SIG-CAPTURE-TSIG" 3 SHAPE:COUNT=
   s" LBL LBL LBL LBL LBL {: ws got scan done bad :}" SHAPE:MUST-LACK
   s" LBL {: nsig :}  LBL {: sigq :}  LBL {: sp1 :}" SHAPE:MUST-LACK
   s" 16 14 1 ADDI,  10 15 14 SUB" SHAPE:MUST-LACK ;

: SSET-TEST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" SHAPE:LOAD
   SSET-TEST-COMMON-HELPERS
   s" : C-SIG-START ( n -- )" SHAPE:MUST-HAVE
   s" : C-SIG-END ( n -- )" SHAPE:MUST-HAVE
   s" C-COLON-MAYBE-SIG" 2 SHAPE:COUNT=
   s" C-SIG-START" 4 SHAPE:COUNT=
   s" C-SIG-CAPTURE-TSIG" 3 SHAPE:COUNT=
   s" LBL {: nsig :}  LBL {: sigq :}  LBL {: sp1 :}" SHAPE:MUST-LACK
   s" 16 14 1 ADDI,  10 15 14 SUB" SHAPE:MUST-LACK ;

: SSET-MAIN ( -- )
   T-RESET
   SSET-TEST-NATIVE
   SSET-TEST-BOOTSTRAP
   T-REPORT
   s" signature-scan-emitter-test: ok" type cr ;

SSET-MAIN
