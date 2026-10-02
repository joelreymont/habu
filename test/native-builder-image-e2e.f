\ native-builder-image-e2e.f - a saved native builder builds its tree as that
\ tree stands when it runs. The builder is saved with a proof word in the
\ prefix, the word is edited, and the product the builder then publishes runs
\ the edit. That product is the normal engine, so C2 programs load on it at
\ both tiers. test/native-builder-image-lib.f has the fixture and the other
\ rows; run alone: bin/hb --load test/native-builder-image-e2e.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/process-cwd.f
require lib/time.f
require test/native-builder-image-lib.f

package NATIVE-BUILDER-IMAGE-TEST

$10000 constant SOURCE-CAP
create SOURCE SOURCE-CAP allot
create SAVED FS-PATH-CAP allot         variable SAVED-U

: SAVED$ ( -- ptr u8 n ) SAVED SAVED-U @ ;

: SEED-PROOF ( -- )
   REPL$ S\" \npackage BUILDER-IMAGE-PROOF\nprivate\n: CALLEE ( n -- n ) 1 + ;\npublic\n: RUN ( -- n ) 41 CALLEE ;\n;package\n" APPEND-FILE ;

: EDIT-CALLEE ( -- )
   REPL$ SOURCE SOURCE-CAP READ-ALL {: size:n :}
   SOURCE size s" : CALLEE ( n -- n ) 1 + ;" FIND-SUB MATCH option
      none OF E-BUILD-SOURCE throw ENDOF
      some OF IDX>N {: at:n :}
         50 SOURCE at + s" : CALLEE ( n -- n ) " nip + c!
      ENDOF
   ;MATCH
   REPL$ SOURCE size WRITE-ALL ;

: RUN-PROOF ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   SAVED$ >LEN TREE$ >LEN
   S\" BUILDER-IMAGE-PROOF:RUN . cr\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS ERR-U @ 0 T=
   OUT OUT-U @ S\" 43\n\n" T$= ;

: EDITED-PRODUCT ( -- )
   s" a saved builder publishes the source edited after it was saved" T-LABEL
   TIME:MONO-NS {: start:n :}
   SAVED$ false false SAVED-BUILD
   start s" saved-product" ELAPSED
   SUCCESS
   RUN-PROOF ;

: RUN-C2 ( ptr u8 n -- ) {: source:ptr sourceu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   source sourceu ARGV+
   SAVED$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   SUCCESS ;

: RUN-C2-TIER1 ( ptr u8 n -- ) {: script:ptr scriptu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   SAVED$ >LEN TREE$ >LEN
   script scriptu >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS ;

: PRODUCT-C2 ( -- )
   s" the normal product loads typed C2 initialization, owners and XML" T-LABEL
   s" test/c2-init-program.f" RUN-C2
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   s" lib/xml/c2.f" RUN-C2
   s" test/c2-owner-producer-program.f" RUN-C2
   OUT OUT-U @ s" c2-owner-producer-program: ok" CONTAINS? TTRUE
   s" test/c2-owner-producer-refusals.f" RUN-C2
   S\" 1 set-tier\nrequire test/c2-init-program.f\n" RUN-C2-TIER1
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   S\" 1 set-tier\nrequire lib/xml/c2.f\n" RUN-C2-TIER1
   S\" 1 set-tier\nrequire test/c2-owner-producer-program.f\n" RUN-C2-TIER1
   OUT OUT-U @ s" c2-owner-producer-program: ok" CONTAINS? TTRUE ;

public

: E2E-MAIN ( -- )
   T-RESET
   s" native-builder-image-e2e" SETUP
   PRIVATE-TREE
   s" test/c2-init-program.f" COPY-MEMBER
   s" test/c2-owner-producer-program.f" COPY-MEMBER
   s" test/c2-owner-producer-refusals.f" COPY-MEMBER
   s" saved-hb" SAVED SAVED-U ROOT-PATH!
   SEED-PROOF
   BUILD-IMAGE
   EDIT-CALLEE
   EDITED-PRODUCT
   PRODUCT-C2
   T-REPORT ;

;package

NATIVE-BUILDER-IMAGE-TEST:E2E-MAIN
