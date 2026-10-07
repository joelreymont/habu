\ native-builder-image-refusals.f - a saved native builder refuses a bad
\ command line, a rejected checked definition after the hook, and a rejected
\ definition in the checker source before the hook. No refusal publishes an
\ output image.
\ test/native-build-entry.f pins the usage wording for the source entry; the
\ saved builder's refusal only has to name the options it accepts.
\ test/native-builder-image-lib.f has the fixture and the other rows; run alone:
\ bin/hb --load test/native-builder-image-refusals.f

require lib/test.f
require lib/fs.f
require lib/string.f
require lib/process-cwd.f
require lib/time.f
require test/native-builder-image-lib.f

package NATIVE-BUILDER-IMAGE-TEST

create REJECTED FS-PATH-CAP allot      variable REJECTED-U
create ROLES FS-PATH-CAP allot         variable ROLES-U
create CHECKER FS-PATH-CAP allot       variable CHECKER-U

: REJECTED$ ( -- ptr u8 n ) REJECTED REJECTED-U @ ;
: ROLES$ ( -- ptr u8 n ) ROLES ROLES-U @ ;
: CHECKER$ ( -- ptr u8 n ) CHECKER CHECKER-U @ ;

\ 74 is tools/native-build-args.f BUILD-RC, its exit for a refused command
\ line; the refusal names both options.
: USAGE-REFUSED ( -- )
   RC @ 74 T=
   ERR ERR-U @ s" whitebox" CONTAINS? TTRUE
   ERR ERR-U @ s" --target" CONTAINS? TTRUE ;

: SAVED-ARGS-RUN ( -- )
   false ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: SAVED-ARGS ( -- )
   s" a saved builder refuses missing and invalid arguments before compilation" T-LABEL
   PROC-CWD:ARGV-ENV-CWD-RESET
   SAVED-ARGS-RUN
   USAGE-REFUSED
   PROC-CWD:ARGV-ENV-CWD-RESET
   REJECTED$ ARGV+ s" wightbox" ARGV+
   SAVED-ARGS-RUN
   USAGE-REFUSED
   PROC-CWD:ARGV-ENV-CWD-RESET
   REJECTED$ ARGV+ s" whitebox" ARGV+ s" extra" ARGV+
   SAVED-ARGS-RUN
   USAGE-REFUSED
   REJECTED$ FILE? 0= TTRUE ;

: REJECT-EDIT ( -- )
   s" a saved builder rejects source made invalid in the first post-hook prefix file" T-LABEL
   ROLES$ S\" \npackage BUILDER-IMAGE-BAD public\n: BAD ( n -- n ) s\" wrong\" ;\n;package\n" APPEND-FILE
   TIME:MONO-NS {: start:n :}
   REJECTED$ false false SAVED-BUILD
   start s" saved-bad-source" ELAPSED
   \ The checker's reject (70) is caught and named; the builder exits BUILD-RC.
   RC @ 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code 70" CONTAINS? TTRUE
   ERR ERR-U @ s" habu: in bad" CONTAINS? TTRUE
   ERR ERR-U @ s" expected:" CONTAINS? TTRUE
   ERR ERR-U @ s" actual:" CONTAINS? TTRUE
   REJECTED$ FILE? 0= TTRUE ;

: REJECT-CHECKER-EDIT ( -- )
   s" a saved builder rejects a mismatched definition in its pre-hook checker source" T-LABEL
   s" src/core/roles.f" TREE$ TREE-COPY:FILE
   CHECKER$ S\" \npackage BUILDER-IMAGE-PREHOOK public\n: BAD ( n -- n ) 0= ;\n;package\n" APPEND-FILE
   REJECTED$ false false SAVED-BUILD
   RC @ 74 T=
   ERR ERR-U @ s" habu: in bad" CONTAINS? TTRUE
   REJECTED$ FILE? 0= TTRUE ;

public

: REFUSALS-MAIN ( -- )
   T-RESET
   s" native-builder-image-refusals" SETUP
   PRIVATE-TREE
   s" src/core/roles.f" ROLES ROLES-U TREE-PATH!
   s" src/core/checker.f" CHECKER CHECKER-U TREE-PATH!
   s" rejected-hb" REJECTED REJECTED-U ROOT-PATH!
   SAVED-ARGS
   REJECT-EDIT
   REJECT-CHECKER-EDIT
   T-REPORT ;

;package

NATIVE-BUILDER-IMAGE-TEST:REFUSALS-MAIN
