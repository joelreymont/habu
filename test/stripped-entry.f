\ Resolve stripped roots by the same global/public-qualified token as the engine.
\ This row holds the global roots: the default MAIN, an explicit global entry,
\ and the refusal when the subject has no global MAIN. The package-qualified
\ roots are test/stripped-entry-qualified.f, a gate row of its own; both share
\ the fixture in test/stripped-entry-lib.f.
require test/stripped-entry-lib.f

package STRIPPED-ENTRY-TEST

: DEFAULT-ENTRY ( -- )
   s" " BUILD
   s" stripped default global MAIN build" BUILT
   S\" global-main\n" s" stripped default global MAIN run" RUN-IMAGE ;

: GLOBAL-PRESEED ( -- )
   s" hLp" BUILD
   s" stripped explicit global HLP build" BUILT
   S\" global-helper\n" s" stripped explicit global HLP run" RUN-IMAGE ;

: MISSING-GLOBAL ( -- )
   NO-GLOBAL$ WRITE-SUBJECT
   s" " BUILD
   s" stripped missing global MAIN refused" REFUSED ;

: BODY ( -- )
   s" stripped-entry" PREPARE
   DEFAULT-ENTRY
   GLOBAL-PRESEED
   MISSING-GLOBAL
   s" PASS: stripped global entry identity" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
