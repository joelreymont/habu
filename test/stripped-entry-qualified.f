\ Resolve stripped roots by the same global/public-qualified token as the engine.
\ This row holds the package-qualified roots: an explicit public entry whose
\ name collides with a private and a global word, and the refusal of a private
\ one. The global roots are test/stripped-entry.f, a gate row of its own; both
\ share the fixture in test/stripped-entry-lib.f.
require test/stripped-entry-lib.f

package STRIPPED-ENTRY-TEST

: PUBLIC-PRESEED ( -- )
   s" stripped-entry-hostile:hLp" BUILD
   s" stripped explicit public HLP build" BUILT
   S\" public-helper\n" s" stripped explicit public HLP run" RUN-IMAGE ;

: PRIVATE-REFUSED ( -- )
   s" STRIPPED-ENTRY-HOSTILE:SECRET" MAKER-BUILD
   s" stripped private entry refused" REFUSED ;

: BODY ( -- )
   s" stripped-entry-qualified" PREPARE
   PUBLIC-PRESEED
   PRIVATE-REFUSED
   s" PASS: stripped public-qualified entry identity" type cr ;

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
