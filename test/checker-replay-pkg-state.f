\ checker-replay-pkg-state.f - a neutral source replay resolves against its OWN
\ package and `using` state, not the caller's (dot habu-own-pkg-state-acf7086c).
\
\ CHECKER-SCOPE-START-NEUTRAL declares that the source about to be replayed is a
\ standalone file, so its context is top level and its imports are exactly the
\ ones the source itself declares. The package half of that was already owned by
\ the checker's mirror; the `using` half was not, and two resolution legs
\ consulted the live engine regardless:
\
\   - CK-OPEN-CLAIMS? asked the engine which wordlist would bind a bare tail,
\     which during a replay is the CALLER's open package. A standalone
\     `: R ( n -- n n ) dup ;` was refused with E-UNDEFINED for `dup` purely
\     because the calling package defined its own DUP.
\   - the used-publics scan was bounded by the engine's live using depth, so the
\     caller's imports were in scope for the replayed file, and a `using` the
\     file itself declared did nothing at all -- verify-source.f had no row for
\     `using` or `;using`.
\
\ Both directions matter and both are asserted here: what the caller has open
\ must not reach the replayed source, and what the source declares must.
\
\ The cases run as interpreted statements INSIDE a real open package that owns
\ DUP and has two real imports live, because that caller context is the thing
\ under test -- the engine's using depth has to be non-zero at the moment the
\ replay runs. A case driven from top level would pass with the bug present, so
\ the top-level block at the end is a control, not the test.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f
\   src/habu/verify-source.f test/checker-replay-pkg-state.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

\ Two packages the caller imports, so its context is genuinely non-empty while
\ every replay runs.
package CRPS-SUPPLIER
public
: WIDGET ( -- n ) 77 ;
: GADGET ( -- n ) 88 ;
;package

package CRPS-OTHER
public
: SPROCKET ( -- n ) 99 ;
;package

package CRPS-CLASH
public
: WIDGET ( -- n ) 31 ;
;package

\ The replay driver. It owns no caller context of its own; every case supplies
\ that by where it is written.
package CRPS
private

variable SRC-A
variable SRC-U

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

create DIAG-BUF $2000 allot

\ A quotation cannot read the enclosing word's locals, so the source span travels
\ to the caught body through these two cells.
: ACT ( -- )
   SRC-A-FIELD @ SRC-U @ VERIFY:SOURCE-BUF ;

public

\ The checker's own error constants are prefix-internal and invisible to later
\ sources, so the code is named here with its checker spelling, the same way
\ test/using-test.f names 7141.
7142 constant E-UNBALANCED     \ E-USING-UNBALANCED: `;using` with no using open in a replay

7144 constant E-AMBIGUOUS      \ two active imported publics claim the same tail

\ One replay at neutral top level, with the checker's diagnostics captured rather
\ than printed: several cases reject deliberately and their text belongs to the
\ case, not to the gate's stderr. The scope is opened and closed around the
\ catch, so a rejecting case still restores the caller.
: REPLAY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A-FIELD !
   u SRC-U !
   DIAG-BUF $2000 DIAG-BUFFER!
   CHECKER-SCOPE-START-NEUTRAL
   [: ACT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   DIAG-BUFFER-OFF
   rc ;

\ Exercise nested neutral scopes while the verifier owns the import mirror.
\ Each scope replaces slot zero with a different real supplier.
TRUSTED: NESTED-IMPORTS ( -- bool bool )
   CHECKER-SCOPE-START-NEUTRAL
   CHECKER-VERIFY-PKG-START
   s" CRPS-SUPPLIER" CHECKER-USING-PUSH
   CHECKER-SCOPE-START-NEUTRAL
   s" CRPS-OTHER" CHECKER-USING-PUSH
   CHECKER-SCOPE-START-NEUTRAL
   s" CRPS-SUPPLIER" CHECKER-USING-PUSH
   CHECKER-SCOPE-DONE
   s" CRPS-INNER ( -- n ) SPROCKET" CHECK! -1 =
   CHECKER-SCOPE-DONE
   s" CRPS-OUTER ( -- n ) WIDGET" CHECK! -1 =
   CHECKER-VERIFY-PKG-DONE
   CHECKER-SCOPE-DONE ;

\ The nested scope replaces a package whose import baseline is one with a
\ package whose baseline is zero. Rollback must restore that baseline too.
TRUSTED: NESTED-PACKAGE-IMPORTS ( -- bool bool )
   CHECKER-SCOPE-START-NEUTRAL
   CHECKER-VERIFY-PKG-START
   s" CRPS-SUPPLIER" CHECKER-USING-PUSH
   s" CRPS-FRAME" CHECKER-PACKAGE
   s" CRPS-OTHER" CHECKER-USING-PUSH
   CHECKER-SCOPE-START-NEUTRAL
   s" CRPS-INNER-FRAME" CHECKER-PACKAGE
   s" CRPS-OTHER" CHECKER-USING-PUSH
   CHECKER-SCOPE-DONE
   CHECKER-END-PACKAGE
   CHECKER-USING-N 1 =
   s" CRPS-FRAME-USE ( -- n ) WIDGET" CHECK! -1 =
   CHECKER-VERIFY-PKG-DONE
   CHECKER-SCOPE-DONE ;

;package

T-RESET

\ ---- the caller context under test -------------------------------------------
\ An open package that publishes its own DUP, with two live imports.
package CRPS-CALLER
using CRPS-SUPPLIER
using CRPS-OTHER
private

\ The caller's own DUP: legal, published into this package's wordlist, and
\ exactly the kind of name that used to reach into a replayed file.
: DUP ( n -- n n ) dup ;

\ Compiled while the imports are open, so it proves the caller's own scope still
\ works after the replays have been through it.
: OWN-IMPORT-USE ( -- n ) WIDGET ;
: OTHER-IMPORT-USE ( -- n ) SPROCKET ;

\ 1. A bare tail that is a GLOBAL must keep resolving to the global even when the
\ caller's open package publishes the same tail. Reproducer A from the dot: this
\ returned 70 with `undefined word 'dup'` before the fix.
s" a standalone body binds the global tail the caller's package also owns" T-LABEL
s" : CRPS-R2 ( n -- n n ) dup ;" CRPS:REPLAY 0 T=

\ 2. The caller's `using` must NOT be in scope for the replayed file. The source
\ names WIDGET bare and declares no import, so it must be refused -- if the
\ caller's `using CRPS-SUPPLIER` leaked, this would certify instead.
s" a caller's using does not put its publics in the replayed file's scope" T-LABEL
s" : CRPS-R1 ( -- n ) WIDGET ;" CRPS:REPLAY 0 <> TTRUE

\ 3. The file's OWN `using` must work. Reproducer B from the dot: the same text
\ loads through the engine and was refused on replay before the fix.
s" a using the replayed source declares resolves that package's publics" T-LABEL
s" using CRPS-SUPPLIER : CRPS-R3 ( -- n ) WIDGET ; ;using" CRPS:REPLAY 0 T=

\ 4. `;using` must actually close the scope: the second definition sits past the
\ closer and may not see the import.
s" ;using closes the import for the rest of the replayed source" T-LABEL
s" using CRPS-SUPPLIER : CRPS-R4 ( -- n ) WIDGET ; ;using : CRPS-R5 ( -- n ) GADGET ;"
   CRPS:REPLAY 0 <> TTRUE

\ 5. A `;using` with nothing open is refused by name rather than clamped to zero.
s" a ;using with no using open is refused by name" T-LABEL
s" ;using" CRPS:REPLAY CRPS:E-UNBALANCED T=

\ Package close restores the depth at package entry, including imports opened
\ outside that package. An explicit inner ;using must give the same result.
s" ;package removes only the package's imports" T-LABEL
s" using CRPS-SUPPLIER package CRPS-CLOSE using CRPS-CLASH public : V ( -- n ) 7 ; ;package : CRPS-CLOSED ( -- n ) WIDGET ; ;using"
   CRPS:REPLAY 0 T=
s" an inner import is unavailable after ;package" T-LABEL
s" package CRPS-GONE using CRPS-OTHER public : V ( -- n ) SPROCKET ; ;package : CRPS-GONE-USE ( -- n ) SPROCKET ;"
   CRPS:REPLAY 70 T=
s" explicit ;using preserves the outer import across ;package" T-LABEL
s" using CRPS-SUPPLIER package CRPS-EXPLICIT using CRPS-OTHER public : V ( -- n ) SPROCKET ; ;using ;package : CRPS-EXPLICIT-USE ( -- n ) WIDGET ; ;using"
   CRPS:REPLAY 0 T=
s" active imports remain ambiguous inside the package" T-LABEL
s" using CRPS-SUPPLIER package CRPS-AMBIG using CRPS-CLASH public : V ( -- n ) WIDGET ; ;package ;using"
   CRPS:REPLAY CRPS:E-AMBIGUOUS T=
s" a closed package import cannot shadow a later global primitive" T-LABEL
s" package CRPS-COUNTS public : COUNT ( -- n ) 19 ; ;package package CRPS-COUNT-USER using CRPS-COUNTS public : V ( -- n ) 7 ; ;package : CRPS-GLOBAL-COUNT ( ptr u8 -- ptr u8 n ) count ;"
   CRPS:REPLAY 0 T=
\ Compile anew after the throwing package replay: an old callable would retain
\ its binding even if restoration had corrupted the caller's import rows.
: AFTER-PACKAGE-REJECT ( -- n ) WIDGET SPROCKET + ;
AFTER-PACKAGE-REJECT 176 T=

\ 6. The caller comes back intact, after the rejecting cases as well as the
\ accepting ones. A further replay is the exact test of the package half:
\ entering the verifier window PROVES the mirror still equals the engine's live
\ package record and refuses with E-PKG-CONTEXT if it does not.
s" a replay still certifies after the rejecting cases" T-LABEL
s" : CRPS-R6 ( n -- n n ) dup ;" CRPS:REPLAY 0 T=
\ Fresh compilation is required: an earlier compiled caller retains its target
\ even if replay corrupts the import mirror. Use the other package in slot zero.
s" using CRPS-OTHER : CRPS-R7 ( -- n ) SPROCKET ; ;using" CRPS:REPLAY 0 T=
: AFTER-ACCEPT ( -- n ) WIDGET ;
AFTER-ACCEPT 77 T=
s" using CRPS-OTHER : CRPS-RBAD ( -- n ) ;" CRPS:REPLAY 0 <> TTRUE
: AFTER-REJECT ( -- n ) WIDGET ;
AFTER-REJECT 77 T=
CRPS:NESTED-IMPORTS TTRUE TTRUE
CRPS:NESTED-PACKAGE-IMPORTS TTRUE TTRUE
: AFTER-NESTED ( -- n ) WIDGET SPROCKET + ;
AFTER-NESTED 176 T=

s" the caller's own imports still resolve after the replays" T-LABEL
OWN-IMPORT-USE 77 T=
OTHER-IMPORT-USE 99 T=

;using
;using
;package

\ ---- control: the same replays from top level --------------------------------
\ No package, no imports. These certify with or without the fix; they are here so
\ a failure inside the caller block is known to be about the caller's context and
\ not about the source text.
s" control: the global-tail body certifies from top level" T-LABEL
s" : CRPS-T1 ( n -- n n ) dup ;" CRPS:REPLAY 0 T=
s" control: source-declared using certifies from top level" T-LABEL
s" using CRPS-SUPPLIER : CRPS-T2 ( -- n ) WIDGET ; ;using" CRPS:REPLAY 0 T=

T-REPORT
s" checker-replay-pkg-state: ok" type cr
