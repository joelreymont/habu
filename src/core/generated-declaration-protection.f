\ generated-declaration-protection.f - the runner DECLARATION-TRANSACTION:RUN.

package DECLARATION-TRANSACTION

: PLAN-PREFLIGHT ( ptr u8 n n -- )
   GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS
   GENERATED-DECL-DICTIONARY:PREFLIGHT ;

: INSTALL ( -- )
   [: PLAN-PREFLIGHT ;] is TYPE-DECL:TDECL-CAPACITY-PREFLIGHT-XT ;
INSTALL

\ DECL-EVENT's savepoint can throw E-PF-TX (TX-PARENT-REQUIRE); only the checker
\ frame's is taken before it, and the three after it cannot fail.
: SAVEPOINTS ( -- )
   CHECKER-DECL-FRAME:PART-SNAPSHOT
   [: DECL-EVENT:PART-SNAPSHOT ;] catch {: rc:n :}
   rc 0 <> IF CHECKER-DECL-FRAME:PART-ROLLBACK rc throw THEN
   GENERATED-DECL-CTOR:PART-SNAPSHOT
   STRUCTURE-MAKE:PART-SNAPSHOT
   GENERATED-DECL-DICTIONARY:SNAPSHOT ;

: PREPARES ( -- )
   CHECKER-DECL-FRAME:PART-PREPARE
   DECL-EVENT:PART-PREPARE
   GENERATED-DECL-CTOR:PART-PREPARE
   STRUCTURE-MAKE:PART-PREPARE ;

\ DECL-EVENT commits first: it advances PF-COMMIT-N over the declaration's field
\ rows, which the constructor and address-surface commits read.
: COMMITS ( -- )
   DECL-EVENT:PART-COMMIT
   GENERATED-DECL-CTOR:PART-COMMIT
   STRUCTURE-MAKE:PART-COMMIT ;

: ROLLBACKS ( -- )
   GENERATED-DECL-DICTIONARY:ROLLBACK
   STRUCTURE-MAKE:PART-ROLLBACK
   GENERATED-DECL-CTOR:PART-ROLLBACK
   DECL-EVENT:PART-ROLLBACK
   CHECKER-DECL-FRAME:PART-ROLLBACK ;

: RELEASES ( -- )
   STRUCTURE-MAKE:PART-RELEASE
   GENERATED-DECL-CTOR:PART-RELEASE
   DECL-EVENT:PART-RELEASE
   CHECKER-DECL-FRAME:PART-RELEASE ;

\ A rollback that cannot complete leaves a savepoint live with nothing left
\ to restore it from: fail closed, as the checker frame already does
\ (checker.f FRAME-MISMATCH), instead of poisoning.
: FAIL ( n -- )
   {: rc:n :}
   [: ROLLBACKS ;] catch {: cleanup:n :}
   cleanup 0 <> IF s" declaration rollback failed" 76 die THEN
   CLOSE
   rc throw ;

public

\ A failure before the commits finish rolls every participant back in reverse
\ order and rethrows its own code; a declaration opened inside another is E-ACTIVE.
\ Type entries are read before it opens: a read inside one dies (TFAM-FIND).
: RUN ( [ -- ] -- )
   {: body :}
   TFAM:READ-PENDING
   OPEN
   [: SAVEPOINTS ;] catch {: save-rc:n :}
   save-rc 0 <> IF CLOSE save-rc throw THEN
   body catch {: body-rc:n :}
   body-rc 0 <> IF body-rc FAIL THEN
   [: PREPARES COMMITS ;] catch {: publish-rc:n :}
   publish-rc 0 <> IF publish-rc FAIL THEN
   RELEASES
   CLOSE ;

private

: INSTALL-RUNNER ( -- ) [: RUN ;] TYPE-DECL:TXN-INSTALL ;
INSTALL-RUNNER

;package

\ The compiled runner holds the only live capability onto each participant step:
\ retire their source-level names, and the one-shot seams above with them.
\ `undefine` resolves an unqualified name in the CURRENT wordlist, so each is
\ retired by its QUALIFIED name, outside any package block.
undefine CHECKER-DECL-FRAME:PART-SNAPSHOT
undefine CHECKER-DECL-FRAME:PART-PREPARE
undefine CHECKER-DECL-FRAME:PART-ROLLBACK
undefine CHECKER-DECL-FRAME:PART-RELEASE
undefine DECL-EVENT:PART-SNAPSHOT
undefine DECL-EVENT:PART-PREPARE
undefine DECL-EVENT:PART-COMMIT
undefine DECL-EVENT:PART-ROLLBACK
undefine DECL-EVENT:PART-RELEASE
undefine GENERATED-DECL-CTOR:PART-SNAPSHOT
undefine GENERATED-DECL-CTOR:PART-PREPARE
undefine GENERATED-DECL-CTOR:PART-COMMIT
undefine GENERATED-DECL-CTOR:PART-ROLLBACK
undefine GENERATED-DECL-CTOR:PART-RELEASE
undefine STRUCTURE-MAKE:PART-SNAPSHOT
undefine STRUCTURE-MAKE:PART-PREPARE
undefine STRUCTURE-MAKE:PART-COMMIT
undefine STRUCTURE-MAKE:PART-ROLLBACK
undefine STRUCTURE-MAKE:PART-RELEASE
undefine GENERATED-DECL-DICTIONARY:SNAPSHOT
undefine GENERATED-DECL-DICTIONARY:ROLLBACK
undefine GENERATED-DECL-DICTIONARY:PREFLIGHT

\ The field owner's cleanup and release vectors are bound into the compiled
\ declaration-event participant, their only caller, so they are retired too, by
\ qualified name since they live in TFAM's public wordlist (a `using` would not
\ help: `undefine` parses, and parsing words resolve outside using-imports).
undefine TFAM:TDECL-FIELD-CLEANUP-XT
undefine TFAM:TDECL-FIELD-RELEASE-XT
