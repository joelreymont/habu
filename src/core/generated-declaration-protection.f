\ generated-declaration-protection.f - the runner DECLARATION-TRANSACTION:RUN.

package TYPE-DECL
private
package DECLARATION-TRANSACTION

\ s is qualified: DICTIONARY-RECORDS asks NAMESPACED? first.
: NAMESPACE-EXISTS? ( str -- bool ) {: s:str :}
   s XREF-QUAL-INDEX {: split:n :}
   s STR:DATA split >str HEAD-ROW XREF-FOUND? ;

\ A generated declaration is qualified exactly when it reserves a constructor
\ namespace; a private family's bare words (TF-CTOR-PRIV$) allocate no wordlist.
: NAMESPACED? ( str -- bool )
   XREF-QUAL-INDEX 0 >= ;

\ words, the count a declaration generates, is at least 1.
: DICTIONARY-RECORDS ( str n -- n ) {: s:str words:n :}
   s NAMESPACED? 0= IF words EXIT THEN
   s NAMESPACE-EXISTS? IF words EXIT THEN
   words 1 + ;

: PLAN-PREFLIGHT ( str n -- ) {: s:str words:n :}
   s  s words DICTIONARY-RECORDS
   GENERATED-DECL-DICTIONARY:PREFLIGHT ;

EXPORT TYPE-DECL:TDECL-CAPACITY-PREFLIGHT-XT
EXPORT TYPE-DECL:TDECL-TXN-XT

: INSTALL ( -- )
   [: PLAN-PREFLIGHT ;] is TDECL-CAPACITY-PREFLIGHT-XT ;
INSTALL

: SAVEPOINTS ( -- )
   CHECKER-DECL-FRAME:PART-SNAPSHOT
   DECL-EVENT:PART-SNAPSHOT
   GENERATED-DECL-CTOR:PART-SNAPSHOT
   STRUCTURE-MAKE:PART-SNAPSHOT
   GENERATED-DECL-DICTIONARY:SNAPSHOT ;

: PREPARES ( -- )
   CHECKER-DECL-FRAME:PART-PREPARE ;

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
   CHECKER-DECL-FRAME:PART-ROLLBACK ;

: RELEASES ( -- )
   STRUCTURE-MAKE:PART-RELEASE
   GENERATED-DECL-CTOR:PART-RELEASE
   CHECKER-DECL-FRAME:PART-RELEASE ;

\ A rollback that cannot complete leaves a savepoint live with nothing left
\ to restore it from: fail closed, as the checker frame already does,
\ instead of poisoning.
: FAIL ( -- )
   [: ROLLBACKS ;] catch {: cleanup:n :}
   cleanup 0 <> IF s" declaration rollback failed" EXIT-STATUS:INTERNAL die THEN ;

public

\ A failure before the commits finish rolls every participant back in reverse
\ order and rethrows its own code. Type entries are read first.
: RUN ( [ -- ] -- )
   {: body :}
   READ-PENDING
   [: SAVEPOINTS ;] catch {: save-rc:n :}
   save-rc 0 <> IF save-rc throw THEN
   body catch {: body-rc:n :}
   body-rc 0 <> IF FAIL body-rc throw THEN
   [: PREPARES COMMITS ;] catch {: publish-rc:n :}
   publish-rc 0 <> IF FAIL publish-rc throw THEN
   RELEASES ;

private

: INSTALL-RUNNER ( -- ) [: RUN ;] is TDECL-TXN-XT ;
INSTALL-RUNNER

;package
;package

\ The compiled runner holds the only live capability onto the checker frame's
\ steps: retire their source-level names, and the one-shot seams above with them.
\ `undefine` resolves an unqualified name in the CURRENT wordlist, so each is
\ retired by its QUALIFIED name, outside any package block.
undefine CHECKER-DECL-FRAME:PART-SNAPSHOT
undefine CHECKER-DECL-FRAME:PART-PREPARE
undefine CHECKER-DECL-FRAME:PART-ROLLBACK
undefine CHECKER-DECL-FRAME:PART-RELEASE
undefine GENERATED-DECL-DICTIONARY:SNAPSHOT
undefine GENERATED-DECL-DICTIONARY:ROLLBACK
undefine GENERATED-DECL-DICTIONARY:PREFLIGHT
