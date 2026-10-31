\ generated-declaration-protection.f - atomic constructor protection owner and
\ the declaration runner DECLARATION-TRANSACTION:RUN. PREPARE proves the
\ irreversible engine registry has room; this owner publishes last, after every
\ reversible owner committed. This is the last participant to load: once the
\ runner is installed, every participant step word and the field owner's cleanup
\ and release seams are retired, and the compiled runner holds the only capabilities.

package GENERATED-DECL-PROTECTION

7169 constant E-PROTECTION-CAP
\ The reason names the registry, so a full registry is not blamed on whichever
\ declaration comes next.
s" the protected-wordlist registry is full" E-PROTECTION-CAP DECL-REJECT:EXPLAIN
$FFFFFFFF constant WID-MAX
4 constant CAP-INIT
$0FFFFFFFFFFFFFFF constant CELL-CAP-MAX

private

create STAGE-BOOT CAP-INIT cells allot
PERSISTED-PTR-VARIABLE STAGE-P   STAGE-BOOT STAGE-P !
variable STAGE-N
variable STAGE-CAP
variable BASE         \ STAGE-N when the declaration took its savepoint

: CELL@ ( ptr a n -- a ) {: base:ptr idx:n :}
   base idx cells + @ ;

: CELL! ( a ptr a n -- ) {: value:a base:ptr idx:n :}
   value base idx cells + ! ;

: GROW-CAP ( n n -- n ) {: need:n cap:n :}
   need 0 <= need CELL-CAP-MAX > or
   cap 0 <= cap CELL-CAP-MAX > or or IF E-PROTECTION-CAP throw THEN
   cap CELL-CAP-MAX 2 / <= IF cap 2 * need max EXIT THEN
   need ;

: STAGE-ENSURE ( -- )
   STAGE-N @ STAGE-CAP @ < IF EXIT THEN
   STAGE-N @ CELL-CAP-MAX >= IF E-PROTECTION-CAP throw THEN
   STAGE-N @ 1 + STAGE-CAP @ GROW-CAP {: cap:n :}
   STAGE-P STAGE-CAP @ cells cap cells REG-GROW1
   cap STAGE-CAP ! ;

: STAGE@ ( n -- n )
   STAGE-P @ swap CELL@ ;

: STAGE+ ( n -- ) {: wid:n :}
   STAGE-ENSURE
   wid STAGE-P @ STAGE-N @ CELL!
   STAGE-N @ 1 + STAGE-N ! ;

: SCRUB-TO-BASE ( -- )
   BASE @ {: keep:n :}
   keep
   BEGIN dup STAGE-N @ < WHILE
      0 STAGE-P @ over CELL!
      1 +
   REPEAT
   drop
   keep STAGE-N ! ;

public

: SNAPSHOT ( -- )
   STAGE-N @ BASE ! ;

: PREPARE ( -- )
   STAGE-N @ BASE @ - prot-wid-room > IF E-PROTECTION-CAP throw THEN ;

: PUBLISH ( -- )
   BASE @
   BEGIN dup STAGE-N @ < WHILE
      dup STAGE@ prot-wid-add
      1 +
   REPEAT
   drop
   SCRUB-TO-BASE ;

: ROLLBACK ( -- )
   SCRUB-TO-BASE ;

private

: STAGE-WORDLIST ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TFAM-CTOR-WORD? 0= IF s" xref: protected-WID constructor mismatch" 76 die THEN
   a u XREF-FIND dup XREF-FOUND? 0= IF
      drop s" xref: protected-WID constructor not found" 76 die
   THEN
   XREF-WORDLIST STAGE+ ;

: PLAN-PREFLIGHT ( ptr u8 n n -- ) {: a:ptr u:n words:n :}
   a u words GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS
      GENERATED-DECL-DICTIONARY:PREFLIGHT
   \ A private family's words go into the declaring package's own private
   \ wordlist: no wordlist is allocated and none is protected, so the
   \ protected-WID registry is not on that path and must not gate it.
   a u GENERATED-DECL-NAME-PREFLIGHT:NAMESPACED? 0= IF EXIT THEN
   a u GENERATED-DECL-NAME-PREFLIGHT:NEW-WORDLIST? IF
      data-base WIDN-CELL + @ dup 0 < swap WID-MAX > or
         IF E-PROTECTION-CAP throw THEN
   THEN
   \ `words` counts generated dictionary entries, not protected wordlists (a
   \ family of twenty words protects one), so this gate refuses only a declaration
   \ that generates anything when the registry is full; PREPARE checks the staged
   \ wordlist count against prot-wid-room.
   words 0 > prot-wid-room 0= and IF E-PROTECTION-CAP throw THEN
   STAGE-ENSURE ;

\ This file anchors the participants' snapshot-reset chain: it loads last, so it
\ owns TDECL-OWNER-SNAPSHOT-XT, which CHECKER-CAPTURE-PREPARE reaches; an earlier
\ participant cannot name a later one, so each tail is a call. Each link drops
\ its grown buffers back to its boot store: a persisted host address is a wild
\ pointer in the process that restores the image.
: SNAPSHOT-RESET ( -- )
   DECLARATION-TRANSACTION:ACTIVE? STAGE-N @ 0 <> or IF E-PROTECTION-CAP throw THEN
   STAGE-P STAGE-BOOT STAGE-CAP @ cells ARENA-BOOT-RESTORE
   CAP-INIT STAGE-CAP !
   GENERATED-DECL-DICTIONARY:SNAPSHOT-RESET
   DECL-EVENT:SNAPSHOT-RESET
   STRUCTURE-DECL:SNAPSHOT-RESET
   ENUM-DECL:SNAPSHOT-RESET
   DECL-REJECT:SNAPSHOT-RESET ;

: INSTALL ( -- )
   0 STAGE-N !
   CAP-INIT STAGE-CAP !
   [: STAGE-WORDLIST ;] is TYPE-DECL:TDECL-PROT-WID-XT
   [: PLAN-PREFLIGHT ;] is TYPE-DECL:TDECL-CAPACITY-PREFLIGHT-XT
   [: SNAPSHOT-RESET ;] is TYPE-DECL:TDECL-OWNER-SNAPSHOT-XT ;

public

: STAGED-COUNT ( -- n ) STAGE-N @ ;

private

INSTALL

get-current prot-wid-add

;package

package DECLARATION-TRANSACTION

\ DECL-EVENT's savepoint can refuse (DEV-OPEN -> TYPE-FIELD-OWNER:OPEN
\ TX-PARENT-REQUIRE throws E-PF-TX); only the checker frame's is taken before
\ it, and the four after it store cells and cannot fail.
: SAVEPOINTS ( -- )
   CHECKER-DECL-FRAME:PART-SNAPSHOT
   [: DECL-EVENT:PART-SNAPSHOT ;] catch {: rc:n :}
   rc 0 <> IF CHECKER-DECL-FRAME:PART-ROLLBACK rc throw THEN
   GENERATED-DECL-CTOR:PART-SNAPSHOT
   STRUCTURE-MAKE:PART-SNAPSHOT
   GENERATED-DECL-DICTIONARY:SNAPSHOT
   GENERATED-DECL-PROTECTION:SNAPSHOT ;

: PREPARES ( -- )
   CHECKER-DECL-FRAME:PART-PREPARE
   DECL-EVENT:PART-PREPARE
   GENERATED-DECL-CTOR:PART-PREPARE
   STRUCTURE-MAKE:PART-PREPARE
   GENERATED-DECL-PROTECTION:PREPARE ;

\ DECL-EVENT commits first: its commit advances PF-COMMIT-N over the
\ declaration's field rows, which the constructor and address-surface commits
\ read through the committed provider. Protection publishes last, after every
\ reversible owner committed.
: COMMITS ( -- )
   DECL-EVENT:PART-COMMIT
   GENERATED-DECL-CTOR:PART-COMMIT
   STRUCTURE-MAKE:PART-COMMIT
   GENERATED-DECL-PROTECTION:PUBLISH ;

: ROLLBACKS ( -- )
   GENERATED-DECL-PROTECTION:ROLLBACK
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
: RUN ( [ -- ] -- )
   {: body :}
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
undefine GENERATED-DECL-DICTIONARY:SNAPSHOT-RESET
undefine STRUCTURE-DECL:SNAPSHOT-RESET
undefine ENUM-DECL:SNAPSHOT-RESET
undefine DECL-REJECT:SNAPSHOT-RESET
undefine GENERATED-DECL-PROTECTION:SNAPSHOT
undefine GENERATED-DECL-PROTECTION:PREPARE
undefine GENERATED-DECL-PROTECTION:PUBLISH
undefine GENERATED-DECL-PROTECTION:ROLLBACK

\ The field owner's cleanup and release vectors are bound into the compiled
\ declaration-event participant, their only caller, so they are retired too, by
\ qualified name since they live in TFAM's public wordlist (a `using` would not
\ help: `undefine` parses, and parsing words resolve outside using-imports).
undefine TFAM:TDECL-FIELD-CLEANUP-XT
undefine TFAM:TDECL-FIELD-RELEASE-XT
