\ generated-declaration-protection.f - atomic constructor protection owner.
\
\ Generated declarations stage constructor wordlists here.  PREPARE proves the
\ irreversible engine registry has capacity, and this last ordered participant
\ publishes only after every reversible owner committed.  Nested declarations
\ retain their staged rows for the outer transaction.  Once this last owner is
\ installed, the source-level registration and provisional-schema seams are
\ retired; their already-compiled callers retain the only live capabilities.

package GENERATED-DECL-PROTECTION

7169 constant E-PROTECTION-CAP
$FFFFFFFF constant WID-MAX
4 constant CAP-INIT
$0FFFFFFFFFFFFFFF constant CELL-CAP-MAX

private

create STAGE-BOOT CAP-INIT cells allot
create BASE-BOOT CAP-INIT cells allot
PTR-VARIABLE STAGE-P   STAGE-BOOT STAGE-P !
PTR-VARIABLE BASE-P    BASE-BOOT BASE-P !
variable STAGE-N
variable STAGE-CAP
variable BASE-CAP

TRUSTED: ARENA-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;

: CELL@ ( ptr a n -- a ) {: base:ptr idx:n :}
   base idx cells + @ ;

: CELL! ( a ptr a n -- ) {: value:a base:ptr idx:n :}
   value base idx cells + ! ;

: GROW-CAP ( n n -- n ) {: need:n cap:n :}
   need 0 <= need CELL-CAP-MAX > or
   cap 0 <= cap CELL-CAP-MAX > or or IF E-PROTECTION-CAP throw THEN
   cap CELL-CAP-MAX 2 / <= IF cap 2 * need max EXIT THEN
   need ;

: GROW ( ptr ptr a n n -- ) {: pvar:ptr oldcap:n newcap:n :}
   pvar @ oldcap cells newcap cells ARENA-GROW pvar ! ;

: STAGE-ENSURE ( -- )
   STAGE-N @ STAGE-CAP @ < IF EXIT THEN
   STAGE-N @ CELL-CAP-MAX >= IF E-PROTECTION-CAP throw THEN
   STAGE-N @ 1 + STAGE-CAP @ GROW-CAP {: cap:n :}
   STAGE-P STAGE-CAP @ cap GROW
   cap STAGE-CAP ! ;

: BASE-ENSURE ( -- )
   GENERATED-DECL:DEPTH BASE-CAP @ <= IF EXIT THEN
   GENERATED-DECL:DEPTH BASE-CAP @ GROW-CAP {: cap:n :}
   BASE-P BASE-CAP @ cap GROW
   cap BASE-CAP ! ;

: BASE-SLOT ( -- n )
   GENERATED-DECL:DEPTH 1 - ;

: BASE@ ( -- n )
   BASE-P @ BASE-SLOT CELL@ ;

: BASE! ( n -- )
   BASE-P @ BASE-SLOT CELL! ;

: STAGE@ ( n -- n )
   STAGE-P @ swap CELL@ ;

: STAGE+ ( n -- ) {: wid:n :}
   STAGE-ENSURE
   wid STAGE-P @ STAGE-N @ CELL!
   STAGE-N @ 1 + STAGE-N ! ;

: SCRUB-TO-BASE ( -- )
   BASE@ {: keep:n :}
   keep
   BEGIN dup STAGE-N @ < WHILE
      0 STAGE-P @ over CELL!
      1 +
   REPEAT
   drop
   keep STAGE-N ! ;

: SNAPSHOT ( n -- n )
   BASE-ENSURE
   STAGE-N @ BASE! ;

: PREPARE ( n -- n )
   STAGE-N @ BASE@ - prot-wid-room > IF E-PROTECTION-CAP throw THEN ;

: PUBLISH ( n -- n )
   GENERATED-DECL:DEPTH 1 <> IF EXIT THEN
   BASE@
   BEGIN dup STAGE-N @ < WHILE
      dup STAGE@ prot-wid-add
      1 +
   REPEAT
   drop
   SCRUB-TO-BASE ;

: ROLLBACK ( n -- n )
   SCRUB-TO-BASE ;

: FINALIZE ( n -- n ) ;

: STAGE-WORDLIST ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TFAM-CTOR-WORD? 0= IF s" xref: protected-WID constructor mismatch" 76 die THEN
   a u XREF-FIND dup XREF-FOUND? 0= IF
      drop s" xref: protected-WID constructor not found" 76 die
   THEN
   XREF-WORDLIST STAGE+ ;

: PLAN-PREFLIGHT ( ptr u8 n n -- ) {: a:ptr u:n words:n :}
   a u words GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS
      GENERATED-DECL-DICTIONARY:PREFLIGHT
   a u GENERATED-DECL-NAME-PREFLIGHT:NEW-WORDLIST? IF
      data-base WIDN-CELL + @ dup 0 < swap WID-MAX > or
         IF E-PROTECTION-CAP throw THEN
   THEN
   words 0 > prot-wid-room 0= and IF E-PROTECTION-CAP throw THEN
   STAGE-ENSURE ;

: SNAPSHOT-RESET ( -- )
   GENERATED-DECL:DEPTH 0 <> STAGE-N @ 0 <> or IF E-PROTECTION-CAP throw THEN
   STAGE-BOOT STAGE-P !
   BASE-BOOT BASE-P !
   CAP-INIT STAGE-CAP !
   CAP-INIT BASE-CAP !
   GENERATED-DECL-DICTIONARY:SNAPSHOT-RESET ;

3 constant PARTICIPANT

: INSTALL ( -- )
   0 STAGE-N !
   CAP-INIT STAGE-CAP !
   CAP-INIT BASE-CAP !
   PARTICIPANT GENERATED-DECL:ORDER-PROTECTION
   [: SNAPSHOT ;]
   [: PREPARE ;]
   [: PUBLISH ;]
   [: ROLLBACK ;]
   [: FINALIZE ;]
   GENERATED-DECL-OWNER:REGISTER-LAST
   [: STAGE-WORDLIST ;] is TDECL-PROT-WID-XT
   [: PLAN-PREFLIGHT ;] is TDECL-CAPACITY-PREFLIGHT-XT
   [: SNAPSHOT-RESET ;] is TDECL-OWNER-SNAPSHOT-XT ;

public

: STAGED-COUNT ( -- n ) STAGE-N @ ;

private

INSTALL

undefine GENERATED-DECL-OWNER:REGISTER
undefine GENERATED-DECL-OWNER:REGISTER-LAST
undefine GENERATED-DECL-OWNER:RUN
undefine GENERATED-DECL-OWNER:COUNT
undefine GENERATED-DECL-OWNER:DEPTH
undefine GENERATED-DECL-OWNER:SEALED?
undefine GENERATED-DECL-OWNER:POISONED?
undefine GENERATED-DECL-OWNER:LAST-FAILURE-PHASE
undefine GENERATED-DECL-OWNER:LAST-FAILURE-PARTICIPANT
undefine GENERATED-DECL-OWNER:LAST-CLEANUP-PARTICIPANT
undefine GENERATED-DECL-DICTIONARY:PREFLIGHT
undefine GENERATED-DECL-DICTIONARY:SNAPSHOT-RESET
undefine TYPE-FIELD-OWNER:TX-SCHEMA-FOR
undefine TYPE-FAMILY-OWNER:PROVISIONAL?
undefine TYPE-FAMILY-OWNER:RESET-ALLOWED?

get-current prot-wid-add

;package
