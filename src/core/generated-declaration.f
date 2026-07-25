\ generated-declaration.f - production generated-declaration transaction.
\
\ This file owns one sealed DECLARATION-TRANSACTION instance.  The checker is
\ the first participant; DECL-EVENT enrolls later in the checked declaration
\ layer; the native dictionary and protection owners enroll after xref; and the
\ protection owner seals the participant set last. RUN refuses to start before
\ that final seal.
\
\ The checker participant's release callback is total, like every other one: by
\ the time the coordinator releases, publication has already happened and no
\ participant is allowed to reject.

\ The checker participant lives in this file, so it owns its identity and order
\ outright. The three orders published below belong to participant modules that
\ enroll from their own files and have to read the value from somewhere.
package CHECKER-DECL-FRAME

1 constant PARTICIPANT
100 constant ORDER

: PART-SNAPSHOT ( n -- n ) {: depth:n :}
   depth START
   depth ;

: PART-PREPARE ( n -- n ) {: depth:n :}
   depth PREPARE 0=
      IF DECLARATION-TRANSACTION:E-PARTICIPANT-DEPTH throw THEN
   depth ;

: PART-COMMIT ( n -- n ) ;

: PART-ROLLBACK ( n -- n ) {: depth:n :}
   depth ROLLBACK
   depth ;

\ The frame word this forwards to is already throw-free by contract; see the
\ RELEASE comment in src/core/checker.f.
: PART-RELEASE ( -- )
   RELEASE ;

public

: INSTALL ( ptr a -- )
   PARTICIPANT ORDER
   [: PART-SNAPSHOT ;]
   [: PART-PREPARE ;]
   [: PART-COMMIT ;]
   [: PART-ROLLBACK ;]
   [: PART-RELEASE ;]
   DECLARATION-TRANSACTION:REGISTER ;

private
;package

package GENERATED-DECL-OWNER

\ Five sealed participants enroll: the checker frame (100), DECL-EVENT (800),
\ constructor generation (820), the native dictionary (850), and protection
\ (900). The boot arena is sized to hold all five, so no ordinary boot performs
\ a participant-table grow; the growth path stays live for a future sixth.
5 constant PARTICIPANT-CAP-INIT

create PARTICIPANT-BOOT
   PARTICIPANT-CAP-INIT DECLARATION-TRANSACTION:ROW-CELLS * cells allot
create STATE DECLARATION-TRANSACTION:STATE-CELLS cells allot

: INIT ( -- )
   STATE PARTICIPANT-BOOT PARTICIPANT-CAP-INIT
   [: DECLARATION-TRANSACTION:DEFAULT-ALLOCATOR ;]
   [: DECLARATION-TRANSACTION:DEFAULT-DIAGNOSTIC ;]
   DECLARATION-TRANSACTION:INIT
   STATE CHECKER-DECL-FRAME:INSTALL ;

public

: REGISTER
   ( n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ -- ] -- )
   {: id:n order:n snapshot prepare commit rollback release :} \ typed-local-lint: allow-bare-local
   STATE id order snapshot prepare commit rollback release
   DECLARATION-TRANSACTION:REGISTER ;

: REGISTER-LAST
   ( n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ -- ] -- )
   REGISTER
   STATE DECLARATION-TRANSACTION:SEAL ;

: RUN ( [ -- ] -- )
   STATE DECLARATION-TRANSACTION:SEALED? 0=
      IF DECLARATION-TRANSACTION:E-REGISTRATION-SEALED throw THEN
   STATE swap DECLARATION-TRANSACTION:RUN ;

: COUNT ( -- n ) STATE DECLARATION-TRANSACTION:COUNT ;
: DEPTH ( -- n ) STATE DECLARATION-TRANSACTION:DEPTH ;
: SEALED? ( -- bool ) STATE DECLARATION-TRANSACTION:SEALED? ;
: POISONED? ( -- bool ) STATE DECLARATION-TRANSACTION:POISONED? ;
: LAST-FAILURE-PHASE ( -- n ) STATE DECLARATION-TRANSACTION:LAST-FAILURE-PHASE ;
: LAST-FAILURE-PARTICIPANT ( -- n ) STATE DECLARATION-TRANSACTION:LAST-FAILURE-PARTICIPANT ;
: LAST-CLEANUP-PARTICIPANT ( -- n ) STATE DECLARATION-TRANSACTION:LAST-CLEANUP-PARTICIPANT ;

private

INIT
get-current prot-wid-add

;package

package GENERATED-DECL

public

DECLARATION-TRANSACTION:E-PARTICIPANT-DUP constant E-PARTICIPANT-DUP
DECLARATION-TRANSACTION:E-REGISTRATION-ACTIVE constant E-REGISTRATION-ACTIVE
DECLARATION-TRANSACTION:E-TRANSACTION-POISONED constant E-TRANSACTION-POISONED
DECLARATION-TRANSACTION:E-REGISTRATION-SEALED constant E-REGISTRATION-SEALED

800 constant ORDER-EVENT
820 constant ORDER-CONSTRUCTOR
850 constant ORDER-DICTIONARY
900 constant ORDER-PROTECTION

DECLARATION-TRANSACTION:PHASE-SNAPSHOT constant PHASE-SNAPSHOT
DECLARATION-TRANSACTION:PHASE-BODY constant PHASE-BODY
DECLARATION-TRANSACTION:PHASE-PREPARE constant PHASE-PREPARE
DECLARATION-TRANSACTION:PHASE-COMMIT constant PHASE-COMMIT
DECLARATION-TRANSACTION:PHASE-ROLLBACK constant PHASE-ROLLBACK

: RUN ( [ -- ] -- ) GENERATED-DECL-OWNER:RUN ;
: COUNT ( -- n ) GENERATED-DECL-OWNER:COUNT ;
: DEPTH ( -- n ) GENERATED-DECL-OWNER:DEPTH ;
: SEALED? ( -- bool ) GENERATED-DECL-OWNER:SEALED? ;
: POISONED? ( -- bool ) GENERATED-DECL-OWNER:POISONED? ;
: LAST-FAILURE-PHASE ( -- n ) GENERATED-DECL-OWNER:LAST-FAILURE-PHASE ;
: LAST-FAILURE-PARTICIPANT ( -- n ) GENERATED-DECL-OWNER:LAST-FAILURE-PARTICIPANT ;
: LAST-CLEANUP-PARTICIPANT ( -- n ) GENERATED-DECL-OWNER:LAST-CLEANUP-PARTICIPANT ;

private

TRUSTED: INSTALL-DECLARATION-RUNNER ( -- )
   [: GENERATED-DECL-OWNER:RUN ;] is TDECL-TXN-XT
   -1 TDECL-TXN-ARMED ! ;

INSTALL-DECLARATION-RUNNER
get-current prot-wid-add

;package

\ ---------------------------------------------------------------------------
\ The constructor-generation participant (ORDER 820).
\
\ It renders, evaluates, certifies and publishes one declared family's checked
\ FAMILY:VARIANT constructors from inside the declaration transaction. The order
\ is the whole design:
\
\   100 checker | 800 DECL-EVENT | 820 here | 850 dictionary | 900 protection
\
\ Commit runs in ascending order (declaration-transaction.f INSERT-AT keeps the
\ table sorted and RUN-FORWARD walks it upward), so by the time this participant
\ commits, DECL-EVENT's commit has already run TYPE-FIELD-OWNER:COMMIT, which
\ advances PF-COMMIT-N over this declaration's field rows. That is exactly the
\ watermark SUMV-NAMED-PAYLOAD? / SUMV-PAY-N read, so the shared generator can be
\ driven from the ordinary COMMITTED provider (TDECL-SUMV-PROVIDER) and needs no
\ provisional reader and no live-token provider of its own.
\
\ Generation is fallible by design. PHASE-COMMIT is the reversible commit phase:
\ a throw here rolls every participant back in reverse order, and because the
\ dictionary owner took its savepoint during PHASE-SNAPSHOT — before the body ran
\ — every word this participant already evaluated is truncated with the rest of
\ the declaration. A failure mid-set therefore publishes no constructor at all.
\
\ This participant reads no ambient last-registered-family register: the family
\ it acts on is armed by the declaration front end into a slot owned here and
\ indexed by this transaction's own nesting depth.
\ ---------------------------------------------------------------------------
package GENERATED-DECL-CTOR

7176 constant E-CTOR-ARM   \ armed family is not a public ENUM family with variants

5 constant PARTICIPANT
-1 constant NO-FAMILY

\ One cell per nesting level. Room is ensured in SNAPSHOT, which is allowed to
\ allocate; every later phase only reads and writes an existing cell, which is
\ what lets RELEASE stay total. Same shape as DECL-EVENT's DEV-PART-* slots.
4 constant CAP-INIT
create ARM-BOOT CAP-INIT cells allot
PTR-VARIABLE ARM-P   ARM-BOOT ARM-P !
variable ARM-CAP     CAP-INIT ARM-CAP !

\ Trusted forwarders to the pre-hook registry and generator words. sumtype.f and
\ type-family.f load before the checker hook, so a post-hook checked body reaches
\ them exactly the way enum-decl.f and structure-decl.f reach their registry
\ seams.
TRUSTED: ARM-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: FAM-PUBLIC? ( n -- bool ) TFAM-PUBLIC? ;
TRUSTED: FAM-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: FAM-ENUM? ( n -- bool ) TFAM-ENUM? ;
TRUSTED: FAM-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: CTOR-COLLIDE ( n n n -- ) TDECL-DERIVE-COLLIDE ;
TRUSTED: CTOR-REQUIRE ( n n n -- ) TDECL-DERIVE-REQUIRE ;
TRUSTED: CTOR-PUBLISH ( n n n -- ) TDECL-CTOR-PUBLISH ;
TRUSTED: CTOR-PROVIDER ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   TDECL-SUMV-PROVIDER ;
TRUSTED: CTOR-GEN ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n )
   TDECL-CTOR-WORDS-BODY ;
TRUSTED: PEND-CLEAR ( -- ) CTOR-PEND-CLEAR ;
TRUSTED: VAR-CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;

: ARM-BASE ( -- ptr a ) ARM-P @ ;
: ARM-SLOT ( -- ptr a )                    \ this nesting level's armed-family cell
   GENERATED-DECL:DEPTH 1 - cells ARM-BASE + ;
: ARM-GROW1 ( -- )
   ARM-CAP @ 2 * {: nc:n :}
   ARM-P @ ARM-CAP @ cells nc cells ARM-GROW ARM-P !
   nc ARM-CAP ! ;
: ARM-ENSURE ( -- )
   GENERATED-DECL:DEPTH ARM-CAP @ <= IF EXIT THEN
   ARM-GROW1 ;

: ARMED-FAM ( -- n ) ARM-SLOT @ ;
: DISARM ( -- ) NO-FAMILY ARM-SLOT ! ;

\ THE GATE, and the one place that spells it.
\
\ A family owns generated constructors when it is PUBLIC (a private family
\ exports nothing and TDECL-CTOR-PUBLISH would leave the constructor package
\ empty), has at least one variant, and is one of the two ENUM-front-end kinds.
\
\ Both kinds, deliberately. The global-token cutover's acceptance is that every
\ existing plain ENUM behaves identically through the new front end, and the
\ legacy sumtype.f definer already publishes constructors for a compact
\ payloadless enum (a declared `LGC:RED` resolves; the same family through
\ ENUM-DECL:ED-RUN was E-UNDEFINED before this participant). Gating on TK-SUM
\ alone would leave the compact mode a permanent parity gap the cutover could
\ never close. TK-PRODUCT is excluded because the STRUCTURE front end owns its
\ own make/unmake generation in structure-make.f; admitting it here would
\ generate a second, conflicting set.
: GEN-OK? ( n -- bool ) {: fam:n :}
   fam FAM-PUBLIC? 0= IF 0 0= 0= EXIT THEN
   fam FAM-VAR-COUNT 0 <= IF 0 0= 0= EXIT THEN
   fam FAM-SUM? fam FAM-ENUM? or ;

\ Has this family's set already been generated? TDPLAN-CTOR+ records the
\ constructor symbol on each variant row as it renders, so a non-zero symbol on
\ the first row means the words are live. This is an EXISTENCE check on the
\ registry, deliberately not a name lookup, and it is kept out of GEN-OK? so that
\ predicate keeps its single meaning — which kinds own constructors — for
\ ED-CLOSE's gate and the public OWNS?.
\
\ It matters because sumtype.f's TDPLAN-NAME+ answers a second plan row for a
\ live constructor with `76 die`, which kills the process outright: no throw, no
\ unwind, no rollback. A caller that arms an already-generated family must be
\ refused here, by a named throw the transaction can roll back, rather than
\ reaching that die. The production path never trips it — ED-CLOSE arms a family
\ whose variant rows were created moments earlier — so this is a boundary guard,
\ and test/enum-decl-suite.f §20g drives it directly.
: GENERATED? ( n -- bool ) {: fam:n :}
   fam FAM-VAR-START VAR-CTOR-SYM 0 <> ;

\ Order matters: GEN-OK? proves the variant range is non-empty, so the first
\ variant row GENERATED? reads exists.
: GEN-REQUIRE ( n -- ) {: fam:n :}
   fam GEN-OK? 0= IF E-CTOR-ARM throw THEN
   fam GENERATED? IF E-CTOR-ARM throw THEN ;

\ The pending-constructor authority queue is plan scratch shared with the
\ generator. Its only legal value at the start of a declaration is empty, so
\ SNAPSHOT establishes that value rather than copying one, and ROLLBACK restores
\ it after a failure that left entries queued.
: PART-SNAPSHOT ( n -- n ) {: depth:n :}
   ARM-ENSURE
   DISARM
   PEND-CLEAR
   depth ;

\ Non-mutating re-proof only. The front end proved this gate when it armed, in
\ the body phase; nothing between then and here may have made the family
\ ineligible, and if something did, the declaration fails before any word is
\ rendered rather than publishing a set nobody validated.
: PART-PREPARE ( n -- n ) {: depth:n :}
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY <> IF fam GEN-REQUIRE THEN
   depth ;

\ The work. Mirrors the legacy sum definer's close sequence (sumtype.f
\ CHECKER-DEFSUM-BODY): reject a variant spelled like a derived word, prove every
\ payload role is derivably comparable, stamp the constructor package on each
\ variant row, then render/evaluate/certify/seal the whole set through the shared
\ generator. Both role gates read committed rows, which is why they work here and
\ could not work in the body phase.
: PART-COMMIT ( n -- n ) {: depth:n :}
   ARMED-FAM {: fam:n :}
   fam NO-FAMILY = IF depth EXIT THEN
   fam GEN-REQUIRE
   fam FAM-VAR-START {: vstart:n :}
   fam FAM-VAR-COUNT {: count:n :}
   fam vstart count CTOR-COLLIDE
   fam vstart count CTOR-REQUIRE
   fam vstart count CTOR-PUBLISH
   CTOR-PROVIDER fam CTOR-GEN drop
   depth ;

: PART-ROLLBACK ( n -- n ) {: depth:n :}
   PEND-CLEAR
   DISARM
   depth ;

\ Total, like every release callback. Publication has already happened; the only
\ thing left is this owner's one armed-family cell, whose slot arithmetic cannot
\ fail because SNAPSHOT ensured the row exists at this depth and the depth is
\ unchanged until the coordinator leaves the transaction.
: PART-RELEASE ( -- )
   DISARM ;

: INSTALL ( -- )
   PARTICIPANT GENERATED-DECL:ORDER-CONSTRUCTOR
   [: PART-SNAPSHOT ;]
   [: PART-PREPARE ;]
   [: PART-COMMIT ;]
   [: PART-ROLLBACK ;]
   [: PART-RELEASE ;]
   GENERATED-DECL-OWNER:REGISTER ;

public

\ The declaration front end names the family whose constructors this transaction
\ owns. Legal only inside an open declaration transaction, and only for a family
\ that passes the gate, so a caller cannot arm a kind this participant refuses to
\ generate for and discover it three phases later.
: ARM ( n -- ) {: fam:n :}
   GENERATED-DECL:DEPTH 0 <= IF E-CTOR-ARM throw THEN
   fam GEN-REQUIRE
   fam ARM-SLOT ! ;

: OWNS? ( n -- bool ) GEN-OK? ;

private

INSTALL
get-current prot-wid-add

;package
