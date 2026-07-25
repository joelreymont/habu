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

4 constant PARTICIPANT-CAP-INIT

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
