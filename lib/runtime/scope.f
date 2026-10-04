\ scope.f - runtime scopes and the host port for the browser runtime
\ (docs/browser-runtime.md §5.1, §24.5, §4.5).
\
\ STORAGE CLASS. PACKAGE-OWNED: one runtime per image, as the port it holds is
\ one. Each scope and request record is an object of the runtime's pools
\ (RT-POOL), which START is handed with the EPOCH-CELLS zeroed cells for the
\ epoch's two identity counters (RT-ID), cells the epoch owns exclusively and
\ leaves closed when it ends. OPEN reserves the scope's record and its
\ lifecycle record, which carries its SCOPE-OPEN and later its SCOPE-CLOSE, so
\ CLOSE reserves nothing; REQUEST reserves an operation's record, and a release
\ reuses the record of the result it releases. A reservation the pools refuse
\ throws E-RT-SCOPE-OOM, and the call keeps no record: one OPEN took before
\ the refusal goes back to the pools at their next reclamation. The live
\ epoch's records are linked in two lists, of scopes and of requests, each
\ record holding the next one's handle in a cell; a link owns nothing, and
\ RT-SCOPE holds each record once, from its reservation until it releases it.
\ Root time and the two port vectors are this package's.
\
\ A scope or request token is an opaque cell holding a serial the image's
\ never-reused counter issued, and names the record on its list holding that
\ serial. A token of a retired scope or request, or of an earlier epoch,
\ whichever pools held it, names no record and is refused (E-RT-SCOPE-STALE).
\ The zero token, NO-SCOPE, is refused before any record is read wherever a
\ live one is needed: an idle lifecycle record holds serial 0.
\
\ BOUNDARY. Outside the package no typed route leads from a number to a token:
\ the converters are private and a cast from a number to either is
\ E-CAST-OWNER. LOCAL-ID and REQUEST-ID answer a token's serial, the wire's
\ localId and correlation, and nothing turns one back. Raw storage passes this
\ as it passes RT-ID's: the public byte and cell views over a cell holding a
\ token store any number there, and a forged serial names its record.
\
\ Lifecycle (§5.1). An Application scope has no parent; every other kind's
\ parent is Open, since its SCOPE-OPEN names the parent's host handle, and a
\ Component's is an Application or Document scope. A parent exists before its
\ child and serials only grow, so parents cannot cycle. OPEN writes the
\ Opening record and queues SCOPE-OPEN; nothing reaches the host until TURN,
\ which the embedding runs when WAKE asks, so no port call reenters the host.
\ Requests of an Opening scope wait; acceptance opens it and the next turn
\ sends them, refusal closes it without sending them: the host holds nothing
\ of it, so it goes from Opening straight to Closed. CLOSE of an Opening
\ scope ends it so at once while its SCOPE-OPEN is unsent; once that is out,
\ the scope stays Opening, taking no request, until the answer closes it
\ unopened or opens it only to close it. CLOSE of an Open scope closes its
\ children first, drops its unsent operations, leaves its owed results
\ Draining, queues a release for each handle it holds, then queues
\ SCOPE-CLOSE: Closing until the host accepts that, Draining until it answers,
\ then Closed. A result owed to a closing scope is drained when it comes, its
\ handle released. A Closed scope's record stays, a tombstone, while any
\ request names it, and only then frees.
\
\ A request the host refuses stays Habu's (§24.5), as does one it accepts and
\ then fails. A refused or failed close leaves its scope Closing with its host
\ handle and every handle it holds, its close idle, until CLOSE asks again. A
\ release owns its handle until the host completes it or the incarnation
\ dies: nothing else drops one, and a refused or failed release goes out again
\ at the next turn, which it asks WAKE for, whatever its scope's state. TURN
\ reports a refused close or release once its turn ends and DELIVER a failed
\ one at once, by E-RT-SCOPE-REFUSED. A stale-epoch answer is the
\ incarnation's death, one transition that §5.1's Revoked and Draining
\ collapse into, since the host has torn down every handle of the incarnation
\ and no result is left to drain: every scope is Closed and every record
\ released at once, so every token of the incarnation is stale, and the
\ counters close.

require lib/errors.f
require lib/type/deftype.f
require lib/runtime/id.f
require lib/runtime/handle.f
require lib/runtime/pool.f

package RT-SCOPE

public

\ The decade -9590..-9599.
-9590 constant E-RT-SCOPE-FIRST
-9599 constant E-RT-SCOPE-LAST
-9590 constant E-RT-SCOPE-NULL     \ the zero scope or request token where a live one is needed
-9591 constant E-RT-SCOPE-STALE    \ a token no record holds: retired, or issued under an earlier epoch
-9592 constant E-RT-SCOPE-PARENT   \ OPEN under a parent that is not Open or not admissible for the kind
-9593 constant E-RT-SCOPE-STATE    \ REQUEST of a scope not Opening or Open, or closing; CLOSE of one closing with its close out
-9594 constant E-RT-SCOPE-OOM      \ the pools refused a scope or request record: RecoverableOOM, and the call keeps none
-9595 constant E-RT-SCOPE-EPOCH    \ no live epoch: before START or after the incarnation's death; or START while one lives
-9596 constant E-RT-SCOPE-TIME     \ TIME! of a nonfinite root time or one earlier than the last
-9597 constant E-RT-SCOPE-OWED     \ DELIVER to a request that owes no result
-9598 constant E-RT-SCOPE-REFUSED  \ the host refused or failed a close or a release, which Habu keeps

DEFTYPE SCOPE
undefine >SCOPE
undefine SCOPE>N

DEFTYPE RT-REQUEST
undefine >RT-REQUEST
undefine RT-REQUEST>N

\ §24.5's submit results, in its order.
ENUM submit-result DERIVE eq accepted backpressure invalid denied unavailable oom stale-epoch host-failed ;ENUM

ENUM scope-kind DERIVE eq application document component gesture job plugin ;ENUM

ENUM scope-state DERIVE eq opening open closing draining closed ;ENUM

\ What a request asks of the host; BROWSER maps each kind to its opcode
\ (§27.4), and an operation's own record names its operation.
ENUM request-kind DERIVE eq scope-open scope-close release operation ;ENUM

\ The host's answer to a request it accepted: done, holding the handle the
\ result made (the scope's for an open, null when none), or failed.
ENUM outcome-record 0
   VARIANT done FIELD held RT-HANDLE:handle ;VARIANT
   VARIANT failed FIELD class RT-ID:error-class FIELD code n ;VARIANT
;ENUM

\ The port (§24.5), each bound once by the embedding with `is`. SUBMIT hands
\ the host a request; WAKE asks for a later TURN and never runs one itself.
defer SUBMIT ( rt-request -- submit-result )
defer WAKE ( -- )

RT-ID:COUNTER-CELLS 2 * constant EPOCH-CELLS

private

CAST: >SCOPE ( n -- scope )
CAST: SCOPE>N ( scope -- n )
CAST: >RT-REQUEST ( n -- rt-request )
CAST: RT-REQUEST>N ( rt-request -- n )
CAST: >SKIND ( n -- scope-kind )
CAST: SKIND>N ( scope-kind -- n )
CAST: >SSTATE ( n -- scope-state )
CAST: SSTATE>N ( scope-state -- n )
CAST: >QKIND ( n -- request-kind )
CAST: QKIND>N ( request-kind -- n )

\ The cells of a record. Both kinds hold the next record's handle and their
\ serial first, 0 in an idle lifecycle record. A scope's: its parent's serial,
\ 0 for none; its kind and state; the host's scope handle once Open; whether
\ CLOSE came while its open was out; its lifecycle record. A request's: its
\ scope's serial; its kind; its phase, QUEUED, OUT (its result owed), HELD
\ (its result holds a handle the scope owns, or the host refused or failed
\ the release, which waits for the next turn) or DRAINING (its scope closed
\ while the result was owed); the handle a release frees or a held result
\ holds. A record's payload starts zeroed: no next record, no handle, false.
0 constant NEXT#
1 constant SERIAL#
2 constant PARENT#
3 constant SKIND#
4 constant SSTATE#
5 constant HOST#
6 constant SHUT#
7 constant LIFE#
2 constant SCOPE#
3 constant QKIND#
4 constant PHASE#
5 constant TARGET#

1 constant QUEUED
2 constant OUT
3 constant HELD
4 constant DRAINING

\ The epoch's mode, 0 before the first START.
1 constant LIVE
2 constant DEAD
variable MODE

\ A refused or failed close or release this turn kept by Habu.
variable REFUSAL

TYPED-VARIABLE POOLS RT-POOL:pools
TYPED-VARIABLE SCOPES RT-HANDLE:handle
TYPED-VARIABLE REQUESTS RT-HANDLE:handle
TYPED-VARIABLE NOW r
TYPED-VARIABLE INSTANCES RT-ID:counter
TYPED-VARIABLE PLACEMENTS RT-ID:counter
RT-ID:COUNTER-CELLS TYPED-BUFFER SERIAL-CELLS n
TYPED-VARIABLE SERIALS RT-ID:counter
0 SERIAL-CELLS RT-ID:COUNTER SERIALS !

\ Scope and request records are objects of the job-state pool (§3.4) with no
\ edge to visit: their links own nothing.
: NO-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   2drop drop RT-HANDLE:NULL ;

TYPED-VARIABLE RECORD RT-POOL:schema

: REGISTER ( -- )
   RT--POOL-KIND:jobs [: NO-EDGE ;] RT-POOL:SCHEMA RECORD ! ;

REGISTER

: S-OPENING ( -- scope-state ) RT--SCOPE-SCOPE--STATE:opening ;
: S-OPEN ( -- scope-state ) RT--SCOPE-SCOPE--STATE:open ;
: S-CLOSING ( -- scope-state ) RT--SCOPE-SCOPE--STATE:closing ;
: S-DRAINING ( -- scope-state ) RT--SCOPE-SCOPE--STATE:draining ;
: S-CLOSED ( -- scope-state ) RT--SCOPE-SCOPE--STATE:closed ;
: K-OPEN ( -- request-kind ) RT--SCOPE-REQUEST--KIND:scope-open ;
: K-CLOSE ( -- request-kind ) RT--SCOPE-REQUEST--KIND:scope-close ;
: K-RELEASE ( -- request-kind ) RT--SCOPE-REQUEST--KIND:release ;
: K-OPERATION ( -- request-kind ) RT--SCOPE-REQUEST--KIND:operation ;

\ ---- record cells -----------------------------------------------------------

: F@ ( RT-HANDLE:handle n -- n )
   POOLS @ RT-POOL:CELL@ ;

: F! ( n RT-HANDLE:handle n -- )
   POOLS @ RT-POOL:CELL! ;

$FFFFFFFF constant U32-MAX

\ A record cell holds a handle as its slot in the low 32 bits and its
\ generation in the high 32, read and rebuilt through RT-HANDLE's public words.
: HANDLE>BITS ( RT-HANDLE:handle -- n )
   {: h:RT-HANDLE:handle :}
   h RT-HANDLE:GENERATION 32 lshift h RT-HANDLE:SLOT or ;

: BITS>HANDLE ( n -- RT-HANDLE:handle )
   {: v:n :}
   v U32-MAX and v 32 rshift RT-HANDLE:HANDLE ;

: H@ ( RT-HANDLE:handle n -- RT-HANDLE:handle )
   F@ BITS>HANDLE ;

: H! ( RT-HANDLE:handle RT-HANDLE:handle n -- )
   {: v:RT-HANDLE:handle r:RT-HANDLE:handle j:n :}
   v HANDLE>BITS r j F! ;

: SAME? ( RT-HANDLE:handle RT-HANDLE:handle -- bool )
   HANDLE>BITS swap HANDLE>BITS = ;

: NEXT@ ( RT-HANDLE:handle -- RT-HANDLE:handle ) NEXT# H@ ;
: SERIAL@ ( RT-HANDLE:handle -- n ) SERIAL# F@ ;
: PHASE@ ( RT-HANDLE:handle -- n ) PHASE# F@ ;
: LIFE@ ( RT-HANDLE:handle -- RT-HANDLE:handle ) LIFE# H@ ;
: HOST@ ( RT-HANDLE:handle -- RT-HANDLE:handle ) HOST# H@ ;
: SHUT@ ( RT-HANDLE:handle -- bool ) SHUT# F@ 0<> ;

: STATE! ( scope-state RT-HANDLE:handle -- )
   swap SSTATE>N swap SSTATE# F! ;

: IN? ( RT-HANDLE:handle scope-state -- bool )
   {: s:RT-HANDLE:handle want:scope-state :}
   s SSTATE# F@ >SSTATE want RT--SCOPE-SCOPE--STATE:EQ ;

: KIND? ( RT-HANDLE:handle request-kind -- bool )
   {: q:RT-HANDLE:handle want:request-kind :}
   q QKIND# F@ >QKIND want RT--SCOPE-REQUEST--KIND:EQ ;

\ ---- lists ------------------------------------------------------------------

: ?LIVE ( -- )
   MODE @ LIVE <> if E-RT-SCOPE-EPOCH throw then ;

: SERIAL ( -- n )
   SERIALS @ RT-ID:NEXT ;

\ A record of the epoch's pools on the list whose head is in the cell, or the
\ null handle when the pools refuse one.
: TAKE ( ptr RT-HANDLE:handle -- RT-HANDLE:handle )
   {: head:ptr :}
   RECORD @ POOLS @ RT-POOL:RESERVE
   MATCH RT-POOL:reservation
      granted OF ENDOF
      oom OF RT-HANDLE:NULL ENDOF
   ;MATCH {: r:RT-HANDLE:handle :}
   r RT-HANDLE:NULL? if r exit then
   head @ r NEXT# H!
   r head !
   r ;

\ The record before the first on the list from the second.
: BEFORE ( RT-HANDLE:handle RT-HANDLE:handle -- RT-HANDLE:handle )
   swap {: r:RT-HANDLE:handle :}
   begin dup NEXT@ r SAME? 0= while NEXT@ repeat ;

\ Take the record off the list whose head is in the cell and release it.
: DISPOSE ( RT-HANDLE:handle ptr RT-HANDLE:handle -- )
   {: r:RT-HANDLE:handle head:ptr :}
   head @ r SAME? if
      r NEXT@ head !
   else
      r NEXT@ r head @ BEFORE NEXT# H!
   then
   r POOLS @ RT-POOL:RELEASE ;

\ The record on the list from the handle holding the serial. Serial 0 is no
\ token and is refused before a record is read, since an idle lifecycle
\ record holds it.
: FIND ( n RT-HANDLE:handle -- RT-HANDLE:handle )
   swap {: id:n :}
   id 0= if E-RT-SCOPE-NULL throw then
   begin dup RT-HANDLE:NULL? 0= while
      dup SERIAL@ id = if exit then
      NEXT@
   repeat
   E-RT-SCOPE-STALE throw ;

: SREC-OF ( n -- RT-HANDLE:handle )
   SCOPES @ FIND ;

: QREC-OF ( n -- RT-HANDLE:handle )
   REQUESTS @ FIND ;

\ ---- scopes and their requests ----------------------------------------------

\ Fill a request record: queued, of the kind, in the scope with the serial.
: ASK ( n request-kind RT-HANDLE:handle -- )
   {: sid:n k:request-kind q:RT-HANDLE:handle :}
   SERIAL q SERIAL# F!
   sid q SCOPE# F!
   k QKIND>N q QKIND# F!
   QUEUED q PHASE# F!
   RT-HANDLE:NULL q TARGET# H! ;

: LIFECYCLE? ( RT-HANDLE:handle -- bool )
   dup K-OPEN KIND? swap K-CLOSE KIND? or ;

\ A request ends: its scope's lifecycle record goes idle, any other's frees.
: END-Q ( RT-HANDLE:handle -- )
   {: q:RT-HANDLE:handle :}
   q LIFECYCLE? if 0 q SERIAL# F! exit then
   q REQUESTS DISPOSE ;

: MINE? ( RT-HANDLE:handle n -- bool )
   {: q:RT-HANDLE:handle sid:n :}
   q SERIAL@ 0<> q SCOPE# F@ sid = and ;

\ Does any request name the scope with the serial?
: NAMED? ( n -- bool )
   {: sid:n :}
   REQUESTS @
   begin dup RT-HANDLE:NULL? 0= while
      dup sid MINE? if drop true exit then
      NEXT@
   repeat
   drop false ;

\ A Closed scope's record is a tombstone while a request names it, then frees
\ with its lifecycle record.
: RETIRE ( RT-HANDLE:handle -- )
   {: s:RT-HANDLE:handle :}
   s S-CLOSED IN? 0= if exit then
   s SERIAL@ NAMED? if exit then
   s LIFE@ REQUESTS DISPOSE
   s SCOPES DISPOSE ;

\ Is the request one of the scope's that never went out, and no release? A
\ release's handle stays live on the host until the host completes it.
: UNSENT? ( RT-HANDLE:handle n -- bool )
   {: q:RT-HANDLE:handle sid:n :}
   q sid MINE? q PHASE@ QUEUED = and q K-RELEASE KIND? 0= and ;

\ End the scope's requests that never went out, keeping its queued releases.
: DROP-QUEUED ( n -- )
   {: sid:n :}
   REQUESTS @
   begin dup RT-HANDLE:NULL? 0= while
      dup NEXT@ swap
      dup sid UNSENT? if END-Q else drop then
   repeat
   drop ;

\ The handle a request holds becomes a queued release of it, a new request.
: TO-RELEASE ( RT-HANDLE:handle -- )
   {: q:RT-HANDLE:handle :}
   SERIAL q SERIAL# F!
   K-RELEASE QKIND>N q QKIND# F!
   QUEUED q PHASE# F! ;

\ An open the host refused, or a close before the open went out: the scope
\ ends with no host side, its queued requests unsent.
: ABANDON ( RT-HANDLE:handle -- )
   {: s:RT-HANDLE:handle :}
   s SERIAL@ DROP-QUEUED
   S-CLOSED s STATE!
   s RETIRE ;

: ENDED ( RT-HANDLE:handle -- )
   {: s:RT-HANDLE:handle :}
   S-CLOSED s STATE!
   s RETIRE ;

\ An Open scope's owed results drain and its held handles are released.
: WIND-DOWN ( n -- )
   {: sid:n :}
   REQUESTS @
   begin dup RT-HANDLE:NULL? 0= while
      dup sid MINE? if
         dup PHASE@ OUT = if DRAINING over PHASE# F! then
         dup PHASE@ HELD = if dup TO-RELEASE then
      then
      NEXT@
   repeat
   drop ;

: LIVE-SCOPE? ( RT-HANDLE:handle -- bool )
   {: s:RT-HANDLE:handle :}
   s S-OPENING IN? s S-OPEN IN? or ;

\ May CLOSE close the scope: Opening or Open and not closing yet, or Closing
\ with its close refused or failed?
: CLOSABLE? ( RT-HANDLE:handle -- bool )
   {: s:RT-HANDLE:handle :}
   s S-CLOSING IN? if s LIFE@ SERIAL@ 0= exit then
   s LIVE-SCOPE? s SHUT@ 0= and ;

\ A child of the scope with the serial that CLOSE may close, or null.
: CHILD ( n -- RT-HANDLE:handle )
   {: sid:n :}
   SCOPES @
   begin dup RT-HANDLE:NULL? 0= while
      dup PARENT# F@ sid = over CLOSABLE? and if exit then
      NEXT@
   repeat ;

\ Close a scope. Opening with its open still queued, it ends at once; with the
\ open out, it stays Opening until the answer comes, which the host may make
\ a scope handle, and closes then. Otherwise it is Closing: its children
\ close first, then its own requests wind down and its SCOPE-CLOSE queues
\ behind them.
: CLOSE-REC ( RT-HANDLE:handle -- )
   {: s:RT-HANDLE:handle :}
   s SERIAL@ {: sid:n :}
   s S-OPENING IN? if
      s LIFE@ PHASE@ QUEUED = if s ABANDON exit then
      1 s SHUT# F! exit
   then
   S-CLOSING s STATE!
   begin sid CHILD dup RT-HANDLE:NULL? 0= while RECURSE repeat
   drop
   sid DROP-QUEUED
   sid WIND-DOWN
   sid K-CLOSE s LIFE@ ASK
   WAKE ;

: OPENED ( RT-HANDLE:handle RT-HANDLE:handle -- )
   {: s:RT-HANDLE:handle h:RT-HANDLE:handle :}
   h s HOST# H!
   S-OPEN s STATE!
   s SHUT@ if s CLOSE-REC exit then
   WAKE ;

\ Release every record on the list whose head is in the cell.
: DROP-ALL ( ptr RT-HANDLE:handle -- )
   {: head:ptr :}
   begin head @ dup RT-HANDLE:NULL? 0= while head DISPOSE repeat
   drop ;

\ The incarnation's death: every scope Closed and every record released at
\ once, the counters closed.
: REVOKE ( -- )
   REQUESTS DROP-ALL
   SCOPES DROP-ALL
   false REFUSAL !
   INSTANCES @ RT-ID:CLOSE
   PLACEMENTS @ RT-ID:CLOSE
   DEAD MODE ! ;

\ A queued request goes out once its scope is Open; an open, close or release
\ goes out whatever its scope's state.
: READY? ( RT-HANDLE:handle -- bool )
   {: q:RT-HANDLE:handle :}
   q SERIAL@ 0= if false exit then
   q PHASE@ QUEUED <> if false exit then
   q K-OPERATION KIND? 0= if true exit then
   q SCOPE# F@ SREC-OF S-OPEN IN? ;

$8000000000000000 constant SIGN-BIT

\ The older of the best request so far, null for none, and the next one;
\ serials compare unsigned.
: OLDER ( RT-HANDLE:handle RT-HANDLE:handle -- RT-HANDLE:handle )
   {: best:RT-HANDLE:handle q:RT-HANDLE:handle :}
   best RT-HANDLE:NULL? if q exit then
   q SERIAL@ SIGN-BIT xor best SERIAL@ SIGN-BIT xor < if q exit then
   best ;

\ The oldest request ready to go out, null when none is.
: NEXT-READY ( -- RT-HANDLE:handle )
   RT-HANDLE:NULL REQUESTS @
   begin dup RT-HANDLE:NULL? 0= while
      dup READY? if swap over OLDER swap then
      NEXT@
   repeat
   drop ;

\ Each release the host refused or failed goes out again, a new request.
: REARM ( -- )
   REQUESTS @
   begin dup RT-HANDLE:NULL? 0= while
      dup PHASE@ HELD = over K-RELEASE KIND? and if dup TO-RELEASE then
      NEXT@
   repeat
   drop ;

: SENT ( RT-HANDLE:handle -- )
   {: q:RT-HANDLE:handle :}
   OUT q PHASE# F!
   q K-CLOSE KIND? if S-DRAINING q SCOPE# F@ SREC-OF STATE! then ;

\ A release the host refused or failed keeps its handle, held until the next
\ turn sends it again, a turn it asks for.
: RETRY ( RT-HANDLE:handle -- )
   HELD swap PHASE# F!
   WAKE ;

\ The host refused the request, which stays Habu's (§24.5). A refused open
\ ends its scope unopened; a refused close leaves its scope Closing, the close
\ idle, and a refused release goes out again at the next turn, both reported
\ when the turn ends; a refused operation ends.
: REFUSED ( RT-HANDLE:handle -- )
   {: q:RT-HANDLE:handle :}
   q SCOPE# F@ SREC-OF {: s:RT-HANDLE:handle :}
   q K-OPEN KIND? if q END-Q s ABANDON exit then
   q K-CLOSE KIND? if q END-Q true REFUSAL ! exit then
   q K-RELEASE KIND? if q RETRY true REFUSAL ! exit then
   q END-Q
   s RETIRE ;

\ Submit the request and act on the answer; false ends the turn.
: SEND ( RT-HANDLE:handle -- bool )
   {: q:RT-HANDLE:handle :}
   q SERIAL@ >RT-REQUEST SUBMIT {: r:submit-result :}
   r RT--SCOPE-SUBMIT--RESULT:accepted RT--SCOPE-SUBMIT--RESULT:EQ if q SENT true exit then
   r RT--SCOPE-SUBMIT--RESULT:backpressure RT--SCOPE-SUBMIT--RESULT:EQ if false exit then
   r RT--SCOPE-SUBMIT--RESULT:stale-epoch RT--SCOPE-SUBMIT--RESULT:EQ if REVOKE false exit then
   q REFUSED true ;

: DONE? ( outcome-record -- bool )
   MATCH outcome-record
      done OF drop true ENDOF
      failed OF drop drop false ENDOF
   ;MATCH ;

: HOLDS ( outcome-record -- RT-HANDLE:handle )
   MATCH outcome-record
      done OF ENDOF
      failed OF drop drop RT-HANDLE:NULL ENDOF
   ;MATCH ;

: REPORT ( -- )
   REFUSAL @ if E-RT-SCOPE-REFUSED throw then ;

public

: NO-SCOPE ( -- scope )
   0 >SCOPE ;

\ Begin an incarnation over the runtime's pools: no scope or request, root
\ time 0, and its two identity counters over the EPOCH-CELLS zeroed cells at
\ the address.
: START ( ptr n RT-POOL:pools -- )
   {: at:ptr p:RT-POOL:pools :}
   MODE @ LIVE = if E-RT-SCOPE-EPOCH throw then
   p POOLS !
   at RT-ID:COUNTER INSTANCES !
   at RT-ID:COUNTER-CELLS cells + RT-ID:COUNTER PLACEMENTS !
   RT-HANDLE:NULL SCOPES !
   RT-HANDLE:NULL REQUESTS !
   false REFUSAL !
   0.0 NOW !
   LIVE MODE ! ;

\ The epoch's next ComponentInstanceId and PlacementId, each never reused.
: NEXT-INSTANCE ( -- n )
   ?LIVE INSTANCES @ RT-ID:NEXT ;

: NEXT-PLACEMENT ( -- n )
   ?LIVE PLACEMENTS @ RT-ID:NEXT ;

\ The host's root time in milliseconds (§4.5), finite and never earlier than
\ the last; RUNTIME reads no clock.
: TIME! ( r -- )
   {: t:r :}
   ?LIVE
   t t f- 0.0 f= 0= if E-RT-SCOPE-TIME throw then
   t NOW @ f< if E-RT-SCOPE-TIME throw then
   t NOW ! ;

: TIME@ ( -- r )
   ?LIVE NOW @ ;

: STATE@ ( scope -- scope-state )
   SCOPE>N SREC-OF SSTATE# F@ >SSTATE ;

\ The kind SCOPE-OPEN names (§5.1).
: SCOPE-KIND ( scope -- scope-kind )
   SCOPE>N SREC-OF SKIND# F@ >SKIND ;

: LOCAL-ID ( scope -- n )
   SCOPE>N SREC-OF SERIAL@ ;

: REQUEST-ID ( rt-request -- n )
   RT-REQUEST>N QREC-OF SERIAL@ ;

: KIND ( rt-request -- request-kind )
   RT-REQUEST>N QREC-OF QKIND# F@ >QKIND ;

: OWNER ( rt-request -- scope )
   RT-REQUEST>N QREC-OF SCOPE# F@ >SCOPE ;

\ The host handle the request names: a release's handle, an open's parent's
\ scope handle (null for an Application scope), else its scope's.
: TARGET ( rt-request -- RT-HANDLE:handle )
   RT-REQUEST>N QREC-OF {: q:RT-HANDLE:handle :}
   q K-RELEASE KIND? if q TARGET# H@ exit then
   q SCOPE# F@ SREC-OF {: s:RT-HANDLE:handle :}
   q K-OPEN KIND? 0= if s HOST@ exit then
   s PARENT# F@ 0= if RT-HANDLE:NULL exit then
   s PARENT# F@ SREC-OF HOST@ ;

private

\ An Application scope has no parent; any other kind's parent is Open, and a
\ Component's is an Application or Document scope.
: ADMIT ( scope scope-kind -- )
   {: p:scope k:scope-kind :}
   k RT--SCOPE-SCOPE--KIND:application RT--SCOPE-SCOPE--KIND:EQ {: root:bool :}
   p SCOPE>N 0= if root 0= if E-RT-SCOPE-PARENT throw then exit then
   root if E-RT-SCOPE-PARENT throw then
   p SCOPE>N SREC-OF {: s:RT-HANDLE:handle :}
   s S-OPEN IN? 0= if E-RT-SCOPE-PARENT throw then
   k RT--SCOPE-SCOPE--KIND:component RT--SCOPE-SCOPE--KIND:EQ 0= if exit then
   s SKIND# F@ >SKIND {: pk:scope-kind :}
   pk RT--SCOPE-SCOPE--KIND:application RT--SCOPE-SCOPE--KIND:EQ
   pk RT--SCOPE-SCOPE--KIND:document RT--SCOPE-SCOPE--KIND:EQ or 0= if E-RT-SCOPE-PARENT throw then ;

public

\ A scope of the kind under the parent, NO-SCOPE for an Application scope: its
\ record and its lifecycle record exist, Opening, before the turn WAKE asks
\ for sends its SCOPE-OPEN.
: OPEN ( scope scope-kind -- scope )
   {: p:scope k:scope-kind :}
   ?LIVE
   p k ADMIT
   SCOPES TAKE {: s:RT-HANDLE:handle :}
   s RT-HANDLE:NULL? if E-RT-SCOPE-OOM throw then
   REQUESTS TAKE {: life:RT-HANDLE:handle :}
   life RT-HANDLE:NULL? if s SCOPES DISPOSE E-RT-SCOPE-OOM throw then
   SERIAL {: id:n :}
   id s SERIAL# F!
   p SCOPE>N s PARENT# F!
   k SKIND>N s SKIND# F!
   S-OPENING s STATE!
   life s LIFE# H!
   id K-OPEN life ASK
   WAKE
   id >SCOPE ;

\ Close an Opening or Open scope that is not closing yet, or retry the close
\ of a Closing one the host refused or failed.
: CLOSE ( scope -- )
   ?LIVE
   SCOPE>N SREC-OF {: s:RT-HANDLE:handle :}
   s CLOSABLE? 0= if E-RT-SCOPE-STATE throw then
   s CLOSE-REC ;

\ A request of another package's operation in the scope: queued while the
\ scope is Opening, out at the next turn once it is Open; a closing scope
\ takes none.
: REQUEST ( scope -- rt-request )
   ?LIVE
   SCOPE>N SREC-OF {: s:RT-HANDLE:handle :}
   s LIVE-SCOPE? 0= if E-RT-SCOPE-STATE throw then
   s SHUT@ if E-RT-SCOPE-STATE throw then
   REQUESTS TAKE {: q:RT-HANDLE:handle :}
   q RT-HANDLE:NULL? if E-RT-SCOPE-OOM throw then
   s SERIAL@ K-OPERATION q ASK
   s S-OPEN IN? if WAKE then
   q SERIAL@ >RT-REQUEST ;

\ Send every ready request, oldest first, each release the host refused or
\ failed among them again. Backpressure keeps the rest for a later turn; a
\ stale-epoch answer ends the incarnation. A refused close or release is
\ reported once the turn ends.
: TURN ( -- )
   ?LIVE
   false REFUSAL !
   REARM
   begin NEXT-READY dup RT-HANDLE:NULL? 0= while SEND 0= if REPORT exit then repeat
   drop
   REPORT ;

\ The host's answer to a request it accepted. An open's success opens the
\ scope and its failure ends it unsent; a close's success closes it, and a
\ release's frees its handle; a failed close leaves its scope Closing, the
\ close idle, and a failed release goes out again at the next turn, each
\ reported. An Open scope's result that holds a handle stays held, owned by
\ the scope; a result owed to a closing scope is drained, its handle released
\ at the next turn.
: DELIVER ( rt-request outcome-record -- )
   {: r:rt-request o:outcome-record :}
   ?LIVE
   r RT-REQUEST>N QREC-OF {: q:RT-HANDLE:handle :}
   q PHASE@ OUT = q PHASE@ DRAINING = or 0= if E-RT-SCOPE-OWED throw then
   q SCOPE# F@ SREC-OF {: s:RT-HANDLE:handle :}
   o DONE? {: ok:bool :}
   o HOLDS {: h:RT-HANDLE:handle :}
   q K-OPEN KIND? if q END-Q ok if s h OPENED else s ABANDON then exit then
   q K-CLOSE KIND? if
      q END-Q
      ok if s ENDED exit then
      S-CLOSING s STATE!
      E-RT-SCOPE-REFUSED throw
   then
   q K-RELEASE KIND? if
      ok if q END-Q s RETIRE exit then
      q RETRY
      E-RT-SCOPE-REFUSED throw
   then
   h RT-HANDLE:NULL? if q END-Q s RETIRE exit then
   h q TARGET# H!
   q PHASE@ OUT = if HELD q PHASE# F! exit then
   q TO-RELEASE
   WAKE ;

;package
