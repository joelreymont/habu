\ scope-test.f - runtime scopes through RT-SCOPE's port against the headless
\ host: opens, refusals, drains, tombstones, owned handles, refused and failed
\ closes and releases, releases retried until the host completes them, a close
\ while the open is out, a close with the pools exhausted, the incarnation's
\ death, root time, the epoch's counters, the host's handle balance, and a
\ trace file a fresh process replays.
\ Run: bin/hb --load test/browser/scope-test.f

require lib/errors.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/test.f
require lib/runtime/id.f
require lib/runtime/handle.f
require lib/runtime/pool.f
require lib/runtime/scope.f
require test/browser/headless-host.f

package SCOPET

\ Scope kind, state, request kind and answer tags, in their ENUM orders.
0 constant APP
1 constant DOC
2 constant COMP
4 constant JOB
0 constant OPENING
1 constant OPEN
2 constant CLOSING
3 constant DRAINING
4 constant CLOSED
1 constant K-CLOSE
2 constant K-RELEASE
3 constant K-OPERATION
1 constant BACKPRESSURE
3 constant DENIED
6 constant STALE-EPOCH

: CODE= ( n -- )
   HEADLESS:LAST-CODE swap T= ;

: STATE= ( n n -- )
   swap HEADLESS:STATE-OF swap T= ;

\ Accept the open, or the request, at the request ref: done, a new handle.
: ANSWERED ( n -- )
   1 HEADLESS:DELIVER-STEP 0 CODE= ;

\ The host completes the release or close at the request ref: done.
: COMPLETED ( n -- )
   0 HEADLESS:DELIVER-STEP 0 CODE= ;

\ The run ends as the incarnation dies: the host answers stale-epoch to the
\ next SUBMIT, which the request at the scope ref makes.
: DIE ( n -- )
   STALE-EPOCH HEADLESS:ANSWER
   HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP ;

: JOBS ( -- RT-POOL:kind ) RT--POOL-KIND:jobs ;

\ The run's pools may charge this many records of the job-state pool, a
\ record being a slot: a header of four cells, then its payload (RT-POOL).
: RECORDS-QUOTA ( n -- )
   JOBS RT-POOL:PAYLOAD-CELLS 4 + cells *
   JOBS HEADLESS:POOLS-OF RT-POOL:QUOTA! ;

\ Return every record the run's pools have queued to its pool.
: RECLAIM ( -- )
   1000 HEADLESS:POOLS-OF RT-POOL:RECLAIM-STEP drop ;

\ ---- the recorded run: refs follow the order the host sees things ---------

: OPEN-FIRST ( -- )
   s" the record exists, Opening, before SCOPE-OPEN goes out" T-LABEL
   0 APP HEADLESS:OPEN-STEP 0 CODE=
   1 OPENING STATE=
   HEADLESS:SUBMIT-COUNT 0 T=
   s" the open waits for a later step, not inside WAKE" T-LABEL
   HEADLESS:PENDING? TTRUE
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 1 T=
   s" the embedding reads the kind SCOPE-OPEN names from its request" T-LABEL
   1 HEADLESS:REQUEST-OF RT-SCOPE:OWNER RT-SCOPE:SCOPE-KIND RT--SCOPE-SCOPE--KIND:TAG APP T=
   1 ANSWERED
   1 OPEN STATE= ;

: REFUSED-OPEN ( -- )
   s" a refused open sends none of its queued requests" T-LABEL
   1 DOC HEADLESS:OPEN-STEP
   2 HEADLESS:REQUEST-STEP 0 CODE=
   2 HEADLESS:REQUEST-STEP 0 CODE=
   DENIED HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 2 T=
   s" and closes it, its record free since nothing names it" T-LABEL
   2 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 2 T=
   s" its queued requests' tokens are stale" T-LABEL
   2 0 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-STALE CODE=
   s" so does an open the host accepts and then fails" T-LABEL
   1 DOC HEADLESS:OPEN-STEP
   3 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 3 T=
   6 2 HEADLESS:DELIVER-STEP 0 CODE=
   3 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 3 T= ;

: DRAINED ( -- )
   s" a result arriving after close is drained and its handle released" T-LABEL
   1 DOC HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   7 ANSWERED
   4 OPEN STATE=
   4 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   4 HEADLESS:CLOSE-STEP 0 CODE=
   4 CLOSING STATE=
   s" a closing scope takes no new request" T-LABEL
   4 HEADLESS:REQUEST-STEP RT-SCOPE:E-RT-SCOPE-STATE CODE=
   4 HEADLESS:CLOSE-STEP RT-SCOPE:E-RT-SCOPE-STATE CODE=
   HEADLESS:WAKE-STEP
   4 DRAINING STATE=
   9 COMPLETED
   s" Closed, its record a tombstone while the late result is owed" T-LABEL
   4 CLOSED STATE=
   s" and the host disposed of the closed scope's handle" T-LABEL
   HEADLESS:HOST-LIVE 1 T=
   8 ANSWERED
   HEADLESS:HOST-LIVE 2 T=
   HEADLESS:WAKE-STEP
   s" the drain releases the late result's handle" T-LABEL
   7 HEADLESS:SUBMITTED 2 T= K-RELEASE T=
   4 CLOSED STATE=
   10 COMPLETED
   HEADLESS:HOST-LIVE 1 T=
   4 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   s" a second answer to a request is refused" T-LABEL
   9 0 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-STALE CODE= ;

: OWNED ( -- )
   s" a closing scope releases the handles it holds" T-LABEL
   1 DOC HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   11 ANSWERED
   5 COMP HEADLESS:OPEN-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   12 HEADLESS:REQUEST-OF RT-SCOPE:OWNER RT-SCOPE:SCOPE-KIND RT--SCOPE-SCOPE--KIND:TAG COMP T=
   12 ANSWERED
   6 HEADLESS:REQUEST-STEP
   6 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   13 ANSWERED
   14 ANSWERED
   HEADLESS:HOST-LIVE 5 T=
   6 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   12 HEADLESS:SUBMITTED drop K-RELEASE T=
   13 HEADLESS:SUBMITTED drop K-RELEASE T=
   14 HEADLESS:SUBMITTED drop K-CLOSE T=
   HEADLESS:HOST-LIVE 5 T=
   15 COMPLETED
   16 COMPLETED
   17 COMPLETED
   s" once the host completes them, it holds only the open scopes' handles" T-LABEL
   HEADLESS:HOST-LIVE 2 T=
   6 HEADLESS:STATE-OF HEADLESS:RETIRED T= ;

: PRESSED ( -- )
   s" backpressure keeps a request for a later turn" T-LABEL
   5 HEADLESS:REQUEST-STEP
   BACKPRESSURE HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 15 T=
   HEADLESS:PENDING? TTRUE
   HEADLESS:WAKE-STEP
   16 HEADLESS:SUBMITTED drop K-OPERATION T=
   15 HEADLESS:SUBMITTED drop K-OPERATION T= ;

: REFUSED-CLOSE ( -- )
   1 JOB HEADLESS:OPEN-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   19 ANSWERED
   HEADLESS:HOST-LIVE 3 T=
   s" a refused close keeps the scope and its handle, and is reported" T-LABEL
   7 HEADLESS:CLOSE-STEP 0 CODE=
   DENIED HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   7 CLOSING STATE=
   HEADLESS:HOST-LIVE 3 T=
   s" so does a close the host accepts and then fails" T-LABEL
   7 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   7 DRAINING STATE=
   21 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   7 CLOSING STATE=
   HEADLESS:HOST-LIVE 3 T=
   s" a later close that succeeds retires it" T-LABEL
   7 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   22 COMPLETED
   7 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   HEADLESS:HOST-LIVE 2 T= ;

: REFUSED-RELEASE ( -- )
   1 JOB HEADLESS:OPEN-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   23 ANSWERED
   8 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   24 ANSWERED
   HEADLESS:HOST-LIVE 4 T=
   s" a refused release keeps its handle held, and is reported" T-LABEL
   8 HEADLESS:CLOSE-STEP 0 CODE=
   DENIED HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   23 HEADLESS:SUBMITTED drop K-RELEASE T=
   HEADLESS:HOST-LIVE 4 T=
   26 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   8 CLOSING STATE=
   s" so does a release the host accepts and then fails" T-LABEL
   8 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   27 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   28 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   HEADLESS:HOST-LIVE 4 T=
   s" until its release succeeds, and then the close retires the scope" T-LABEL
   8 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   29 COMPLETED
   30 COMPLETED
   8 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   HEADLESS:HOST-LIVE 2 T= ;

: DEATH ( -- )
   s" the incarnation's death closes every scope and frees every record" T-LABEL
   5 JOB HEADLESS:OPEN-STEP 0 CODE=
   5 DIE
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   5 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   9 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   s" while the host tears down every handle of the incarnation" T-LABEL
   HEADLESS:HOST-LIVE 0 T=
   s" and leaves no live epoch" T-LABEL
   [: RT-SCOPE:NEXT-INSTANCE drop ;] RT-SCOPE:E-RT-SCOPE-EPOCH TTHROWSQ
   1 HEADLESS:REQUEST-STEP RT-SCOPE:E-RT-SCOPE-EPOCH CODE=
   s" every record goes back to its pool" T-LABEL
   RECLAIM
   JOBS HEADLESS:POOLS-OF RT-POOL:CHARGED 0 T= ;

\ ---- the trace file, replayed by a fresh process ----------------------------

$1000 constant CAP
120000 constant TIMEOUT-MS
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: TRACE-BUF
variable TRACE-U

: TRACE$ ( -- ptr u8 n ) TRACE-BUF TRACE-U @ ;

\ The program the fresh process reads: replay the trace file its first
\ argument names.
: REPLAYER$ ( -- ptr u8 n )
   S\" require test/browser/headless-host.f\n: SCOPET-REPLAY ( -- )\n   0 SCRIPT-ARGV$ HEADLESS:REPLAY-FILE 0= if s\" scope replay: differs\" 1 die then\n   s\" scope replay: same\" type cr ;\nSCOPET-REPLAY\n" ;

: REPLAYED ( -- )
   s" the trace file a fresh process replays reproduces every outcome" T-LABEL
   s" scope-trace" HB-TMP-MKDIR {: a:ptr u:n :}
   a u s" trace" TRACE-BUF JOIN-PATH TRACE-U !
   TRACE$ HEADLESS:EXPORT
   s" scope-test: trace file " type TRACE$ type cr
   TRACE$ FILE-SIZE HEADLESS:TRACE-RECORDS 6 * cells T=
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+
   TRACE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN REPLAYER$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         OUT outu LEN>N S\" scope replay: same\n" T$=
         erru LEN>N 0 T=
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         OUT outu LEN>N type ERR erru LEN>N type
         rc RC>N 0 T=
      ENDOF
   ;MATCH ;

: RECORDED ( -- )
   HEADLESS:BEGIN-RUN
   OPEN-FIRST
   REFUSED-OPEN
   DRAINED
   OWNED
   PRESSED
   REFUSED-CLOSE
   REFUSED-RELEASE
   DEATH
   HEADLESS:TRACE-RECORDS 100 > TTRUE
   REPLAYED ;

\ ---- a tombstone holds its record ------------------------------------------

\ Open Application scopes until the pools refuse one.
: FILL ( -- )
   begin 0 APP HEADLESS:OPEN-STEP HEADLESS:LAST-CODE 0<> until ;

: TOMBSTONE ( -- )
   HEADLESS:BEGIN-RUN
   8 RECORDS-QUOTA
   0 APP HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   1 ANSWERED
   1 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   1 HEADLESS:CLOSE-STEP
   HEADLESS:WAKE-STEP
   3 COMPLETED
   1 CLOSED STATE=
   s" a record is not reused while its tombstone is referenced" T-LABEL
   FILL
   RT-SCOPE:E-RT-SCOPE-OOM CODE=
   1 CLOSED STATE=
   2 0 HEADLESS:DELIVER-STEP 0 CODE=
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   s" nor once it retires, until the pools reclaim it" T-LABEL
   0 APP HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-OOM CODE=
   RECLAIM
   0 APP HEADLESS:OPEN-STEP 0 CODE=
   s" and the retired scope's token is refused, not its record's new scope" T-LABEL
   1 HEADLESS:CLOSE-STEP RT-SCOPE:E-RT-SCOPE-STALE CODE=
   2 DIE ;

\ ---- a close while the open is out -----------------------------------------

: SHUT ( -- )
   HEADLESS:BEGIN-RUN
   0 APP HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   1 HEADLESS:REQUEST-STEP 0 CODE=
   s" a close while the open is out leaves the scope Opening" T-LABEL
   1 HEADLESS:CLOSE-STEP 0 CODE=
   1 OPENING STATE=
   s" and it takes no new request and no second close" T-LABEL
   1 HEADLESS:REQUEST-STEP RT-SCOPE:E-RT-SCOPE-STATE CODE=
   1 HEADLESS:CLOSE-STEP RT-SCOPE:E-RT-SCOPE-STATE CODE=
   s" the accepted open then closes it, its queued request unsent" T-LABEL
   1 ANSWERED
   1 CLOSING STATE=
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 2 T=
   2 HEADLESS:SUBMITTED drop K-CLOSE T=
   1 DRAINING STATE=
   3 COMPLETED
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   HEADLESS:HOST-LIVE 0 T=
   s" and every record of the closed scope goes back to its pool" T-LABEL
   RECLAIM
   JOBS HEADLESS:POOLS-OF RT-POOL:CHARGED 0 T=
   s" its unsent request's token is stale" T-LABEL
   2 0 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-STALE CODE=
   0 APP HEADLESS:OPEN-STEP
   STALE-EPOCH HEADLESS:ANSWER
   HEADLESS:WAKE-STEP ;

\ ---- a root scope with the pools exhausted ----------------------------------

: ROOT ( -- )
   HEADLESS:BEGIN-RUN
   8 RECORDS-QUOTA
   0 APP HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   1 ANSWERED
   begin 1 HEADLESS:REQUEST-STEP HEADLESS:LAST-CODE 0<> until
   RT-SCOPE:E-RT-SCOPE-OOM CODE=
   HEADLESS:WAKE-STEP
   s" a close reserves no record" T-LABEL
   1 HEADLESS:CLOSE-STEP 0 CODE=
   1 CLOSING STATE=
   STALE-EPOCH HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T= ;

\ ---- send order ---------------------------------------------------------------

: ORDER ( -- )
   HEADLESS:BEGIN-RUN
   0 APP HEADLESS:OPEN-STEP
   0 APP HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   1 ANSWERED
   2 ANSWERED
   s" requests go out oldest first, whichever record each one reuses" T-LABEL
   2 HEADLESS:CLOSE-STEP 0 CODE=
   1 HEADLESS:REQUEST-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   3 HEADLESS:SUBMITTED drop K-CLOSE T=
   4 HEADLESS:SUBMITTED drop K-OPERATION T=
   1 DIE ;

\ ---- a release goes out until the host completes it -------------------------

\ A run with an Application scope Open, request 1 its open, and an operation
\ out, request 2.
: OPERATING ( -- )
   HEADLESS:BEGIN-RUN
   0 APP HEADLESS:OPEN-STEP
   HEADLESS:WAKE-STEP
   1 ANSWERED
   1 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP ;

\ The run ends as the incarnation dies at a new scope's open.
: END-RUN ( -- )
   0 APP HEADLESS:OPEN-STEP
   STALE-EPOCH HEADLESS:ANSWER
   HEADLESS:WAKE-STEP ;

: RECLOSED ( -- )
   OPERATING
   1 HEADLESS:CLOSE-STEP
   HEADLESS:WAKE-STEP
   3 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   2 ANSWERED
   s" a close retried before the turn keeps the late result's release" T-LABEL
   1 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   4 HEADLESS:SUBMITTED 2 T= K-RELEASE T=
   5 HEADLESS:SUBMITTED drop K-CLOSE T=
   4 COMPLETED
   5 COMPLETED
   s" and the host holds no handle once both complete" T-LABEL
   HEADLESS:HOST-LIVE 0 T=
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   END-RUN ;

\ A run whose scope closed with its operation owed, and whose late result
\ then came with a handle, host ref 1 again: request 3 its close, its
\ release queued.
: CLOSED-OWING ( -- )
   OPERATING
   1 HEADLESS:CLOSE-STEP
   HEADLESS:WAKE-STEP
   3 COMPLETED
   1 CLOSED STATE=
   2 ANSWERED ;

: LATE-DENIED ( -- )
   CLOSED-OWING
   s" a release the host refuses after its scope closed goes out again" T-LABEL
   DENIED HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   HEADLESS:WAKE-STEP
   5 HEADLESS:SUBMITTED 1 T= K-RELEASE T=
   5 COMPLETED
   s" until the host frees its handle, and the tombstone then retires" T-LABEL
   HEADLESS:HOST-LIVE 0 T=
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   RECLAIM
   JOBS HEADLESS:POOLS-OF RT-POOL:CHARGED 0 T=
   END-RUN ;

: LATE-FAILED ( -- )
   CLOSED-OWING
   HEADLESS:WAKE-STEP
   s" so does one the host accepts and then fails" T-LABEL
   4 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   HEADLESS:WAKE-STEP
   5 HEADLESS:SUBMITTED 1 T= K-RELEASE T=
   5 COMPLETED
   HEADLESS:HOST-LIVE 0 T=
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   END-RUN ;

: RETRIED ( -- )
   OPERATING
   1 HEADLESS:CLOSE-STEP
   HEADLESS:WAKE-STEP
   2 ANSWERED
   s" a refused release goes out again while its scope is Draining" T-LABEL
   DENIED HEADLESS:ANSWER
   DENIED HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   HEADLESS:WAKE-STEP
   5 HEADLESS:SUBMITTED 2 T= K-RELEASE T=
   1 DRAINING STATE=
   s" and while it is Closing, with no close retried" T-LABEL
   3 2 HEADLESS:DELIVER-STEP RT-SCOPE:E-RT-SCOPE-REFUSED CODE=
   HEADLESS:WAKE-STEP
   6 HEADLESS:SUBMITTED 2 T= K-RELEASE T=
   6 COMPLETED
   1 CLOSING STATE=
   HEADLESS:HOST-LIVE 1 T=
   1 HEADLESS:CLOSE-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   7 COMPLETED
   HEADLESS:HOST-LIVE 0 T=
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   END-RUN ;

: STILL-HELD ( -- )
   OPERATING
   2 ANSWERED
   s" a turn sends no release of a handle an Open scope holds" T-LABEL
   1 HEADLESS:REQUEST-STEP
   HEADLESS:WAKE-STEP
   HEADLESS:SUBMIT-COUNT 3 T=
   1 DIE ;

\ ---- parents, refusals, root time, counters --------------------------------

: PARENTS ( -- )
   s" a parent is live and of an admissible kind" T-LABEL
   0 COMP HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-PARENT CODE=
   0 APP HEADLESS:OPEN-STEP 0 CODE=
   1 DOC HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-PARENT CODE=
   HEADLESS:WAKE-STEP
   1 ANSWERED
   1 APP HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-PARENT CODE=
   1 COMP HEADLESS:OPEN-STEP 0 CODE=
   1 JOB HEADLESS:OPEN-STEP 0 CODE=
   HEADLESS:WAKE-STEP
   2 ANSWERED
   3 ANSWERED
   s" a Component's parent is an Application or Document scope" T-LABEL
   2 COMP HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-PARENT CODE=
   3 COMP HEADLESS:OPEN-STEP RT-SCOPE:E-RT-SCOPE-PARENT CODE=
   3 DOC HEADLESS:OPEN-STEP 0 CODE= ;

\ A request cell nothing has filled holds the zero token.
TYPED-VARIABLE UNFILLED RT-SCOPE:rt-request

: REFUSALS ( -- )
   s" the zero token is refused before a record is read" T-LABEL
   [: RT-SCOPE:NO-SCOPE RT-SCOPE:STATE@ drop ;] RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   [: RT-SCOPE:NO-SCOPE RT-SCOPE:SCOPE-KIND drop ;] RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   [: RT-SCOPE:NO-SCOPE RT-SCOPE:CLOSE ;] RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   [: RT-SCOPE:NO-SCOPE RT-SCOPE:REQUEST drop ;] RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   [: UNFILLED @ RT-SCOPE:KIND drop ;] RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   [: UNFILLED @ RT-HANDLE:NULL RT--SCOPE-OUTCOME--RECORD:done RT-SCOPE:DELIVER ;]
   RT-SCOPE:E-RT-SCOPE-NULL TTHROWSQ
   s" an answer to a request not yet out is refused" T-LABEL
   1 HEADLESS:REQUEST-STEP
   [: 4 HEADLESS:REQUEST-OF RT-HANDLE:NULL RT--SCOPE-OUTCOME--RECORD:done RT-SCOPE:DELIVER ;]
   RT-SCOPE:E-RT-SCOPE-OWED TTHROWSQ ;

: CLOCK ( -- )
   s" root time is the host's input, finite and never earlier" T-LABEL
   RT-SCOPE:TIME@ 0.0 f= TTRUE
   5 HEADLESS:TIME-STEP 0 CODE=
   RT-SCOPE:TIME@ 5.0 f= TTRUE
   3 HEADLESS:TIME-STEP RT-SCOPE:E-RT-SCOPE-TIME CODE=
   RT-SCOPE:TIME@ 5.0 f= TTRUE
   [: 0.0 0.0 f/ RT-SCOPE:TIME! ;] RT-SCOPE:E-RT-SCOPE-TIME TTHROWSQ
   [: 1.0 0.0 f/ RT-SCOPE:TIME! ;] RT-SCOPE:E-RT-SCOPE-TIME TTHROWSQ ;

RT-SCOPE:EPOCH-CELLS TYPED-BUFFER SPARE n

: COUNTERS ( -- )
   s" the epoch draws instance and placement ids from its own counters" T-LABEL
   RT-SCOPE:NEXT-INSTANCE 1 T=
   RT-SCOPE:NEXT-INSTANCE 2 T=
   RT-SCOPE:NEXT-PLACEMENT 1 T=
   s" a second epoch does not start while one lives" T-LABEL
   [: 0 SPARE HEADLESS:POOLS-OF RT-SCOPE:START ;] RT-SCOPE:E-RT-SCOPE-EPOCH TTHROWSQ ;

: CLOSE-TREE ( -- )
   s" closing a scope closes its live children first" T-LABEL
   1 HEADLESS:CLOSE-STEP 0 CODE=
   1 CLOSING STATE=
   2 CLOSING STATE=
   3 CLOSING STATE=
   s" a child whose open never went out ends at once" T-LABEL
   4 HEADLESS:STATE-OF HEADLESS:RETIRED T=
   STALE-EPOCH HEADLESS:ANSWER
   HEADLESS:WAKE-STEP
   1 HEADLESS:STATE-OF HEADLESS:RETIRED T= ;

: CASTS ( -- )
   s" no cast turns a number into a scope or request token outside RT-SCOPE" T-LABEL
   s" CAST: SCOPET-FORGE-SCOPE ( n -- RT-SCOPE:scope )" TEST-EVAL:RC E-CAST-OWNER T=
   s" CAST: SCOPET-FORGE-REQUEST ( n -- RT-SCOPE:rt-request )" TEST-EVAL:RC E-CAST-OWNER T= ;

: CHECKED ( -- )
   HEADLESS:BEGIN-RUN
   PARENTS
   REFUSALS
   CLOCK
   COUNTERS
   s" a trace saved while its epoch lives replays too" T-LABEL
   REPLAYED
   CLOSE-TREE ;

: MAIN ( -- )
   T-RESET
   RECORDED
   TOMBSTONE
   SHUT
   ROOT
   ORDER
   RECLOSED
   LATE-DENIED
   LATE-FAILED
   RETRIED
   STILL-HELD
   CHECKED
   CASTS ;

MAIN

;package

\ The epoch's counters close with it: white-box, since only RT-SCOPE holds
\ them.
package RT-SCOPE

: SCOPET-CLOSED ( -- )
   s" the epoch's counters close with it" T-LABEL
   [: INSTANCES @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-CLOSED TTHROWSQ
   [: PLACEMENTS @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-CLOSED TTHROWSQ ;

SCOPET-CLOSED

;package

T-REPORT
