\ model.f - package MODEL, a Habu library: a client for OpenAI- and
\ Ollama-shaped chat and embedding providers over CURL, its API key read from
\ the native credential store through SECRET, with a caller's journal and
\ recorder and bounded inputs. Test: src/lib/net/model-test.f.
\ STORAGE CLASS. CALLER-OWNED: each call and settings record, and the arena
\ OPEN allocates for a call until CLOSE; the module keeps no state.
require src/lib/base64.f
require src/lib/byte-buffer.f
require src/lib/f64-text.f
require src/lib/json-read.f
require src/lib/json-write.f
require src/lib/memory.f
require src/lib/net/curl.f
require src/lib/os-memory.f
require src/lib/secret.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f
require src/lib/time.f

\ One exclusive call context per worker. Results borrow its arena until the
\ next CHAT/EMBED or CLOSE. CANCEL and REQUEST-STOP may run concurrently
\ with a call. Generation and embedding are each configured explicitly, with
\ no implicit model or fallback. The caller starts Habu's AIO and CURL loops
\ before HTTP calls and keeps model work outside PostgreSQL transactions. No
\ API key reaches a request body, response, error or producer record: a key is
\ read only into the HTTP header, which is wiped once libcurl copies it.
package MODEL

private
-9700 constant E-FIRST
-9799 constant E-LAST
public
\ From CHAT and EMBED, a provider's 404 (an unknown model) is also E-CONFIG and
\ its 401 or 403 E-KEY; HTTP-STATUS keeps the status.
-9700 constant E-CONFIG
-9701 constant E-KEY
\ From CHAT and EMBED: the provider refused the request for its size; INPUT+
\ also refuses an input past INPUT-MAX with it. An invalid setter or builder
\ argument is E-CONFIG.
-9702 constant E-CAPACITY
\ libcurl could not start or finish the transfer.
-9703 constant E-TRANSPORT
\ A status other than 200 that no code above or E-UNAVAILABLE names.
-9704 constant E-HTTP
\ The provider's response, or a journal record replayed for it, is malformed
\ or inconsistent.
-9705 constant E-RESPONSE
\ The chat response's message carries a refusal.
-9706 constant E-REFUSED
\ The chat response finished for a reason other than stop: FINISH-REASON$.
-9707 constant E-INCOMPLETE
\ The call's timeout ran out before its response arrived.
-9708 constant E-DEADLINE
\ CANCEL ended the call, or REQUEST-STOP came before or during it.
-9709 constant E-CANCELLED
\ A word out of turn: an unopened or busy call, settings closed or opened
\ twice, a part added before PARTS-RESET or after a failed builder, VECTOR@
\ before EMBED, or a recorded call without a purpose.
-9710 constant E-STATE
\ The provider is temporarily unavailable: status 408, 429 or 5xx.
-9711 constant E-UNAVAILABLE
60000 constant DEFAULT-MS
\ The most inputs one EMBED call takes.
8 constant INPUT-MAX

STRUCTURE allocation DERIVE addr
   FIELD base ptr u8
   FIELD size n
;STRUCTURE

STRUCTURE call DERIVE addr
   FIELD arena allocation
   FIELD body-w JSON-WRITE:writer
   FIELD parts-w JSON-WRITE:writer
   FIELD journal-parts-w JSON-WRITE:writer
   FIELD gate TASK:sem
   FIELD gate-ready bool
   FIELD handle option<CURL:handle>
   FIELD transport option<CURL:handle>
   FIELD busy bool
   FIELD cancelled bool
   FIELD stopped bool
   FIELD key-split n
   FIELD dims n
   FIELD timeout-ms n
   FIELD prompt-tokens n
   FIELD cached-tokens n
   FIELD deadline-ns n
   FIELD response-u n
   FIELD content-u n
   FIELD input-count n
   FIELD input-u n
   FIELD vector-at n
   FIELD embedded bool
   FIELD parts-count n
   FIELD parts-open bool
   FIELD parts-valid bool
   FIELD content-seen bool
   FIELD parsed-index n
   FIELD vector-seen bool
   FIELD endpoint bool
   FIELD component-count n
   FIELD data-count n
   FIELD http-status n
   FIELD finish-u n
   FIELD embedding-kind n
   FIELD journal-context n
   FIELD journal-get [ n ptr u8 n ptr n -- bool ]
   FIELD journal-put [ n ptr u8 n ptr u8 n -- ]
   FIELD journal-ready bool
   FIELD replay-kind n
;STRUCTURE

private

\ The last reply's usage: whether it carried a usage object, and each count,
\ none when the reply omits it or gives null. It lives in the call's arena,
\ keeping the by-value call record within the native width ceiling.
STRUCTURE tokens DERIVE addr
   FIELD seen bool
   FIELD prompt option<n>
   FIELD cached option<n>
   FIELD completion option<n>
   FIELD reasoning option<n>
;STRUCTURE

CAST: >TOKENS ( ptr u8 -- ptr tokens )

\ The installed recorder and the current call's accounting: the purpose
\ PURPOSE! left pending, the label of the call in progress, whether its request
\ reached the backend and when, its recorded call id and the recorder's fault.
STRUCTURE ledger DERIVE addr
   FIELD context n
   FIELD record [ n ptr u8 n -- n ]
   FIELD reject [ n n -- ]
   FIELD ready bool
   FIELD pending-u n
   FIELD label-u n
   FIELD dispatched bool
   FIELD started-ns n
   FIELD call-id n
   FIELD fault n
   FIELD kind n
   FIELD code n
   FIELD cancelled bool
;STRUCTURE

CAST: >LEDGER ( ptr u8 -- ptr ledger )

\ The arena's JSON reader record, reused by every reader the call opens.
STRUCTURE reading DERIVE addr
   FIELD cursor JR:cursor
;STRUCTURE

CAST: >READING ( ptr u8 -- ptr reading )

256 constant SCRATCH-INITIAL
1000000 constant NS-PER-MS
28 constant CURL-TIMED-OUT
1 constant OPENAI
2 constant OLLAMA

0 constant SCRATCH-BASE-AT
SCRATCH-BASE-AT 1 cells + constant GEN-HDR-AT
GEN-HDR-AT BUF:HDR-BYTES + constant EMBED-HDR-AT
EMBED-HDR-AT BUF:HDR-BYTES + constant KEY-SPEC-HDR-AT
KEY-SPEC-HDR-AT BUF:HDR-BYTES + constant SYSTEM-HDR-AT
SYSTEM-HDR-AT BUF:HDR-BYTES + constant SCHEMA-HDR-AT
SCHEMA-HDR-AT BUF:HDR-BYTES + constant PARTS-HDR-AT
PARTS-HDR-AT BUF:HDR-BYTES + constant JOURNAL-PARTS-HDR-AT
JOURNAL-PARTS-HDR-AT BUF:HDR-BYTES + constant BODY-HDR-AT
BODY-HDR-AT BUF:HDR-BYTES + constant INPUT-HDR-AT
INPUT-HDR-AT BUF:HDR-BYTES + constant URI-HDR-AT
URI-HDR-AT BUF:HDR-BYTES + constant EFFORT-HDR-AT
EFFORT-HDR-AT BUF:HDR-BYTES + constant TIER-HDR-AT
TIER-HDR-AT BUF:HDR-BYTES + constant SERVED-HDR-AT
SERVED-HDR-AT BUF:HDR-BYTES + constant ENDPOINT-HDR-AT
ENDPOINT-HDR-AT BUF:HDR-BYTES + constant IDENTITY-HDR-AT
IDENTITY-HDR-AT BUF:HDR-BYTES + constant BASE-URL-HDR-AT
BASE-URL-HDR-AT BUF:HDR-BYTES + constant URL-HDR-AT
URL-HDR-AT BUF:HDR-BYTES + constant GEN-CONFIG-HDR-AT
GEN-CONFIG-HDR-AT BUF:HDR-BYTES + constant EMBED-CONFIG-HDR-AT
EMBED-CONFIG-HDR-AT BUF:HDR-BYTES + constant JOURNAL-INPUT-HDR-AT
JOURNAL-INPUT-HDR-AT BUF:HDR-BYTES + constant JOURNAL-RECORD-HDR-AT
JOURNAL-RECORD-HDR-AT BUF:HDR-BYTES + constant JOURNAL-OUT-HDR-AT
JOURNAL-OUT-HDR-AT BUF:HDR-BYTES + constant REPLAY-CONTENT-HDR-AT
REPLAY-CONTENT-HDR-AT BUF:HDR-BYTES + constant RESPONSE-HDR-AT
RESPONSE-HDR-AT BUF:HDR-BYTES + constant CONTENT-HDR-AT
CONTENT-HDR-AT BUF:HDR-BYTES + constant ERROR-HDR-AT
ERROR-HDR-AT BUF:HDR-BYTES + constant KEY-LINE-HDR-AT
KEY-LINE-HDR-AT BUF:HDR-BYTES + constant VECTORS-HDR-AT
VECTORS-HDR-AT BUF:HDR-BYTES + constant PURPOSE-HDR-AT
PURPOSE-HDR-AT BUF:HDR-BYTES + constant ROWS-HDR-AT
ROWS-HDR-AT BUF:HDR-BYTES + constant ROW-TEXT-HDR-AT
ROW-TEXT-HDR-AT BUF:HDR-BYTES + constant CACHE-KEY-HDR-AT
CACHE-KEY-HDR-AT BUF:HDR-BYTES + constant COUNTS-AT
COUNTS-AT TOKENS-BYTES + constant ACCOUNTS-AT
ACCOUNTS-AT LEDGER-BYTES + constant HASH-AT
\ A purpose row: the offset and length in the row text of its purpose, model,
\ effort and service tier.
8 cells constant ROW-BYTES
\ The row the call in progress uses, plus one, then a row being added.
HASH-AT SHA256-CTX-BYTES + 96 + 7 + 7 invert and constant PICK-AT
PICK-AT 1 cells + constant ROW-NEW-AT
ROW-NEW-AT ROW-BYTES + constant READER-AT
READER-AT READING-BYTES + constant INPUT-OFF-AT
INPUT-OFF-AT INPUT-MAX cells + constant INPUT-LEN-AT
INPUT-LEN-AT INPUT-MAX cells + constant VECTOR-OFF-AT
VECTOR-OFF-AT INPUT-MAX cells + constant VECTOR-LEN-AT
VECTOR-LEN-AT INPUT-MAX cells + constant ARENA-BYTES

: INIT-BUFFERS ( ptr call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ {: base:ptr :}
   base GEN-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base EMBED-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base KEY-SPEC-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base SYSTEM-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base SCHEMA-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base PARTS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base JOURNAL-PARTS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base BODY-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base INPUT-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base URI-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base EFFORT-HDR-AT + CELL-VIEW 32 BUF:N>BLEN BUF:INIT
   base TIER-HDR-AT + CELL-VIEW 32 BUF:N>BLEN BUF:INIT
   base SERVED-HDR-AT + CELL-VIEW 32 BUF:N>BLEN BUF:INIT
   base ENDPOINT-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base IDENTITY-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base BASE-URL-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base URL-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base GEN-CONFIG-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base EMBED-CONFIG-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base JOURNAL-INPUT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base JOURNAL-RECORD-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base JOURNAL-OUT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base REPLAY-CONTENT-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base RESPONSE-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base CONTENT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base ERROR-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base KEY-LINE-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base VECTORS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base PURPOSE-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base ROWS-HDR-AT + CELL-VIEW ROW-BYTES BUF:N>BLEN BUF:INIT
   base ROW-TEXT-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base CACHE-KEY-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT ;

: DISPOSE-BUFFERS ( ptr u8 -- )
   {: base:ptr :}
   base GEN-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EMBED-HDR-AT + CELL-VIEW BUF:DISPOSE
   base KEY-SPEC-HDR-AT + CELL-VIEW BUF:DISPOSE
   base SYSTEM-HDR-AT + CELL-VIEW BUF:DISPOSE
   base SCHEMA-HDR-AT + CELL-VIEW BUF:DISPOSE
   base PARTS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base JOURNAL-PARTS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base BODY-HDR-AT + CELL-VIEW BUF:DISPOSE
   base INPUT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base URI-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EFFORT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base TIER-HDR-AT + CELL-VIEW BUF:DISPOSE
   base SERVED-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ENDPOINT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base IDENTITY-HDR-AT + CELL-VIEW BUF:DISPOSE
   base BASE-URL-HDR-AT + CELL-VIEW BUF:DISPOSE
   base URL-HDR-AT + CELL-VIEW BUF:DISPOSE
   base GEN-CONFIG-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EMBED-CONFIG-HDR-AT + CELL-VIEW BUF:DISPOSE
   base JOURNAL-INPUT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base JOURNAL-RECORD-HDR-AT + CELL-VIEW BUF:DISPOSE
   base JOURNAL-OUT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base REPLAY-CONTENT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base RESPONSE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CONTENT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ERROR-HDR-AT + CELL-VIEW BUF:DISPOSE
   base KEY-LINE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base VECTORS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base PURPOSE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ROWS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ROW-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CACHE-KEY-HDR-AT + CELL-VIEW BUF:DISPOSE ;

: STORAGE ( ptr call -- SPAN:span<u8> )
   {: c :}
   c MODEL-CALL:ARENA {: at :}
   at MODEL-ALLOCATION:BASE @ at MODEL-ALLOCATION:SIZE @
   MEM:BYTES-ALLOC-LEN MEM:ALLOCATION>SPAN ;

: REGION ( ptr call n n -- SPAN:span<u8> )
   {: c off:n size:n :}
   c STORAGE off size SPAN:SUB ;

: SCRATCH-BASE-CELL ( ptr call -- ptr n )
   SCRATCH-BASE-AT 1 cells REGION SPAN:$ drop CELL-VIEW ;

: HEADER ( ptr call n -- ptr n )
   {: c off:n :}
   c off BUF:HDR-BYTES REGION SPAN:$ drop CELL-VIEW ;

: GEN-BUF ( ptr call -- ptr n ) GEN-HDR-AT HEADER ;
: EMBED-BUF ( ptr call -- ptr n ) EMBED-HDR-AT HEADER ;
: KEY-SPEC-BUF ( ptr call -- ptr n ) KEY-SPEC-HDR-AT HEADER ;
: SYSTEM-BUF ( ptr call -- ptr n ) SYSTEM-HDR-AT HEADER ;
: SCHEMA-BUF ( ptr call -- ptr n ) SCHEMA-HDR-AT HEADER ;
: PARTS-BUF ( ptr call -- ptr n ) PARTS-HDR-AT HEADER ;
\ The parts as the journal input names them: an image by its digest.
: JOURNAL-PARTS-BUF ( ptr call -- ptr n ) JOURNAL-PARTS-HDR-AT HEADER ;
: BODY-BUF ( ptr call -- ptr n ) BODY-HDR-AT HEADER ;
: INPUT-BUF ( ptr call -- ptr n ) INPUT-HDR-AT HEADER ;
: URI-BUF ( ptr call -- ptr n ) URI-HDR-AT HEADER ;
: EFFORT-BUF ( ptr call -- ptr n ) EFFORT-HDR-AT HEADER ;
\ The service tier generation requests without a purpose row name; the
\ one the last reply says served it.
: TIER-BUF ( ptr call -- ptr n ) TIER-HDR-AT HEADER ;
: SERVED-BUF ( ptr call -- ptr n ) SERVED-HDR-AT HEADER ;
: ENDPOINT-BUF ( ptr call -- ptr n ) ENDPOINT-HDR-AT HEADER ;
: IDENTITY-BUF ( ptr call -- ptr n ) IDENTITY-HDR-AT HEADER ;
: BASE-URL-BUF ( ptr call -- ptr n ) BASE-URL-HDR-AT HEADER ;
: URL-BUF ( ptr call -- ptr n ) URL-HDR-AT HEADER ;
: GEN-CONFIG-BUF ( ptr call -- ptr n ) GEN-CONFIG-HDR-AT HEADER ;
: EMBED-CONFIG-BUF ( ptr call -- ptr n ) EMBED-CONFIG-HDR-AT HEADER ;
: JOURNAL-INPUT-BUF ( ptr call -- ptr n ) JOURNAL-INPUT-HDR-AT HEADER ;
: JOURNAL-RECORD-BUF ( ptr call -- ptr n ) JOURNAL-RECORD-HDR-AT HEADER ;
: JOURNAL-OUT-BUF ( ptr call -- ptr n ) JOURNAL-OUT-HDR-AT HEADER ;
: REPLAY-CONTENT-BUF ( ptr call -- ptr n ) REPLAY-CONTENT-HDR-AT HEADER ;
: RESPONSE-BUF ( ptr call -- ptr n ) RESPONSE-HDR-AT HEADER ;
: CONTENT-BUF ( ptr call -- ptr n ) CONTENT-HDR-AT HEADER ;
\ The last failed reply's provider message.
: ERROR-BUF ( ptr call -- ptr n ) ERROR-HDR-AT HEADER ;
: KEY-LINE-BUF ( ptr call -- ptr n ) KEY-LINE-HDR-AT HEADER ;
: VECTORS-BUF ( ptr call -- ptr n ) VECTORS-HDR-AT HEADER ;
: PURPOSE-BUF ( ptr call -- ptr n ) PURPOSE-HDR-AT HEADER ;
: ROWS-BUF ( ptr call -- ptr n ) ROWS-HDR-AT HEADER ;
: ROW-TEXT-BUF ( ptr call -- ptr n ) ROW-TEXT-HDR-AT HEADER ;
: CACHE-KEY-BUF ( ptr call -- ptr n ) CACHE-KEY-HDR-AT HEADER ;
\ A buffer's whole capacity. Writers that keep their own length write here.
: HELD ( ptr n -- SPAN:span<u8> )
   {: buf:ptr :}
   buf BUF:SPAN$ drop buf BUF:CAP@ BUF:BLEN>N SPAN:MAKE ;
\ The buffer's capacity, at least need bytes, for writing afresh: the bytes it
\ held are not kept.
: ROOM ( n ptr n -- SPAN:span<u8> )
   {: need:n buf:ptr :}
   buf BUF:CLEAR
   buf need BUF:N>BLEN BUF:ENSURE
   buf HELD ;
: TOKENS ( ptr call -- ptr tokens )
   COUNTS-AT TOKENS-BYTES REGION SPAN:$ drop >TOKENS ;
: LEDGER ( ptr call -- ptr ledger )
   ACCOUNTS-AT LEDGER-BYTES REGION SPAN:$ drop >LEDGER ;
\ A sha256 context, then the digest, then its hex.
: HASH-WORK ( ptr call -- ptr u8 )
   HASH-AT SHA256-CTX-BYTES 96 + REGION SPAN:$ drop ;
: LABEL$ ( ptr call -- ptr u8 n )
   {: c :}
   c PURPOSE-BUF HELD c LEDGER LEDGER-LABEL-U @ SPAN:TAKE SPAN:$ ;
\ The request is leaving for the backend: from here on the call is recorded.
: DISPATCHED ( ptr call -- )
   LEDGER {: l :}
   true l LEDGER-DISPATCHED !
   TIME:MONO-NS l LEDGER-STARTED-NS ! ;
: READER-STORE ( ptr call -- ptr JR:cursor )
   READER-AT READING-BYTES REGION SPAN:$ drop >READING READING-CURSOR ;
: INPUT-OFF ( ptr call n -- ptr n )
   {: c idx:n :}
   c INPUT-OFF-AT idx cells + 1 cells REGION SPAN:$ drop CELL-VIEW ;
: INPUT-LEN ( ptr call n -- ptr n )
   {: c idx:n :}
   c INPUT-LEN-AT idx cells + 1 cells REGION SPAN:$ drop CELL-VIEW ;
: VECTOR-OFF ( ptr call n -- ptr n )
   {: c idx:n :}
   c VECTOR-OFF-AT idx cells + 1 cells REGION SPAN:$ drop CELL-VIEW ;
: VECTOR-LEN ( ptr call n -- ptr n )
   {: c idx:n :}
   c VECTOR-LEN-AT idx cells + 1 cells REGION SPAN:$ drop CELL-VIEW ;

: ROW-COUNT ( ptr call -- n ) ROWS-BUF BUF:LEN@ BUF:BLEN>N ROW-BYTES / ;
\ The row the call in progress uses, plus one; 0 is none.
: PICK-CELL ( ptr call -- ptr n )
   PICK-AT 1 cells REGION SPAN:$ drop CELL-VIEW ;
\ Cell k of the row being added.
: ROW-NEW ( n ptr call -- ptr n )
   {: k:n c :}
   c ROW-NEW-AT k cells + 1 cells REGION SPAN:$ drop CELL-VIEW ;
\ Cell k of row idx.
: ROW-CELL ( n n ptr call -- n )
   {: idx:n k:n c :}
   c ROWS-BUF BUF:SPAN idx ROW-BYTES * k cells + 1 cells SPAN:SUB
   SPAN:$ drop CELL-VIEW @ ;
\ Field 0, 1, 2 or 3 of row idx: its purpose, model, effort or service tier.
: ROW$ ( n n ptr call -- ptr u8 n )
   {: idx:n field:n c :}
   c ROW-TEXT-BUF BUF:SPAN
   idx field 2 * c ROW-CELL idx field 2 * 1+ c ROW-CELL SPAN:SUB SPAN:$ ;
\ Adds field k of the row being added to the row text.
: ROW-TEXT+ ( ptr u8 n n ptr call -- )
   {: a:ptr u:n k:n c :}
   c ROW-TEXT-BUF BUF:LEN@ BUF:BLEN>N k 2 * c ROW-NEW !
   u k 2 * 1+ c ROW-NEW !
   a u BUF:N>BLEN c ROW-TEXT-BUF BUF:APPEND-SPAN ;
\ The row of the purpose, plus one; 0 is none.
: ROW-OF ( ptr u8 n ptr call -- n )
   {: a:ptr u:n c :}
   c ROW-COUNT 0 ?do
      i 0 c ROW$ a u STR= if i 1+ unloop exit then
   loop 0 ;

: BUFFER$ ( ptr n -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;
\ The generation model, effort and tier: the purpose row's while the call in
\ progress, or the last one, has one, else the call's own.
: GEN$ ( ptr call -- ptr u8 n )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 1 c ROW$ exit then
   c GEN-BUF BUFFER$ ;
: EMBED-MODEL$ ( ptr call -- ptr u8 n ) EMBED-BUF BUFFER$ ;
: KEY-SPEC$ ( ptr call -- ptr u8 n ) KEY-SPEC-BUF BUFFER$ ;
: SYSTEM$ ( ptr call -- ptr u8 n ) SYSTEM-BUF BUFFER$ ;
: SCHEMA$ ( ptr call -- ptr u8 n ) SCHEMA-BUF BUFFER$ ;
: PARTS$ ( ptr call -- ptr u8 n ) PARTS-BUF BUFFER$ ;
: JOURNAL-PARTS$ ( ptr call -- ptr u8 n ) JOURNAL-PARTS-BUF BUFFER$ ;
: BODY$ ( ptr call -- ptr u8 n ) BODY-BUF BUFFER$ ;
: EFFORT$ ( ptr call -- ptr u8 n )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 2 c ROW$ exit then
   c EFFORT-BUF BUFFER$ ;
: CACHE-KEY$ ( ptr call -- ptr u8 n ) CACHE-KEY-BUF BUFFER$ ;
: TIER$ ( ptr call -- ptr u8 n )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 3 c ROW$ exit then
   c TIER-BUF BUFFER$ ;
: SERVED$ ( ptr call -- ptr u8 n ) SERVED-BUF BUFFER$ ;
: ENDPOINT$ ( ptr call -- ptr u8 n ) ENDPOINT-BUF BUFFER$ ;
: IDENTITY$ ( ptr call -- ptr u8 n ) IDENTITY-BUF BUFFER$ ;
: BASE-URL$ ( ptr call -- ptr u8 n ) BASE-URL-BUF BUFFER$ ;
: GEN-CONFIG$ ( ptr call -- ptr u8 n ) GEN-CONFIG-BUF BUFFER$ ;
: EMBED-CONFIG$ ( ptr call -- ptr u8 n ) EMBED-CONFIG-BUF BUFFER$ ;
: JOURNAL-INPUT$ ( ptr call -- ptr u8 n ) JOURNAL-INPUT-BUF BUFFER$ ;
: JOURNAL-RECORD$ ( ptr call -- ptr u8 n ) JOURNAL-RECORD-BUF BUFFER$ ;
: JOURNAL-OUT$ ( ptr call -- ptr u8 n ) JOURNAL-OUT-BUF BUFFER$ ;
: REPLAY-CONTENT$ ( ptr call -- ptr u8 n ) REPLAY-CONTENT-BUF BUFFER$ ;
: RESPONSE$ ( ptr call -- ptr u8 n )
   {: c :}
   c RESPONSE-BUF HELD c MODEL-CALL:RESPONSE-U @ SPAN:TAKE SPAN:$ ;

: REQUIRE-IDLE ( ptr call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then
   c MODEL-CALL:BUSY @ if E-STATE throw then ;

: REPLACE-SOME ( ptr u8 n ptr n -- )
   {: a:ptr u:n buf:ptr :}
   u 0 <= if E-CONFIG throw then
   a u BUF:N>BLEN buf BUF:REPLACE ;

: RESET-SIZED ( ptr n n ptr call -- )
   {: buf:ptr used:n c :}
   c SCRATCH-BASE-CELL @
   begin dup used < while 2 * repeat {: target:n :}
   buf BUF:CAP@ BUF:BLEN>N target > if
      buf BUF:DISPOSE
      buf target BUF:N>BLEN BUF:INIT
   else
      buf BUF:CLEAR
   then ;

: RESET-SCRATCH ( ptr n ptr call -- )
   {: buf:ptr c :}
   buf buf BUF:LEN@ BUF:BLEN>N c RESET-SIZED ;

\ No reply usage seen yet.
: RESET-USAGE ( ptr call -- )
   TOKENS {: t :}
   false t TOKENS-SEEN !
   OPTION:NONE t TOKENS-PROMPT !
   OPTION:NONE t TOKENS-CACHED !
   OPTION:NONE t TOKENS-COMPLETION !
   OPTION:NONE t TOKENS-REASONING ! ;

: OPEN-PREP ( ptr call -- ptr call )
   {: c :}
   SCRATCH-INITIAL OS-MEMORY:PAGE-SIZE max c SCRATCH-BASE-CELL !
   c INIT-BUFFERS
   TASK:NEW-SEMAPHORE c MODEL-CALL:GATE !
   true c MODEL-CALL:GATE-READY !
   1 c MODEL-CALL:GATE @ TASK:SEMAPHORE-INIT
   OPTION:NONE c MODEL-CALL:HANDLE !
   OPTION:NONE c MODEL-CALL:TRANSPORT !
   false c MODEL-CALL:BUSY !
   false c MODEL-CALL:CANCELLED !
   false c MODEL-CALL:STOPPED !
   OPENAI c MODEL-CALL:EMBEDDING-KIND !
   0 c MODEL-CALL:DIMS !
   0 c MODEL-CALL:INPUT-COUNT !
   0 c MODEL-CALL:INPUT-U !
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:CONTENT-U !
   0 c MODEL-CALL:FINISH-U !
   0 c MODEL-CALL:PARTS-COUNT !
   false c MODEL-CALL:PARTS-OPEN !
   false c MODEL-CALL:PARTS-VALID !
   false c MODEL-CALL:EMBEDDED !
   false c MODEL-CALL:JOURNAL-READY !
   0 c MODEL-CALL:REPLAY-KIND !
   0 c PICK-CELL !
   DEFAULT-MS c MODEL-CALL:TIMEOUT-MS !
   0 c MODEL-CALL:PROMPT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   c RESET-USAGE
   c ;

: RELEASE-OPEN ( ptr call -- )
   {: c :}
   c MODEL-CALL:GATE-READY @ if
      c MODEL-CALL:GATE @ TASK:FREE-SEMAPHORE
      false c MODEL-CALL:GATE-READY !
   then
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ {: base:ptr :}
   base DISPOSE-BUFFERS
   c READER-STORE JR:DISPOSE
   base ARENA-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE !
   0 c MODEL-CALL:ARENA MODEL-ALLOCATION:SIZE ! ;

: SCHEME-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u s" https://" STARTS-WITH? if 8 exit then
   a u s" http://" STARTS-WITH? if 7 exit then
   0 ;

\ A base byte is printable and no userinfo, query or fragment delimiter, so
\ no credential rides in a URL that producer configs and journals record.
: BASE-BYTE? ( n -- bool )
   {: b:n :}
   b 32 > b 127 < and
   b [char] @ <> and b [char] ? <> and b [char] # <> and ;

\ The provider base every OpenAI-compatible route extends when none is set.
: OPENAI-BASE$ ( -- ptr u8 n ) s" https://api.openai.com/v1" ;

\ The configured base, or OpenAI's when none is set.
: BASE$ ( ptr call -- ptr u8 n )
   BASE-URL$ {: base:ptr base-u:n :}
   base-u 0= if OPENAI-BASE$ else base base-u then ;

\ The base followed by the route; valid until the next rendering.
: URL$ ( ptr u8 n ptr call -- ptr u8 n )
   {: route:ptr route-u:n c :}
   c URL-BUF BUF:CLEAR
   c BASE$ BUF:N>BLEN c URL-BUF BUF:APPEND-SPAN
   route route-u BUF:N>BLEN c URL-BUF BUF:APPEND-SPAN
   c URL-BUF BUFFER$ ;

: CHAT-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   s" /chat/completions" c URL$ ;

: EMBED-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   s" /embeddings" c URL$ ;

public

: OPEN ( ptr call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= 0= if E-STATE throw then
   ARENA-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   \ Fresh anonymous pages are zero; leave unused arena regions untouched.
   base c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE !
   ARENA-BYTES c MODEL-CALL:ARENA MODEL-ALLOCATION:SIZE !
   false c MODEL-CALL:GATE-READY !
   c [: OPEN-PREP ;] catch {: code:n :} drop
   code 0<> if c RELEASE-OPEN code throw then ;

: CLOSE ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   c RELEASE-OPEN ;

\ Resumes completed calls from the caller's durable store. ctx passes unchanged
\ to get, which fills the MODEL-owned buffer with one record on a hit, and to
\ put, which commits a newly accepted record before returning. Both run while
\ MODEL is idle, and a callback failure propagates. A hit restores the
\ record's content, vectors and producing configuration without a provider
\ call. OPEN disables the journal.
: JOURNAL! ( n [ n ptr u8 n ptr n -- bool ] [ n ptr u8 n ptr u8 n -- ] ptr call -- )
   {: ctx:n get put c :}
   c REQUIRE-IDLE
   ctx c MODEL-CALL:JOURNAL-CONTEXT !
   get c MODEL-CALL:JOURNAL-GET !
   put c MODEL-CALL:JOURNAL-PUT !
   true c MODEL-CALL:JOURNAL-READY ! ;

\ Installs the recorder. After each request a backend receives, served or
\ failed, record gets the context and the call's JSON record and returns its
\ call id; reject gets the context and the id of a call whose reply the
\ library rejected. A journal replay sends nothing and records nothing.
: RECORDER! ( n [ n ptr u8 n -- n ] [ n n -- ] ptr call -- )
   {: ctx:n record reject c :}
   c REQUIRE-IDLE
   c LEDGER {: l :}
   ctx l LEDGER-CONTEXT !
   record l LEDGER-RECORD !
   reject l LEDGER-REJECT !
   true l LEDGER-READY ! ;

\ What the next CHAT or EMBED is for: nonblank.
\ Every CHAT and EMBED consumes it; with a recorder installed, one without a
\ purpose is E-STATE.
: PURPOSE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0= if false exit then
   false u 0 ?do a i + c@ 32 > or loop ;

: PURPOSE! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   a u PURPOSE? 0= if E-CONFIG throw then
   a u BUF:N>BLEN c PURPOSE-BUF BUF:REPLACE
   u c LEDGER LEDGER-PENDING-U ! ;

: BASE-URL? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0< if false exit then
   a u SCHEME-LEN {: scheme-u:n :}
   scheme-u 0= u scheme-u <= or if false exit then
   a u + 1- c@ [char] / = if false exit then
   true u 0 ?do a i + c@ BASE-BYTE? 0= if drop false then loop ;

\ The OpenAI-compatible base the chat and embedding routes extend; empty
\ restores OpenAI's.
: BASE-URL! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   u 0<> if a u BASE-URL? 0= if E-CONFIG throw then then
   0 c MODEL-CALL:REPLAY-KIND !
   u 0= if c BASE-URL-BUF BUF:CLEAR exit then
   a u c BASE-URL-BUF REPLACE-SOME ;

: GENERATION-MODEL! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   a u c GEN-BUF REPLACE-SOME
   c MODEL-CALL:REPLAY-KIND @ 1 = if 0 c MODEL-CALL:REPLAY-KIND ! then ;

\ The reasoning effort of every generation call without a purpose row; empty
\ sends none.
: EFFORT! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   u 0= if c EFFORT-BUF BUF:CLEAR else a u c EFFORT-BUF REPLACE-SOME then
   c MODEL-CALL:REPLAY-KIND @ 1 = if 0 c MODEL-CALL:REPLAY-KIND ! then ;

\ The provider's prompt cache key of every later generation call: requests
\ sharing a key share cache routing. Empty sends none. It leaves the reply,
\ so the journal input and replay, unchanged.
: CACHE-KEY! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   u 0= if c CACHE-KEY-BUF BUF:CLEAR else a u c CACHE-KEY-BUF REPLACE-SOME then ;

\ The service tier every generation request without a purpose row names,
\ such as flex; empty sends none, so the provider serves its default.
\ Embedding requests carry none. The tier changes no content, so replay does
\ not depend on it.
: SERVICE-TIER! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   u 0= if c TIER-BUF BUF:CLEAR else a u c TIER-BUF REPLACE-SOME then ;

\ The generation model, effort and service tier of every later call for the
\ purpose, in place of the call's own; empty effort or tier sends none, even
\ when SERVICE-TIER! set one. A purpose takes one row: a repeated purpose is
\ E-CONFIG.
: PURPOSE-MODEL! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr call -- )
   {: p:ptr p-u:n m:ptr m-u:n e:ptr e-u:n t:ptr t-u:n c :}
   c REQUIRE-IDLE
   p p-u PURPOSE? 0= if E-CONFIG throw then
   m-u 0 <= e-u 0< or t-u 0< or if E-CONFIG throw then
   p p-u c ROW-OF 0<> if E-CONFIG throw then
   p p-u 0 c ROW-TEXT+
   m m-u 1 c ROW-TEXT+
   e e-u 2 c ROW-TEXT+
   t t-u 3 c ROW-TEXT+
   c ROW-NEW-AT ROW-BYTES REGION SPAN:$ BUF:N>BLEN c ROWS-BUF BUF:APPEND-SPAN
   c MODEL-CALL:REPLAY-KIND @ 1 = if 0 c MODEL-CALL:REPLAY-KIND ! then ;

: EMBEDDING-MODEL! ( ptr u8 n n ptr call -- )
   {: a:ptr u:n dims:n c :}
   c REQUIRE-IDLE
   dims 0 <= if E-CONFIG throw then
   a u c EMBED-BUF REPLACE-SOME
   dims c MODEL-CALL:DIMS !
   c MODEL-CALL:REPLAY-KIND @ 2 = if 0 c MODEL-CALL:REPLAY-KIND ! then
   OPENAI c MODEL-CALL:EMBEDDING-KIND ! ;

\ Names the native credential store's item, by service and account, holding
\ the API key: the only source of a key. Each CHAT and EMBED reads it with
\ SECRET:READ into the request header. An idle open call is required, else
\ E-STATE; an empty service or account is E-CONFIG. At CHAT and EMBED a
\ missing, empty or non-printable item is E-KEY, and SECRET:E-CONFIG,
\ SECRET:E-ACCESS and SECRET:E-UNAVAILABLE pass through; a call with no item
\ named fails there with E-CONFIG. The key spec holds the service, then the
\ account from byte key-split.
: API-KEY! ( ptr u8 n ptr u8 n ptr call -- )
   {: svc:ptr su:n acct:ptr au:n c :}
   c REQUIRE-IDLE
   su 0 <= au 0 <= or if E-CONFIG throw then
   svc su c KEY-SPEC-BUF REPLACE-SOME
   acct au BUF:N>BLEN c KEY-SPEC-BUF BUF:APPEND-SPAN
   su c MODEL-CALL:KEY-SPLIT ! ;

: TIMEOUT! ( n ptr call -- )
   {: ms:n c :}
   c REQUIRE-IDLE
   ms 0 <= ms 2147483647 > or if E-CONFIG throw then
   ms c MODEL-CALL:TIMEOUT-MS ! ;

: SYSTEM! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   a u c SYSTEM-BUF REPLACE-SOME ;

: SCHEMA! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   a u c SCHEMA-BUF REPLACE-SOME ;

\ Model settings a host files in the native credential store under one
\ service of its choice, one generic-password item per fixed account:
\ "model" names the generation model (required), "base-url" an
\ OpenAI-compatible base BASE-URL! accepts (optional; absent or empty is
\ OpenAI's) and "api-key" holds the key API-KEY! names. A host keeps one in
\ zeroed storage (1 TYPED-BUFFER S MODEL:settings); SETTINGS-OPEN reads the
\ model and base once into bytes the settings own until SETTINGS-CLOSE.
STRUCTURE settings DERIVE addr
   FIELD bufs ptr u8
   FIELD ready n
   FIELD model-u n
   FIELD base-u n
;STRUCTURE

private

0 constant SETTING-SERVICE
1 constant SETTING-MODEL
2 constant SETTING-BASE
3 constant SETTING-BUFFERS
64 constant SETTING-INITIAL

: SETTING-BUF ( ptr settings n -- ptr n )
   {: s idx:n :}
   s MODEL-SETTINGS:BUFS @ idx BUF:HDR-BYTES * + CELL-VIEW ;

: SETTING-ACCOUNT$ ( n -- ptr u8 n )
   case
      SETTING-MODEL of s" model" endof
      SETTING-BASE of s" base-url" endof
      E-STATE throw
   endcase ;

: SETTING$ ( ptr settings n n -- ptr u8 n )
   {: s idx:n u:n :}
   s idx SETTING-BUF HELD u SPAN:TAKE SPAN:$ ;


\ Reads the item of setting idx into its buffer's capacity and returns its
\ length; a missing item reads as empty.
: SETTING-READ ( ptr settings n -- ptr settings n )
   {: s idx:n :}
   s SETTING-SERVICE SETTING-BUF BUFFER$ idx SETTING-ACCOUNT$
   s idx SETTING-BUF HELD SECRET:READ {: u:n found:bool :}
   s found if u else 0 then ;


\ An item larger than the buffer grows it and is read again: the store copies
\ no byte of an item it refuses. The store's own failures pass through.
: SETTING-LOAD ( ptr settings n -- n )
   {: s idx:n :}
   begin
      s idx [: SETTING-READ ;] catch {: fault:n :}
      fault 0= if nip exit then
      2drop
      fault SECRET:E-CAPACITY <> if fault throw then
      s idx SETTING-BUF {: buf:ptr :}
      buf BUF:CAP@ BUF:BLEN>N 2 * {: need:n :}
      buf BUF:CLEAR
      buf need BUF:N>BLEN BUF:ENSURE
   again ;


: SETTINGS-FILL ( ptr u8 n ptr settings -- ptr u8 n ptr settings )
   {: svc:ptr su:n s :}
   SETTING-BUFFERS 0 ?do
      s i SETTING-BUF SETTING-INITIAL BUF:N>BLEN BUF:INIT
      i 1+ s MODEL-SETTINGS:READY !
   loop
   svc su BUF:N>BLEN s SETTING-SERVICE SETTING-BUF BUF:REPLACE
   s SETTING-MODEL SETTING-LOAD {: mu:n :}
   mu 0= if E-CONFIG throw then
   mu s MODEL-SETTINGS:MODEL-U !
   s SETTING-BASE SETTING-LOAD {: bu:n :}
   bu 0<> if s SETTING-BASE bu SETTING$ BASE-URL? 0= if E-CONFIG throw then then
   bu s MODEL-SETTINGS:BASE-U !
   svc su s ;


: SETTINGS-RELEASE ( ptr settings -- )
   {: s :}
   s MODEL-SETTINGS:READY @ 0 ?do s i SETTING-BUF BUF:DISPOSE loop
   s MODEL-SETTINGS:BUFS @ SETTING-BUFFERS BUF:HDR-BYTES *
      MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR s MODEL-SETTINGS:BUFS !
   0 s MODEL-SETTINGS:READY !
   0 s MODEL-SETTINGS:MODEL-U !
   0 s MODEL-SETTINGS:BASE-U ! ;

: SETTINGS-OPEN? ( ptr settings -- bool )
   MODEL-SETTINGS:BUFS @ NULL-PTR = 0= ;

public

\ Reads service's model and base-url items into the closed settings s.
\ An open s is E-STATE. An empty service, a missing or empty model item, or a
\ non-empty base-url item BASE-URL! refuses is E-CONFIG; SECRET:E-CONFIG,
\ SECRET:E-ACCESS and SECRET:E-UNAVAILABLE pass through. A failed open
\ leaves s closed.
: SETTINGS-OPEN ( ptr u8 n ptr settings -- )
   {: svc:ptr su:n s :}
   s SETTINGS-OPEN? if E-STATE throw then
   su 0 <= if E-CONFIG throw then
   SETTING-BUFFERS BUF:HDR-BYTES * MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop
      s MODEL-SETTINGS:BUFS !
   0 s MODEL-SETTINGS:READY !
   svc su s [: SETTINGS-FILL ;] catch {: code:n :}
   drop 2drop
   code 0<> if s SETTINGS-RELEASE code throw then ;

\ Releases the settings' bytes; closed settings are E-STATE.
: SETTINGS-CLOSE ( ptr settings -- )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTINGS-RELEASE ;

\ The open settings' service, model and base URL (empty when its item is
\ absent or empty). The bytes stay owned by the settings until SETTINGS-CLOSE.
\ Closed settings are E-STATE.
: SETTINGS-SERVICE$ ( ptr settings -- ptr u8 n )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-SERVICE SETTING-BUF BUFFER$ ;

: SETTINGS-MODEL$ ( ptr settings -- ptr u8 n )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-MODEL s MODEL-SETTINGS:MODEL-U @ SETTING$ ;

: SETTINGS-BASE$ ( ptr settings -- ptr u8 n )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-BASE s MODEL-SETTINGS:BASE-U @ SETTING$ ;

\ Sets the call's generation model, base URL and the service's "api-key" item
\ as its key, copying each into the call: the settings may close after.
\ A busy or unopened call, or closed settings, is E-STATE.
: SETTINGS-APPLY ( ptr settings ptr call -- )
   {: s c :}
   c REQUIRE-IDLE
   s SETTINGS-MODEL$ c GENERATION-MODEL!
   s SETTINGS-BASE$ c BASE-URL!
   s SETTINGS-SERVICE$ s" api-key" c API-KEY! ;

private

: WRITER ( ptr call -- ptr JSON-WRITE:writer ) MODEL-CALL:BODY-W ;
: PART-WRITER ( ptr call -- ptr JSON-WRITE:writer ) MODEL-CALL:PARTS-W ;
: JOURNAL-PART-WRITER ( ptr call -- ptr JSON-WRITE:writer ) MODEL-CALL:JOURNAL-PARTS-W ;

: BUILD-GEN-CONFIG ( ptr call -- )
   {: c :}
   c WRITER c GEN-CONFIG-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" backend" s" openai-chat-completions" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" model" c GEN$ JSON-WRITE:FIELD-S
   c EFFORT$ nip 0<> if JSON-WRITE:COMMA s" effort" c EFFORT$ JSON-WRITE:FIELD-S then
   JSON-WRITE:COMMA s" base_url" c BASE$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

: BUILD-EMBED-CONFIG ( ptr call -- )
   {: c :}
   c WRITER c EMBED-CONFIG-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" backend" c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      s" ollama" else s" openai-embeddings" then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" model" c EMBED-MODEL$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" dimensions" c MODEL-CALL:DIMS @ JSON-WRITE:FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON-WRITE:COMMA s" identity" c IDENTITY$ JSON-WRITE:FIELD-S
      JSON-WRITE:COMMA s" truncate" false JSON-WRITE:FIELD-BOOL
   else
      JSON-WRITE:COMMA s" base_url" c BASE$ JSON-WRITE:FIELD-S
   then
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

public

: GENERATION-CONFIG$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-IDLE
   c MODEL-CALL:REPLAY-KIND @ 1 <> if c BUILD-GEN-CONFIG then
   c GEN-CONFIG$ ;

: EMBEDDING-CONFIG$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-IDLE
   c MODEL-CALL:REPLAY-KIND @ 2 <> if c BUILD-EMBED-CONFIG then
   c EMBED-CONFIG$ ;

\ Ollama's native /api/embed at endpoint. identity names the model artifact,
\ such as its manifest digest, and is the vector space's identity: a changed
\ artifact needs a new one even under the same tag, and the endpoint is a
\ producer input outside it. Requests send truncate:false because Ollama
\ truncates by default; a context overflow fails the call.
: OLLAMA! ( ptr u8 n ptr u8 n ptr u8 n n ptr call -- )
   {: endpoint:ptr endpoint-u:n model:ptr model-u:n identity:ptr identity-u:n dims:n c :}
   c REQUIRE-IDLE
   dims 0 <= if E-CONFIG throw then
   endpoint endpoint-u c ENDPOINT-BUF REPLACE-SOME
   model model-u c EMBED-BUF REPLACE-SOME
   identity identity-u c IDENTITY-BUF REPLACE-SOME
   dims c MODEL-CALL:DIMS !
   OLLAMA c MODEL-CALL:EMBEDDING-KIND !
   c MODEL-CALL:REPLAY-KIND @ 2 = if 0 c MODEL-CALL:REPLAY-KIND ! then ;

private

: PART-OPEN ( ptr call -- )
   {: c :}
   c PART-WRITER c PARTS-BUF JSON-WRITE:OPEN-BUF JSON-WRITE:ARRAY-START drop
   c JOURNAL-PART-WRITER c JOURNAL-PARTS-BUF JSON-WRITE:OPEN-BUF
   JSON-WRITE:ARRAY-START drop ;

: PART-FINISH ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-OPEN @ 0= if E-STATE throw then
   c MODEL-CALL:PARTS-VALID @ 0= if E-STATE throw then
   false c MODEL-CALL:PARTS-VALID !
   c PART-WRITER JSON-WRITE:ARRAY-END JSON-WRITE:CLOSE
   c JOURNAL-PART-WRITER JSON-WRITE:ARRAY-END JSON-WRITE:CLOSE
   false c MODEL-CALL:PARTS-OPEN ! ;

: PART-COMMA ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 0 > if
      c PART-WRITER JSON-WRITE:COMMA drop
      c JOURNAL-PART-WRITER JSON-WRITE:COMMA drop
   then ;

: PART-ADDED ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 1+ c MODEL-CALL:PARTS-COUNT !
   true c MODEL-CALL:PARTS-VALID ! ;

: TEXT-PART ( ptr u8 n ptr JSON-WRITE:writer -- )
   {: a:ptr u:n w :}
   w JSON-WRITE:OBJECT-START
   s" type" s" text" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" text" a u JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END drop ;

public

: PARTS-RESET ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   0 c MODEL-CALL:HTTP-STATUS !
   c PARTS-BUF c RESET-SCRATCH
   c JOURNAL-PARTS-BUF c RESET-SCRATCH
   0 c MODEL-CALL:PARTS-COUNT !
   c PART-OPEN
   true c MODEL-CALL:PARTS-OPEN !
   true c MODEL-CALL:PARTS-VALID ! ;

: TEXT+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   c MODEL-CALL:PARTS-OPEN @ 0= if E-STATE throw then
   c MODEL-CALL:PARTS-VALID @ 0= if E-STATE throw then
   false c MODEL-CALL:PARTS-VALID !
   c PART-COMMA
   a u c PART-WRITER TEXT-PART
   a u c JOURNAL-PART-WRITER TEXT-PART
   c PART-ADDED ;

private

: IMAGE-MIME? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" image/png" STR= if true exit then
   a u s" image/jpeg" STR= if true exit then
   a u s" image/webp" STR= if true exit then
   a u s" image/gif" STR= ;

: DIGEST-KEEP ( ptr u8 n ptr call -- ptr u8 n ptr call )
   {: data:ptr du:n c :}
   data du du 4 / 3 * c URI-BUF ROOM BASE64:DECODE {: n:n :}
   c HASH-WORK {: w:ptr :}
   w c URI-BUF BUF:SPAN$ drop n w SHA256-CTX-BYTES + SHA256-IN
   w SHA256-CTX-BYTES + w SHA256-CTX-BYTES + 32 + SHA256>HEX
   data du c ;

\ The SHA-256 hex, in HASH-WORK, of the bytes base64 data encodes. Data that
\ is not base64 as BASE64:ENCODE writes it is E-CONFIG.
: IMAGE-DIGEST ( ptr u8 n ptr call -- ptr u8 n )
   {: data:ptr du:n c :}
   du 0= if E-CONFIG throw then
   data du c [: DIGEST-KEEP ;] catch {: code:n :} drop 2drop
   code 0<> if c URI-BUF c RESET-SCRATCH then
   code E-BASE64-FIRST <= code E-BASE64-LAST >= and if E-CONFIG throw then
   code 0<> if code throw then
   c HASH-WORK SHA256-CTX-BYTES + 32 + 64 ;

public

\ The request carries the image inline; the journal input names it by media
\ type and the SHA-256 of its bytes, so a saved step stays small and the same
\ image replays it.
: IMAGE+ ( ptr u8 n ptr u8 n ptr call -- )
   {: mime:ptr mu:n data:ptr du:n c :}
   c REQUIRE-IDLE
   c MODEL-CALL:PARTS-OPEN @ 0= if E-STATE throw then
   c MODEL-CALL:PARTS-VALID @ 0= if E-STATE throw then
   mime mu IMAGE-MIME? 0= if E-CONFIG throw then
   data du c IMAGE-DIGEST {: hex:ptr hex-u:n :}
   c URI-BUF BUF:CLEAR
   s" data:" BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   mime mu BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   s" ;base64," BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   data du BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   false c MODEL-CALL:PARTS-VALID !
   c PART-COMMA
   c PART-WRITER
   JSON-WRITE:OBJECT-START
   s" type" s" image_url" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" image_url" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" url" c URI-BUF BUFFER$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END drop
   c JOURNAL-PART-WRITER
   JSON-WRITE:OBJECT-START
   s" type" s" image" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" media_type" mime mu JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" sha256" hex hex-u JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END drop
   c URI-BUF BUF:CLEAR
   c URI-BUF c RESET-SCRATCH
   c PART-ADDED ;

private

\ A fault of the JSON reader: bytes that are not the JSON read expects.
: JR-FAULT? ( n -- bool )
   {: code:n :}
   code E-JR-FIRST <= code E-JR-LAST >= and ;

: JSON-OBJECT? ( ptr u8 n ptr call -- bool )
   {: a:ptr u:n c :}
   c READER-STORE a u JR:INIT
   JR:NEXT JR:T-OBJ = if
      JR:SKIP-VALUE
      JR:NEXT JR:T-END = {: valid:bool :}
      JR:CLOSE valid exit
   then
   JR:CLOSE false ;

: SCHEMA-STEP ( ptr call -- ptr call )
   {: c :}
   c SCHEMA$ c JSON-OBJECT? 0= if E-CONFIG throw then
   c ;

: CHECK-SCHEMA ( ptr call -- )
   {: c :}
   c [: SCHEMA-STEP ;] catch {: code:n :} drop
   code JR-FAULT? if E-CONFIG throw then
   code 0<> if code throw then ;

: CHECK-CHAT ( ptr call -- )
   {: c :}
   c GEN$ nip 0=
   c SYSTEM$ nip 0= or c MODEL-CALL:PARTS-COUNT @ 0= or if
      E-CONFIG throw
   then
   c CHECK-SCHEMA ;

: BODY-OPEN ( ptr call -- )
   {: c :}
   c BODY-BUF c RESET-SCRATCH
   c WRITER c BODY-BUF JSON-WRITE:OPEN-BUF drop ;

: BODY-DONE ( ptr call -- )
   {: c :}
   c WRITER JSON-WRITE:CLOSE ;

: CHAT-BODY ( ptr call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if
      c CHECK-CHAT
      c PART-FINISH
   then
   c BODY-OPEN
   c WRITER
   JSON-WRITE:OBJECT-START
   s" model" c GEN$ JSON-WRITE:FIELD-S
   c EFFORT$ nip 0<> if
      JSON-WRITE:COMMA s" reasoning_effort" c EFFORT$ JSON-WRITE:FIELD-S
   then
   c CACHE-KEY$ nip 0<> if
      JSON-WRITE:COMMA s" prompt_cache_key" c CACHE-KEY$ JSON-WRITE:FIELD-S
   then
   c TIER$ nip 0<> if
      JSON-WRITE:COMMA s" service_tier" c TIER$ JSON-WRITE:FIELD-S
   then
   JSON-WRITE:COMMA
   s" messages" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START
   JSON-WRITE:OBJECT-START
   s" role" s" system" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" content" c SYSTEM$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:COMMA
   JSON-WRITE:OBJECT-START
   s" role" s" user" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" content" c PARTS$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:OBJECT-END
   JSON-WRITE:ARRAY-END
   JSON-WRITE:COMMA
   s" response_format" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" type" s" json_schema" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" json_schema" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" name" s" answer" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" strict" true JSON-WRITE:FIELD-BOOL
   JSON-WRITE:COMMA
   s" schema" c SCHEMA$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END drop
   c BODY-DONE ;

: INPUT$ ( n ptr call -- ptr u8 n )
   {: idx:n c :}
   c INPUT-BUF BUF:SPAN
   c idx INPUT-OFF @ c idx INPUT-LEN @ SPAN:SUB SPAN:$ ;

: CHECK-EMBED ( ptr call -- )
   {: c :}
   c EMBED-MODEL$ nip 0= c MODEL-CALL:DIMS @ 0= or
   c MODEL-CALL:INPUT-COUNT @ 0= or if E-CONFIG throw then ;

: EMBED-BODY ( ptr call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if c CHECK-EMBED then
   c BODY-OPEN
   c WRITER
   JSON-WRITE:OBJECT-START
   s" model" c EMBED-MODEL$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" dimensions" c MODEL-CALL:DIMS @ JSON-WRITE:FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON-WRITE:COMMA
      s" truncate" false JSON-WRITE:FIELD-BOOL
   then
   JSON-WRITE:COMMA
   s" input" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START
   c MODEL-CALL:INPUT-COUNT @ 0 ?do
      i 0 > if JSON-WRITE:COMMA then
      i c INPUT$ JSON-WRITE:STRING
   loop
   JSON-WRITE:ARRAY-END
   JSON-WRITE:OBJECT-END drop
   c BODY-DONE ;

public

: INPUT-RESET ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   0 c MODEL-CALL:HTTP-STATUS !
   c INPUT-BUF BUF:CLEAR
   0 c MODEL-CALL:INPUT-U !
   0 c MODEL-CALL:INPUT-COUNT !
   false c MODEL-CALL:EMBEDDED ! ;

: INPUT+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-IDLE
   u 0= if E-CONFIG throw then
   c MODEL-CALL:INPUT-COUNT @ INPUT-MAX >= if E-CAPACITY throw then
   c MODEL-CALL:INPUT-U @ c c MODEL-CALL:INPUT-COUNT @ INPUT-OFF !
   u c c MODEL-CALL:INPUT-COUNT @ INPUT-LEN !
   a u BUF:N>BLEN c INPUT-BUF BUF:APPEND-SPAN
   c MODEL-CALL:INPUT-U @ u + c MODEL-CALL:INPUT-U !
   c MODEL-CALL:INPUT-COUNT @ 1+ c MODEL-CALL:INPUT-COUNT !
   false c MODEL-CALL:EMBEDDED ! ;

private

: JOURNAL-GEN-INPUT ( ptr call -- )
   {: c :}
   c JOURNAL-INPUT-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-INPUT-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" kind" s" generation" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" configuration" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" backend" s" openai-chat-completions" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" model" c GEN$ JSON-WRITE:FIELD-S
   c EFFORT$ nip 0<> if JSON-WRITE:COMMA s" effort" c EFFORT$ JSON-WRITE:FIELD-S then
   JSON-WRITE:COMMA s" endpoint" c CHAT-URL$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:COMMA s" instructions" c SYSTEM$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" schema" c SCHEMA$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:COMMA s" parts" c JOURNAL-PARTS$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

: JOURNAL-EMBED-INPUT ( ptr call -- )
   {: c :}
   c JOURNAL-INPUT-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-INPUT-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" kind" s" embedding" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" configuration" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" backend" c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      s" ollama" else s" openai-embeddings" then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" model" c EMBED-MODEL$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" dimensions" c MODEL-CALL:DIMS @ JSON-WRITE:FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON-WRITE:COMMA s" identity" c IDENTITY$ JSON-WRITE:FIELD-S
      JSON-WRITE:COMMA s" truncate" false JSON-WRITE:FIELD-BOOL
      JSON-WRITE:COMMA s" endpoint" c ENDPOINT$ JSON-WRITE:FIELD-S
   else
      JSON-WRITE:COMMA s" endpoint" c EMBED-URL$ JSON-WRITE:FIELD-S
   then
   JSON-WRITE:OBJECT-END
   JSON-WRITE:COMMA s" input" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START
   c MODEL-CALL:INPUT-COUNT @ 0 ?do
      i 0 > if JSON-WRITE:COMMA then
      i c INPUT$ JSON-WRITE:STRING
   loop
   JSON-WRITE:ARRAY-END
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

: CHECK-STATUS ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop E-TRANSPORT throw ENDOF
   ;MATCH ;

: OPEN-HANDLE ( -- CURL:handle )
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop E-TRANSPORT throw ENDOF
   ;MATCH ;

: HEADER-WIPE ( ptr call -- )
   {: c :}
   0 c KEY-LINE-BUF HELD SPAN:FILL ;

\ Wipes the header after a step that ended with fault, which is thrown first.
\ Only a header a failed resize left dead refuses the wipe, and it holds no
\ storage, so its refusal never hides the step's fault.
: WIPE-AFTER ( n ptr call -- )
   {: fault:n c :}
   c [: dup HEADER-WIPE ;] catch {: wiped:n :} drop
   fault 0<> if fault throw then
   wiped 0<> if wiped throw then ;

: KEY-PREFIX$ ( -- ptr u8 n ) s" Authorization: Bearer " ;

\ Room in the header for its prefix and then a key of n bytes, with the
\ prefix written. The whole header is wiped before it is resized, so released
\ storage never holds a key byte.
: KEY-ROOM ( n ptr call -- )
   {: ku:n c :}
   KEY-PREFIX$ {: prefix:ptr pu:n :}
   c HEADER-WIPE
   c KEY-LINE-BUF pu ku + c RESET-SIZED
   pu ku + c KEY-LINE-BUF ROOM {: line:SPAN:span<u8> :}
   prefix pu line SPAN:COPY ;

: KEY-VALID? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0= if false exit then
   u 0 ?do
      a i + c@ {: b:n :}
      b 33 < b 126 > or if false unloop exit then
   loop
   true ;


\ Reads the store's item into the header after its pu-byte prefix and returns
\ the line's length; a missing, empty or non-printable item is E-KEY.
: KEY-READ ( ptr call n -- ptr call n )
   {: c pu:n :}
   c KEY-SPEC$ {: spec:ptr spec-u:n :}
   c MODEL-CALL:KEY-SPLIT @ {: su:n :}
   spec su spec su + spec-u su - c KEY-LINE-BUF HELD pu SPAN:SKIP SECRET:READ
   {: ku:n found:bool :}
   found 0= if E-KEY throw then
   c KEY-LINE-BUF HELD pu SPAN:SKIP ku SPAN:TAKE SPAN:$ KEY-VALID? 0= if E-KEY throw then
   c pu ku + ;


\ An item larger than the header after its prefix grows the header and is
\ read again: the store copies no byte of an item it refuses. The store's own
\ failures pass through.
: KEY-FETCH ( ptr call n -- n )
   {: c pu:n :}
   0 c KEY-ROOM
   begin
      c pu [: KEY-READ ;] catch {: fault:n :}
      fault 0= if nip exit then
      2drop
      fault SECRET:E-CAPACITY <> if fault throw then
      c KEY-LINE-BUF BUF:CAP@ BUF:BLEN>N c KEY-ROOM
   again ;


: KEY-LINE ( ptr call -- ptr u8 n )
   {: c :}
   c KEY-SPEC$ nip 0= if E-CONFIG throw then
   c KEY-PREFIX$ nip KEY-FETCH {: line-u:n :}
   c KEY-LINE-BUF HELD line-u SPAN:TAKE SPAN:$ ;

: KEY-HEADER-KEEP ( CURL:handle ptr call -- CURL:handle ptr call )
   2dup KEY-LINE CURL:HEADER+ CHECK-STATUS ;

: SET-HEADERS ( CURL:handle ptr call -- )
   {: h:CURL:handle c :}
   h s" Content-Type: application/json" CURL:HEADER+ CHECK-STATUS
   c MODEL-CALL:ENDPOINT @ c MODEL-CALL:EMBEDDING-KIND @ OLLAMA <> or if
      h c [: KEY-HEADER-KEEP ;] catch {: fault:n :} 2drop
      fault c WIPE-AFTER
   then ;

: DEADLINE-OPEN ( ptr call -- )
   {: c :}
   TIME:MONO-NS c MODEL-CALL:TIMEOUT-MS @ NS-PER-MS * +
   c MODEL-CALL:DEADLINE-NS ! ;

: REMAINING-MS ( ptr call -- n )
   {: c :}
   c MODEL-CALL:DEADLINE-NS @ TIME:MONO-NS - {: left:n :}
   left 0 <= if E-DEADLINE throw then
   left NS-PER-MS 1- + NS-PER-MS / ;

: CHECK-CANCEL-LOCKED ( ptr call -- )
   MODEL-CALL:CANCELLED @ if E-CANCELLED throw then ;

: START-LOCKED ( ptr call -- ptr call )
   {: c :}
   c CHECK-CANCEL-LOCKED
   c MODEL-CALL:TRANSPORT @ MATCH option
      none OF ENDOF
      some OF drop E-STATE throw ENDOF
   ;MATCH
   c MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF {: h:CURL:handle :}
         h CURL:STREAM-START CHECK-STATUS
         h OPTION:SOME c MODEL-CALL:TRANSPORT !
         c DISPATCHED
      ENDOF
   ;MATCH
   c ;

: START-TRANSFER ( ptr call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT
   c [: START-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL
   code 0<> if code throw then ;

\ Clears the call's transfer, answering whether the call was cancelled.
: END-TRANSFER ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT
   OPTION:NONE c MODEL-CALL:TRANSPORT !
   c MODEL-CALL:CANCELLED @
   c MODEL-CALL:GATE @ TASK:SIGNAL ;

\ Statuses by which the provider reports itself temporarily unavailable:
\ request timeout, rate limit and every server error.
: UNAVAILABLE? ( n -- bool )
   {: status:n :}
   status 408 = status 429 = or status 500 >= status 600 < and or ;

: WIRE-READER ( ptr call -- JR:reader )
   {: c :}
   c READER-STORE c RESPONSE$ JR:INIT ;

\ The reply's current string, decoded into the content buffer, which a failed
\ call leaves unused.
: REPLY-STR ( JR:reader ptr call -- JR:reader ptr u8 n )
   {: c :}
   c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JR:STR {: u:n :}
   c CONTENT-BUF HELD u SPAN:TAKE SPAN:$ ;

\ Ollama's error text for an input longer than the model's context.
: OLLAMA-SIZE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" exceeds the context length" CONTAINS?
   a u s" exceeds maximum context length" CONTAINS? or ;

\ Whether a member of OpenAI's error object, the reader at its value, names a
\ refusal for size: by its code, or by its message where the code is null, as
\ in an embeddings refusal.
: SIZE-MEMBER? ( JR:reader ptr u8 n ptr call -- JR:reader bool )
   {: key:ptr key-u:n c :}
   JR:TOKEN JR:T-STR <> if JR:SKIP-VALUE false exit then
   key key-u s" code" STR= if
      s" context_length_exceeded" JR:STR-EQ? if true exit then
      s" string_above_max_length" JR:STR-EQ? exit
   then
   key key-u s" message" STR= if
      c REPLY-STR s" This model's maximum context length" STARTS-WITH? exit
   then
   false ;

: OPENAI-SIZE? ( JR:reader ptr call -- JR:reader bool )
   {: c :}
   begin JR:NEXT dup JR:T-OBJ-END <> while
      drop
      JR:SPAN$ {: key:ptr key-u:n :}
      JR:NEXT drop
      key key-u c SIZE-MEMBER? if true exit then
   repeat
   drop false ;

\ The reader at the reply's error member.
: SIZE-ERROR? ( JR:reader ptr call -- JR:reader bool )
   {: c :}
   JR:TOKEN JR:T-STR = if c REPLY-STR OLLAMA-SIZE? exit then
   JR:TOKEN JR:T-OBJ = if c OPENAI-SIZE? exit then
   false ;

\ The provider's message, kept from the reply's error member, the reader at
\ it: the member itself when a string, as Ollama sends it, else its message,
\ as OpenAI and Azure send it.
: ERROR-TEXT ( JR:reader ptr call -- JR:reader )
   {: c :}
   JR:TOKEN JR:T-OBJ = if
      s" message" JR:FIND-KEY 0= if exit then
   then
   JR:TOKEN JR:T-STR <> if exit then
   c REPLY-STR BUF:N>BLEN c ERROR-BUF BUF:REPLACE ;

: ERROR-BODY ( ptr call -- ptr call )
   {: c :}
   c WIRE-READER
   JR:NEXT JR:T-OBJ = if
      s" error" JR:FIND-KEY if c ERROR-TEXT then
   then
   JR:CLOSE c ;

\ Keeps a failed reply's provider message. A body that is not such JSON has
\ none.
: KEEP-ERROR ( ptr call -- )
   {: c :}
   c [: ERROR-BODY ;] catch {: code:n :} drop
   code JR-FAULT? if exit then
   code 0<> if code throw then ;

: SIZE-BODY? ( ptr call -- bool )
   {: c :}
   c WIRE-READER
   JR:NEXT JR:T-OBJ <> if JR:CLOSE false exit then
   s" error" JR:FIND-KEY 0= if JR:CLOSE false exit then
   c SIZE-ERROR? {: size:bool :}
   JR:CLOSE size ;

\ Whether a failed reply refuses the request for its size: HTTP 413, or a 400
\ whose body names the refusal. A body that is not such JSON is not one.
: SIZE-REFUSAL? ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:HTTP-STATUS @ 413 = if true exit then
   c MODEL-CALL:HTTP-STATUS @ 400 <> if false exit then
   c false [: drop dup SIZE-BODY? ;] catch {: code:n :}
   code 0= if nip exit then
   2drop
   code JR-FAULT? if false exit then
   code throw ;


\ Room in the response buffer for at least one byte past those received.
: GROW-REPLY ( ptr call -- ptr call )
   {: c :}
   c RESPONSE-BUF {: buf:ptr :}
   buf buf BUF:LEN@ BUF:BLEN>N 1+ BUF:N>BLEN BUF:ENSURE
   c ;


\ Gives up a transfer whose reply cannot be kept: cancels it and collects its
\ end, so its handle has no transfer left for cleanup to refuse.
: DROP-TRANSFER ( CURL:handle ptr n -- )
   {: h:CURL:handle buf:ptr :}
   h CURL:CANCEL
   begin
      h buf HELD SPAN:$ >LEN CURL:NEXT
      MATCH CURL:chunk-result
         chunk OF 2drop false ENDOF
         response OF drop true ENDOF
         failed OF drop true ENDOF
      ;MATCH
   until ;


\ Receives the reply into the response buffer, which grows with it: NEXT
\ writes each chunk into the buffer's room past the bytes received, and
\ appending the chunk from where it lies adds it to them. Answers the HTTP
\ status and libcurl code the transfer ended with, a failed one without a
\ status, and the fault that kept the buffer from growing, after which the
\ transfer is dropped.
: RECEIVE ( CURL:handle ptr call -- n n n )
   {: h:CURL:handle c :}
   c RESPONSE-BUF {: buf:ptr :}
   buf BUF:CLEAR
   begin
      c [: GROW-REPLY ;] catch {: fault:n :} drop
      fault 0<> if h buf DROP-TRANSFER 0 0 fault exit then
      buf HELD buf BUF:LEN@ BUF:BLEN>N SPAN:SKIP SPAN:$ {: room:ptr room-u:n :}
      h room room-u >LEN CURL:NEXT
      MATCH CURL:chunk-result
         chunk OF nip LEN>N {: got:n :}
            room got BUF:N>BLEN buf BUF:APPEND-SPAN
         ENDOF
         response OF CURL:HTTP-STATUS>N 0 0 exit ENDOF
         failed OF CURL:CODE>N 0 swap 0 exit ENDOF
      ;MATCH
   again ;


: AWAIT-TRANSFER ( ptr call -- )
   {: c :}
   c MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF c RECEIVE ENDOF
   ;MATCH {: status:n rc:n fault:n :}
   c END-TRANSFER {: cancelled:bool :}
   fault 0<> if fault throw then
   cancelled if E-CANCELLED throw then
   rc CURL-TIMED-OUT = if E-DEADLINE throw then
   rc 0<> if E-TRANSPORT throw then
   status c MODEL-CALL:HTTP-STATUS !
   c RESPONSE-BUF BUF:LEN@ BUF:BLEN>N c MODEL-CALL:RESPONSE-U !
   status 200 <> if
      c KEEP-ERROR
      status UNAVAILABLE? if E-UNAVAILABLE throw then
      c SIZE-REFUSAL? if E-CAPACITY throw then
      status 401 = status 403 = or if E-KEY throw then
      status 404 = if E-CONFIG throw then
      E-HTTP throw
   then ;

: TRANSFER ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF {: h:CURL:handle :}
         h c MODEL-CALL:ENDPOINT @ if c CHAT-URL$ else
            c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
               c ENDPOINT$
            else c EMBED-URL$ then
         then
         CURL:URL! CHECK-STATUS
         h s" POST" CURL:METHOD! CHECK-STATUS
         h c BODY$ CURL:BODY! CHECK-STATUS
         h c SET-HEADERS
         h c REMAINING-MS >MS CURL:TIMEOUT! CHECK-STATUS
         c START-TRANSFER
         c AWAIT-TRANSFER
      ENDOF
   ;MATCH
   c ;

: SEND ( ptr call -- )
   {: c :}
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:HTTP-STATUS !
   OPEN-HANDLE OPTION:SOME c MODEL-CALL:HANDLE !
   c [: TRANSFER ;] catch {: code:n :} drop
   c MODEL-CALL:HANDLE @ MATCH option
      none OF ENDOF
      some OF CURL:CLEANUP ENDOF
   ;MATCH
   OPTION:NONE c MODEL-CALL:HANDLE !
   code c WIPE-AFTER
   c REMAINING-MS drop ;

: EXPECT ( JR:reader n -- JR:reader )
   {: want:n :}
   JR:NEXT want <> if JR:CLOSE E-RESPONSE throw then ;

: DESCEND ( JR:reader ptr u8 n -- JR:reader )
   {: key:ptr key-u:n :}
   key key-u JR:FIND-KEY 0= if JR:CLOSE E-RESPONSE throw then ;

: CHOICE ( ptr call -- JR:reader )
   {: c :}
   c WIRE-READER
   JR:T-OBJ EXPECT
   s" choices" DESCEND
   JR:TOKEN JR:T-ARR <> if JR:CLOSE E-RESPONSE throw then
   JR:T-OBJ EXPECT ;

\ A reply that did not finish with stop is incomplete and keeps its reason in
\ the content buffer: an incomplete reply has no content, and its decoded reason
\ is no longer than the response that holds it.
: READ-FINISH ( ptr call -- )
   {: c :}
   c CHOICE
   s" finish_reason" DESCEND
   JR:TOKEN JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
   s" stop" JR:STR-EQ? if JR:CLOSE exit then
   c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JR:STR c MODEL-CALL:FINISH-U !
   JR:CLOSE E-INCOMPLETE throw ;

: MESSAGE-MEMBER ( JR:reader ptr u8 n ptr call -- JR:reader )
   {: key:ptr key-u:n c :}
   key key-u s" refusal" STR= if
      JR:TOKEN JR:T-NULL <> if JR:CLOSE E-REFUSED throw then
      JR:SKIP-VALUE exit
   then
   key key-u s" content" STR= if
      c MODEL-CALL:CONTENT-SEEN @ if JR:CLOSE E-RESPONSE throw then
      JR:TOKEN JR:T-NULL = if JR:SKIP-VALUE exit then
      JR:TOKEN JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
      c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JR:STR {: got:n :}
      got c MODEL-CALL:CONTENT-U !
      true c MODEL-CALL:CONTENT-SEEN !
      exit
   then
   JR:SKIP-VALUE ;

: READ-MESSAGE ( ptr call -- )
   {: c :}
   false c MODEL-CALL:CONTENT-SEEN !
   0 c MODEL-CALL:CONTENT-U !
   c CHOICE
   s" message" DESCEND
   JR:TOKEN JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   begin JR:NEXT dup JR:T-OBJ-END <> while
      JR:T-KEY <> if JR:CLOSE E-RESPONSE throw then
      JR:SPAN$ {: key:ptr key-u:n :}
      JR:NEXT drop
      key key-u c MESSAGE-MEMBER
   repeat
   drop JR:CLOSE
   c MODEL-CALL:CONTENT-SEEN @ 0= if E-RESPONSE throw then ;

: ACCEPT-CHAT ( ptr call -- )
   {: c :}
   c RESPONSE$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c READ-FINISH
   c READ-MESSAGE
   c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
   c JSON-OBJECT? 0= if E-RESPONSE throw then ;

: VECTOR-BYTE+ ( n ptr call -- ) VECTORS-BUF BUF:APPEND-BYTE ;

: VECTOR-BYTES+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   a u BUF:N>BLEN c VECTORS-BUF BUF:APPEND-SPAN ;

: VECTORS-U ( ptr call -- n ) VECTORS-BUF BUF:LEN@ BUF:BLEN>N ;

\ The vector just read is the one at idx.
: VECTOR-KEEP ( n ptr call -- )
   {: idx:n c :}
   c MODEL-CALL:VECTOR-AT @ {: at:n :}
   at c idx VECTOR-OFF !
   c VECTORS-U at - c idx VECTOR-LEN ! ;

\ No vector read yet; the vectors keep the room the last reply's took.
: VECTORS-RESET ( ptr call -- )
   {: c :}
   INPUT-MAX 0 ?do 0 c i VECTOR-LEN ! loop
   c VECTORS-BUF c RESET-SCRATCH ;

: READ-VECTOR ( JR:reader ptr call -- JR:reader )
   {: c :}
   JR:TOKEN JR:T-ARR <> if JR:CLOSE E-RESPONSE throw then
   c VECTORS-U c MODEL-CALL:VECTOR-AT !
   0 c MODEL-CALL:COMPONENT-COUNT !
   91 c VECTOR-BYTE+
   begin JR:NEXT dup JR:T-ARR-END <> while
      dup JR:T-INT = swap JR:T-FLOAT = or 0= if
         JR:CLOSE E-RESPONSE throw
      then
      c MODEL-CALL:COMPONENT-COUNT @ {: idx:n :}
      idx c MODEL-CALL:DIMS @ >= if JR:CLOSE E-RESPONSE throw then
      JR:SPAN$ {: a:ptr u:n :}
      a u F64-TEXT:PARSE MATCH result
         ok OF drop ENDOF
         err OF drop JR:CLOSE E-RESPONSE throw ENDOF
      ;MATCH
      idx 0 > if 44 c VECTOR-BYTE+ then
      a u c VECTOR-BYTES+
      idx 1+ c MODEL-CALL:COMPONENT-COUNT !
   repeat
   drop
   c MODEL-CALL:COMPONENT-COUNT @ c MODEL-CALL:DIMS @ <> if
      JR:CLOSE E-RESPONSE throw
   then
   93 c VECTOR-BYTE+
   true c MODEL-CALL:VECTOR-SEEN ! ;

: EMBED-MEMBER ( JR:reader ptr u8 n ptr call -- JR:reader )
   {: key:ptr key-u:n c :}
   key key-u s" index" STR= if
      c MODEL-CALL:PARSED-INDEX @ -1 <> if JR:CLOSE E-RESPONSE throw then
      JR:TOKEN JR:T-INT <> if JR:CLOSE E-RESPONSE throw then
      JR:INT {: idx:n :}
      idx 0 < idx c MODEL-CALL:INPUT-COUNT @ >= or if
         JR:CLOSE E-RESPONSE throw
      then
      idx c MODEL-CALL:PARSED-INDEX ! exit
   then
   key key-u s" embedding" STR= if
      c MODEL-CALL:VECTOR-SEEN @ if JR:CLOSE E-RESPONSE throw then
      c READ-VECTOR exit
   then
   JR:SKIP-VALUE ;

: READ-EMBED-OBJECT ( JR:reader ptr call -- JR:reader )
   {: c :}
   -1 c MODEL-CALL:PARSED-INDEX !
   false c MODEL-CALL:VECTOR-SEEN !
   begin JR:NEXT dup JR:T-OBJ-END <> while
      JR:T-KEY <> if JR:CLOSE E-RESPONSE throw then
      JR:SPAN$ {: key:ptr key-u:n :}
      JR:NEXT drop
      key key-u c EMBED-MEMBER
   repeat
   drop
   c MODEL-CALL:PARSED-INDEX @ {: idx:n :}
   idx -1 = c MODEL-CALL:VECTOR-SEEN @ 0= or if
      JR:CLOSE E-RESPONSE throw
   then
   c idx VECTOR-LEN @ 0<> if JR:CLOSE E-RESPONSE throw then
   idx c VECTOR-KEEP ;

: ACCEPT-EMBED ( ptr call -- )
   {: c :}
   c VECTORS-RESET
   0 c MODEL-CALL:DATA-COUNT !
   c RESPONSE$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c WIRE-READER
   JR:T-OBJ EXPECT
   s" data" DESCEND
   JR:TOKEN JR:T-ARR <> if JR:CLOSE E-RESPONSE throw then
   begin JR:NEXT dup JR:T-ARR-END <> while
      JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
      c READ-EMBED-OBJECT
      c MODEL-CALL:DATA-COUNT @ 1+ c MODEL-CALL:DATA-COUNT !
   repeat
   drop JR:CLOSE
   c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ <> if
      E-RESPONSE throw
   then ;

: ACCEPT-OLLAMA ( ptr call -- )
   {: c :}
   c VECTORS-RESET
   0 c MODEL-CALL:DATA-COUNT !
   c RESPONSE$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c WIRE-READER JR:T-OBJ EXPECT
   s" embeddings" DESCEND
   JR:TOKEN JR:T-ARR <> if JR:CLOSE E-RESPONSE throw then
   begin JR:NEXT dup JR:T-ARR-END <> while
      c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ >= if
         drop JR:CLOSE E-RESPONSE throw
      then
      drop
      c READ-VECTOR
      c MODEL-CALL:DATA-COUNT @ c VECTOR-KEEP
      c MODEL-CALL:DATA-COUNT @ 1+ c MODEL-CALL:DATA-COUNT !
   repeat
   drop JR:CLOSE
   c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ <> if
      E-RESPONSE throw
   then ;

: RESPONSE-ERROR ( n -- )
   {: code:n :}
   code JR-FAULT? if E-RESPONSE throw then
   code 0<> if code throw then ;

: PARSE-CHAT ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-CHAT ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

\ A reader at the usage member of the JSON object in the bytes, and whether
\ that member is an object.
: USAGE ( ptr u8 n ptr call -- JR:reader bool )
   {: a:ptr u:n c :}
   c READER-STORE a u JR:INIT
   JR:T-OBJ EXPECT
   s" usage" JR:FIND-KEY 0= if false exit then
   JR:TOKEN JR:T-OBJ = ;

\ The current usage count: an integer, or none for null.
: USAGE-COUNT ( JR:reader -- JR:reader option<n> )
   JR:TOKEN dup JR:T-INT = if drop JR:INT OPTION:SOME exit then
   JR:T-NULL <> if JR:CLOSE E-RESPONSE throw then
   OPTION:NONE ;

\ A count, 0 when absent.
: OR-ZERO ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The usage count at the key: none when the key is absent or null.
: COUNT-AT ( JR:reader ptr u8 n -- JR:reader option<n> )
   JR:FIND-KEY 0= if OPTION:NONE exit then
   USAGE-COUNT ;

\ The count at the key of the bytes' usage object, which is present. Each
\ count takes its own pass because providers order the members freely.
: USAGE-AT ( ptr u8 n ptr call ptr u8 n -- option<n> )
   {: a:ptr u:n c key:ptr key-u:n :}
   a u c USAGE drop
   key key-u COUNT-AT {: got :}
   JR:CLOSE got ;

\ The count at the key of a usage member object; none when that member is
\ absent or not an object.
: DETAIL-AT ( ptr u8 n ptr call ptr u8 n ptr u8 n -- option<n> )
   {: a:ptr u:n c detail:ptr detail-u:n key:ptr key-u:n :}
   a u c USAGE drop
   detail detail-u JR:FIND-KEY 0= if JR:CLOSE OPTION:NONE exit then
   JR:TOKEN JR:T-OBJ <> if JR:CLOSE OPTION:NONE exit then
   key key-u COUNT-AT {: got :}
   JR:CLOSE got ;

\ Whether the bytes carry a usage object; marks the call's usage seen.
: USAGE? ( ptr u8 n ptr call -- bool )
   {: a:ptr u:n c :}
   a u c USAGE {: present:bool :}
   JR:CLOSE
   present if true c TOKENS TOKENS-SEEN ! then
   present ;

\ The HTTP reply's usage counts.
: ACCEPT-USAGE ( ptr call -- )
   {: c :}
   c RESPONSE$ {: a:ptr u:n :}
   a u c USAGE? 0= if exit then
   a u c s" prompt_tokens" USAGE-AT c TOKENS TOKENS-PROMPT !
   a u c s" completion_tokens" USAGE-AT c TOKENS TOKENS-COMPLETION !
   a u c s" prompt_tokens_details" s" cached_tokens" DETAIL-AT
   c TOKENS TOKENS-CACHED !
   a u c s" completion_tokens_details" s" reasoning_tokens" DETAIL-AT
   c TOKENS TOKENS-REASONING ! ;

: PARSE-USAGE ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-USAGE ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

\ The tier the reply says served it, when it names one: a nonempty string, or
\ null for none.
: ACCEPT-TIER ( ptr call -- )
   {: c :}
   c READER-STORE c RESPONSE$ JR:INIT
   JR:T-OBJ EXPECT
   s" service_tier" JR:FIND-KEY 0= if JR:CLOSE exit then
   JR:TOKEN dup JR:T-NULL = if drop JR:CLOSE exit then
   JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
   c REPLY-STR {: a:ptr u:n :}
   JR:CLOSE
   u 0= if E-RESPONSE throw then
   a u c SERVED-BUF REPLACE-SOME ;

: PARSE-TIER ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-TIER ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

: PARSE-EMBED ( ptr call -- )
   {: c :}
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      c [: dup ACCEPT-OLLAMA ;] catch
   else
      c [: dup ACCEPT-EMBED ;] catch
   then {: code:n :} drop
   code RESPONSE-ERROR ;

: BEGIN-LOCKED ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:STOPPED @ if E-CANCELLED throw then
   c MODEL-CALL:BUSY @ if E-STATE throw then
   \ The reply buffers keep the room the last call's reply took. They are
   \ resized before the call is busy, so a failed resize leaves it idle.
   c CONTENT-BUF c MODEL-CALL:CONTENT-U @ c MODEL-CALL:FINISH-U @ max c RESET-SIZED
   c RESPONSE-BUF c MODEL-CALL:RESPONSE-U @ c RESET-SIZED
   c ERROR-BUF c RESET-SCRATCH
   c SERVED-BUF BUF:CLEAR
   true c MODEL-CALL:BUSY !
   false c MODEL-CALL:CANCELLED !
   0 c MODEL-CALL:CONTENT-U !
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:FINISH-U !
   false c MODEL-CALL:EMBEDDED !
   0 c MODEL-CALL:REPLAY-KIND !
   c RESET-USAGE
   c ;

: BEGIN-CALL ( ptr call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then
   c MODEL-CALL:GATE @ TASK:WAIT
   c [: BEGIN-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL
   code 0<> if code throw then
   c DEADLINE-OPEN ;

: END-CALL ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT
   c MODEL-CALL:CANCELLED @ {: cancelled:bool :}
   false c MODEL-CALL:BUSY !
   0 c MODEL-CALL:DEADLINE-NS !
   c MODEL-CALL:GATE @ TASK:SIGNAL
   cancelled ;

: RUN-CHAT ( ptr call -- ptr call )
   {: c :}
   c CHAT-BODY
   true c MODEL-CALL:ENDPOINT !
   c SEND
   c PARSE-USAGE
   c PARSE-TIER
   c PARSE-CHAT
   c TOKENS TOKENS-PROMPT @ OR-ZERO c MODEL-CALL:PROMPT-TOKENS !
   c TOKENS TOKENS-CACHED @ OR-ZERO c MODEL-CALL:CACHED-TOKENS !
   c REMAINING-MS drop
   c ;

: RUN-EMBED ( ptr call -- ptr call )
   {: c :}
   c EMBED-BODY
   false c MODEL-CALL:ENDPOINT !
   c SEND
   c PARSE-USAGE
   c PARSE-EMBED
   c REMAINING-MS drop
   c ;

: JOURNAL-GET? ( ptr call -- bool )
   {: c :}
   c JOURNAL-OUT-BUF c RESET-SCRATCH
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT$
   c JOURNAL-OUT-BUF c MODEL-CALL:JOURNAL-GET @ execute ;

: JOURNAL-FIELD ( ptr u8 n ptr call -- ptr u8 n )
   {: key:ptr key-u:n c :}
   c READER-STORE c JOURNAL-OUT$ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   key key-u JR:FIND-KEY 0= if JR:CLOSE E-RESPONSE throw then
   JR:TOKEN {: kind:n :}
   kind JR:T-OBJ = if
      JR:VALUE-SPAN$ {: a:ptr u:n :} JR:CLOSE a u exit
   then
   kind JR:T-STR = if
      JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a 1- u 2 + exit
   then
   JR:CLOSE E-RESPONSE throw ;

: JOURNAL-DECODE ( ptr u8 n SPAN:span<u8> ptr call -- n )
   {: raw:ptr raw-u:n target:SPAN:span<u8> c :}
   c READER-STORE raw raw-u JR:INIT
   JR:NEXT JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
   target SPAN:$ JR:STR {: got:n :}
   JR:CLOSE got ;

: JOURNAL-STRING-BUF ( ptr u8 n ptr n ptr call -- )
   {: key:ptr key-u:n buf:ptr c :}
   key key-u c JOURNAL-FIELD {: raw:ptr raw-u:n :}
   buf raw-u c RESET-SIZED
   buf raw-u BUF:N>BLEN BUF:ENSURE
   raw raw-u buf BUF:SPAN$ drop raw-u SPAN:MAKE c JOURNAL-DECODE
   {: got:n :}
   buf BUF:SPAN$ drop got BUF:N>BLEN buf BUF:REPLACE ;

: JOURNAL-RESPONSE ( ptr call -- )
   {: c :}
   s" response" c JOURNAL-FIELD {: raw:ptr raw-u:n :}
   raw raw-u raw-u c RESPONSE-BUF ROOM c JOURNAL-DECODE
   c MODEL-CALL:RESPONSE-U ! ;

: JOURNAL-ORIGIN ( ptr call -- )
   {: c :}
   s" origin" c JOURNAL-FIELD {: raw:ptr raw-u:n :}
   c READER-STORE raw raw-u JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   s" kind" JR:FIND-KEY 0= if JR:CLOSE E-RESPONSE throw then
   JR:TOKEN JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
   JR:SPAN$ nip 0= if JR:CLOSE E-RESPONSE throw then
   JR:CLOSE ;

\ The recorded call id a journal record carries, 0 when it has none.
: JOURNAL-CALL-ID ( ptr call -- n )
   {: c :}
   c READER-STORE c JOURNAL-OUT$ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   s" call_id" JR:FIND-KEY 0= if JR:CLOSE 0 exit then
   JR:TOKEN JR:T-INT <> if JR:CLOSE E-RESPONSE throw then
   JR:INT {: id:n :} JR:CLOSE id ;

\ The fault a journaled refusal records, 0 for a reply.
: JOURNAL-REFUSAL-FAULT ( ptr call -- n )
   {: c :}
   c READER-STORE c JOURNAL-OUT$ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   s" refusal" JR:FIND-KEY 0= if JR:CLOSE 0 exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE E-RESPONSE throw then
   s" capacity" JR:STR-EQ? if JR:CLOSE E-CAPACITY exit then
   s" incomplete" JR:STR-EQ? if JR:CLOSE E-INCOMPLETE exit then
   JR:CLOSE E-RESPONSE throw ;

: JOURNAL-RESTORE ( n ptr call -- )
   {: kind:n c :}
   0 c MODEL-CALL:HTTP-STATUS !
   c JOURNAL-OUT$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c JOURNAL-REFUSAL-FAULT {: refusal:n :}
   refusal 0<> if refusal throw then
   c JOURNAL-ORIGIN
   s" configuration" c JOURNAL-FIELD {: config:ptr config-u:n :}
   config config-u c JSON-OBJECT? 0= if E-RESPONSE throw then
   kind 1 = if
      config config-u BUF:N>BLEN c GEN-CONFIG-BUF BUF:REPLACE
   else
      config config-u BUF:N>BLEN c EMBED-CONFIG-BUF BUF:REPLACE
   then
   c JOURNAL-RESPONSE
   s" answer" c REPLAY-CONTENT-BUF c JOURNAL-STRING-BUF
   kind 1 = if
      c PARSE-CHAT
      c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
      c REPLAY-CONTENT$ STR= 0= if E-RESPONSE throw then
   else
      c REPLAY-CONTENT$ nip 0<> if E-RESPONSE throw then
      c PARSE-EMBED
      true c MODEL-CALL:EMBEDDED !
   then
   c JOURNAL-CALL-ID c LEDGER LEDGER-CALL-ID !
   kind c MODEL-CALL:REPLAY-KIND ! ;

\ A usage count, null when the reply omitted it.
: COUNT-FIELD ( ptr JSON-WRITE:writer ptr u8 n option<n> -- ptr JSON-WRITE:writer )
   MATCH option
      none OF JSON-WRITE:FIELD-NULL ENDOF
      some OF JSON-WRITE:FIELD-INT ENDOF
   ;MATCH ;

\ The call's kind, configured model and usage counts, when the reply carried
\ usage.
: USAGE-FIELD ( ptr JSON-WRITE:writer n ptr call -- ptr JSON-WRITE:writer )
   {: kind:n c :}
   c TOKENS TOKENS-SEEN @ 0= if exit then
   JSON-WRITE:COMMA s" usage" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" kind" kind 1 = if s" generation" else s" embedding" then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" model" kind 1 = if c GEN$ else c EMBED-MODEL$ then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" prompt_tokens" c TOKENS TOKENS-PROMPT @ COUNT-FIELD
   JSON-WRITE:COMMA s" cached_tokens" c TOKENS TOKENS-CACHED @ COUNT-FIELD
   JSON-WRITE:COMMA s" completion_tokens" c TOKENS TOKENS-COMPLETION @ COUNT-FIELD
   JSON-WRITE:COMMA s" reasoning_tokens" c TOKENS TOKENS-REASONING @ COUNT-FIELD
   JSON-WRITE:OBJECT-END ;

\ Opens the journal record with the call's configuration.
: RECORD-OPEN ( n ptr call -- ptr JSON-WRITE:writer )
   {: kind:n c :}
   kind 1 = if c BUILD-GEN-CONFIG else c BUILD-EMBED-CONFIG then
   c JOURNAL-RECORD-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-RECORD-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" configuration" kind 1 = if c GEN-CONFIG$ else c EMBED-CONFIG$ then
   JSON-WRITE:FIELD-RAW ;

: JOURNAL-RECORD ( n ptr call -- )
   {: kind:n c :}
   kind c RECORD-OPEN
   JSON-WRITE:COMMA s" answer" kind 1 = if
      c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
   else s" " then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" response" c RESPONSE$ JSON-WRITE:FIELD-S
   kind c USAGE-FIELD
   c LEDGER LEDGER-CALL-ID @ {: id:n :}
   id 0<> if JSON-WRITE:COMMA s" call_id" id JSON-WRITE:FIELD-INT then
   JSON-WRITE:COMMA s" origin" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" kind" s" live" JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

\ A nonzero count, else none.
: NONZERO ( n -- option<n> ) dup 0<> if OPTION:SOME else drop OPTION:NONE then ;

: BACKEND$ ( n ptr call -- ptr u8 n )
   {: kind:n c :}
   kind 1 <> c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = and if
      s" ollama"
   else s" openai" then ;


\ The sha256 hex of the model, system text and schema every generation
\ request of a purpose starts with, NUL-separated.
: PREFIX-HASH$ ( ptr call -- ptr u8 n )
   {: c :}
   c HASH-WORK {: w :}
   w SHA256-BEGIN
   w c GEN$ SHA256-FEED
   w s\" \x00" SHA256-FEED
   w c SYSTEM$ SHA256-FEED
   w s\" \x00" SHA256-FEED
   w c SCHEMA$ SHA256-FEED
   w w SHA256-CTX-BYTES + SHA256-END
   w SHA256-CTX-BYTES + w SHA256-CTX-BYTES + 32 + SHA256>HEX
   w SHA256-CTX-BYTES + 32 + 64 ;

\ The call's accounting record, the JSON object RECORDER!'s record receives.
: CALL-RECORD ( ptr call -- )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-KIND @ {: kind:n :}
   l LEDGER-CANCELLED @ if E-CANCELLED else l LEDGER-CODE @ then {: fault:n :}
   c JOURNAL-RECORD-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-RECORD-BUF JSON-WRITE:OPEN-BUF drop
   c WRITER JSON-WRITE:OBJECT-START
   s" kind" kind 1 = if s" generation" else s" embedding" then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" backend" kind c BACKEND$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" model" kind 1 = if c GEN$ else c EMBED-MODEL$ then
   JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" effort" kind 1 = c EFFORT$ nip 0<> and if
      c EFFORT$ JSON-WRITE:FIELD-S
   else JSON-WRITE:FIELD-NULL then
   JSON-WRITE:COMMA s" service_tier" c SERVED$ nip 0<> if
      c SERVED$ JSON-WRITE:FIELD-S
   else JSON-WRITE:FIELD-NULL then
   JSON-WRITE:COMMA s" requested_tier" kind 1 = c TIER$ nip 0<> and if
      c TIER$ JSON-WRITE:FIELD-S
   else JSON-WRITE:FIELD-NULL then
   JSON-WRITE:COMMA s" purpose" c LABEL$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" outcome" l LEDGER-CANCELLED @ if s" cancelled"
   else fault 0<> if s" failed" else s" ok" then then JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" fault" fault NONZERO COUNT-FIELD
   JSON-WRITE:COMMA s" http_status" c MODEL-CALL:HTTP-STATUS @ NONZERO COUNT-FIELD
   JSON-WRITE:COMMA s" finish_reason" c MODEL-CALL:FINISH-U @ 0<> if
      c CONTENT-BUF HELD c MODEL-CALL:FINISH-U @ SPAN:TAKE SPAN:$ JSON-WRITE:FIELD-S
   else JSON-WRITE:FIELD-NULL then
   JSON-WRITE:COMMA s" request_bytes" c BODY$ nip JSON-WRITE:FIELD-INT
   kind 1 = if
      JSON-WRITE:COMMA s" prefix_bytes" c GEN$ nip c SYSTEM$ nip + c SCHEMA$ nip +
      JSON-WRITE:FIELD-INT
      JSON-WRITE:COMMA s" prefix_hash" c PREFIX-HASH$ JSON-WRITE:FIELD-S
   then
   JSON-WRITE:COMMA s" duration_ms" TIME:MONO-NS l LEDGER-STARTED-NS @ - NS-PER-MS /
   JSON-WRITE:FIELD-INT
   JSON-WRITE:COMMA s" prompt_tokens" c TOKENS TOKENS-PROMPT @ COUNT-FIELD
   JSON-WRITE:COMMA s" cached_tokens" c TOKENS TOKENS-CACHED @ COUNT-FIELD
   JSON-WRITE:COMMA s" completion_tokens" c TOKENS TOKENS-COMPLETION @ COUNT-FIELD
   JSON-WRITE:COMMA s" reasoning_tokens" c TOKENS TOKENS-REASONING @ COUNT-FIELD
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE ;

: REJECT-RUN ( ptr ledger -- ptr ledger )
   {: l :}
   l LEDGER-CONTEXT @ l LEDGER-CALL-ID @ l LEDGER-REJECT @ execute
   l ;

: RECORD-RUN ( ptr call -- ptr call )
   {: c :}
   c CALL-RECORD
   c LEDGER {: l :}
   l LEDGER-CONTEXT @ c JOURNAL-RECORD$ l LEDGER-RECORD @ execute
   l LEDGER-CALL-ID !
   c ;

\ A call starts: its pending purpose becomes its label, required when a
\ recorder is installed, and selects the purpose's row. A fault before it
\ sends reports no HTTP status, as PARTS-RESET and INPUT-RESET ensure for the
\ builders.
: START-LEDGER ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   0 c MODEL-CALL:HTTP-STATUS !
   c LEDGER {: l :}
   l LEDGER-PENDING-U @ {: u:n :}
   u l LEDGER-LABEL-U !
   0 l LEDGER-PENDING-U !
   c LABEL$ c ROW-OF c PICK-CELL !
   false l LEDGER-DISPATCHED !
   0 l LEDGER-CALL-ID !
   0 l LEDGER-FAULT !
   l LEDGER-READY @ u 0= and if E-STATE throw then ;

\ Records a request the backend received. The recorder's fault is kept for
\ RECORD-FAULT and returned only when the call succeeded, so a model fault
\ stays primary.
: END-LEDGER ( n n bool ptr call -- n )
   {: kind:n code:n cancelled:bool c :}
   c LEDGER {: l :}
   l LEDGER-READY @ l LEDGER-DISPATCHED @ and 0= if 0 exit then
   kind l LEDGER-KIND !
   code l LEDGER-CODE !
   cancelled l LEDGER-CANCELLED !
   c [: RECORD-RUN ;] catch {: rc:n :} drop
   rc l LEDGER-FAULT !
   code 0= cancelled 0= and if rc else 0 then ;

: JOURNAL-PUT ( n ptr call -- )
   {: kind:n c :}
   kind c JOURNAL-RECORD
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

\ Whether the provider's reply refused the request for its size or did not
\ finish: a fault the caller may answer by asking for less.
: REFUSAL? ( n -- bool ) dup E-CAPACITY = swap E-INCOMPLETE = or ;

: REFUSAL-PUT ( n n ptr call -- )
   {: kind:n code:n c :}
   kind c RECORD-OPEN
   JSON-WRITE:COMMA s" refusal" code E-CAPACITY = if s" capacity" else
      s" incomplete" then JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END drop
   c WRITER JSON-WRITE:CLOSE
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

: REFUSAL-KEEP ( n n ptr call -- n n ptr call )
   {: kind:n code:n c :}
   kind code c REFUSAL-PUT kind code c ;

\ A live journaled call the provider refused for size or left unfinished is
\ journaled as its refusal, so a replay meets it at the same step and asks
\ for less without a request. The call's fault stays primary: a put fault is
\ dropped.
: JOURNAL-REFUSAL ( n n bool ptr call -- )
   {: kind:n code:n hit:bool c :}
   c MODEL-CALL:JOURNAL-READY @ 0= hit or if exit then
   code REFUSAL? 0= if exit then
   kind code c [: REFUSAL-KEEP ;] catch drop drop 2drop ;

: PREP-JOURNAL-CHAT ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   c CHECK-CHAT
   c PART-FINISH
   c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-EMBED ( ptr call -- )
   {: c :}
   c REQUIRE-IDLE
   c CHECK-EMBED
   c JOURNAL-EMBED-INPUT ;

: RUN-CHAT-SELECT ( ptr call bool -- ptr call bool )
   {: c hit:bool :}
   hit if 1 c JOURNAL-RESTORE else c RUN-CHAT drop then
   c hit ;

: RUN-EMBED-SELECT ( ptr call bool -- ptr call bool )
   {: c hit:bool :}
   hit if 2 c JOURNAL-RESTORE else c RUN-EMBED drop then
   c hit ;

: CANCEL-LOCKED ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:BUSY @ 0= if
      false c MODEL-CALL:CANCELLED !
      c exit
   then
   true c MODEL-CALL:CANCELLED !
   c MODEL-CALL:TRANSPORT @ MATCH option
      none OF ENDOF
      some OF CURL:CANCEL ENDOF
   ;MATCH
   c ;

: STOP-LOCKED ( ptr call -- ptr call )
   {: c :}
   true c MODEL-CALL:STOPPED !
   c CANCEL-LOCKED ;

public

: CANCEL ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then
   c MODEL-CALL:GATE @ TASK:WAIT
   c [: CANCEL-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:CANCELLED @ {: selected:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL
   code 0<> code CURL:E-STATE <> and if code throw then
   selected ;

: REQUEST-STOP ( ptr call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then
   c MODEL-CALL:GATE @ TASK:WAIT
   c [: STOP-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL
   code 0<> code CURL:E-STATE <> and if code throw then ;

: CHAT ( ptr call -- ptr u8 n )
   {: c :}
   c START-LEDGER
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-CHAT
      c JOURNAL-GET?
   else false then {: hit:bool :}
   0 c MODEL-CALL:PROMPT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   c BEGIN-CALL
   c hit [: RUN-CHAT-SELECT ;] catch {: code:n :} 2drop
   c END-CALL {: cancelled:bool :}
   1 code cancelled c END-LEDGER {: recorded:n :}
   cancelled if E-CANCELLED throw then
   code 0<> if 1 code hit c JOURNAL-REFUSAL code throw then
   c MODEL-CALL:JOURNAL-READY @ hit 0= and if 1 c JOURNAL-PUT then
   recorded 0<> if recorded throw then
   c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$ ;

: EMBED ( ptr call -- )
   {: c :}
   c START-LEDGER
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-EMBED
      c JOURNAL-GET?
   else false then {: hit:bool :}
   c BEGIN-CALL
   c hit [: RUN-EMBED-SELECT ;] catch {: code:n :} 2drop
   c END-CALL {: cancelled:bool :}
   2 code cancelled c END-LEDGER {: recorded:n :}
   cancelled if E-CANCELLED throw then
   code 0<> if 2 code hit c JOURNAL-REFUSAL code throw then
   c MODEL-CALL:JOURNAL-READY @ hit 0= and if 2 c JOURNAL-PUT then
   recorded 0<> if recorded throw then
   true c MODEL-CALL:EMBEDDED ! ;

: VECTOR@ ( n ptr call -- ptr u8 n )
   {: idx:n c :}
   c MODEL-CALL:EMBEDDED @ 0= if E-STATE throw then
   idx 0 < idx c MODEL-CALL:INPUT-COUNT @ >= or if E-CONFIG throw then
   c VECTORS-BUF BUF:SPAN c idx VECTOR-OFF @ c idx VECTOR-LEN @ SPAN:SUB SPAN:$ ;

: GENERATION-MODEL$ ( ptr call -- ptr u8 n ) GEN$ ;
: HTTP-STATUS ( ptr call -- n ) MODEL-CALL:HTTP-STATUS @ ;
: FINISH-REASON$ ( ptr call -- ptr u8 n )
   {: c :}
   c CONTENT-BUF HELD c MODEL-CALL:FINISH-U @ SPAN:TAKE SPAN:$ ;
\ The last call's provider message from its refused HTTP reply; empty when none.
: ERROR$ ( ptr call -- ptr u8 n ) ERROR-BUF BUFFER$ ;
: PROMPT-TOKENS ( ptr call -- n ) MODEL-CALL:PROMPT-TOKENS @ ;
\ The last call's recorded id: its own row, or for a replayed reply the row
\ its live call recorded; 0 when none.
: CALL-ID ( ptr call -- n ) LEDGER LEDGER-CALL-ID @ ;
\ The recorder's fault from the last call, 0 when recording succeeded or did
\ not run. A caller that rethrows the call's own fault reports this one too.
: RECORD-FAULT ( ptr call -- n ) LEDGER LEDGER-FAULT @ ;
\ The library rejected the last call's reply: mark its recorded row. A
\ recorder fault is kept for RECORD-FAULT, unless one already is, and thrown.
: REJECTED ( ptr call -- )
   LEDGER {: l :}
   l LEDGER-READY @ 0= if exit then
   l LEDGER-CALL-ID @ 0= if exit then
   l [: REJECT-RUN ;] catch {: rc:n :} drop
   rc 0= if exit then
   l LEDGER-FAULT @ 0= if rc l LEDGER-FAULT ! then
   rc throw ;
: CACHED-TOKENS ( ptr call -- n ) MODEL-CALL:CACHED-TOKENS @ ;

;package
