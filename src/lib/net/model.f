\ model.f - package MODEL, a Habu library: a client for model providers over
\ CURL. Chat speaks OpenAI's Responses protocol; embeddings go to an
\ OpenAI-shaped provider or Ollama's native route. A chat call answers a
\ schema with one JSON object (CONTENT$) or offers tools and answers one tool
\ call (OUTPUT$); COUNT-INPUT counts a chat request's input tokens without
\ generating, and COMPACT replaces its input with the provider's compacted
\ window. The client's API key is read from the native credential store
\ through SECRET, with a caller's journal and recorder and bounded inputs.
\ Each failure the provider or the transport caused is logged through LOG
\ (tag `model`), never the request body.
\ Test: src/lib/net/model-test.f.
\ STORAGE CLASS. CALLER-OWNED: each call and settings record, and the arena
\ OPEN allocates for a call until CLOSE; the module keeps no state.
require src/lib/base64.f
require src/lib/byte-buffer.f
require src/lib/f64-text.f
require src/lib/float32.f
require src/lib/float32-buffer.f
require src/lib/fmt.f
require src/lib/fsm.f
require src/lib/json.f
require src/lib/log.f
require src/lib/memory.f
require src/lib/net/curl.f
require src/lib/net/event-stream.f
require src/lib/os-memory.f
require src/lib/secret.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f
require src/lib/time.f

\ One exclusive call context per worker. Results borrow its arena until the
\ next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED, REPLAY or CLOSE.
\ CANCEL and REQUEST-STOP may run concurrently with a call. Generation and
\ embedding are each configured explicitly, with no implicit model or fallback.
\ The caller starts Habu's AIO and CURL loops before HTTP calls and keeps model
\ work outside PostgreSQL transactions. No API key reaches a request body,
\ response, error, log line or producer record: a key is read only into the
\ HTTP header, which is wiped once libcurl copies it.
\ A caller that looks its journal up itself, such as on another task, finishes
\ the request with JOURNAL-INPUT$, looks that input up, then sends the request
\ with CHAT or restores the saved record with REPLAY. COUNT-INPUT counts a
\ built request and leaves it built for the CHAT or START that sends it;
\ COMPACT leaves the compacted window built in its place. A chat, count,
\ compaction or embed that faults keeps its request: PURPOSE! and the same word
\ send it again, its body and journal input built from the settings then in
\ force, so the first try's when none changed. A chat that succeeds uses its
\ request up. A task drives many calls by STARTing each,
\ SERVEing each until it answers true, then AWAITing it. It TASK:WAITs only
\ straight after a round in which every SERVE answered false: landing a call
\ runs its journal and recorder, and a wait there takes the CURL loop's hint
\ for another call. SERVE and AWAIT of a started call run
\ on the task that started it; a halt while AWAIT waits lands the call first.
\ A task that may be halted CANCELs, then AWAITs, each call still started in a
\ finally, catching each AWAIT's fault.

\ A journal's and a recorder's callbacks each get the caller's ctx unchanged.
\ get fills the MODEL-owned buffer with one record on a hit, which restores
\ the record's content, vectors and producing configuration without a
\ provider call; put commits a newly accepted record before returning. After
\ each request dispatched to a backend, served or failed, record gets the
\ call's JSON record and returns its call id; reject gets the id of a call whose
\ reply the library rejected. A journal replay sends nothing and records
\ nothing. Neither get nor put runs with a transfer out. put and record run
\ with the call landed, reject inside REJECTED; none of the three may CLOSE
\ it. A get, put or reject failure propagates, as does a record failure from a
\ call that otherwise succeeded; RECORD-FAULT keeps record's either way. A halt
\ of the starter pending while put or record runs, one that came during the
\ transfer included, ends it at its first TASK:PAUSE, the call landed
\ E-CANCELLED.

package MODEL

private
-9700 constant E-FIRST
-9799 constant E-LAST
\ idle; prepared: JOURNAL-INPUT$ finished the chat request; started: START,
\ START-EMBED, COUNT-INPUT or COMPACT began it; landed: ended, not yet
\ awaited. Busy: started or landed.
ENUM call-state idle prepared started landed ;ENUM
ENUM call-kind chat embed tally compaction ;ENUM
\ What the last awaited call left for CONTENT$ or VECTOR@.
ENUM call-result none content vectors ;ENUM
\ How SERVE and AWAIT resume a started call's transfer: reading feeds each
\ chunk; dropping discards them until it ends.
ENUM receive-rest reading dropping ;ENUM
\ A call's chat input: none, building (PARTS-RESET started it, every write
\ done), broken (a write did not complete) or finished (PART-FINISH closed it;
\ a count keeps it finished for the CHAT or START that sends it, and a
\ compaction replaces its parts and items with the window and keeps it
\ finished).
ENUM input-state none building broken finished ;ENUM
\ What a call's reply answers: nothing set yet, its schema or its tools.
ENUM reply-form none schema tools ;ENUM
public
\ From CHAT, COUNT-INPUT, COMPACT and EMBED, a provider's 404 to the request
\ (an unknown model) is also E-CONFIG and its 401 or 403 E-KEY; HTTP-STATUS
\ keeps the status.
-9700 constant E-CONFIG
-9701 constant E-KEY
\ From CHAT, COUNT-INPUT, COMPACT and EMBED: the provider refused the request
\ for its size; INPUT+ also refuses an input past INPUT-MAX with it. An invalid
\ setter or builder argument is E-CONFIG.
-9702 constant E-CAPACITY
\ The transfer failed, stalled, or its stream ended before the response did;
\ the provider may still finish and bill the response.
-9703 constant E-TRANSPORT
\ A status other than 200, or a failed response's error code, that no code
\ above or E-UNAVAILABLE names.
-9704 constant E-HTTP
\ The provider's response, or a journal record replayed for it, is malformed
\ or inconsistent.
-9705 constant E-RESPONSE
\ The chat response's answer message carries a refusal.
-9706 constant E-REFUSED
\ The response ended incomplete: INCOMPLETE-REASON$ names why.
-9707 constant E-INCOMPLETE
\ CANCEL ended the call, REQUEST-STOP came before or during it, or a halt cut
\ its recorder or journal put.
-9709 constant E-CANCELLED
\ Out of turn: a busy or unopened call, a part or item outside building, SERVE
\ or AWAIT unstarted or on another task, VECTOR@ or CONTENT$ without its result.
-9710 constant E-STATE
\ The provider is temporarily unavailable: status 408, 429 or 5xx, or a failed
\ response's error code rate_limit_exceeded or server_error.
-9711 constant E-UNAVAILABLE
\ OpenAI's limits: https://platform.openai.com/docs/api-reference/embeddings
\ MODEL counts no embed input's tokens; INPUT-MAX bounds an Ollama call too.
\ The most inputs one EMBED call takes.
2048 constant INPUT-MAX
\ The most tokens one EMBED call's inputs may sum to.
300000 constant TOKENS-PER-EMBED-MAX
\ The most tokens one input may hold.
8192 constant TOKENS-PER-INPUT-MAX

STRUCTURE allocation DERIVE addr
   FIELD base ptr u8
   FIELD size n
;STRUCTURE

STRUCTURE call DERIVE addr
   FIELD arena allocation
   FIELD body-w JSON:writer
   FIELD parts-w JSON:writer
   FIELD journal-parts-w JSON:writer
   FIELD reader JSON:cursor
   FIELD gate TASK:sem
   FIELD gate-ready bool
   FIELD handle option<CURL:handle>
   FIELD transport option<CURL:handle>
   FIELD state call-state
   FIELD cancelled bool
   FIELD stopped bool
   FIELD key-split n
   FIELD dims n
   FIELD input-tokens n
   FIELD cached-tokens n
   FIELD counted n
   FIELD response-u n
   FIELD content-u n
   FIELD input-count n
   FIELD input-u n
   FIELD vector-at n
   FIELD result call-result
   FIELD parts-count n
   FIELD chat-input input-state
   FIELD form reply-form
   FIELD content-seen bool
   FIELD parsed-index n
   FIELD vector-seen bool
   FIELD kind call-kind
   FIELD receive receive-rest
   FIELD component-count n
   FIELD data-count n
   FIELD http-status n
   FIELD incomplete-u n
   FIELD embedding-kind n
   FIELD journal-context n
   FIELD journal-get [ n ptr u8 n ptr n -- bool ]
   FIELD journal-put [ n ptr u8 n ptr u8 n -- ]
   FIELD journal-ready bool
   FIELD replay-kind option<call-kind>
   FIELD starter n
;STRUCTURE

private

\ The last reply's usage: whether it carried a usage object, and each count,
\ none when the reply omits it or gives null. It lives in the call's arena,
\ keeping the by-value call record within the native width ceiling.
STRUCTURE tokens DERIVE addr
   FIELD seen bool
   FIELD input option<n>
   FIELD cached option<n>
   FIELD written option<n>
   FIELD output option<n>
   FIELD reasoning option<n>
;STRUCTURE

CAST: >TOKENS ( ptr u8 -- ptr tokens )

\ The installed recorder and the current call's accounting: the purpose
\ PURPOSE! left pending, the label of the call in progress, whether its request
\ reached the backend and when, its recorded call id, the recorder's fault and
\ the libcurl code of the call's failed step, 0 when none.
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
   FIELD code n
   FIELD cancelled bool
   FIELD transport n
;STRUCTURE

CAST: >LEDGER ( ptr u8 -- ptr ledger )

\ A chat's event stream: whether it delivered an event, whether the response
\ ended, an error event's fault, its first output's TIME:MONO-NS, 0 if none,
\ and the decoder reading it.
STRUCTURE stream DERIVE addr
   FIELD heard bool
   FIELD done bool
   FIELD fault n
   FIELD output-ns n
   FIELD decoder option<SSE:decoder>
;STRUCTURE

CAST: >STREAM ( ptr u8 -- ptr stream )

\ The land machine's steps: settling ends the call's transfer and lands it;
\ recording runs the recorder; serving and refusing run it, then the put.
ENUM land-step settling recording serving refusing landed ;ENUM

\ END-TRANSFER's status, code, spoiling fault and cut; outcome, the fault it
\ throws; step, the land machine's next; outer, the LANDING ACCOUNT replaced.
STRUCTURE receipt DERIVE addr
   FIELD status CURL:http-status
   FIELD code n
   FIELD fault n
   FIELD cut bool
   FIELD replayed bool
   FIELD outcome n
   FIELD step land-step
   FIELD outer n
;STRUCTURE

CAST: >RECEIPT ( ptr u8 -- ptr receipt )

256 constant SCRATCH-INITIAL
1000000 constant NS-PER-MS
\ No byte for this long ends an exchange, curl 28 logged. A thinking model's
\ reasoning items arrive at most 9 s apart (OpenAI, measured to 2 minutes).
60 constant STALL-SECONDS
\ The room each read of an event stream lands in; an event may span reads.
16384 constant CHUNK-BYTES
1 constant OPENAI
2 constant OLLAMA

0 constant SCRATCH-BASE-AT
SCRATCH-BASE-AT 1 cells + constant GEN-HDR-AT
GEN-HDR-AT BUF:HDR-BYTES + constant EMBED-HDR-AT
EMBED-HDR-AT BUF:HDR-BYTES + constant KEY-SPEC-HDR-AT
KEY-SPEC-HDR-AT BUF:HDR-BYTES + constant SYSTEM-HDR-AT
SYSTEM-HDR-AT BUF:HDR-BYTES + constant FORM-HDR-AT
FORM-HDR-AT BUF:HDR-BYTES + constant PARTS-HDR-AT
PARTS-HDR-AT BUF:HDR-BYTES + constant JOURNAL-PARTS-HDR-AT
JOURNAL-PARTS-HDR-AT BUF:HDR-BYTES + constant ITEMS-HDR-AT
ITEMS-HDR-AT BUF:HDR-BYTES + constant BODY-HDR-AT
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
CACHE-KEY-HDR-AT BUF:HDR-BYTES + constant RESPONSE-ID-HDR-AT
RESPONSE-ID-HDR-AT BUF:HDR-BYTES + constant CHUNK-HDR-AT
CHUNK-HDR-AT BUF:HDR-BYTES + constant EVENT-ROWS-HDR-AT
EVENT-ROWS-HDR-AT BUF:HDR-BYTES + constant EVENT-TEXT-HDR-AT
EVENT-TEXT-HDR-AT BUF:HDR-BYTES + constant COUNTS-AT
COUNTS-AT TOKENS-BYTES + constant ACCOUNTS-AT
ACCOUNTS-AT LEDGER-BYTES + constant EVENTS-AT
EVENTS-AT STREAM-BYTES + constant DECODER-AT
DECODER-AT SSE:STORAGE-BYTES + constant ENDING-AT
ENDING-AT RECEIPT-BYTES + constant HASH-AT
\ A purpose row: the offset and length in the row text of its purpose, model,
\ effort and service tier.
8 cells constant ROW-BYTES
\ An event row: the offset and length in the event text of an event type, and
\ how many events of it the stream carried.
3 cells constant EVENT-ROW-BYTES
\ The row the call in progress uses, plus one, the chat output limit, then a
\ row being added.
HASH-AT SHA256-CTX-BYTES + 96 + 7 + 7 invert and constant PICK-AT
PICK-AT 1 cells + constant MAX-OUTPUT-AT
MAX-OUTPUT-AT 1 cells + constant ROW-NEW-AT
ROW-NEW-AT ROW-BYTES + constant INPUT-OFF-AT
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
   base FORM-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base PARTS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base JOURNAL-PARTS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base ITEMS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
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
   base CACHE-KEY-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base RESPONSE-ID-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base CHUNK-HDR-AT + CELL-VIEW CHUNK-BYTES BUF:N>BLEN BUF:INIT
   base EVENT-ROWS-HDR-AT + CELL-VIEW EVENT-ROW-BYTES BUF:N>BLEN BUF:INIT
   base EVENT-TEXT-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT ;

: DISPOSE-BUFFERS ( ptr u8 -- )
   {: base:ptr :}
   base GEN-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EMBED-HDR-AT + CELL-VIEW BUF:DISPOSE
   base KEY-SPEC-HDR-AT + CELL-VIEW BUF:DISPOSE
   base SYSTEM-HDR-AT + CELL-VIEW BUF:DISPOSE
   base FORM-HDR-AT + CELL-VIEW BUF:DISPOSE
   base PARTS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base JOURNAL-PARTS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ITEMS-HDR-AT + CELL-VIEW BUF:DISPOSE
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
   base CACHE-KEY-HDR-AT + CELL-VIEW BUF:DISPOSE
   base RESPONSE-ID-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CHUNK-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EVENT-ROWS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EVENT-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE ;

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
\ The schema or the tools, as the call's form names.
: FORM-BUF ( ptr call -- ptr n ) FORM-HDR-AT HEADER ;
: PARTS-BUF ( ptr call -- ptr n ) PARTS-HDR-AT HEADER ;
\ The parts as the journal input names them: an image by its digest.
: JOURNAL-PARTS-BUF ( ptr call -- ptr n ) JOURNAL-PARTS-HDR-AT HEADER ;
\ The history items as given, joined by single commas.
: ITEMS-BUF ( ptr call -- ptr n ) ITEMS-HDR-AT HEADER ;
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
\ The event types the call's stream carried, one row each in first-seen order.
: EVENT-ROWS-BUF ( ptr call -- ptr n ) EVENT-ROWS-HDR-AT HEADER ;
: EVENT-TEXT-BUF ( ptr call -- ptr n ) EVENT-TEXT-HDR-AT HEADER ;
: CACHE-KEY-BUF ( ptr call -- ptr n ) CACHE-KEY-HDR-AT HEADER ;
\ The id of the call's response, empty until its reply names it.
: RESPONSE-ID-BUF ( ptr call -- ptr n ) RESPONSE-ID-HDR-AT HEADER ;
\ The bytes of the event stream's last read.
: CHUNK-BUF ( ptr call -- ptr n ) CHUNK-HDR-AT HEADER ;
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
: STREAM ( ptr call -- ptr stream )
   EVENTS-AT STREAM-BYTES REGION SPAN:$ drop >STREAM ;
: RECEIPT ( ptr call -- ptr receipt )
   ENDING-AT RECEIPT-BYTES REGION SPAN:$ drop >RECEIPT ;
: DECODER-STORAGE ( ptr call -- ptr n n )
   DECODER-AT SSE:STORAGE-BYTES REGION SPAN:$ {: a:ptr u:n :}
   a CELL-VIEW u ;
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
\ The most output tokens a chat request asks for; 0 asks for no limit.
: MAX-OUTPUT-CELL ( ptr call -- ptr n )
   MAX-OUTPUT-AT 1 cells REGION SPAN:$ drop CELL-VIEW ;
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

: EVENT-ROW-COUNT ( ptr call -- n )
   EVENT-ROWS-BUF BUF:LEN@ BUF:BLEN>N EVENT-ROW-BYTES / ;
\ Cell k of event row idx.
: EVENT-CELL ( n n ptr call -- ptr n )
   {: idx:n k:n c :}
   c EVENT-ROWS-BUF BUF:SPAN idx EVENT-ROW-BYTES * k cells + 1 cells SPAN:SUB
   SPAN:$ drop CELL-VIEW ;
: EVENT-TYPE$ ( n ptr call -- ptr u8 n )
   {: idx:n c :}
   c EVENT-TEXT-BUF BUF:SPAN idx 0 c EVENT-CELL @ idx 1 c EVENT-CELL @ SPAN:SUB
   SPAN:$ ;
\ Counts one event of the type, adding its row the first time it is seen.
: COUNT-EVENT ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c EVENT-ROW-COUNT {: rows:n :}
   rows rows 0 ?do i c EVENT-TYPE$ a u STR= if drop i leave then loop {: idx:n :}
   idx rows = if
      c EVENT-TEXT-BUF BUF:LEN@ BUF:BLEN>N {: off:n :}
      a u BUF:N>BLEN c EVENT-TEXT-BUF BUF:APPEND-SPAN
      c EVENT-ROWS-BUF EVENT-ROW-BYTES BUF:N>BLEN BUF:GROW SPAN:$ drop CELL-VIEW
      {: row:ptr :}
      off row !  u row 1 cells + !  0 row 2 cells + !
   then
   1 idx 2 c EVENT-CELL +! ;

: BUFFER$ ( ptr n -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;
\ The generation model, effort and tier: the purpose row's when the latest call
\ or JOURNAL-INPUT$ selected one, else the call's own.
: GEN$ ( ptr call -- ptr u8 n )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 1 c ROW$ exit then
   c GEN-BUF BUFFER$ ;
: EMBED-MODEL$ ( ptr call -- ptr u8 n ) EMBED-BUF BUFFER$ ;
: KEY-SPEC$ ( ptr call -- ptr u8 n ) KEY-SPEC-BUF BUFFER$ ;
: SYSTEM$ ( ptr call -- ptr u8 n ) SYSTEM-BUF BUFFER$ ;
: FORM$ ( ptr call -- ptr u8 n ) FORM-BUF BUFFER$ ;
: PARTS$ ( ptr call -- ptr u8 n ) PARTS-BUF BUFFER$ ;
: JOURNAL-PARTS$ ( ptr call -- ptr u8 n ) JOURNAL-PARTS-BUF BUFFER$ ;
: ITEMS$ ( ptr call -- ptr u8 n ) ITEMS-BUF BUFFER$ ;
: BODY$ ( ptr call -- ptr u8 n ) BODY-BUF BUFFER$ ;
: EFFORT$ ( ptr call -- ptr u8 n )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 2 c ROW$ exit then
   c EFFORT-BUF BUFFER$ ;
: CACHE-KEY$ ( ptr call -- ptr u8 n ) CACHE-KEY-BUF BUFFER$ ;
: RESPONSE-ID$ ( ptr call -- ptr u8 n ) RESPONSE-ID-BUF BUFFER$ ;
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
: JOURNAL-INPUT-SPAN$ ( ptr call -- ptr u8 n ) JOURNAL-INPUT-BUF BUFFER$ ;
: JOURNAL-RECORD$ ( ptr call -- ptr u8 n ) JOURNAL-RECORD-BUF BUFFER$ ;
: JOURNAL-OUT$ ( ptr call -- ptr u8 n ) JOURNAL-OUT-BUF BUFFER$ ;
: REPLAY-CONTENT$ ( ptr call -- ptr u8 n ) REPLAY-CONTENT-BUF BUFFER$ ;
: RESPONSE$ ( ptr call -- ptr u8 n )
   {: c :}
   c RESPONSE-BUF HELD c MODEL-CALL:RESPONSE-U @ SPAN:TAKE SPAN:$ ;

: REQUIRE-OPEN ( ptr call -- )
   MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then ;

: IDLE! ( ptr call -- ) construct call-state idle swap MODEL-CALL:STATE ! ;


\ The setters that keep the finished request run it.
: REQUIRE-REST ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH ;


\ Whether a kind's call runs the generation configuration: a chat, a count and
\ a compaction do.
: GENERATES? ( call-kind -- bool )
   MATCH call-kind
      chat OF true ENDOF
      embed OF false ENDOF
      tally OF true ENDOF
      compaction OF true ENDOF
   ;MATCH ;

\ Whether a kind's generation configuration and journal input carry the effort,
\ and its journal input the form: a chat's and a count's do, a compaction's and
\ an embedding's do not.
: SHAPED? ( call-kind -- bool )
   MATCH call-kind
      chat OF true ENDOF
      embed OF false ENDOF
      tally OF true ENDOF
      compaction OF false ENDOF
   ;MATCH ;

\ Whether a kind's journal input names the output limit: a chat's alone.
: LIMITED? ( call-kind -- bool )
   MATCH call-kind
      chat OF true ENDOF
      embed OF false ENDOF
      tally OF false ENDOF
      compaction OF false ENDOF
   ;MATCH ;


\ Whether kind k's configuration is the one the last call restored from a
\ journal record, with no setter of it since: the record's kind builds the
\ configuration k builds.
: REPLAYED? ( call-kind ptr call -- bool )
   {: k:call-kind c :}
   c MODEL-CALL:REPLAY-KIND @ MATCH option
      none OF false ENDOF
      some OF
         dup GENERATES? bool>n k GENERATES? bool>n =
         swap SHAPED? bool>n k SHAPED? bool>n = and
      ENDOF
   ;MATCH ;

\ Drops a generation configuration the last call restored from a journal
\ record: a setter changed what generation calls send.
: FORGET-GEN-REPLAY ( ptr call -- )
   {: c :}
   c MODEL-CALL:REPLAY-KIND @ MATCH option
      none OF ENDOF
      some OF GENERATES? if OPTION:NONE c MODEL-CALL:REPLAY-KIND ! then ENDOF
   ;MATCH ;


: REQUIRE-BUILDING ( ptr call -- )
   MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF E-STATE throw ENDOF
      building OF ENDOF
      broken OF E-STATE throw ENDOF
      finished OF E-STATE throw ENDOF
   ;MATCH ;


\ Makes a call idle and answers whether JOURNAL-INPUT$ had finished its
\ request, which stays finished for START to send.
: TAKE-PREPARED ( ptr call -- bool )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF false ENDOF
      prepared OF true ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   c IDLE! ;

\ Drops the request JOURNAL-INPUT$ finished, leaving no input, and answers
\ whether there was one; JOURNAL-INPUT$ names the words that discard it.
: UNPREPARE ( ptr call -- bool )
   {: c :}
   c TAKE-PREPARED dup if
      construct input-state none c MODEL-CALL:CHAT-INPUT !
   then ;


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
   OPTION:NONE t TOKENS-INPUT !
   OPTION:NONE t TOKENS-CACHED !
   OPTION:NONE t TOKENS-WRITTEN !
   OPTION:NONE t TOKENS-OUTPUT !
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
   c IDLE!
   false c MODEL-CALL:CANCELLED !
   false c MODEL-CALL:STOPPED !
   OPENAI c MODEL-CALL:EMBEDDING-KIND !
   construct call-kind chat c MODEL-CALL:KIND !
   0 c MODEL-CALL:DIMS !
   0 c MODEL-CALL:INPUT-COUNT !
   0 c MODEL-CALL:INPUT-U !
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:CONTENT-U !
   0 c MODEL-CALL:INCOMPLETE-U !
   0 c MODEL-CALL:PARTS-COUNT !
   construct input-state none c MODEL-CALL:CHAT-INPUT !
   construct reply-form none c MODEL-CALL:FORM !
   construct call-result none c MODEL-CALL:RESULT !
   false c MODEL-CALL:JOURNAL-READY !
   OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   0 c PICK-CELL !
   0 c MAX-OUTPUT-CELL !
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   0 c MODEL-CALL:COUNTED !
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
   c MODEL-CALL:READER JSON:READ-DISPOSE
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
   s" /responses" c URL$ ;

: EMBED-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   s" /embeddings" c URL$ ;

: COUNT-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   s" /responses/input_tokens" c URL$ ;

: COMPACT-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   s" /responses/compact" c URL$ ;

: REQUEST-URL$ ( ptr call -- ptr u8 n )
   {: c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c CHAT-URL$ ENDOF
      embed OF
         c MODEL-CALL:EMBEDDING-KIND @ OLLAMA =
         if c ENDPOINT$ else c EMBED-URL$ then
      ENDOF
      tally OF c COUNT-URL$ ENDOF
      compaction OF c COMPACT-URL$ ENDOF
   ;MATCH ;

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


\ Frees the open call, a landed one included; a started one is E-STATE.
: CLOSE ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF ENDOF
      started OF E-STATE throw ENDOF
      landed OF ENDOF
   ;MATCH
   c RELEASE-OPEN ;


\ Installs the journal, which resumes completed calls from the caller's
\ durable store through get and put under the header's rules. OPEN disables it.
: JOURNAL! ( n [ n ptr u8 n ptr n -- bool ] [ n ptr u8 n ptr u8 n -- ] ptr call -- )
   {: ctx:n get put c :}
   c REQUIRE-REST
   ctx c MODEL-CALL:JOURNAL-CONTEXT !
   get c MODEL-CALL:JOURNAL-GET !
   put c MODEL-CALL:JOURNAL-PUT !
   true c MODEL-CALL:JOURNAL-READY ! ;

\ Installs the recorder, which records each request a backend receives through
\ record and each rejected reply through reject, under the header's rules.
: RECORDER! ( n [ n ptr u8 n -- n ] [ n n -- ] ptr call -- )
   {: ctx:n record reject c :}
   c REQUIRE-REST
   c LEDGER {: l :}
   ctx l LEDGER-CONTEXT !
   record l LEDGER-RECORD !
   reject l LEDGER-REJECT !
   true l LEDGER-READY ! ;

\ What the next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED or REPLAY
\ is for: nonblank.
\ Each consumes it; with a recorder installed, one without a purpose is E-STATE.
: PURPOSE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0= if false exit then
   false u 0 ?do a i + c@ 32 > or loop ;

: PURPOSE! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
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

\ The OpenAI-compatible base the chat, count, compaction and embedding routes
\ extend; empty restores OpenAI's.
: BASE-URL! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   u 0<> if a u BASE-URL? 0= if E-CONFIG throw then then
   OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   u 0= if c BASE-URL-BUF BUF:CLEAR exit then
   a u c BASE-URL-BUF REPLACE-SOME ;

: GENERATION-MODEL! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   a u c GEN-BUF REPLACE-SOME
   c FORGET-GEN-REPLAY ;

\ The reasoning effort of every chat and count without a purpose row; empty
\ sends none.
: EFFORT! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   u 0= if c EFFORT-BUF BUF:CLEAR else a u c EFFORT-BUF REPLACE-SOME then
   c FORGET-GEN-REPLAY ;

\ The provider's prompt cache key of every later generation call: requests
\ sharing a key share cache routing. Empty sends none. It leaves the reply,
\ so the journal input and replay, unchanged.
: CACHE-KEY! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   u 0= if c CACHE-KEY-BUF BUF:CLEAR else a u c CACHE-KEY-BUF REPLACE-SOME then ;

\ The most output tokens, reasoning included, of each later chat request alone,
\ 0 none; a chat's journal input names it, so a refusal replays only under it.
: MAX-OUTPUT! ( n ptr call -- )
   {: n:n c :}
   c UNPREPARE drop
   n 0< if E-CONFIG throw then
   n c MAX-OUTPUT-CELL ! ;

\ The service tier every generation request without a purpose row names,
\ such as flex; empty sends none, so the provider serves its default.
\ Embedding requests carry none. The tier changes no content, so replay does
\ not depend on it.
: SERVICE-TIER! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   u 0= if c TIER-BUF BUF:CLEAR else a u c TIER-BUF REPLACE-SOME then ;

\ The generation model, effort and service tier of every later call for the
\ purpose, in place of the call's own; empty effort or tier sends none, even
\ when SERVICE-TIER! set one. A purpose takes one row: a repeated purpose is
\ E-CONFIG.
: PURPOSE-MODEL! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr call -- )
   {: p:ptr p-u:n m:ptr m-u:n e:ptr e-u:n t:ptr t-u:n c :}
   c UNPREPARE drop
   p p-u PURPOSE? 0= if E-CONFIG throw then
   m-u 0 <= e-u 0< or t-u 0< or if E-CONFIG throw then
   p p-u c ROW-OF 0<> if E-CONFIG throw then
   p p-u 0 c ROW-TEXT+
   m m-u 1 c ROW-TEXT+
   e e-u 2 c ROW-TEXT+
   t t-u 3 c ROW-TEXT+
   c ROW-NEW-AT ROW-BYTES REGION SPAN:$ BUF:N>BLEN c ROWS-BUF BUF:APPEND-SPAN
   c FORGET-GEN-REPLAY ;

: EMBEDDING-MODEL! ( ptr u8 n n ptr call -- )
   {: a:ptr u:n dims:n c :}
   c REQUIRE-REST
   dims 0 <= if E-CONFIG throw then
   a u c EMBED-BUF REPLACE-SOME
   dims c MODEL-CALL:DIMS !
   construct call-kind embed c REPLAYED? if
      OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   then
   OPENAI c MODEL-CALL:EMBEDDING-KIND ! ;

\ Names the native credential store's item, by service and account, holding
\ the API key: the only source of a key. Each CHAT, COUNT-INPUT, COMPACT and
\ EMBED reads it with SECRET:READ into the request header. An open call that is
\ idle or holds a finished request is required, else E-STATE; an empty service
\ or account is E-CONFIG. At CHAT, COUNT-INPUT, COMPACT and EMBED a missing,
\ empty or non-printable item is E-KEY, and SECRET:E-CONFIG, SECRET:E-ACCESS
\ and SECRET:E-UNAVAILABLE pass through; a call with no item named fails there
\ with E-CONFIG. The key spec holds the service, then the account from byte
\ key-split.
: API-KEY! ( ptr u8 n ptr u8 n ptr call -- )
   {: svc:ptr su:n acct:ptr au:n c :}
   c REQUIRE-REST
   su 0 <= au 0 <= or if E-CONFIG throw then
   svc su c KEY-SPEC-BUF REPLACE-SOME
   acct au BUF:N>BLEN c KEY-SPEC-BUF BUF:APPEND-SPAN
   su c MODEL-CALL:KEY-SPLIT ! ;

: SYSTEM! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   a u c SYSTEM-BUF REPLACE-SOME ;

: SCHEMA! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   a u c FORM-BUF REPLACE-SOME
   construct reply-form schema c MODEL-CALL:FORM ! ;

\ The tools a call offers, a JSON array of OpenAI function tools checked when
\ the request is finished; the last of SCHEMA! and TOOLS! decides its form.
: TOOLS! ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   a u c FORM-BUF REPLACE-SOME
   construct reply-form tools c MODEL-CALL:FORM ! ;

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
   c REQUIRE-REST
   s SETTINGS-MODEL$ c GENERATION-MODEL!
   s SETTINGS-BASE$ c BASE-URL!
   s SETTINGS-SERVICE$ s" api-key" c API-KEY! ;

private

: WRITER ( ptr call -- ptr JSON:writer ) MODEL-CALL:BODY-W ;
: PART-WRITER ( ptr call -- ptr JSON:writer ) MODEL-CALL:PARTS-W ;
: JOURNAL-PART-WRITER ( ptr call -- ptr JSON:writer ) MODEL-CALL:JOURNAL-PARTS-W ;

\ The flag says whether the configuration carries the effort, when one is set.
: BUILD-GEN-CONFIG ( bool ptr call -- )
   {: shaped:bool c :}
   c WRITER c GEN-CONFIG-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" backend" s" openai-responses" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" model" c GEN$ JSON:WRITE-FIELD-S
   shaped c EFFORT$ nip 0<> and if
      JSON:WRITE-COMMA s" effort" c EFFORT$ JSON:WRITE-FIELD-S
   then
   JSON:WRITE-COMMA s" base_url" c BASE$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

: BUILD-EMBED-CONFIG ( ptr call -- )
   {: c :}
   c WRITER c EMBED-CONFIG-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" backend" c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      s" ollama" else s" openai-embeddings" then JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c EMBED-MODEL$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" dimensions" c MODEL-CALL:DIMS @ JSON:WRITE-FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON:WRITE-COMMA s" identity" c IDENTITY$ JSON:WRITE-FIELD-S
      JSON:WRITE-COMMA s" truncate" false JSON:WRITE-FIELD-BOOL
   else
      JSON:WRITE-COMMA s" base_url" c BASE$ JSON:WRITE-FIELD-S
   then
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

public

: GENERATION-CONFIG$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-REST
   construct call-kind chat c REPLAYED? 0= if true c BUILD-GEN-CONFIG then
   c GEN-CONFIG$ ;

: EMBEDDING-CONFIG$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-REST
   construct call-kind embed c REPLAYED? 0= if c BUILD-EMBED-CONFIG then
   c EMBED-CONFIG$ ;

\ Ollama's native /api/embed at endpoint. identity names the model artifact,
\ such as its manifest digest, and is the vector space's identity: a changed
\ artifact needs a new one even under the same tag, and the endpoint is a
\ producer input outside it. Requests send truncate:false because Ollama
\ truncates by default; a context overflow fails the call.
: OLLAMA! ( ptr u8 n ptr u8 n ptr u8 n n ptr call -- )
   {: endpoint:ptr endpoint-u:n model:ptr model-u:n identity:ptr identity-u:n dims:n c :}
   c REQUIRE-REST
   dims 0 <= if E-CONFIG throw then
   endpoint endpoint-u BASE-URL? 0= if E-CONFIG throw then
   endpoint endpoint-u c ENDPOINT-BUF REPLACE-SOME
   model model-u c EMBED-BUF REPLACE-SOME
   identity identity-u c IDENTITY-BUF REPLACE-SOME
   dims c MODEL-CALL:DIMS !
   OLLAMA c MODEL-CALL:EMBEDDING-KIND !
   construct call-kind embed c REPLAYED? if
      OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   then ;

private

: PART-OPEN ( ptr call -- )
   {: c :}
   c PART-WRITER c PARTS-BUF JSON:WRITE-OPEN-BUF JSON:WRITE-ARRAY-START drop
   c JOURNAL-PART-WRITER c JOURNAL-PARTS-BUF JSON:WRITE-OPEN-BUF
   JSON:WRITE-ARRAY-START drop ;

: PART-FINISH ( ptr call -- )
   {: c :}
   c REQUIRE-BUILDING
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c PART-WRITER JSON:WRITE-ARRAY-END JSON:WRITE-CLOSE
   c JOURNAL-PART-WRITER JSON:WRITE-ARRAY-END JSON:WRITE-CLOSE
   construct input-state finished c MODEL-CALL:CHAT-INPUT ! ;

\ Finishes a building input and passes a finished one, as a count or a
\ compaction leaves it.
: FINISH-INPUT ( ptr call -- )
   {: c :}
   c MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF E-STATE throw ENDOF
      building OF c PART-FINISH ENDOF
      broken OF E-STATE throw ENDOF
      finished OF ENDOF
   ;MATCH ;

: PART-COMMA ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 0 > if
      c PART-WRITER JSON:WRITE-COMMA drop
      c JOURNAL-PART-WRITER JSON:WRITE-COMMA drop
   then ;

: PART-ADDED ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 1+ c MODEL-CALL:PARTS-COUNT !
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

: TEXT-PART ( ptr u8 n ptr JSON:writer -- )
   {: a:ptr u:n w :}
   w JSON:WRITE-OBJECT-START
   s" type" s" input_text" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" text" a u JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop ;

\ Drops the input's parts, their journal copy and its items.
: EMPTY-INPUT ( ptr call -- )
   {: c :}
   c PARTS-BUF c RESET-SCRATCH
   c JOURNAL-PARTS-BUF c RESET-SCRATCH
   c ITEMS-BUF c RESET-SCRATCH
   0 c MODEL-CALL:PARTS-COUNT ! ;

public

: PARTS-RESET ( ptr call -- )
   {: c :}
   c UNPREPARE drop
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   0 c MODEL-CALL:HTTP-STATUS !
   c EMPTY-INPUT
   c PART-OPEN
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

: TEXT+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   c REQUIRE-BUILDING
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
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
   c UNPREPARE drop
   c REQUIRE-BUILDING
   mime mu IMAGE-MIME? 0= if E-CONFIG throw then
   data du c IMAGE-DIGEST {: hex:ptr hex-u:n :}
   c URI-BUF BUF:CLEAR
   s" data:" BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   mime mu BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   s" ;base64," BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   data du BUF:N>BLEN c URI-BUF BUF:APPEND-SPAN
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c PART-COMMA
   c PART-WRITER
   JSON:WRITE-OBJECT-START
   s" type" s" input_image" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" image_url" c URI-BUF BUFFER$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c JOURNAL-PART-WRITER
   JSON:WRITE-OBJECT-START
   s" type" s" image" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" media_type" mime mu JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" sha256" hex hex-u JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c URI-BUF BUF:CLEAR
   c URI-BUF c RESET-SCRATCH
   c PART-ADDED ;

private

\ A fault of the JSON reader: bytes that are not the JSON read expects.
: JSON-FAULT? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-FIRST <= code E-JSON-READ-LAST >= and ;

: JSON-OBJECT? ( ptr u8 n ptr call -- bool )
   {: a:ptr u:n c :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if
      JSON:READ-SKIP-VALUE
      JSON:READ-NEXT JSON:T-END = {: valid:bool :}
      JSON:READ-CLOSE valid exit
   then
   JSON:READ-CLOSE false ;

: OBJECT-STEP ( ptr u8 n ptr call -- ptr u8 n ptr call )
   {: a:ptr u:n c :}
   a u c JSON-OBJECT? 0= if E-CONFIG throw then
   a u c ;

\ Bytes that are not exactly one JSON object, a reader fault included, are
\ E-CONFIG.
: CHECK-OBJECT ( ptr u8 n ptr call -- )
   [: OBJECT-STEP ;] catch {: code:n :} drop 2drop
   code JSON-FAULT? if E-CONFIG throw then
   code 0<> if code throw then ;

: JSON-TOOLS? ( ptr u8 n ptr call -- bool )
   {: a:ptr u:n c :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if JSON:READ-CLOSE false exit then
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE false exit then
   begin JSON:READ-SKIP-VALUE JSON:READ-NEXT dup JSON:T-OBJ = while drop repeat
   JSON:T-ARR-END = if JSON:READ-NEXT JSON:T-END = else false then
   {: valid:bool :}
   JSON:READ-CLOSE valid ;

: TOOLS-STEP ( ptr u8 n ptr call -- ptr u8 n ptr call )
   {: a:ptr u:n c :}
   a u c JSON-TOOLS? 0= if E-CONFIG throw then
   a u c ;

\ Bytes that are not one non-empty JSON array of objects, a reader fault
\ included, are E-CONFIG.
: CHECK-TOOLS ( ptr u8 n ptr call -- )
   [: TOOLS-STEP ;] catch {: code:n :} drop 2drop
   code JSON-FAULT? if E-CONFIG throw then
   code 0<> if code throw then ;

\ Appends an item's bytes to the items, after a single comma once there are
\ any.
: ITEM-JOIN ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c ITEMS-BUF BUF:LEN@ BUF:BLEN>N 0<> if
      s" ," BUF:N>BLEN c ITEMS-BUF BUF:APPEND-SPAN
   then
   a u BUF:N>BLEN c ITEMS-BUF BUF:APPEND-SPAN ;

public

\ One history item in OpenAI's shape; E-CONFIG unless one JSON object. Items
\ follow the parts' message, in the order added, verbatim in body and journal.
: ITEM+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c UNPREPARE drop
   c REQUIRE-BUILDING
   a u c CHECK-OBJECT
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   a u c ITEM-JOIN
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

private

\ A request's input: a model, instructions and at least one part or item, else
\ E-CONFIG.
: CHECK-INPUT ( ptr call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 0= c ITEMS$ nip 0= and {: empty:bool :}
   c GEN$ nip 0=
   c SYSTEM$ nip 0= or empty or if
      E-CONFIG throw
   then ;

: CHECK-CHAT ( ptr call -- )
   {: c :}
   c CHECK-INPUT
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF E-CONFIG throw ENDOF
      schema OF c FORM$ c CHECK-OBJECT ENDOF
      tools OF c FORM$ c CHECK-TOOLS ENDOF
   ;MATCH ;

: BODY-OPEN ( ptr call -- )
   {: c :}
   c BODY-BUF c RESET-SCRATCH
   c WRITER c BODY-BUF JSON:WRITE-OPEN-BUF drop ;

: BODY-DONE ( ptr call -- )
   {: c :}
   c WRITER JSON:WRITE-CLOSE ;

\ The members after a schema call's input: its strict JSON schema format.
: SCHEMA-TAIL ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   JSON:WRITE-COMMA
   s" text" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" format" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" type" s" json_schema" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" name" s" answer" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" strict" true JSON:WRITE-FIELD-BOOL
   JSON:WRITE-COMMA
   s" schema" c FORM$ JSON:WRITE-FIELD-RAW
   JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END ;

\ The members after a tools call's input: one required call to one of its
\ tools.
: TOOL-MEMBERS ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   JSON:WRITE-COMMA s" tools" c FORM$ JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" tool_choice" s" required" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" parallel_tool_calls" false JSON:WRITE-FIELD-BOOL ;

\ The effort member, when the call sends one.
: EFFORT-MEMBER ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   c EFFORT$ nip 0<> if
      JSON:WRITE-COMMA s" reasoning" JSON:WRITE-KEY
      JSON:WRITE-OBJECT-START
      s" effort" c EFFORT$ JSON:WRITE-FIELD-S
      JSON:WRITE-OBJECT-END
   then ;

\ The prompt cache key and the service tier, each when the call sets one.
: SERVICE-MEMBERS ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   c CACHE-KEY$ nip 0<> if
      JSON:WRITE-COMMA s" prompt_cache_key" c CACHE-KEY$ JSON:WRITE-FIELD-S
   then
   c TIER$ nip 0<> if
      JSON:WRITE-COMMA s" service_tier" c TIER$ JSON:WRITE-FIELD-S
   then ;

\ The output limit member, when the call sets one.
: OUTPUT-MEMBER ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   c MAX-OUTPUT-CELL @ 0 > if
      JSON:WRITE-COMMA s" max_output_tokens" c MAX-OUTPUT-CELL @ JSON:WRITE-FIELD-U
   then ;

\ The instructions and the input: the parts' message, then the items.
: INPUT-MEMBERS ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   JSON:WRITE-COMMA s" instructions" c SYSTEM$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" input" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START
   c MODEL-CALL:PARTS-COUNT @ 0 > {: head:bool :}
   head if
      JSON:WRITE-OBJECT-START
      s" role" s" user" JSON:WRITE-FIELD-S
      JSON:WRITE-COMMA
      s" content" c PARTS$ JSON:WRITE-FIELD-RAW
      JSON:WRITE-OBJECT-END
   then
   c ITEMS$ nip 0<> if
      head if JSON:WRITE-COMMA then
      c ITEMS$ JSON:WRITE-RAW
   then
   JSON:WRITE-ARRAY-END ;

: CHAT-BODY ( ptr call -- )
   {: c :}
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c GEN$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" store" false JSON:WRITE-FIELD-BOOL
   JSON:WRITE-COMMA s" stream" true JSON:WRITE-FIELD-BOOL
   c EFFORT-MEMBER
   c SERVICE-MEMBERS
   c OUTPUT-MEMBER
   c INPUT-MEMBERS
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF E-CONFIG throw ENDOF
      schema OF c SCHEMA-TAIL ENDOF
      tools OF
         \ The reasoning items a later turn sends back.
         c TOOL-MEMBERS
         JSON:WRITE-COMMA s" include" JSON:WRITE-KEY
         JSON:WRITE-ARRAY-START
         s" reasoning.encrypted_content" JSON:WRITE-STRING
         JSON:WRITE-ARRAY-END
      ENDOF
   ;MATCH
   JSON:WRITE-OBJECT-END drop
   c BODY-DONE ;

\ A count's body: the chat body's members the count endpoint takes, without
\ store, stream, include, prompt_cache_key, service_tier or max_output_tokens.
: COUNT-BODY ( ptr call -- )
   {: c :}
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c GEN$ JSON:WRITE-FIELD-S
   c EFFORT-MEMBER
   c INPUT-MEMBERS
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF E-CONFIG throw ENDOF
      schema OF c SCHEMA-TAIL ENDOF
      tools OF c TOOL-MEMBERS ENDOF
   ;MATCH
   JSON:WRITE-OBJECT-END drop
   c BODY-DONE ;

\ The compact endpoint rejects tools, reasoning, text, store, stream and
\ include.
: COMPACT-BODY ( ptr call -- )
   {: c :}
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c GEN$ JSON:WRITE-FIELD-S
   c SERVICE-MEMBERS
   c INPUT-MEMBERS
   JSON:WRITE-OBJECT-END drop
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
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c EMBED-MODEL$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" dimensions" c MODEL-CALL:DIMS @ JSON:WRITE-FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON:WRITE-COMMA
      s" truncate" false JSON:WRITE-FIELD-BOOL
   else
      JSON:WRITE-COMMA
      s" encoding_format" s" base64" JSON:WRITE-FIELD-S
   then
   JSON:WRITE-COMMA
   s" input" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START
   c MODEL-CALL:INPUT-COUNT @ 0 ?do
      i 0 > if JSON:WRITE-COMMA then
      i c INPUT$ JSON:WRITE-STRING
   loop
   JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END drop
   c BODY-DONE ;

\ A change to the inputs drops the vectors they gave; a chat's content stays.
: DROP-VECTORS ( ptr call -- )
   {: c :}
   c MODEL-CALL:RESULT @ MATCH call-result
      none OF ENDOF
      content OF ENDOF
      vectors OF construct call-result none c MODEL-CALL:RESULT ! ENDOF
   ;MATCH ;

public

: INPUT-RESET ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   0 c MODEL-CALL:HTTP-STATUS !
   c INPUT-BUF BUF:CLEAR
   0 c MODEL-CALL:INPUT-U !
   0 c MODEL-CALL:INPUT-COUNT !
   c DROP-VECTORS ;

: INPUT+ ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   c REQUIRE-REST
   u 0= if E-CONFIG throw then
   c MODEL-CALL:INPUT-COUNT @ INPUT-MAX >= if E-CAPACITY throw then
   c MODEL-CALL:INPUT-U @ c c MODEL-CALL:INPUT-COUNT @ INPUT-OFF !
   u c c MODEL-CALL:INPUT-COUNT @ INPUT-LEN !
   a u BUF:N>BLEN c INPUT-BUF BUF:APPEND-SPAN
   c MODEL-CALL:INPUT-U @ u + c MODEL-CALL:INPUT-U !
   c MODEL-CALL:INPUT-COUNT @ 1+ c MODEL-CALL:INPUT-COUNT !
   c DROP-VECTORS ;

private

\ The member that carries the form's text in a chat's journal input.
: FORM-KEY$ ( ptr call -- ptr u8 n )
   MODEL-CALL:FORM @ MATCH reply-form
      none OF E-CONFIG throw ENDOF
      schema OF s" schema" ENDOF
      tools OF s" tools" ENDOF
   ;MATCH ;

\ A generation kind's journal kind and its endpoint; an embedding's is
\ JOURNAL-EMBED-INPUT.
: GEN-INPUT-KIND ( call-kind ptr call -- ptr u8 n ptr u8 n )
   {: k:call-kind c :}
   k MATCH call-kind
      chat OF s" generation" c CHAT-URL$ ENDOF
      embed OF E-STATE throw ENDOF
      tally OF s" input_tokens" c COUNT-URL$ ENDOF
      compaction OF s" compaction" c COMPACT-URL$ ENDOF
   ;MATCH ;

\ A generation call's journal input for the kind.
: JOURNAL-GEN-INPUT ( call-kind ptr call -- )
   {: k:call-kind c :}
   k c GEN-INPUT-KIND {: kind:ptr kind-u:n url:ptr url-u:n :}
   k SHAPED? {: shaped:bool :}
   c JOURNAL-INPUT-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-INPUT-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" kind" kind kind-u JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" configuration" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" backend" s" openai-responses" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c GEN$ JSON:WRITE-FIELD-S
   shaped c EFFORT$ nip 0<> and if
      JSON:WRITE-COMMA s" effort" c EFFORT$ JSON:WRITE-FIELD-S
   then
   k LIMITED? if c OUTPUT-MEMBER then
   JSON:WRITE-COMMA s" endpoint" url url-u JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END
   JSON:WRITE-COMMA s" instructions" c SYSTEM$ JSON:WRITE-FIELD-S
   shaped if JSON:WRITE-COMMA c FORM-KEY$ c FORM$ JSON:WRITE-FIELD-RAW then
   JSON:WRITE-COMMA s" parts" c JOURNAL-PARTS$ JSON:WRITE-FIELD-RAW
   c ITEMS$ nip 0<> if
      JSON:WRITE-COMMA s" items" JSON:WRITE-KEY
      JSON:WRITE-ARRAY-START c ITEMS$ JSON:WRITE-RAW JSON:WRITE-ARRAY-END
   then
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

: JOURNAL-EMBED-INPUT ( ptr call -- )
   {: c :}
   c JOURNAL-INPUT-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-INPUT-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" kind" s" embedding" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" configuration" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" backend" c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      s" ollama" else s" openai-embeddings" then JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c EMBED-MODEL$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" dimensions" c MODEL-CALL:DIMS @ JSON:WRITE-FIELD-U
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      JSON:WRITE-COMMA s" identity" c IDENTITY$ JSON:WRITE-FIELD-S
      JSON:WRITE-COMMA s" truncate" false JSON:WRITE-FIELD-BOOL
      JSON:WRITE-COMMA s" endpoint" c ENDPOINT$ JSON:WRITE-FIELD-S
   else
      JSON:WRITE-COMMA s" endpoint" c EMBED-URL$ JSON:WRITE-FIELD-S
   then
   JSON:WRITE-OBJECT-END
   JSON:WRITE-COMMA s" input" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START
   c MODEL-CALL:INPUT-COUNT @ 0 ?do
      i 0 > if JSON:WRITE-COMMA then
      i c INPUT$ JSON:WRITE-STRING
   loop
   JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

: TRANSPORT-FAILED ( CURL:code ptr call -- )
   LEDGER {: code:CURL:code l :}
   code CURL:CODE>N l LEDGER-TRANSPORT !
   E-TRANSPORT throw ;

: CHECK-STATUS ( CURL:status ptr call -- )
   {: s:CURL:status c :}
   s MATCH CURL:status
      ok OF ENDOF
      failed OF c TRANSPORT-FAILED ENDOF
   ;MATCH ;

: OPEN-HANDLE ( ptr call -- CURL:handle )
   {: c :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF c TRANSPORT-FAILED ENDOF
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
   2dup KEY-LINE CURL:HEADER+ over CHECK-STATUS ;

: SET-HEADERS ( CURL:handle ptr call -- )
   {: h:CURL:handle c :}
   h s" Content-Type: application/json" CURL:HEADER+ c CHECK-STATUS
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF true ENDOF
      embed OF c MODEL-CALL:EMBEDDING-KIND @ OLLAMA <> ENDOF
      tally OF true ENDOF
      compaction OF true ENDOF
   ;MATCH if
      h c [: KEY-HEADER-KEEP ;] catch {: fault:n :} 2drop
      fault c WIPE-AFTER
   then ;

\ Starts an exchange as the call's transport, which CANCEL ends.
: START-LOCKED ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:CANCELLED @ if E-CANCELLED throw then
   c MODEL-CALL:TRANSPORT @ MATCH option
      none OF ENDOF
      some OF drop E-STATE throw ENDOF
   ;MATCH
   c MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF {: h:CURL:handle :}
         h CURL:STREAM-START c CHECK-STATUS
         h OPTION:SOME c MODEL-CALL:TRANSPORT !
         c LEDGER LEDGER-DISPATCHED @ 0= if c DISPATCHED then
      ENDOF
   ;MATCH
   c ;

: START-TRANSFER ( ptr call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: START-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> if code throw then ;

\ Clears the call's transfer, keeping in the receipt whether a CANCEL cut it:
\ CANCEL-LOCKED clears the transfer it cuts.
: END-TRANSFER ( ptr call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:TRANSPORT @ MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH c RECEIPT RECEIPT-CUT !
   OPTION:NONE c MODEL-CALL:TRANSPORT !
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

: WIRE-READER ( ptr call -- JSON:reader )
   {: c :}
   c MODEL-CALL:READER c RESPONSE$ JSON:READ-INIT ;

\ The reply's current string, decoded into the content buffer, which a failed
\ call leaves unused.
: REPLY-STR ( JSON:reader ptr call -- JSON:reader ptr u8 n )
   {: c :}
   c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JSON:READ-STR {: u:n :}
   c CONTENT-BUF HELD u SPAN:TAKE SPAN:$ ;

\ Ollama's error text for an input longer than the model's context.
: OLLAMA-SIZE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" exceeds the context length" CONTAINS?
   a u s" exceeds maximum context length" CONTAINS? or ;

\ Whether a member of OpenAI's error object, the reader at its value, names a
\ refusal for size: by its code, or by its message where the code is null, as
\ in an embeddings refusal.
: SIZE-MEMBER? ( JSON:reader ptr u8 n ptr call -- JSON:reader bool )
   {: key:ptr key-u:n c :}
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-SKIP-VALUE false exit then
   key key-u s" code" STR= if
      s" context_length_exceeded" JSON:READ-STR-EQ? if true exit then
      s" string_above_max_length" JSON:READ-STR-EQ? exit
   then
   key key-u s" message" STR= if
      c REPLY-STR s" This model's maximum context length" STARTS-WITH? exit
   then
   false ;

: OPENAI-SIZE? ( JSON:reader ptr call -- JSON:reader bool )
   {: c :}
   begin JSON:READ-NEXT dup JSON:T-OBJ-END <> while
      drop
      JSON:READ-SPAN$ {: key:ptr key-u:n :}
      JSON:READ-NEXT drop
      key key-u c SIZE-MEMBER? if true exit then
   repeat
   drop false ;

\ The reader at the reply's error member.
: SIZE-ERROR? ( JSON:reader ptr call -- JSON:reader bool )
   {: c :}
   JSON:READ-TOKEN JSON:T-STR = if c REPLY-STR OLLAMA-SIZE? exit then
   JSON:READ-TOKEN JSON:T-OBJ = if c OPENAI-SIZE? exit then
   false ;

\ The provider's message, kept from the reply's error member, the reader at
\ it: the member itself when a string, as Ollama sends it, else its message,
\ as OpenAI and Azure send it.
: ERROR-TEXT ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   JSON:READ-TOKEN JSON:T-OBJ = if
      s" message" JSON:READ-FIND-KEY 0= if exit then
   then
   JSON:READ-TOKEN JSON:T-STR <> if exit then
   c REPLY-STR BUF:N>BLEN c ERROR-BUF BUF:REPLACE ;

: ERROR-BODY ( ptr call -- ptr call )
   {: c :}
   c WIRE-READER
   JSON:READ-NEXT JSON:T-OBJ = if
      s" error" JSON:READ-FIND-KEY if c ERROR-TEXT then
   then
   JSON:READ-CLOSE c ;

\ Keeps a failed reply's provider message. A body that is not such JSON has
\ none.
: KEEP-ERROR ( ptr call -- )
   {: c :}
   c [: ERROR-BODY ;] catch {: code:n :} drop
   code JSON-FAULT? if exit then
   code 0<> if code throw then ;

: SIZE-BODY? ( ptr call -- bool )
   {: c :}
   c WIRE-READER
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE false exit then
   s" error" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE false exit then
   c SIZE-ERROR? {: size:bool :}
   JSON:READ-CLOSE size ;

\ Whether a failed reply refuses the request for its size: HTTP 413, or a 400
\ whose body names the refusal. A body that is not such JSON is not one.
: SIZE-REFUSAL? ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:HTTP-STATUS @ 413 = if true exit then
   c MODEL-CALL:HTTP-STATUS @ 400 <> if false exit then
   c false [: drop dup SIZE-BODY? ;] catch {: code:n :}
   code 0= if nip exit then
   2drop
   code JSON-FAULT? if false exit then
   code throw ;


\ Room in the response buffer for at least one byte past those received.
: GROW-REPLY ( ptr call -- ptr call )
   {: c :}
   c RESPONSE-BUF {: buf:ptr :}
   buf buf BUF:LEN@ BUF:BLEN>N 1+ BUF:N>BLEN BUF:ENSURE
   c ;


\ The fault a refused reply's status names when its body names no refusal for
\ size.
: STATUS-FAULT ( CURL:http-status -- n )
   {: status:CURL:http-status :}
   status CURL:UNAVAILABLE? if E-UNAVAILABLE exit then
   status CURL:HTTP-STATUS>N {: code:n :}
   code 401 = code 403 = or if E-KEY exit then
   code 404 = if E-CONFIG exit then
   E-HTTP ;


\ Keeps the transfer's status and reply; throws the fault of a cut or failed
\ exchange or a refused reply.
: RECEIPT-CHECK ( ptr call -- ptr call )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-STATUS @ r RECEIPT-CODE @ r RECEIPT-FAULT @ r RECEIPT-CUT @
   {: status:CURL:http-status rc:n fault:n cut:bool :}
   status CURL:HTTP-STATUS>N c MODEL-CALL:HTTP-STATUS !
   c RESPONSE-BUF BUF:LEN@ BUF:BLEN>N c MODEL-CALL:RESPONSE-U !
   cut if E-CANCELLED throw then
   fault 0<> if fault throw then
   rc c LEDGER LEDGER-TRANSPORT !
   rc 0<> if E-TRANSPORT throw then
   status CURL:HTTP-STATUS>N 200 <> if
      c KEEP-ERROR
      c SIZE-REFUSAL? if E-CAPACITY throw then
      status STATUS-FAULT throw
   then
   c ;

: CALL-HANDLE ( ptr call -- CURL:handle )
   MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ Sets the next exchange: a POST of the body to the url.
: AS-POST ( ptr u8 n ptr u8 n ptr call -- )
   {: url:ptr url-u:n body:ptr body-u:n c :}
   c CALL-HANDLE {: h:CURL:handle :}
   h url url-u CURL:URL! c CHECK-STATUS
   h s" POST" CURL:METHOD! c CHECK-STATUS
   h body body-u CURL:BODY! c CHECK-STATUS ;

: EXPECT ( JSON:reader n -- JSON:reader )
   {: want:n :}
   JSON:READ-NEXT want <> if JSON:READ-CLOSE E-RESPONSE throw then ;

: DESCEND ( JSON:reader ptr u8 n -- JSON:reader )
   {: key:ptr key-u:n :}
   key key-u JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE E-RESPONSE throw then ;

\ The reader at the reply's member under the key.
: REPLY-AT ( ptr u8 n ptr call -- JSON:reader )
   {: key:ptr key-u:n c :}
   c WIRE-READER
   JSON:T-OBJ EXPECT
   key key-u DESCEND ;

\ The reader at the string type of the object it was at.
: TYPE-AT ( JSON:reader -- JSON:reader )
   s" type" DESCEND
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then ;

\ Moves the reader from an array's opener to its element at the index, an
\ object; false past the array's end.
: ELEMENT ( JSON:reader n -- JSON:reader bool )
   0 ?do
      JSON:READ-NEXT JSON:T-ARR-END = if false unloop exit then
      JSON:READ-SKIP-VALUE
   loop
   JSON:READ-NEXT dup JSON:T-ARR-END = if drop false exit then
   JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   true ;

\ The reader past the opener of the reply's output array.
: OUTPUT-AT ( ptr call -- JSON:reader )
   {: c :}
   s" output" c REPLY-AT
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE E-RESPONSE throw then ;

\ The reader at the output item at the index; false past the last item.
: ITEM ( n ptr call -- JSON:reader bool )
   {: idx:n c :}
   c OUTPUT-AT idx ELEMENT ;

\ The reader at the content part at the index of the message item at the
\ index; false past the last part.
: PART ( n n ptr call -- JSON:reader bool )
   {: idx:n part:n c :}
   idx c ITEM drop
   s" content" DESCEND
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE E-RESPONSE throw then
   part ELEMENT ;

\ The text of the answer message's output_text part (ANSWER?) is the content;
\ a reply holds one, so a second, in any answer message, is E-RESPONSE.
: READ-TEXT ( n n ptr call -- )
   {: idx:n part:n c :}
   c MODEL-CALL:CONTENT-SEEN @ if E-RESPONSE throw then
   idx part c PART drop
   s" text" DESCEND
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JSON:READ-STR c MODEL-CALL:CONTENT-U !
   JSON:READ-CLOSE
   true c MODEL-CALL:CONTENT-SEEN ! ;

\ Reads the message's content part at the index; false past the last part.
: READ-PART ( n n ptr call -- bool )
   {: idx:n part:n c :}
   idx part c PART 0= if JSON:READ-CLOSE false exit then
   TYPE-AT
   s" refusal" JSON:READ-STR-EQ? if JSON:READ-CLOSE E-REFUSED throw then
   s" output_text" JSON:READ-STR-EQ? {: text:bool :}
   JSON:READ-CLOSE
   text if idx part c READ-TEXT then
   true ;

\ Whether the message item at the index is the answer: no phase, null or
\ final_answer is; commentary is a progress note; any other is E-RESPONSE.
: ANSWER? ( n ptr call -- bool )
   {: idx:n c :}
   idx c ITEM drop
   s" phase" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE true exit then
   JSON:READ-TOKEN dup JSON:T-NULL = if drop JSON:READ-CLOSE true exit then
   JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" final_answer" JSON:READ-STR-EQ? {: final:bool :}
   s" commentary" JSON:READ-STR-EQ? {: note:bool :}
   JSON:READ-CLOSE
   final note or 0= if E-RESPONSE throw then
   final ;

\ Reads the output item at the index if it is an answer message; false past
\ the last. Members come in any order, so type and phase each take a pass.
: READ-ITEM ( n ptr call -- bool )
   {: idx:n c :}
   idx c ITEM 0= if JSON:READ-CLOSE false exit then
   TYPE-AT s" message" JSON:READ-STR-EQ? {: message:bool :}
   JSON:READ-CLOSE
   message if
      idx c ANSWER? if
         0 begin idx over c READ-PART while 1+ repeat drop
      then
   then
   true ;

: READ-OUTPUT ( ptr call -- )
   {: c :}
   false c MODEL-CALL:CONTENT-SEEN !
   0 c MODEL-CALL:CONTENT-U !
   0 begin dup c READ-ITEM while 1+ repeat drop
   c MODEL-CALL:CONTENT-SEEN @ 0= if E-RESPONSE throw then ;

\ Reads the content part at the index of the message item at the index; false
\ past the last part, E-REFUSED on a refusal.
: PART-REFUSED ( n n ptr call -- bool )
   {: idx:n part:n c :}
   idx part c PART 0= if JSON:READ-CLOSE false exit then
   TYPE-AT s" refusal" JSON:READ-STR-EQ? {: refused:bool :}
   JSON:READ-CLOSE
   refused if E-REFUSED throw then
   true ;

\ A message in a tools reply: E-REFUSED if a part refuses, else E-RESPONSE.
: TOOL-MESSAGE ( n ptr call -- )
   {: idx:n c :}
   0 begin idx over c PART-REFUSED while 1+ repeat drop
   E-RESPONSE throw ;

\ Counts a function call at the index and skips reasoning; a message is
\ TOOL-MESSAGE's, any other type E-RESPONSE; false past the last item.
: READ-TOOL ( n n ptr call -- n n bool )
   {: idx:n count:n c :}
   idx c ITEM 0= if JSON:READ-CLOSE idx count false exit then
   TYPE-AT
   s" function_call" JSON:READ-STR-EQ? {: called:bool :}
   s" reasoning" JSON:READ-STR-EQ? {: thought:bool :}
   s" message" JSON:READ-STR-EQ? {: message:bool :}
   JSON:READ-CLOSE
   message if idx c TOOL-MESSAGE then
   called thought or 0= if E-RESPONSE throw then
   idx count called if 1+ then true ;

\ A tools reply answers exactly one function call, read whole by OUTPUT$; it
\ leaves no content.
: READ-TOOLS ( ptr call -- )
   {: c :}
   0 c MODEL-CALL:CONTENT-U !
   0 0 begin c READ-TOOL while swap 1+ swap repeat nip
   1 <> if E-RESPONSE throw then ;

\ An incomplete reply keeps its reason in the content buffer: it has no
\ content, and its decoded reason is no longer than the response that holds it.
: READ-INCOMPLETE ( ptr call -- )
   {: c :}
   s" incomplete_details" c REPLY-AT
   JSON:READ-TOKEN JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" reason" DESCEND
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   c MODEL-CALL:RESPONSE-U @ c CONTENT-BUF ROOM SPAN:$ JSON:READ-STR c MODEL-CALL:INCOMPLETE-U !
   JSON:READ-CLOSE E-INCOMPLETE throw ;

: ERROR-OBJECT ( ptr call -- JSON:reader )
   {: c :}
   s" error" c REPLY-AT
   JSON:READ-TOKEN JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then ;

\ The fault a failed reply's error code names, the reader at its error object.
: CODE-FAULT ( JSON:reader -- JSON:reader n )
   s" code" JSON:READ-FIND-KEY 0= if E-HTTP exit then
   JSON:READ-TOKEN JSON:T-STR <> if E-HTTP exit then
   s" context_length_exceeded" JSON:READ-STR-EQ? if E-CAPACITY exit then
   s" rate_limit_exceeded" JSON:READ-STR-EQ? if E-UNAVAILABLE exit then
   s" server_error" JSON:READ-STR-EQ? if E-UNAVAILABLE exit then
   E-HTTP ;

\ A failed reply keeps its error message as the call's error text.
: READ-FAILURE ( ptr call -- )
   {: c :}
   c ERROR-OBJECT c ERROR-TEXT JSON:READ-CLOSE
   c ERROR-OBJECT CODE-FAULT {: fault:n :}
   JSON:READ-CLOSE fault throw ;

\ Throws the fault of a reply whose status is other than completed.
: REPLY-STATUS ( ptr call -- )
   {: c :}
   s" status" c REPLY-AT
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" incomplete" JSON:READ-STR-EQ? if JSON:READ-CLOSE c READ-INCOMPLETE then
   s" failed" JSON:READ-STR-EQ? if JSON:READ-CLOSE c READ-FAILURE then
   s" completed" JSON:READ-STR-EQ? {: done:bool :}
   JSON:READ-CLOSE
   done 0= if E-RESPONSE throw then ;

: ACCEPT-CHAT ( ptr call -- )
   {: c :}
   c RESPONSE$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c REPLY-STATUS
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF E-RESPONSE throw ENDOF
      schema OF
         c READ-OUTPUT
         c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
         c JSON-OBJECT? 0= if E-RESPONSE throw then
      ENDOF
      tools OF c READ-TOOLS ENDOF
   ;MATCH ;

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

\ A vector's length in bytes.
: VECTOR-BYTES ( ptr call -- n ) MODEL-CALL:DIMS @ 4 * ;

\ Whether binary32 bits have an exponent field of all ones: NaN or infinity.
: NONFINITE? ( n -- bool ) $7F800000 and $7F800000 = ;


\ Grows the vectors by one vector's room, which VECTOR-AT marks, and answers
\ its address.
: VECTOR-ROOM ( ptr call -- ptr u8 )
   {: c :}
   c VECTORS-U c MODEL-CALL:VECTOR-AT !
   c VECTORS-BUF c VECTOR-BYTES BUF:N>BLEN BUF:GROW SPAN:$ drop ;


\ A JSON array of DIMS numbers, each narrowed to binary32.
: READ-NUMBERS ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE E-RESPONSE throw then
   0 c MODEL-CALL:COMPONENT-COUNT !
   c VECTOR-ROOM {: v:ptr :}
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      dup JSON:T-INT = swap JSON:T-FLOAT = or 0= if
         JSON:READ-CLOSE E-RESPONSE throw
      then
      c MODEL-CALL:COMPONENT-COUNT @ {: idx:n :}
      idx c MODEL-CALL:DIMS @ >= if JSON:READ-CLOSE E-RESPONSE throw then
      JSON:READ-SPAN$ F64-TEXT:PARSE MATCH result
         ok OF F32:NARROW ENDOF
         err OF drop JSON:READ-CLOSE E-RESPONSE throw ENDOF
      ;MATCH {: bits:n :}
      bits NONFINITE? if JSON:READ-CLOSE E-RESPONSE throw then
      bits v idx 4 * + F32-BUF:STORE
      idx 1+ c MODEL-CALL:COMPONENT-COUNT !
   repeat
   drop
   c MODEL-CALL:COMPONENT-COUNT @ c MODEL-CALL:DIMS @ <> if
      JSON:READ-CLOSE E-RESPONSE throw
   then
   true c MODEL-CALL:VECTOR-SEEN ! ;


\ Decodes base64 a u into the vector's room, answering the decoded length in
\ place of the n under it, so a catch keeps the shape.
: DECODE-VECTOR ( n ptr u8 n ptr call -- n ptr u8 n ptr call )
   {: a:ptr u:n c :}
   drop
   a u c VECTORS-BUF BUF:SPAN c MODEL-CALL:VECTOR-AT @ c VECTOR-BYTES SPAN:SUB
   BASE64:DECODE a u c ;


\ Base64 of DIMS little-endian binary32 values. An escaped string is unescaped
\ into the room before its vector, unused until the next reply.
: READ-BASE64 ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   JSON:READ-SPAN$ {: a:ptr u:n :}
   a u [char] \ INDEX-OF MATCH option
      none OF a u c VECTOR-ROOM drop ENDOF
      some OF
         drop c VECTORS-U {: t:n :}
         c VECTORS-BUF u BUF:N>BLEN BUF:GROW SPAN:$ JSON:READ-STR {: m:n :}
         c VECTOR-ROOM drop
         c VECTORS-BUF BUF:SPAN SPAN:$ drop t + m
      ENDOF
   ;MATCH
   0 -rot c [: DECODE-VECTOR ;] catch {: code:n :}
   code 0<> if 2drop 2drop JSON:READ-CLOSE E-RESPONSE throw then
   2drop drop {: len:n :}
   len c VECTOR-BYTES <> if JSON:READ-CLOSE E-RESPONSE throw then
   c VECTORS-BUF BUF:SPAN SPAN:$ drop c MODEL-CALL:VECTOR-AT @ + {: v:ptr :}
   c MODEL-CALL:DIMS @ 0 ?do
      v i 4 * + F32-BUF:LOAD NONFINITE? if JSON:READ-CLOSE E-RESPONSE throw then
   loop
   true c MODEL-CALL:VECTOR-SEEN ! ;


: EMBED-MEMBER ( JSON:reader ptr u8 n ptr call -- JSON:reader )
   {: key:ptr key-u:n c :}
   key key-u s" index" STR= if
      c MODEL-CALL:PARSED-INDEX @ -1 <> if JSON:READ-CLOSE E-RESPONSE throw then
      JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE E-RESPONSE throw then
      JSON:READ-INT {: idx:n :}
      idx 0 < idx c MODEL-CALL:INPUT-COUNT @ >= or if
         JSON:READ-CLOSE E-RESPONSE throw
      then
      idx c MODEL-CALL:PARSED-INDEX ! exit
   then
   key key-u s" embedding" STR= if
      c MODEL-CALL:VECTOR-SEEN @ if JSON:READ-CLOSE E-RESPONSE throw then
      JSON:READ-TOKEN JSON:T-STR = if c READ-BASE64 else c READ-NUMBERS then
      exit
   then
   JSON:READ-SKIP-VALUE ;

: READ-EMBED-OBJECT ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   -1 c MODEL-CALL:PARSED-INDEX !
   false c MODEL-CALL:VECTOR-SEEN !
   begin JSON:READ-NEXT dup JSON:T-OBJ-END <> while
      JSON:T-KEY <> if JSON:READ-CLOSE E-RESPONSE throw then
      JSON:READ-SPAN$ {: key:ptr key-u:n :}
      JSON:READ-NEXT drop
      key key-u c EMBED-MEMBER
   repeat
   drop
   c MODEL-CALL:PARSED-INDEX @ {: idx:n :}
   idx -1 = c MODEL-CALL:VECTOR-SEEN @ 0= or if
      JSON:READ-CLOSE E-RESPONSE throw
   then
   c idx VECTOR-LEN @ 0<> if JSON:READ-CLOSE E-RESPONSE throw then
   idx c VECTOR-KEEP ;

: READ-DATA ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE E-RESPONSE throw then
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
      c READ-EMBED-OBJECT
      c MODEL-CALL:DATA-COUNT @ 1+ c MODEL-CALL:DATA-COUNT !
   repeat
   drop ;

: READ-EMBEDDINGS ( JSON:reader ptr call -- JSON:reader )
   {: c :}
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE E-RESPONSE throw then
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ >= if
         drop JSON:READ-CLOSE E-RESPONSE throw
      then
      drop
      c READ-NUMBERS
      c MODEL-CALL:DATA-COUNT @ c VECTOR-KEEP
      c MODEL-CALL:DATA-COUNT @ 1+ c MODEL-CALL:DATA-COUNT !
   repeat
   drop ;

\ The reader at the reply's top object, under no usage bytes, no usage member
\ and no vectors read. Kept usage has a length: an object's bytes are not empty.
: REPLY-START ( ptr call -- ptr u8 n bool bool JSON:reader )
   {: c :}
   c VECTORS-RESET
   0 c MODEL-CALL:DATA-COUNT !
   s" " false false c WIRE-READER
   JSON:T-OBJ EXPECT ;

\ Whether the reader's key is the first under these bytes, which leaves it at
\ the value; the flag under the reader says whether one came.
: FIRST-KEY? ( bool JSON:reader ptr u8 n -- bool JSON:reader bool )
   {: key:ptr key-u:n :}
   >r {: seen:bool :} r>
   key key-u JSON:READ-STR-EQ? seen 0= and {: hit:bool :}
   hit if JSON:READ-NEXT drop then
   >r seen hit or r> hit ;

\ The reader at a member's key, under the usage bytes and whether a usage member
\ came: the first one is kept there when an object; any other is skipped.
: USAGE-MEMBER ( ptr u8 n bool bool JSON:reader -- ptr u8 n bool bool JSON:reader )
   >r {: o:ptr o-u:n came:bool seen:bool :} r>
   s" usage" JSON:READ-STR-EQ? came 0= and {: first:bool :}
   JSON:READ-NEXT JSON:T-OBJ = first and if
      JSON:READ-VALUE-SPAN$ {: v:ptr v-u:n :}
      >r v v-u true seen r> exit
   then
   JSON:READ-SKIP-VALUE
   >r o o-u came first or seen r> ;

\ The usage bytes and whether there are any, once the reader has closed the
\ top object; a reply with no vectors, or not one per input, is refused.
: REPLY-END ( ptr u8 n bool bool JSON:reader ptr call -- ptr u8 n bool )
   {: c :}
   JSON:READ-FINISH
   0= if E-RESPONSE throw then
   c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ <> if
      E-RESPONSE throw
   then
   drop dup 0<> ;

\ An embeddings reply's vectors, and the bytes of its usage object and whether
\ it has one, in one walk of its top object.
: ACCEPT-EMBED ( ptr call -- ptr u8 n bool )
   {: c :}
   c REPLY-START
   begin JSON:READ-NEXT dup JSON:T-OBJ-END <> while
      JSON:T-KEY <> if JSON:READ-CLOSE E-RESPONSE throw then
      s" data" FIRST-KEY? if c READ-DATA else USAGE-MEMBER then
   repeat
   drop c REPLY-END ;

: ACCEPT-OLLAMA ( ptr call -- ptr u8 n bool )
   {: c :}
   c REPLY-START
   begin JSON:READ-NEXT dup JSON:T-OBJ-END <> while
      JSON:T-KEY <> if JSON:READ-CLOSE E-RESPONSE throw then
      s" embeddings" FIRST-KEY? if c READ-EMBEDDINGS else USAGE-MEMBER then
   repeat
   drop c REPLY-END ;

: RESPONSE-ERROR ( n -- )
   {: code:n :}
   code JSON-FAULT? if E-RESPONSE throw then
   code 0<> if code throw then ;

: PARSE-CHAT ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-CHAT ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

\ The bytes of the usage member of the JSON object in the bytes, and whether
\ it is there as an object; no bytes when it is absent or not one.
: USAGE ( ptr u8 n ptr call -- ptr u8 n bool )
   {: a:ptr u:n c :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:T-OBJ EXPECT
   s" usage" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE a 0 false exit then
   JSON:READ-TOKEN JSON:T-OBJ <> if JSON:READ-CLOSE a 0 false exit then
   JSON:READ-VALUE-SPAN$ {: o:ptr o-u:n :}
   JSON:READ-CLOSE o o-u true ;

\ The current usage count: an integer, or none for null.
: USAGE-COUNT ( JSON:reader -- JSON:reader option<n> )
   JSON:READ-TOKEN dup JSON:T-INT = if drop JSON:READ-INT OPTION:SOME exit then
   JSON:T-NULL <> if JSON:READ-CLOSE E-RESPONSE throw then
   OPTION:NONE ;

\ A count, 0 when absent.
: OR-ZERO ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The usage count at the key: none when the key is absent or null.
: COUNT-AT ( JSON:reader ptr u8 n -- JSON:reader option<n> )
   JSON:READ-FIND-KEY 0= if OPTION:NONE exit then
   USAGE-COUNT ;

\ The count at the key of the JSON object the bytes hold, such as a usage
\ object. Each count reads the object again, as providers order its members
\ freely.
: USAGE-AT ( ptr u8 n ptr call ptr u8 n -- option<n> )
   {: a:ptr u:n c key:ptr key-u:n :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:T-OBJ EXPECT
   key key-u COUNT-AT {: got :}
   JSON:READ-CLOSE got ;

\ The count at the key of a member object of the usage object the bytes hold;
\ none when that member is absent or not an object.
: DETAIL-AT ( ptr u8 n ptr call ptr u8 n ptr u8 n -- option<n> )
   {: a:ptr u:n c detail:ptr detail-u:n key:ptr key-u:n :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:T-OBJ EXPECT
   detail detail-u JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE OPTION:NONE exit then
   JSON:READ-TOKEN JSON:T-OBJ <> if JSON:READ-CLOSE OPTION:NONE exit then
   key key-u COUNT-AT {: got :}
   JSON:READ-CLOSE got ;

\ The bytes' usage object and whether they carry one; marks the call's usage
\ seen.
: USAGE? ( ptr u8 n ptr call -- ptr u8 n bool )
   {: a:ptr u:n c :}
   a u c USAGE {: o:ptr o-u:n present:bool :}
   present if true c TOKENS TOKENS-SEEN ! then
   o o-u present ;

\ A chat or compaction reply's usage counts, in the Responses protocol's
\ names.
: ACCEPT-USAGE ( ptr call -- )
   {: c :}
   c RESPONSE$ c USAGE? {: a:ptr u:n present:bool :}
   present 0= if exit then
   a u c s" input_tokens" USAGE-AT c TOKENS TOKENS-INPUT !
   a u c s" output_tokens" USAGE-AT c TOKENS TOKENS-OUTPUT !
   a u c s" input_tokens_details" s" cached_tokens" DETAIL-AT
   c TOKENS TOKENS-CACHED !
   a u c s" input_tokens_details" s" cache_write_tokens" DETAIL-AT
   c TOKENS TOKENS-WRITTEN !
   a u c s" output_tokens_details" s" reasoning_tokens" DETAIL-AT
   c TOKENS TOKENS-REASONING ! ;

\ An embeddings reply's prompt_tokens, from its usage object's bytes.
: EMBED-TOKENS ( ptr u8 n ptr call -- ptr u8 n ptr call )
   {: a:ptr u:n c :}
   a u c s" prompt_tokens" USAGE-AT c TOKENS TOKENS-INPUT !
   a u c ;

\ The prompt_tokens of a reply the walk rejected, read from the whole reply;
\ the walk's fault is the one passed on.
: FAULT-USAGE ( ptr call -- )
   [: dup >r RESPONSE$ r@ USAGE? if r@ EMBED-TOKENS drop then 2drop r> ;]
   catch {: code:n :} drop ;

: PARSE-USAGE ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-USAGE ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

\ The tier the reply says served it, when it names one: a nonempty string, or
\ null for none.
: ACCEPT-TIER ( ptr call -- )
   {: c :}
   c MODEL-CALL:READER c RESPONSE$ JSON:READ-INIT
   JSON:T-OBJ EXPECT
   s" service_tier" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE exit then
   JSON:READ-TOKEN dup JSON:T-NULL = if drop JSON:READ-CLOSE exit then
   JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   c REPLY-STR {: a:ptr u:n :}
   JSON:READ-CLOSE
   u 0= if E-RESPONSE throw then
   a u c SERVED-BUF REPLACE-SOME ;

: PARSE-TIER ( ptr call -- )
   {: c :}
   c [: dup ACCEPT-TIER ;] catch {: code:n :} drop
   code RESPONSE-ERROR ;

\ The reply's vectors, and its usage object's bytes and whether it has one.
\ One catch: a code merged from two catches proves nothing, E-STALE-READ.
: PARSE-EMBED ( ptr call -- ptr u8 n bool )
   {: c :}
   s" " false c
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      [: >r 2drop drop r@ ACCEPT-OLLAMA r> ;]
   else
      [: >r 2drop drop r@ ACCEPT-EMBED r> ;]
   then catch {: code:n :}
   code 0= if drop exit then
   2drop 2drop
   code JSON-FAULT? if E-RESPONSE throw then
   code throw ;

: BEGIN-LOCKED ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:STOPPED @ if E-CANCELLED throw then
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   \ The reply buffers keep the room the last call's reply took. They are
   \ resized before the call is started, so a failed resize leaves it idle.
   c CONTENT-BUF c MODEL-CALL:CONTENT-U @ c MODEL-CALL:INCOMPLETE-U @ max c RESET-SIZED
   c RESPONSE-BUF c MODEL-CALL:RESPONSE-U @ c RESET-SIZED
   c ERROR-BUF c RESET-SCRATCH
   c SERVED-BUF BUF:CLEAR
   c RESPONSE-ID-BUF BUF:CLEAR
   0 c STREAM STREAM-FAULT !
   false c STREAM STREAM-DONE !
   false c STREAM STREAM-HEARD !
   0 c STREAM STREAM-OUTPUT-NS !
   OPTION:NONE c STREAM STREAM-DECODER !
   c EVENT-ROWS-BUF BUF:CLEAR
   c EVENT-TEXT-BUF BUF:CLEAR
   TASK:SELF-N c MODEL-CALL:STARTER !
   construct call-state started c MODEL-CALL:STATE !
   false c MODEL-CALL:CANCELLED !
   0 c MODEL-CALL:CONTENT-U !
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:INCOMPLETE-U !
   construct call-result none c MODEL-CALL:RESULT !
   OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   c RESET-USAGE
   c ;

: BEGIN-CALL ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: BEGIN-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> if code throw then ;

\ A stopped call is E-CANCELLED before its request is checked.
: REQUIRE-GOING ( ptr call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:STOPPED @ {: stopped:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   stopped if E-CANCELLED throw then ;

: END-CALL ( ptr call -- bool )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:CANCELLED @ {: cancelled:bool :}
   construct call-state landed c MODEL-CALL:STATE !
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   cancelled ;

\ Keeps the reply's response id, which a failure's log line names.
: KEEP-ID ( ptr call -- )
   {: c :}
   s" id" c REPLY-AT
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   c REPLY-STR BUF:N>BLEN c RESPONSE-ID-BUF BUF:REPLACE
   JSON:READ-CLOSE ;

\ Whether the reply's status says the response is still queued or running.
: RUNNING? ( ptr call -- bool )
   {: c :}
   s" status" c REPLY-AT
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" queued" JSON:READ-STR-EQ? {: queued:bool :}
   s" in_progress" JSON:READ-STR-EQ? {: going:bool :}
   JSON:READ-CLOSE
   queued going or ;

: CODE$ ( n -- ptr u8 n )
   {: code:n :}
   code E-CONFIG = if s" MODEL:E-CONFIG" exit then
   code E-KEY = if s" MODEL:E-KEY" exit then
   code E-CAPACITY = if s" MODEL:E-CAPACITY" exit then
   code E-TRANSPORT = if s" MODEL:E-TRANSPORT" exit then
   code E-HTTP = if s" MODEL:E-HTTP" exit then
   code E-RESPONSE = if s" MODEL:E-RESPONSE" exit then
   code E-REFUSED = if s" MODEL:E-REFUSED" exit then
   code E-INCOMPLETE = if s" MODEL:E-INCOMPLETE" exit then
   code E-UNAVAILABLE = if s" MODEL:E-UNAVAILABLE" exit then
   code E-CANCELLED = if s" MODEL:E-CANCELLED" exit then
   code FMT:INT$ ;


: CURL+ ( n -- )
   {: rc:n :}
   s"  curl " LOG:TEXT+ rc LOG:INT+
   s" : " LOG:TEXT+ rc CURL:>CODE CURL:ERROR$ LOG:TEXT+ ;


\ The text becomes the call's reply.
: KEEP-REPLY ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   a u BUF:N>BLEN c RESPONSE-BUF BUF:REPLACE
   u c MODEL-CALL:RESPONSE-U ! ;

\ The reader at the event data's member under the key, and whether it is there.
: DATA-AT ( ptr u8 n ptr u8 n ptr call -- JSON:reader bool )
   {: a:ptr u:n key:ptr key-u:n c :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:T-OBJ EXPECT
   key key-u JSON:READ-FIND-KEY ;

\ The reader at the error event's error object: its error member when an
\ object, else the event.
: ERROR-AT ( ptr call -- JSON:reader )
   {: c :}
   c WIRE-READER JSON:T-OBJ EXPECT
   s" error" JSON:READ-FIND-KEY if
      JSON:READ-TOKEN JSON:T-OBJ = if exit then
   then
   JSON:READ-CLOSE c WIRE-READER JSON:T-OBJ EXPECT ;

\ An error event fails the response as a failed status does: the event is the
\ reply.
: ERROR-EVENT ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   a u c KEEP-REPLY
   c ERROR-AT c ERROR-TEXT JSON:READ-CLOSE
   c ERROR-AT CODE-FAULT {: fault:n :}
   JSON:READ-CLOSE
   c STREAM {: s :}
   fault s STREAM-FAULT !
   true s STREAM-DONE ! ;

\ An event carrying the response object makes it the reply; the first names
\ the response's id, and a status other than queued or in_progress ends the
\ stream.
: RESPONSE-EVENT ( ptr u8 n ptr call -- )
   {: a:ptr u:n c :}
   a u s" response" c DATA-AT 0= if JSON:READ-CLOSE exit then
   JSON:READ-TOKEN JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-VALUE-SPAN$ {: r:ptr r-u:n :}
   JSON:READ-CLOSE
   r r-u c KEEP-REPLY
   c RESPONSE-ID$ nip 0= if c KEEP-ID then
   c RUNNING? 0= if true c STREAM STREAM-DONE ! then ;

\ One event, its data a JSON object with a string type, counted by type; the
\ call's first event starts its reply afresh, and a keepalive is skipped.
: READ-EVENT ( ptr call ptr u8 n -- ptr call ptr u8 n )
   {: c a:ptr u:n :}
   c STREAM {: s :}
   s STREAM-HEARD @ 0= if
      c RESPONSE-BUF BUF:CLEAR
      0 c MODEL-CALL:RESPONSE-U !
      true s STREAM-HEARD !
   then
   a u c JSON-OBJECT? 0= if E-RESPONSE throw then
   a u s" type" c DATA-AT 0= if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   \ The span of a string token holds its bytes without the quotes.
   JSON:READ-SPAN$ {: t:ptr t-u:n :}
   t t-u c COUNT-EVENT
   s STREAM-OUTPUT-NS @ 0= t t-u s" response.output_" STARTS-WITH? and if
      TIME:MONO-NS s STREAM-OUTPUT-NS !
   then
   s" keepalive" JSON:READ-STR-EQ? {: keepalive:bool :}
   s" error" JSON:READ-STR-EQ? {: failed:bool :}
   JSON:READ-CLOSE
   keepalive if c a u exit then
   failed if a u c ERROR-EVENT c a u exit then
   a u c RESPONSE-EVENT
   c a u ;


\ Bytes that are not UTF-8 or JSON make an unusable reply.
: READ-FAULT ( n -- n )
   {: code:n :}
   code SSE:E-UTF8 = code JSON-FAULT? or if E-RESPONSE exit then
   code ;


\ An event the call cannot read becomes the reply, as an error event does,
\ and fails the stream E-RESPONSE; none is read once the response ended.
: EVENT ( ptr call ptr u8 n ptr u8 n -- )
   {: c name:ptr name-u:n a:ptr u:n :}
   c STREAM STREAM-DONE @ if exit then
   c a u [: READ-EVENT ;] catch {: code:n :} drop 2drop
   code 0= if exit then
   code READ-FAULT E-RESPONSE = if a u c KEEP-REPLY then
   code throw ;

: KEEP-CHUNK ( ptr call -- )
   {: c :}
   c CHUNK-BUF BUFFER$ BUF:N>BLEN c RESPONSE-BUF BUF:APPEND-SPAN ;

: FEED-EVENTS ( SSE:decoder ptr call -- SSE:decoder ptr call )
   {: d:SSE:decoder c :}
   d c CHUNK-BUF BUFFER$ c [: EVENT ;] SSE:FEED
   d c ;


\ The chunk is the reply until the first event, so a 200 that is no event
\ stream is, and so is one whose event the decoder refuses as not UTF-8.
: FEED-CHUNK ( SSE:decoder ptr call -- )
   {: d:SSE:decoder c :}
   c STREAM {: s :}
   s STREAM-DONE @ if exit then
   s STREAM-HEARD @ 0= if c KEEP-CHUNK then
   d c [: FEED-EVENTS ;] catch {: code:n :} 2drop
   code 0= if exit then
   code SSE:E-UTF8 = if c CHUNK-BUF BUFFER$ c KEEP-REPLY then
   code throw ;


ENUM receive-step reading waiting dropping drop-waiting parked parked-dropping ended lost ;ENUM
ENUM answer fed spoiled done ;ENUM
\ What SERVE's takes answer: none while the armed read holds nothing yet.
ENUM poll none fed spoiled done ;ENUM

\ Records how the transfer ended.
: KEEP-ENDING ( CURL:http-status n n ptr call -- )
   RECEIPT {: status:CURL:http-status rc:n fault:n r :}
   status r RECEIPT-STATUS !
   rc r RECEIPT-CODE !
   fault r RECEIPT-FAULT ! ;


\ The response buffer's room past the bytes received: an embed, count or
\ compaction read lands there, and appending it from where it lies adds it to
\ them.
: REPLY-ROOM ( ptr call -- ptr u8 n )
   RESPONSE-BUF {: buf:ptr :}
   buf HELD buf BUF:LEN@ BUF:BLEN>N SPAN:SKIP SPAN:$ ;


\ Room for the next read of an embed, count or compaction reply; a fault spoils
\ the reply.
: MAKE-ROOM ( ptr call -- answer )
   {: c :}
   c [: GROW-REPLY ;] catch {: fault:n :} drop
   fault 0= if construct answer fed exit then
   0 CURL:>HTTP-STATUS 0 fault c KEEP-ENDING
   construct answer spoiled ;


: CLEAR-REPLY ( ptr call -- answer )
   {: c :}
   0 CURL:>HTTP-STATUS 0 0 c KEEP-ENDING
   c RESPONSE-BUF BUF:CLEAR
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF construct answer fed ENDOF
      embed OF c MAKE-ROOM ENDOF
      tally OF c MAKE-ROOM ENDOF
      compaction OF c MAKE-ROOM ENDOF
   ;MATCH ;


\ A 200 chunk feeds the decoder; any other is kept as the reply.
: CHAT-CHUNK ( CURL:http-status n ptr call -- answer )
   {: status:CURL:http-status got:n c :}
   c CHUNK-BUF HELD SPAN:$ drop {: room:ptr :}
   c STREAM STREAM-DECODER @ MATCH option
      none OF E-STATE throw ENDOF
      some OF ENDOF
   ;MATCH {: d:SSE:decoder :}
   c CHUNK-BUF BUF:CLEAR
   room got BUF:N>BLEN c CHUNK-BUF BUF:APPEND-SPAN
   status CURL:HTTP-STATUS>N 200 = if
      d c [: 2dup FEED-CHUNK ;] catch
   else
      d c [: dup KEEP-CHUNK ;] catch
   then {: fault:n :} 2drop
   fault 0= if construct answer fed exit then
   status 0 fault READ-FAULT c KEEP-ENDING
   construct answer spoiled ;


: EMBED-CHUNK ( n ptr call -- answer )
   {: got:n c :}
   c REPLY-ROOM drop {: room:ptr :}
   room got BUF:N>BLEN c RESPONSE-BUF BUF:APPEND-SPAN
   c MAKE-ROOM ;


\ A chat whose response already ended keeps it, however the transfer ends.
: FAILED-END ( n ptr call -- )
   {: rc:n c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c STREAM STREAM-DONE @ ENDOF
      embed OF false ENDOF
      tally OF false ENDOF
      compaction OF false ENDOF
   ;MATCH
   if 200 CURL:>HTTP-STATUS 0 0 c KEEP-ENDING exit then
   0 CURL:>HTTP-STATUS rc 0 c KEEP-ENDING ;


: TAKE-WAIT ( ptr call -- answer )
   {: c :}
   c CALL-HANDLE CURL:NEXT-TAKE MATCH CURL:chunk-result
      chunk OF LEN>N {: got:n :}
         c MODEL-CALL:KIND @ MATCH call-kind
            chat OF got c CHAT-CHUNK ENDOF
            embed OF drop got c EMBED-CHUNK ENDOF
            tally OF drop got c EMBED-CHUNK ENDOF
            compaction OF drop got c EMBED-CHUNK ENDOF
         ;MATCH
      ENDOF
      response OF 0 0 c KEEP-ENDING construct answer done ENDOF
      failed OF CURL:CODE>N c FAILED-END construct answer done ENDOF
   ;MATCH ;


\ The receipt keeps the fault that spoiled the reply, however the drained
\ transfer ends.
: DISCARD-WAIT ( ptr call -- answer )
   CALL-HANDLE CURL:NEXT-TAKE MATCH CURL:chunk-result
      chunk OF 2drop construct answer fed ENDOF
      response OF drop construct answer done ENDOF
      failed OF drop construct answer done ENDOF
   ;MATCH ;


: POLLED ( answer -- poll )
   MATCH answer
      fed OF construct poll fed ENDOF
      spoiled OF construct poll spoiled ENDOF
      done OF construct poll done ENDOF
   ;MATCH ;


\ As TAKE-WAIT, but answers none at once while the armed read holds nothing.
: TAKE ( ptr call -- poll )
   {: c :}
   c CALL-HANDLE CURL:NEXT-READY? 0= if construct poll none exit then
   c TAKE-WAIT POLLED ;


\ As DISCARD-WAIT, but answers none at once while the armed read holds
\ nothing.
: DISCARD ( ptr call -- poll )
   {: c :}
   c CALL-HANDLE CURL:NEXT-READY? 0= if construct poll none exit then
   c DISCARD-WAIT POLLED ;


: REARM ( ptr call -- )
   {: c :}
   c CALL-HANDLE
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c CHUNK-BUF HELD SPAN:$ ENDOF
      embed OF c REPLY-ROOM ENDOF
      tally OF c REPLY-ROOM ENDOF
      compaction OF c REPLY-ROOM ENDOF
   ;MATCH >LEN CURL:NEXT-ARM ;


\ Every kind drains into the chunk buffer, so the bytes of a dropped embed,
\ count or compaction reply stay as they arrived.
: REARM-DROP ( ptr call -- )
   {: c :}
   c CALL-HANDLE c CHUNK-BUF HELD SPAN:$ >LEN CURL:NEXT-ARM ;


: DROP-REPLY ( ptr call -- )
   {: c :}
   c CALL-HANDLE CURL:CANCEL
   c REARM-DROP ;


\ An input the state cannot receive.
: LOST ( ptr call -- ) drop E-STATE throw ;


\ Arms a started transfer's first read and keeps how SERVE and AWAIT resume it:
\ a reply the call cannot hold is dropped and drained.
: ARM ( ptr call -- )
   {: c :}
   c CLEAR-REPLY MATCH answer
      fed OF c REARM construct receive-rest reading ENDOF
      spoiled OF c DROP-REPLY construct receive-rest dropping ENDOF
      done OF E-STATE throw ENDOF
   ;MATCH c MODEL-CALL:RECEIVE ! ;


\ Keeps where SERVE or AWAIT resumes a parked transfer.
: PARK ( ptr call -- )
   construct receive-rest reading swap MODEL-CALL:RECEIVE ! ;
: PARK-DROP ( ptr call -- )
   construct receive-rest dropping swap MODEL-CALL:RECEIVE ! ;


\ Ending before the response with no event means no event stream: E-RESPONSE.
: STREAM-CHECK ( ptr call -- ptr call )
   {: c :}
   c STREAM {: s :}
   s STREAM-DONE @ 0= if
      s STREAM-HEARD @ if E-TRANSPORT throw then
      E-RESPONSE throw
   then
   s STREAM-FAULT @ 0<> if s STREAM-FAULT @ throw then
   c ;

\ Reads a chat reply's usage, service tier and content.
: CHAT-PARSE ( ptr call -- ptr call )
   {: c :}
   c PARSE-USAGE
   c PARSE-TIER
   c PARSE-CHAT
   c TOKENS TOKENS-INPUT @ OR-ZERO c MODEL-CALL:INPUT-TOKENS !
   c TOKENS TOKENS-CACHED @ OR-ZERO c MODEL-CALL:CACHED-TOKENS !
   c ;

\ Reads an embed reply's vectors and usage.
: EMBED-PARSE ( ptr call -- ptr call )
   {: c :}
   s" " false c [: >r 2drop drop r@ PARSE-EMBED r> ;] catch {: walk:n :}
   walk 0<> if 2drop 2drop c FAULT-USAGE walk throw then
   drop {: a:ptr u:n present:bool :}
   present if
      true c TOKENS TOKENS-SEEN !
      a u c [: EMBED-TOKENS ;] catch {: code:n :} 2drop drop
      code RESPONSE-ERROR
   then
   c ;

\ A count reply's input_tokens, at its top level: an integer.
: ACCEPT-COUNT ( ptr call -- )
   {: c :}
   c RESPONSE$ c s" input_tokens" USAGE-AT MATCH option
      none OF E-RESPONSE throw ENDOF
      some OF c MODEL-CALL:COUNTED ! ENDOF
   ;MATCH ;

\ Reads a count reply's input tokens into COUNTED.
: COUNT-PARSE ( ptr call -- ptr call )
   {: c :}
   c [: dup ACCEPT-COUNT ;] catch {: code:n :} drop
   code RESPONSE-ERROR
   c ;

\ The reader past the window's next item, an object, and the item's bytes;
\ false past the last.
: NEXT-ITEM ( JSON:reader -- JSON:reader ptr u8 n bool )
   JSON:READ-NEXT dup JSON:T-ARR-END = if drop s" " false exit then
   JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-VALUE-SPAN$ true ;

\ Keeps the item's bytes under the reader in place of the item before it.
: KEEP-LAST ( ptr u8 n JSON:reader ptr u8 n -- ptr u8 n JSON:reader )
   {: a:ptr u:n :}
   >r 2drop a u r> ;

\ Whether the item's bytes are a compaction item.
: COMPACTION? ( ptr u8 n ptr call -- bool )
   {: a:ptr u:n c :}
   c MODEL-CALL:READER a u JSON:READ-INIT
   JSON:T-OBJ EXPECT TYPE-AT
   s" compaction" JSON:READ-STR-EQ? {: hit:bool :}
   JSON:READ-CLOSE hit ;

\ A compaction reply is one JSON object whose output is a nonempty array of
\ objects, the last a compaction item; else E-RESPONSE.
: CHECK-WINDOW ( ptr call -- )
   {: c :}
   c RESPONSE$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   s" " c OUTPUT-AT
   begin NEXT-ITEM while KEEP-LAST repeat
   2drop JSON:READ-CLOSE {: last:ptr last-u:n :}
   last-u 0= if E-RESPONSE throw then
   last last-u c COMPACTION? 0= if E-RESPONSE throw then ;

\ Replaces the finished input with the reply's window: its output items, in
\ order, and no parts; the input is broken until the window is whole.
: TAKE-WINDOW ( ptr call -- )
   {: c :}
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c EMPTY-INPUT
   c OUTPUT-AT
   begin NEXT-ITEM while c ITEM-JOIN repeat
   2drop JSON:READ-CLOSE
   c PART-OPEN
   construct input-state building c MODEL-CALL:CHAT-INPUT !
   c PART-FINISH ;

\ A compaction reply, checked before the input changes: its usage and served
\ tier, then its window as the call's input.
: ACCEPT-COMPACT ( ptr call -- )
   {: c :}
   c ACCEPT-USAGE
   c ACCEPT-TIER
   c CHECK-WINDOW
   c TAKE-WINDOW
   c TOKENS TOKENS-INPUT @ OR-ZERO c MODEL-CALL:INPUT-TOKENS !
   c TOKENS TOKENS-CACHED @ OR-ZERO c MODEL-CALL:CACHED-TOKENS ! ;

\ Reads a compaction reply into the call's input and usage.
: COMPACT-PARSE ( ptr call -- ptr call )
   {: c :}
   c [: dup ACCEPT-COMPACT ;] catch {: code:n :} drop
   code RESPONSE-ERROR
   c ;

: JOURNAL-GET? ( ptr call -- bool )
   {: c :}
   c JOURNAL-OUT-BUF c RESET-SCRATCH
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-OUT-BUF c MODEL-CALL:JOURNAL-GET @ execute ;

: JOURNAL-FIELD ( ptr u8 n ptr call -- ptr u8 n )
   {: key:ptr key-u:n c :}
   c MODEL-CALL:READER c JOURNAL-OUT$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   key key-u JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-OBJ = if
      JSON:READ-VALUE-SPAN$ {: a:ptr u:n :} JSON:READ-CLOSE a u exit
   then
   kind JSON:T-STR = if
      JSON:READ-SPAN$ {: a:ptr u:n :} JSON:READ-CLOSE a 1- u 2 + exit
   then
   JSON:READ-CLOSE E-RESPONSE throw ;

: JOURNAL-DECODE ( ptr u8 n SPAN:span<u8> ptr call -- n )
   {: raw:ptr raw-u:n target:SPAN:span<u8> c :}
   c MODEL-CALL:READER raw raw-u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   target SPAN:$ JSON:READ-STR {: got:n :}
   JSON:READ-CLOSE got ;

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
   c MODEL-CALL:READER raw raw-u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" kind" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-SPAN$ nip 0= if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-CLOSE ;

\ The recorded call id a journal record carries, 0 when it has none.
: JOURNAL-CALL-ID ( ptr call -- n )
   {: c :}
   c MODEL-CALL:READER c JOURNAL-OUT$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" call_id" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE 0 exit then
   JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE E-RESPONSE throw then
   JSON:READ-INT {: id:n :} JSON:READ-CLOSE id ;

\ The fault a journaled refusal records, 0 for a reply.
: JOURNAL-REFUSAL-FAULT ( ptr call -- n )
   {: c :}
   c MODEL-CALL:READER c JOURNAL-OUT$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" refusal" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE 0 exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE E-RESPONSE throw then
   s" capacity" JSON:READ-STR-EQ? if JSON:READ-CLOSE E-CAPACITY exit then
   s" incomplete" JSON:READ-STR-EQ? if JSON:READ-CLOSE E-INCOMPLETE exit then
   JSON:READ-CLOSE E-RESPONSE throw ;

: JOURNAL-RESTORE ( ptr call -- )
   {: c :}
   0 c MODEL-CALL:HTTP-STATUS !
   c JOURNAL-OUT$ c JSON-OBJECT? 0= if E-RESPONSE throw then
   c JOURNAL-REFUSAL-FAULT {: refusal:n :}
   refusal 0<> if refusal throw then
   c JOURNAL-ORIGIN
   s" configuration" c JOURNAL-FIELD {: config:ptr config-u:n :}
   config config-u c JSON-OBJECT? 0= if E-RESPONSE throw then
   config config-u BUF:N>BLEN c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN-CONFIG-BUF ENDOF
      embed OF c EMBED-CONFIG-BUF ENDOF
      tally OF c GEN-CONFIG-BUF ENDOF
      compaction OF c GEN-CONFIG-BUF ENDOF
   ;MATCH BUF:REPLACE
   c JOURNAL-RESPONSE
   s" content" c REPLAY-CONTENT-BUF c JOURNAL-STRING-BUF
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         c PARSE-CHAT
         c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
         c REPLAY-CONTENT$ STR= 0= if E-RESPONSE throw then
      ENDOF
      embed OF
         c REPLAY-CONTENT$ nip 0<> if E-RESPONSE throw then
         c PARSE-EMBED 2drop drop
      ENDOF
      tally OF
         c REPLAY-CONTENT$ nip 0<> if E-RESPONSE throw then
         c COUNT-PARSE drop
      ENDOF
      compaction OF
         c REPLAY-CONTENT$ nip 0<> if E-RESPONSE throw then
         c COMPACT-PARSE drop
      ENDOF
   ;MATCH
   c JOURNAL-CALL-ID c LEDGER LEDGER-CALL-ID !
   c MODEL-CALL:KIND @ OPTION:SOME c MODEL-CALL:REPLAY-KIND ! ;

\ A usage count, null when the reply omitted it.
: COUNT-FIELD ( ptr JSON:writer ptr u8 n option<n> -- ptr JSON:writer )
   MATCH option
      none OF JSON:WRITE-FIELD-NULL ENDOF
      some OF JSON:WRITE-FIELD-INT ENDOF
   ;MATCH ;

\ The call's kind, configured model and usage counts, when the reply carried
\ usage.
: USAGE-FIELD ( ptr JSON:writer ptr call -- ptr JSON:writer )
   {: c :}
   c TOKENS TOKENS-SEEN @ 0= if exit then
   JSON:WRITE-COMMA s" usage" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" kind" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF s" generation" ENDOF
      embed OF s" embedding" ENDOF
      tally OF s" input_tokens" ENDOF
      compaction OF s" compaction" ENDOF
   ;MATCH JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN$ ENDOF
      embed OF c EMBED-MODEL$ ENDOF
      tally OF c GEN$ ENDOF
      compaction OF c GEN$ ENDOF
   ;MATCH JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" input_tokens" c TOKENS TOKENS-INPUT @ COUNT-FIELD
   JSON:WRITE-COMMA s" cached_tokens" c TOKENS TOKENS-CACHED @ COUNT-FIELD
   JSON:WRITE-COMMA s" cache_write_tokens" c TOKENS TOKENS-WRITTEN @ COUNT-FIELD
   JSON:WRITE-COMMA s" output_tokens" c TOKENS TOKENS-OUTPUT @ COUNT-FIELD
   JSON:WRITE-COMMA s" reasoning_tokens" c TOKENS TOKENS-REASONING @ COUNT-FIELD
   JSON:WRITE-OBJECT-END ;

\ Opens the journal record with the call's configuration.
: RECORD-OPEN ( ptr call -- ptr JSON:writer )
   {: c :}
   c MODEL-CALL:KIND @ {: k:call-kind :}
   k GENERATES? if k SHAPED? c BUILD-GEN-CONFIG else c BUILD-EMBED-CONFIG then
   c JOURNAL-RECORD-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-RECORD-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" configuration" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN-CONFIG$ ENDOF
      embed OF c EMBED-CONFIG$ ENDOF
      tally OF c GEN-CONFIG$ ENDOF
      compaction OF c GEN-CONFIG$ ENDOF
   ;MATCH JSON:WRITE-FIELD-RAW ;

: JOURNAL-RECORD ( ptr call -- )
   {: c :}
   c RECORD-OPEN
   JSON:WRITE-COMMA s" content" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$ ENDOF
      embed OF s" " ENDOF
      tally OF s" " ENDOF
      compaction OF s" " ENDOF
   ;MATCH JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" response" c RESPONSE$ JSON:WRITE-FIELD-S
   c USAGE-FIELD
   c LEDGER LEDGER-CALL-ID @ {: id:n :}
   id 0<> if JSON:WRITE-COMMA s" call_id" id JSON:WRITE-FIELD-INT then
   JSON:WRITE-COMMA s" origin" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" kind" s" live" JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

\ A nonzero count, else none.
: NONZERO ( n -- option<n> ) dup 0<> if OPTION:SOME else drop OPTION:NONE then ;

: BACKEND$ ( ptr call -- ptr u8 n )
   {: c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF s" openai" ENDOF
      embed OF
         c MODEL-CALL:EMBEDDING-KIND @ OLLAMA =
         if s" ollama" else s" openai" then
      ENDOF
      tally OF s" openai" ENDOF
      compaction OF s" openai" ENDOF
   ;MATCH ;


\ The sha256 hex of the model, system text and schema or tools every
\ generation request of a purpose starts with, NUL-separated.
: PREFIX-HASH$ ( ptr call -- ptr u8 n )
   {: c :}
   c HASH-WORK {: w :}
   w SHA256-BEGIN
   w c GEN$ SHA256-FEED
   w s\" \x00" SHA256-FEED
   w c SYSTEM$ SHA256-FEED
   w s\" \x00" SHA256-FEED
   w c FORM$ SHA256-FEED
   w w SHA256-CTX-BYTES + SHA256-END
   w SHA256-CTX-BYTES + w SHA256-CTX-BYTES + 32 + SHA256>HEX
   w SHA256-CTX-BYTES + 32 + 64 ;

\ The call's accounting record, the JSON object RECORDER!'s record receives.
: CALL-RECORD ( ptr call -- )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-CANCELLED @ if E-CANCELLED else l LEDGER-CODE @ then {: fault:n :}
   c JOURNAL-RECORD-BUF c RESET-SCRATCH
   c WRITER c JOURNAL-RECORD-BUF JSON:WRITE-OPEN-BUF drop
   c WRITER JSON:WRITE-OBJECT-START
   s" kind" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF s" generation" ENDOF
      embed OF s" embedding" ENDOF
      tally OF s" input_tokens" ENDOF
      compaction OF s" compaction" ENDOF
   ;MATCH JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" backend" c BACKEND$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN$ ENDOF
      embed OF c EMBED-MODEL$ ENDOF
      tally OF c GEN$ ENDOF
      compaction OF c GEN$ ENDOF
   ;MATCH JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" effort" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c EFFORT$ nip 0<> ENDOF
      embed OF false ENDOF
      tally OF c EFFORT$ nip 0<> ENDOF
      compaction OF false ENDOF
   ;MATCH if
      c EFFORT$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" service_tier" c SERVED$ nip 0<> if
      c SERVED$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" requested_tier" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c TIER$ nip 0<> ENDOF
      embed OF false ENDOF
      tally OF false ENDOF
      compaction OF c TIER$ nip 0<> ENDOF
   ;MATCH if
      c TIER$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" purpose" c LABEL$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" outcome" l LEDGER-CANCELLED @ if s" cancelled"
   else fault 0<> if s" failed" else s" ok" then then JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" fault" fault NONZERO COUNT-FIELD
   JSON:WRITE-COMMA s" http_status" c MODEL-CALL:HTTP-STATUS @ NONZERO COUNT-FIELD
   JSON:WRITE-COMMA s" incomplete_reason" c MODEL-CALL:INCOMPLETE-U @ 0<> if
      c CONTENT-BUF HELD c MODEL-CALL:INCOMPLETE-U @ SPAN:TAKE SPAN:$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" request_bytes" c BODY$ nip JSON:WRITE-FIELD-INT
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         JSON:WRITE-COMMA s" prefix_bytes"
         c GEN$ nip c SYSTEM$ nip + c FORM$ nip + JSON:WRITE-FIELD-INT
         JSON:WRITE-COMMA s" prefix_hash" c PREFIX-HASH$ JSON:WRITE-FIELD-S
      ENDOF
      embed OF ENDOF
      tally OF ENDOF
      compaction OF ENDOF
   ;MATCH
   JSON:WRITE-COMMA s" duration_ms" TIME:MONO-NS l LEDGER-STARTED-NS @ - NS-PER-MS /
   JSON:WRITE-FIELD-INT
   JSON:WRITE-COMMA s" input_tokens" c TOKENS TOKENS-INPUT @ COUNT-FIELD
   JSON:WRITE-COMMA s" cached_tokens" c TOKENS TOKENS-CACHED @ COUNT-FIELD
   JSON:WRITE-COMMA s" cache_write_tokens" c TOKENS TOKENS-WRITTEN @ COUNT-FIELD
   JSON:WRITE-COMMA s" output_tokens" c TOKENS TOKENS-OUTPUT @ COUNT-FIELD
   JSON:WRITE-COMMA s" reasoning_tokens" c TOKENS TOKENS-REASONING @ COUNT-FIELD
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE ;

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

\ The row of the purpose PURPOSE! left pending, plus one; 0 is none.
: PENDING-ROW ( ptr call -- n )
   {: c :}
   c PURPOSE-BUF HELD c LEDGER LEDGER-PENDING-U @ SPAN:TAKE SPAN:$ c ROW-OF ;

\ A call starts: its pending purpose becomes its label, required when a
\ recorder is installed, and selects the purpose's row. A fault before it
\ sends reports no HTTP status, as PARTS-RESET and INPUT-RESET ensure for the
\ builders.
: START-LEDGER ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   0 c MODEL-CALL:HTTP-STATUS !
   c LEDGER {: l :}
   l LEDGER-PENDING-U @ {: u:n :}
   u l LEDGER-LABEL-U !
   c PENDING-ROW c PICK-CELL !
   0 l LEDGER-PENDING-U !
   false l LEDGER-DISPATCHED !
   0 l LEDGER-CALL-ID !
   0 l LEDGER-FAULT !
   0 l LEDGER-TRANSPORT !
   l LEDGER-READY @ u 0= and if E-STATE throw then ;

\ Keeps a dispatched request's fault and cut for its record.
: END-LEDGER ( n bool ptr call -- )
   {: code:n cancelled:bool c :}
   c LEDGER {: l :}
   l LEDGER-READY @ l LEDGER-DISPATCHED @ and 0= if exit then
   code l LEDGER-CODE !
   cancelled l LEDGER-CANCELLED ! ;

: JOURNAL-PUT ( ptr call -- )
   {: c :}
   c JOURNAL-RECORD
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

\ Whether the provider's reply refused the request for its size or did not
\ finish: a fault the caller may answer by asking for less.
: REFUSAL? ( n -- bool ) dup E-CAPACITY = swap E-INCOMPLETE = or ;

: REFUSAL-PUT ( n ptr call -- )
   {: code:n c :}
   c RECORD-OPEN
   JSON:WRITE-COMMA s" refusal" code E-CAPACITY = if s" capacity" else
      s" incomplete" then JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:WRITE-CLOSE
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

: PREP-JOURNAL-CHAT ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-CHAT
   c FINISH-INPUT
   construct call-kind chat c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-COUNT ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-CHAT
   c FINISH-INPUT
   construct call-kind tally c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-COMPACT ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-INPUT
   c FINISH-INPUT
   construct call-kind compaction c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-EMBED ( ptr call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-EMBED
   c JOURNAL-EMBED-INPUT ;

: CANCEL-LOCKED ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF false c MODEL-CALL:CANCELLED ! ENDOF
      prepared OF false c MODEL-CALL:CANCELLED ! ENDOF
      started OF
         true c MODEL-CALL:CANCELLED !
         c MODEL-CALL:TRANSPORT @ MATCH option
            none OF ENDOF
            some OF OPTION:NONE c MODEL-CALL:TRANSPORT ! CURL:CANCEL ENDOF
         ;MATCH
      ENDOF
      landed OF false c MODEL-CALL:CANCELLED ! ENDOF
   ;MATCH
   c ;

: STOP-LOCKED ( ptr call -- ptr call )
   {: c :}
   true c MODEL-CALL:STOPPED !
   c CANCEL-LOCKED ;

public

: INCOMPLETE-REASON$ ( ptr call -- ptr u8 n )
   {: c :}
   c CONTENT-BUF HELD c MODEL-CALL:INCOMPLETE-U @ SPAN:TAKE SPAN:$ ;
\ The last call's provider message from its refused HTTP reply or failed
\ response; empty when none.
: ERROR$ ( ptr call -- ptr u8 n ) ERROR-BUF BUFFER$ ;

private

\ STREAM-CHECK's E-TRANSPORT is the only one with a status.
: REPLY+ ( n ptr call -- )
   {: code:n c :}
   c MODEL-CALL:HTTP-STATUS @ {: status:n :}
   code E-RESPONSE = code E-REFUSED = or
   code E-TRANSPORT = status 200 = and or if
      s"  status " LOG:TEXT+ status LOG:INT+
      s"  body: " LOG:TEXT+ c RESPONSE$ LOG:TEXT+ exit
   then
   status 0= if exit then
   c STREAM STREAM-FAULT @ 0<> {: event:bool :}
   status 200 = c ERROR$ nip 0= and event 0= and if exit then
   s"  status " LOG:TEXT+ status LOG:INT+
   c ERROR$ nip 0<> if
      s" : " LOG:TEXT+ c ERROR$ LOG:TEXT+
      event 0= if exit then
   then
   c RESPONSE$ nip 0= if exit then
   s"  body: " LOG:TEXT+ c RESPONSE$ LOG:TEXT+ ;


: CAUSE+ ( n ptr call -- )
   {: code:n c :}
   code E-INCOMPLETE = c INCOMPLETE-REASON$ nip 0<> and if
      s"  incomplete " LOG:TEXT+ c INCOMPLETE-REASON$ LOG:TEXT+
   then
   c LEDGER LEDGER-TRANSPORT @ {: rc:n :}
   rc 0<> if rc CURL+ then ;


\ The count of each event type the stream carried and how long after dispatch
\ its first output event came; nothing when it carried none.
: EVENTS+ ( ptr call -- )
   {: c :}
   c EVENT-ROW-COUNT {: rows:n :}
   rows 0= if exit then
   s" ; events" LOG:TEXT+
   rows 0 ?do
      s"  " LOG:TEXT+ i c EVENT-TYPE$ LOG:TEXT+
      s" =" LOG:TEXT+ i 2 c EVENT-CELL @ LOG:INT+
   loop
   c STREAM STREAM-OUTPUT-NS @ {: at:n :}
   at 0= if s" ; no output" LOG:TEXT+ exit then
   s" ; first output after " LOG:TEXT+
   at c LEDGER LEDGER-STARTED-NS @ - NS-PER-MS / LOG:INT+ s"  ms" LOG:TEXT+ ;


\ One line for a failed chat, embed, count or compaction call: its model and
\ URL, response id once known, the code's name and cause, and a chat stream's
\ events.
: REPORT ( n ptr call -- )
   {: code:n c :}
   code REFUSAL? if LOG:WARN else LOG:ERROR then s" model" LOG:LINE
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF s" chat " LOG:TEXT+ c GEN$ ENDOF
      embed OF s" embed " LOG:TEXT+ c EMBED-MODEL$ ENDOF
      tally OF s" count " LOG:TEXT+ c GEN$ ENDOF
      compaction OF s" compact " LOG:TEXT+ c GEN$ ENDOF
   ;MATCH LOG:TEXT+
   s"  " LOG:TEXT+ c REQUEST-URL$ LOG:TEXT+
   c RESPONSE-ID$ nip 0<> if s"  " LOG:TEXT+ c RESPONSE-ID$ LOG:TEXT+ then
   s"  " LOG:TEXT+ code CODE$ LOG:TEXT+
   code c REPLY+
   code c CAUSE+
   c EVENTS+
   LOG:;LINE ;

ENUM ending cut replayed replay-failed served unsent refused failed ;ENUM
ENUM record-event kept faulted ;ENUM

\ Keeps n as the call's outcome unless one is kept.
: KEEP-FAULT ( n ptr call -- )
   RECEIPT {: fault:n r :}
   r RECEIPT-OUTCOME @ 0= if fault r RECEIPT-OUTCOME ! then ;

: RUN-STEP ( ptr call [ ptr call -- ptr call ] -- ptr call [ ptr call -- ptr call ] )
   {: c step :}
   c step execute drop
   c step ;

\ Runs the step on the call while it keeps no outcome, keeping the step's fault.
: CHECK-STEP ( ptr call [ ptr call -- ptr call ] -- )
   {: c step :}
   c RECEIPT RECEIPT-OUTCOME @ 0<> if exit then
   c step [: RUN-STEP ;] catch {: code:n :} 2drop
   code c KEEP-FAULT ;


\ Ends the call's transfer, decoder and handle, wipes its header and parses its
\ reply, keeping the first fault; then lands the call and answers its ending.
: SETTLE-SENSE ( ptr call -- ending )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-REPLAYED @ {: replayed:bool :}
   c END-TRANSFER
   replayed 0= if c [: RECEIPT-CHECK ;] CHECK-STEP then
   c STREAM STREAM-DECODER @ MATCH option
      none OF ENDOF
      some OF
         [: dup SSE:CLOSE ;] catch {: closed:n :} drop
         closed c KEEP-FAULT
      ENDOF
   ;MATCH
   OPTION:NONE c STREAM STREAM-DECODER !
   replayed 0= if
      c MODEL-CALL:KIND @ MATCH call-kind
         chat OF c [: STREAM-CHECK ;] CHECK-STEP ENDOF
         embed OF ENDOF
         tally OF ENDOF
         compaction OF ENDOF
      ;MATCH
   then
   c MODEL-CALL:HANDLE @ MATCH option
      none OF ENDOF
      some OF
         [: dup CURL:CLEANUP ;] catch {: freed:n :} drop
         freed c KEEP-FAULT
      ENDOF
   ;MATCH
   OPTION:NONE c MODEL-CALL:HANDLE !
   replayed 0= if
      c [: dup HEADER-WIPE ;] catch {: wiped:n :} drop
      wiped c KEEP-FAULT
      c MODEL-CALL:KIND @ MATCH call-kind
         chat OF c [: CHAT-PARSE ;] CHECK-STEP ENDOF
         embed OF c [: EMBED-PARSE ;] CHECK-STEP ENDOF
         tally OF c [: COUNT-PARSE ;] CHECK-STEP ENDOF
         compaction OF c [: COMPACT-PARSE ;] CHECK-STEP ENDOF
      ;MATCH
   then
   c END-CALL {: cancelled:bool :}
   r RECEIPT-OUTCOME @ {: fault:n :}
   fault cancelled c END-LEDGER
   cancelled if construct ending cut exit then
   replayed if
      fault 0= if construct ending replayed exit then
      construct ending replay-failed exit
   then
   fault 0= if construct ending served exit then
   c LEDGER {: l :}
   l LEDGER-DISPATCHED @ l LEDGER-TRANSPORT @ 0<> or 0= if
      construct ending unsent exit
   then
   fault REFUSAL? if construct ending refused exit then
   construct ending failed ;

: LAND-CUT ( ptr call -- ) E-CANCELLED swap RECEIPT RECEIPT-OUTCOME ! ;


\ Records a dispatched request, keeping the recorder's fault.
: RECORD-SENSE ( ptr call -- record-event )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-READY @ l LEDGER-DISPATCHED @ and 0= if
      construct record-event kept exit
   then
   c [: RECORD-RUN ;] catch {: rc:n :} drop
   rc l LEDGER-FAULT !
   rc 0<> if construct record-event faulted exit then
   construct record-event kept ;

\ A put fault replaces the outcome.
: LAND-SERVED ( ptr call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if exit then
   c [: dup JOURNAL-PUT ;] catch {: put:n :} drop
   put 0<> if put c RECEIPT RECEIPT-OUTCOME ! then ;

\ A served call whose recorder failed keeps the recorder's fault as its outcome
\ and is journaled as served.
: LAND-UNRECORDED ( ptr call -- )
   dup LEDGER LEDGER-FAULT @ over RECEIPT RECEIPT-OUTCOME !
   LAND-SERVED ;

\ Journals a live refusal so a replay meets it at the same step and asks for
\ less with no request. A put fault replaces the outcome.
: LAND-REFUSED ( ptr call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if exit then
   c RECEIPT RECEIPT-OUTCOME @ c [: 2dup REFUSAL-PUT ;] catch {: put:n :} 2drop
   put 0<> if put c RECEIPT RECEIPT-OUTCOME ! then ;

: REPORT-OUTCOME ( ptr call -- ) dup RECEIPT RECEIPT-OUTCOME @ swap REPORT ;

\ Lands a call once its run ends: what each ending reports, records, saves and
\ leaves as the outcome. Settling runs in SETTLE, the rest in ACCOUNT.
FSM:DEFINE LAND-MACHINE ( ptr call ) land-step ending
   IN settling  SETTLE-SENSE
      cut            LAND-CUT        >recording
      replayed       -               >landed
      replay-failed  -               >landed
      served         -               >serving
      unsent         -               >landed
      refused        REPORT-OUTCOME  >refusing
      failed         REPORT-OUTCOME  >recording
   IN recording  RECORD-SENSE ( record-event )
      kept     -  >landed
      faulted  -  >landed
   IN serving  RECORD-SENSE ( record-event )
      kept     LAND-SERVED      >landed
      faulted  LAND-UNRECORDED  >landed
   IN refusing  RECORD-SENSE ( record-event )
      kept     LAND-REFUSED  >landed
      faulted  LAND-REFUSED  >landed
   FINAL landed  -
;DEFINE


ENUM send-step looking sending out ended ;ENUM
ENUM lookup-event missed replayed faulted ;ENUM
ENUM dispatch-event armed faulted ;ENUM

\ Restores the journal record in JOURNAL-OUT-BUF as the call's reply.
: RESTORE ( ptr call -- )
   {: c :}
   true c RECEIPT RECEIPT-REPLAYED !
   c [: dup JOURNAL-RESTORE ;] catch {: code:n :} drop
   code c KEEP-FAULT ;

: LOOK-UP ( ptr call -- lookup-event )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if construct lookup-event missed exit then
   false c [: >r drop r@ JOURNAL-GET? r> ;] catch {: code:n :} drop
   code 0<> if drop code c KEEP-FAULT construct lookup-event faulted exit then
   if c RESTORE construct lookup-event replayed exit then
   construct lookup-event missed ;

\ Sends the request on a fresh handle the land machine cleans up; the key is
\ read once and wiped once libcurl copies it.
: POST ( ptr call -- ptr call )
   {: c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c CHAT-BODY ENDOF
      embed OF c EMBED-BODY ENDOF
      tally OF c COUNT-BODY ENDOF
      compaction OF c COMPACT-BODY ENDOF
   ;MATCH
   0 c MODEL-CALL:RESPONSE-U !
   0 c MODEL-CALL:HTTP-STATUS !
   c OPEN-HANDLE OPTION:SOME c MODEL-CALL:HANDLE !
   c CALL-HANDLE {: h:CURL:handle :}
   h c SET-HEADERS
   h 1 STALL-SECONDS CURL:LOW-SPEED! c CHECK-STATUS
   c REQUEST-URL$ c BODY$ c AS-POST
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         c DECODER-STORAGE SSE:OPEN OPTION:SOME c STREAM STREAM-DECODER !
      ENDOF
      embed OF ENDOF
      tally OF ENDOF
      compaction OF ENDOF
   ;MATCH
   c START-TRANSFER
   c ARM
   c ;

: DISPATCH ( ptr call -- dispatch-event )
   {: c :}
   c [: POST ;] catch {: code:n :} drop
   code 0= if construct dispatch-event armed exit then
   code c KEEP-FAULT
   construct dispatch-event faulted ;

\ The call whose recorder or journal put this task runs (ACCOUNT), for the
\ cleanup a halt's unwind runs, which takes nothing.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 8 TASK:+USER LANDING drop
CAST: CALL>N ( ptr call -- n )
CAST: N>CALL ( n -- ptr call )

: STEP! ( land-step ptr call -- ) RECEIPT RECEIPT-STEP ! ;

\ A fault the machine throws becomes the outcome, and the call ends landed.
: LAND-THREW ( n ptr call -- )
   {: rc:n c :}
   rc c RECEIPT RECEIPT-OUTCOME !
   construct land-step landed c STEP! ;

: SETTLE-RUN ( ptr call -- ptr call )
   {: c :}
   c construct land-step settling LAND-MACHINE-STEP drop
   c STEP!
   c ;

: SETTLE-CAUGHT ( ptr call -- )
   {: c :}
   c [: SETTLE-RUN ;] catch {: rc:n :} drop
   rc 0= if exit then
   c END-CALL drop
   rc c LAND-THREW ;

\ Lands a call once its run ends, keeping the next step for ACCOUNT. It runs
\ deferred: a halt while it runs leaves the call landed.
: SETTLE ( ptr call -- )
   TASK:DEFER-ENTER
   [: SETTLE-CAUGHT ;] [: TASK:DEFER-LEAVE ;] finally ;

\ Whether the call's recorder or journal put is still to run.
: ACCOUNTING? ( land-step -- bool )
   MATCH land-step
      settling OF false ENDOF
      recording OF true ENDOF
      serving OF true ENDOF
      refusing OF true ENDOF
      landed OF false ENDOF
   ;MATCH ;

: ACCOUNT-RUN ( ptr call -- ptr call )
   {: c :}
   c RECEIPT {: r :}
   begin
      c r RECEIPT-STEP @ LAND-MACHINE-STEP
      swap r RECEIPT-STEP !
   until
   c ;

: ACCOUNT-CAUGHT ( ptr call -- )
   {: c :}
   c [: ACCOUNT-RUN ;] catch {: rc:n :} drop
   rc 0= if exit then
   rc c LAND-THREW ;

\ Restores the landing this task held before; a halt's unwind out of the
\ recorder or put leaves the call cut.
: LANDING-END ( -- )
   LANDING @ N>CALL {: c :}
   c RECEIPT {: r :}
   r RECEIPT-OUTER @ LANDING !
   r RECEIPT-STEP @ ACCOUNTING? 0= if exit then
   c LAND-CUT
   construct land-step landed c STEP! ;

\ Runs a settled call's recorder and journal put after SETTLE's deferred
\ section: a halt served while they wait leaves the call landed E-CANCELLED.
: ACCOUNT ( ptr call -- )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-STEP @ ACCOUNTING? 0= if exit then
   LANDING @ r RECEIPT-OUTER !
   c CALL>N LANDING !
   c [: ACCOUNT-CAUGHT ;] [: LANDING-END ;] finally ;

\ Settles the call, then runs its recorder and journal put.
: LAND ( ptr call -- )
   dup SETTLE ACCOUNT ;

\ Receives the reply, or drops one the call cannot keep and drains its
\ transfer, settling the call when it ends; reading and dropping park, not wait.
FSM:DEFINE RECEIVE-MACHINE ( ptr call ) receive-step answer
   IN reading       TAKE ( poll )
      none     -           >parked
      fed      REARM       >reading
      spoiled  DROP-REPLY  >dropping
      done     -           >ended
   IN waiting       TAKE-WAIT
      fed      REARM       >waiting
      spoiled  DROP-REPLY  >drop-waiting
      done     -           >ended
   IN dropping      DISCARD ( poll )
      none     -           >parked-dropping
      fed      REARM-DROP  >dropping
      spoiled  -           >lost
      done     -           >ended
   IN drop-waiting  DISCARD-WAIT
      fed      REARM-DROP  >drop-waiting
      spoiled  -           >lost
      done     -           >ended
   FINAL parked           PARK
   FINAL parked-dropping  PARK-DROP
   FINAL ended            SETTLE
   FINAL lost             LOST
;DEFINE


: RECEIVE ( ptr call -- )
   {: c :}
   c c MODEL-CALL:RECEIVE @ MATCH receive-rest
      reading OF construct receive-step waiting ENDOF
      dropping OF construct receive-step drop-waiting ENDOF
   ;MATCH RECEIVE-MACHINE ;

: RECEIVE-READY ( ptr call -- )
   {: c :}
   c c MODEL-CALL:RECEIVE @ MATCH receive-rest
      reading OF construct receive-step reading ENDOF
      dropping OF construct receive-step dropping ENDOF
   ;MATCH RECEIVE-MACHINE ;

\ Settles a call whose receive threw n before the machine ended; 0: the machine
\ settled or parked it.
: LAND-FAULT ( n ptr call -- )
   {: code:n c :}
   code 0= if exit then
   code c KEEP-FAULT
   c SETTLE ;

\ Sends a begun call's request, or restores its journal hit: out with the
\ transfer armed, or landed.
FSM:DEFINE SEND-MACHINE ( ptr call ) send-step lookup-event
   IN looking  LOOK-UP
      missed    -  >sending
      replayed  -  >ended
      faulted   -  >ended
   IN sending  DISPATCH ( dispatch-event )
      armed     -  >out
      faulted   -  >ended
   FINAL out    -
   FINAL ended  LAND
;DEFINE

: BEGIN-SEND ( ptr call -- )
   {: c :}
   c BEGIN-CALL
   false c RECEIPT RECEIPT-REPLAYED !
   0 c RECEIPT RECEIPT-OUTCOME !
   construct land-step settling c STEP!
   c construct send-step looking SEND-MACHINE ;


\ A chat AWAIT ended without a fault has used its finished input; a chat that
\ ended in a fault keeps it for the next START or CHAT to send again.
: USED ( ptr call -- )
   {: c :}
   c MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF ENDOF
      building OF ENDOF
      broken OF ENDOF
      finished OF construct input-state none c MODEL-CALL:CHAT-INPUT ! ENDOF
   ;MATCH ;


\ The starter is a TCB, as CURL's record owner is: a later task on that TCB
\ passes, and the main thread's null TCB is a starter like any other.
: REQUIRE-STARTER ( ptr call -- )
   MODEL-CALL:STARTER @ TASK:SELF-N <> if E-STATE throw then ;


: RECEIVE-CAUGHT ( [ ptr call -- ptr call ] ptr call -- )
   {: receive c :}
   c receive catch {: code:n :} drop code c LAND-FAULT ;


\ The receive runs deferred: a halt pending while AWAIT waits settles the call,
\ one in SERVE leaves it started; ACCOUNT follows outside it, where it is served.
: RECEIVE-STARTED ( [ ptr call -- ptr call ] ptr call -- )
   {: receive c :}
   c REQUIRE-STARTER
   TASK:DEFER-ENTER
   receive c [: RECEIVE-CAUGHT ;] [: TASK:DEFER-LEAVE ;] finally
   c ACCOUNT
   0 TASK:HALT-UNWIND ;

public

\ Makes a started call end with E-CANCELLED, cutting any transfer at once, and
\ answers whether the call was started; the provider, not told, may bill it.
: CANCEL ( ptr call -- bool )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: CANCEL-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:CANCELLED @ {: selected:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> code CURL:E-STATE <> and if code throw then
   selected ;

: REQUEST-STOP ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: STOP-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> code CURL:E-STATE <> and if code throw then ;

\ Sends the chat request JOURNAL-INPUT$ finished, else the one built since
\ PARTS-RESET; a fault before the call begins is thrown, a later one AWAIT's.
\ A request a failed chat kept is sent again, its body and journal input built
\ from the settings and pending purpose then in force: the first try's when
\ neither changed.
: START ( ptr call -- )
   {: c :}
   c TAKE-PREPARED {: ready:bool :}
   construct call-result none c MODEL-CALL:RESULT !
   c START-LEDGER
   c REQUIRE-GOING
   ready 0= if
      c MODEL-CALL:JOURNAL-READY @ if
         c PREP-JOURNAL-CHAT
      else
         c CHECK-CHAT
         c FINISH-INPUT
      then
   then
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   construct call-kind chat c MODEL-CALL:KIND !
   c BEGIN-SEND ;

\ Sends the embed request as START sends a chat's.
: START-EMBED ( ptr call -- )
   {: c :}
   c UNPREPARE drop
   construct call-result none c MODEL-CALL:RESULT !
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if c PREP-JOURNAL-EMBED else c CHECK-EMBED then
   construct call-kind embed c MODEL-CALL:KIND !
   c BEGIN-SEND ;

\ Takes what a started call's transfer holds without waiting for more and
\ answers whether the call landed, as a landed call has; AWAIT ends it. A take
\ that faults gives the transfer up and waits until the loop leaves it.
: SERVE ( ptr call -- bool )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF E-STATE throw ENDOF
      prepared OF E-STATE throw ENDOF
      started OF [: dup RECEIVE-READY ;] c RECEIVE-STARTED ENDOF
      landed OF ENDOF
   ;MATCH
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF E-STATE throw ENDOF
      prepared OF E-STATE throw ENDOF
      started OF false ENDOF
      landed OF true ENDOF
   ;MATCH ;

\ Waits for a started call to land, makes it idle and throws its fault, else
\ keeps its result for CONTENT$ or VECTOR@. A count keeps its finished input
\ for the next CHAT or START and leaves no result; so does a compaction, its
\ input the window. A chat uses its finished input up only when it ends
\ without a fault: one that faults keeps it, as a count, a compaction and an
\ embed keep theirs, and PURPOSE! then START or CHAT sends it again.
: AWAIT ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF E-STATE throw ENDOF
      prepared OF E-STATE throw ENDOF
      started OF [: dup RECEIVE ;] c RECEIVE-STARTED ENDOF
      landed OF ENDOF
   ;MATCH
   c IDLE!
   c RECEIPT RECEIPT-OUTCOME @ {: fault:n :}
   fault 0<> if fault throw then
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c USED construct call-result content ENDOF
      embed OF construct call-result vectors ENDOF
      tally OF construct call-result none ENDOF
      compaction OF construct call-result none ENDOF
   ;MATCH c MODEL-CALL:RESULT ! ;

\ The content of the chat AWAIT last ended, read on an idle call and borrowed
\ until the next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED, REPLAY or
\ CLOSE.
: CONTENT$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   c MODEL-CALL:RESULT @ MATCH call-result
      none OF E-STATE throw ENDOF
      content OF ENDOF
      vectors OF E-STATE throw ENDOF
   ;MATCH
   c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$ ;

\ The output array of the chat reply last awaited or replayed, its own bytes,
\ read and borrowed as CONTENT$ is; ITEM+ takes each item for the next turn.
: OUTPUT$ ( ptr call -- ptr u8 n )
   {: c :}
   c CONTENT$ 2drop
   s" output" c REPLY-AT JSON:READ-VALUE-SPAN$ {: a:ptr u:n :}
   JSON:READ-CLOSE a u ;

\ Finishes the chat request built since PARTS-RESET as CHAT does before its
\ journal lookup and answers the journal input a journal's get and put receive
\ for it, valid until the next JOURNAL-INPUT$, CHAT, START, COUNT-INPUT, COMPACT,
\ EMBED, START-EMBED or CLOSE. The pending purpose selects the model and effort
\ as at CHAT, so PURPOSE! comes first. A second call answers the same input.
\ The next CHAT or START sends that request, or REPLAY restores a saved reply
\ for it. Until then a call that changes the chat request (PARTS-RESET, TEXT+,
\ IMAGE+, ITEM+, PURPOSE!, PURPOSE-MODEL! and the generation model, base URL,
\ system, schema, tools, effort, service tier, cache key and output limit
\ setters) or EMBED or START-EMBED discards it, and it is built again from
\ PARTS-RESET. Once CHAT, START or REPLAY takes it, one that faults leaves it
\ finished and only PARTS-RESET discards it: PURPOSE! then JOURNAL-INPUT$, CHAT
\ or START answer or send it again under the settings then in force, the same
\ input when none changed.
: JOURNAL-INPUT$ ( ptr call -- ptr u8 n )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF
         c PENDING-ROW c PICK-CELL !
         c PREP-JOURNAL-CHAT
         construct call-state prepared c MODEL-CALL:STATE !
      ENDOF
      prepared OF ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   c JOURNAL-INPUT-SPAN$ ;

\ A journal hit replays the recorded reply; otherwise one streaming request is
\ read event by event until the response ends. START, AWAIT and CONTENT$.
: CHAT ( ptr call -- ptr u8 n )
   {: c :}
   c START
   c AWAIT
   c CONTENT$ ;

\ Counts the input tokens of the chat request built since PARTS-RESET without
\ generating: the request stays finished, so the next CHAT or START sends what
\ was counted. Recorded and journaled as a call; it consumes the pending purpose
\ and leaves no result. A prepared, started or landed call is E-STATE.
: COUNT-INPUT ( ptr call -- n )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   construct call-result none c MODEL-CALL:RESULT !
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-COUNT
   else
      c CHECK-CHAT
      c FINISH-INPUT
   then
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   0 c MODEL-CALL:COUNTED !
   construct call-kind tally c MODEL-CALL:KIND !
   c BEGIN-SEND
   c AWAIT
   c MODEL-CALL:COUNTED @ ;

\ Replaces the input of the chat request built since PARTS-RESET with the
\ provider's compacted window, kept finished for the next COUNT-INPUT, CHAT or
\ START; it needs no schema or tools and is otherwise as COUNT-INPUT.
: COMPACT ( ptr call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   construct call-result none c MODEL-CALL:RESULT !
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-COMPACT
   else
      c CHECK-INPUT
      c FINISH-INPUT
   then
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   construct call-kind compaction c MODEL-CALL:KIND !
   c BEGIN-SEND
   c AWAIT ;

\ Restores, for the request JOURNAL-INPUT$ finished, a record a journal's put
\ received for a chat call, as a journal hit does: its configuration, response,
\ content and the call id its live call recorded; it answers the content as
\ CHAT does and consumes the pending purpose. A saved refusal throws E-CAPACITY
\ or E-INCOMPLETE. It sends nothing and records nothing. Without a request
\ JOURNAL-INPUT$ finished since the last CHAT, START or REPLAY it is E-STATE;
\ one that throws keeps the request as JOURNAL-INPUT$ says.
: REPLAY ( ptr u8 n ptr call -- ptr u8 n )
   {: a:ptr u:n c :}
   c TAKE-PREPARED 0= if E-STATE throw then
   construct call-result none c MODEL-CALL:RESULT !
   c START-LEDGER
   c REQUIRE-GOING
   c JOURNAL-OUT-BUF c RESET-SCRATCH
   a u BUF:N>BLEN c JOURNAL-OUT-BUF BUF:REPLACE
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   construct call-kind chat c MODEL-CALL:KIND !
   c BEGIN-CALL
   0 c RECEIPT RECEIPT-OUTCOME !
   c RESTORE
   c LAND
   c AWAIT
   c CONTENT$ ;

\ START-EMBED and AWAIT.
: EMBED ( ptr call -- )
   {: c :}
   c START-EMBED
   c AWAIT ;

\ Input idx's vector: DIMS little-endian IEEE-754 binary32 values, borrowed
\ until the next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED, REPLAY or
\ CLOSE.
: VECTOR@ ( n ptr call -- ptr u8 n )
   {: idx:n c :}
   c MODEL-CALL:RESULT @ MATCH call-result
      none OF E-STATE throw ENDOF
      content OF E-STATE throw ENDOF
      vectors OF ENDOF
   ;MATCH
   idx 0 < idx c MODEL-CALL:INPUT-COUNT @ >= or if E-CONFIG throw then
   c VECTORS-BUF BUF:SPAN c idx VECTOR-OFF @ c idx VECTOR-LEN @ SPAN:SUB SPAN:$ ;

: GENERATION-MODEL$ ( ptr call -- ptr u8 n ) GEN$ ;
: EMBEDDING-MODEL$ ( ptr call -- ptr u8 n ) EMBED-MODEL$ ;
: HTTP-STATUS ( ptr call -- n ) MODEL-CALL:HTTP-STATUS @ ;
: INPUT-TOKENS ( ptr call -- n ) MODEL-CALL:INPUT-TOKENS @ ;
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
