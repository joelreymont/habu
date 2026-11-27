\ model.f - package MODEL, a Habu library: a client for model providers over
\ CURL. Chat speaks OpenAI's Responses protocol; embeddings go to an
\ OpenAI-shaped provider or Ollama's native route. A chat call answers a
\ schema with one JSON object (CONTENT$) or offers tools and answers one tool
\ call (OUTPUT$); COUNT-INPUT counts a chat request's input tokens without
\ generating, and COMPACT replaces its input with the provider's compacted
\ window. The client's API key is read from the native credential store
\ through SECRET, with a caller's journal and recorder and bounded inputs.
\ Each failure the provider, the transport or the key's store caused is logged
\ through LOG (tag `model`) with the caller's LOG-NOTE!, never the request body.
\ Test: src/lib/net/model-test.f.
\ STORAGE CLASS. CALLER-OWNED: each call and settings record, and the arena
\ OPEN allocates for a call until CLOSE; the module keeps no state.
require src/lib/base64.f
require src/lib/byte-buffer.f
require src/lib/entropy.f
require src/lib/f64-text.f
require src/lib/error.f
require src/lib/float32.f
require src/lib/float32-buffer.f
require src/lib/fmt.f
require src/lib/fsm.f
require src/lib/heap.f
require src/lib/json.f
require src/lib/log.f
require src/lib/memory.f
require src/lib/net/curl.f
require src/lib/net/event-stream.f
require src/lib/os-memory.f
require src/lib/reason.f
require src/lib/secret.f
require src/lib/span.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/task.f
require src/lib/time.f
require src/lib/vector.f
require src/lib/json/write.f
require src/lib/json/read.f
require src/lib/json/value.f

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
\ straight after a round in which every SERVE answered false, and parks no
\ longer than the earliest RESEND-DUE of its resting calls: landing a call
\ runs its journal and recorder, and a wait there takes the CURL loop's hint
\ for another call. With TIER-RESEND!, a call whose send fails E-TIER is sent
\ again inside SERVE or AWAIT, and SERVE keeps answering false. With
\ RESEND-WAIT!, a send that fails in RESENDS?'s class once its request left,
\ or in its key read at dispatch, has its line written, a dispatched one
\ recorded, and the call rests, started, as long as the provider's
\ Retry-After asked and a random time after it, whose bound doubles with each
\ resend; SERVE answers false until RESEND-DUE, then it or AWAIT sends the
\ same request again: body, journal input, purpose, model, tier as last sent
\ and cache key. A CANCEL or REQUEST-STOP during the rest wakes the starter,
\ and the call lands E-CANCELLED with a cancelled record when its request was
\ dispatched. SERVE and AWAIT of a started call run on the task that started
\ it; a halt pending at their take gives the transfer up, a ready answer
\ included, and lands the call first, and one at a SERVE with nothing to take
\ leaves it started; one served during a rest leaves it resting, and one
\ served where a send that ended E-TIER is to be sent again lands the call
\ with that send's outcome. A task that may be halted CANCELs, then AWAITs,
\ each call still started in a finally, catching each AWAIT's fault.

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
\ HTTP-STATUS keeps the status of a 404, 401 or 403. E-CONFIG's REASON:TEXT$
\ is `expected <e>, found <f>; <fix>`, or `<argument> L:C: …` for JSON or base64.
-9700 s" a setter, builder or key spec was given an invalid argument, or the provider answered 404 for an unknown model"
   ERROR:CODE E-CONFIG
-9701 s" the provider rejected the key with 401 or 403, or the key item is missing, empty or not printable"
   ERROR:CODE E-KEY
-9702 s" the provider rejected the request for its size, or INPUT+ was given an input past INPUT-MAX"
   ERROR:CODE E-CAPACITY
-9703 s" the transfer failed, stalled, or its stream ended before the response did, and the provider may still finish and bill the response"
   ERROR:CODE E-TRANSPORT
-9704 s" the provider answered a status other than 200, or a failed response's error code, that no other MODEL code names"
   ERROR:CODE E-HTTP
-9705 s" the provider's response, or a journal record replayed for it, is malformed or inconsistent"
   ERROR:CODE E-RESPONSE
-9706 s" the chat response's answer message carries a refusal" ERROR:CODE E-REFUSED
-9707 s" the response ended incomplete: INCOMPLETE-REASON$ names why" ERROR:CODE E-INCOMPLETE
-9709 s" CANCEL ended the call, REQUEST-STOP came before or during it, or a halt cut its recorder or journal put"
   ERROR:CODE E-CANCELLED
-9710 s" a MODEL word ran out of turn: a busy or unopened call, a part or item outside building, SERVE or AWAIT unstarted or on another task, VECTOR@ or CONTENT$ without its result"
   ERROR:FAULT E-STATE
-9711 s" the provider is unavailable for now, with status 408, 429 or 5xx or a failed response's error code rate_limit_exceeded, slow_down, server_error or server_is_overloaded: send the call again later"
   ERROR:CODE E-UNAVAILABLE
-9712 s" the provider cannot serve the request at the service tier it names for now (error code flex_unavailable) and generated nothing: TIER-RESEND! sends such a call again"
   ERROR:CODE E-TIER
\ OpenAI's limits: https://platform.openai.com/docs/api-reference/embeddings
\ MODEL counts no embed input's tokens; INPUT-MAX bounds an Ollama call too.
\ The most inputs one EMBED call takes.
2048 constant INPUT-MAX
\ The most tokens one EMBED call's inputs may sum to.
300000 constant TOKENS-PER-EMBED-MAX
\ The most tokens one input may hold.
8192 constant TOKENS-PER-INPUT-MAX

STRUCTURE allocation DERIVE addr
   FIELD base ^u8
   FIELD size n
;STRUCTURE

STRUCTURE call DERIVE addr
   FIELD arena allocation
   FIELD body-w JSON:writer
   FIELD parts-w JSON:writer
   FIELD journal-parts-w JSON:writer
   FIELD gate TASK:sem
   FIELD gate-ready bool
   FIELD handle option<CURL:handle>
   FIELD transport option<CURL:handle>
   FIELD state call-state
   FIELD cancelled bool
   FIELD stopped bool
   FIELD key-split n
   FIELD dims n
   FIELD deadline n
   FIELD input-tokens n
   FIELD cached-tokens n
   FIELD output-tokens n
   FIELD reasoning-tokens n
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
   FIELD kind call-kind
   FIELD receive receive-rest
   FIELD component-count n
   FIELD data-count n
   FIELD http-status n
   FIELD incomplete-u n
   FIELD embedding-kind n
   FIELD journal-context n
   FIELD journal-get [ n str ^n -- bool ]
   FIELD journal-put [ n str str -- ]
   FIELD journal-ready bool
   FIELD tier-resend bool
   FIELD resend-wait n
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

CAST: >TOKENS ( ^u8 -- ^tokens )

\ The installed recorder and the current call's accounting: the purpose
\ PURPOSE! left pending, the label of the call in progress, whether its request
\ reached the backend and when, its recorded call id, the recorder's fault,
\ the libcurl code of the call's failed step, 0 when none, and whether the
\ provider throttled the send: HTTP 429, or an error coded
\ rate_limit_exceeded or slow_down.
STRUCTURE ledger DERIVE addr
   FIELD context n
   FIELD record [ n str -- n ]
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
   FIELD throttled bool
;STRUCTURE

CAST: >LEDGER ( ^u8 -- ^ledger )

\ A chat's event stream: whether it delivered an event, whether the response
\ ended, an error event's fault, its first output's TIME:MONO-NS, 0 if none,
\ the decoder reading it and the decoder's state.
STRUCTURE stream DERIVE addr
   FIELD heard bool
   FIELD done bool
   FIELD fault n
   FIELD output-ns n
   FIELD decoder option<SSE:decoder>
   FIELD sse SSE:state
;STRUCTURE

CAST: >STREAM ( ^u8 -- ^stream )

\ What the last reply names of what served it: its tier and model, each empty
\ for none, and its created_at, whole seconds since the epoch, none when the
\ reply omits it or gives null.
STRUCTURE served-by DERIVE addr
   FIELD tier ^VEC:vector<u8>
   FIELD model ^VEC:vector<u8>
   FIELD created option<n>
;STRUCTURE

CAST: >SERVED-BY ( ^u8 -- ^served-by )

\ The land machine's steps: settling ends the call's transfer and lands it;
\ recording runs the recorder; serving and rejecting run it, then the put.
ENUM land-step settling recording serving rejecting landed ;ENUM
\ How a run ended: cut by a CANCEL, alone or after an unsent or a sent
\ send's failure; or replayed, replay-failed, served, unsent, rejected or
\ failed.
ENUM ending
   cut cut-unsent cut-failed
   replayed replay-failed served unsent rejected failed
;ENUM
\ The tier machine's states: preferred sends the call's tier; resuming begins
\ the call again after a send that ended E-TIER; standard sends none; done
\ sends no more. A landed send is settled, to be sent again (tier-out), or
\ halted, a halt of its starter served there.
ENUM tier-step preferred resuming standard done ;ENUM
ENUM tier-event settled tier-out halted ;ENUM
\ The resend machine's states: sending, a send is out or has landed; begun, a
\ rest just begun, sensed once more in the step that began it; resting, a
\ send RESENDS? accepts waits its rest out; resuming, a due rest's
\ resend begins; done sends no more.
\ A landed send is settled, outside the work or stopped; a rest is early, due
\ or stopped. A landed call's next send is begun, rejected by a stop or CANCEL,
\ or faulted.
ENUM resend-step sending begun resting resuming done ;ENUM
ENUM sent-event settled outside stopped ;ENUM
ENUM rest-event early due stopped ;ENUM
ENUM begin-event begun rejected faulted ;ENUM
\ How the send's key failed: none when it did not; the item missing, empty
\ or not printable (E-KEY); the store's access denied (SECRET:E-ACCESS), the
\ store not answering for now (SECRET:E-UNAVAILABLE), or no store set up or
\ the service or account invalid (SECRET:E-CONFIG); or the provider rejected
\ the key it was sent (E-KEY).
ENUM key-fault
   none missing empty unprintable denied unavailable config rejected
;ENUM

\ END-TRANSFER's status, code, spoiling fault and cut; outcome, the fault it
\ throws; step, the land machine's next; tier, the tier machine's; sent, the
\ tier step the last send went out at; resend, the resend machine's state;
\ due, the TIME:MONO-NS a resting call is sent again, 0 when it is not
\ resting; resends, how many rests this start has begun; retry-after, the
\ seconds the last send's reply asked to wait, in its header or its error
\ object, 0 for none or no reply; report, the fault of a failed send whose
\ line is still to be written, 0 when none; outer, the LANDING ACCOUNT
\ replaced; key-read, how the send's key failed, in its read or at the
\ provider; ending, the land machine's ending, none until the call lands and
\ again once a resend's begin faults.
STRUCTURE receipt DERIVE addr
   FIELD status CURL:http-status
   FIELD code n
   FIELD fault n
   FIELD cut bool
   FIELD replayed bool
   FIELD outcome n
   FIELD step land-step
   FIELD tier tier-step
   FIELD sent tier-step
   FIELD resend resend-step
   FIELD due n
   FIELD resends n
   FIELD retry-after n
   FIELD report n
   FIELD outer n
   FIELD key-read key-fault
   FIELD ending option<ending>
;STRUCTURE

CAST: >RECEIPT ( ^u8 -- ^receipt )

256 constant SCRATCH-INITIAL
1000000 constant NS-PER-MS
\ The first rest's bound, doubled with each resend; 33 doublings of it in ns
\ are the most a cell holds.
1000 constant FIRST-REST-MS
33 constant DOUBLINGS-MAX
\ The longest wait a Retry-After is given.
900 constant RETRY-SECONDS-MAX
\ No byte for this long ends a chat, count or compaction, curl 28 logged; an
\ embedding reply comes whole, so its call ends at its caller's deadline. A
\ thinking model's reasoning items arrive at most 9 s apart (OpenAI, measured
\ to 2 minutes).
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
FORM-HDR-AT BUF:HDR-BYTES + constant PARTS-AT
PARTS-AT 1 cells + constant JOURNAL-PARTS-AT
JOURNAL-PARTS-AT 1 cells + constant ITEMS-HDR-AT
ITEMS-HDR-AT BUF:HDR-BYTES + constant BODY-AT
BODY-AT 1 cells + constant INPUT-HDR-AT
INPUT-HDR-AT BUF:HDR-BYTES + constant URI-HDR-AT
URI-HDR-AT BUF:HDR-BYTES + constant EFFORT-HDR-AT
EFFORT-HDR-AT BUF:HDR-BYTES + constant TIER-HDR-AT
TIER-HDR-AT BUF:HDR-BYTES + constant ENDPOINT-HDR-AT
ENDPOINT-HDR-AT BUF:HDR-BYTES + constant IDENTITY-HDR-AT
IDENTITY-HDR-AT BUF:HDR-BYTES + constant BASE-URL-HDR-AT
BASE-URL-HDR-AT BUF:HDR-BYTES + constant URL-HDR-AT
URL-HDR-AT BUF:HDR-BYTES + constant GEN-CONFIG-AT
GEN-CONFIG-AT 1 cells + constant EMBED-CONFIG-AT
EMBED-CONFIG-AT 1 cells + constant JOURNAL-INPUT-AT
JOURNAL-INPUT-AT 1 cells + constant JOURNAL-RECORD-AT
JOURNAL-RECORD-AT 1 cells + constant JOURNAL-OUT-HDR-AT
JOURNAL-OUT-HDR-AT BUF:HDR-BYTES + constant REPLAY-CONTENT-HDR-AT
REPLAY-CONTENT-HDR-AT BUF:HDR-BYTES + constant RESPONSE-HDR-AT
RESPONSE-HDR-AT BUF:HDR-BYTES + constant CONTENT-HDR-AT
CONTENT-HDR-AT BUF:HDR-BYTES + constant ERROR-HDR-AT
ERROR-HDR-AT BUF:HDR-BYTES + constant KEY-LINE-AT
KEY-LINE-AT 1 cells + constant VECTORS-HDR-AT
VECTORS-HDR-AT BUF:HDR-BYTES + constant PURPOSE-HDR-AT
PURPOSE-HDR-AT BUF:HDR-BYTES + constant ROWS-HDR-AT
ROWS-HDR-AT BUF:HDR-BYTES + constant ROW-TEXT-HDR-AT
ROW-TEXT-HDR-AT BUF:HDR-BYTES + constant CACHE-KEY-HDR-AT
CACHE-KEY-HDR-AT BUF:HDR-BYTES + constant RESPONSE-ID-HDR-AT
RESPONSE-ID-HDR-AT BUF:HDR-BYTES + constant CHUNK-HDR-AT
CHUNK-HDR-AT BUF:HDR-BYTES + constant EVENT-ROWS-HDR-AT
EVENT-ROWS-HDR-AT BUF:HDR-BYTES + constant EVENT-TEXT-HDR-AT
EVENT-TEXT-HDR-AT BUF:HDR-BYTES + constant NOTE-HDR-AT
NOTE-HDR-AT BUF:HDR-BYTES + constant OUTCOME-TEXT-HDR-AT
OUTCOME-TEXT-HDR-AT BUF:HDR-BYTES + constant RECORD-TEXT-HDR-AT
RECORD-TEXT-HDR-AT BUF:HDR-BYTES + constant COUNTS-AT
COUNTS-AT TOKENS-BYTES + constant ACCOUNTS-AT
ACCOUNTS-AT LEDGER-BYTES + constant EVENTS-AT
EVENTS-AT STREAM-BYTES + constant ENDING-AT
ENDING-AT RECEIPT-BYTES + constant REPLY-AT
REPLY-AT SERVED-BY-BYTES + constant HASH-AT
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

\ Views a cell that holds a vector.
CAST: >VEC-CELL ( ^u8 -- ^^VEC:vector<u8> )

\ Zeroed before it is freed, so a freed block holds no byte the store read.
: WIPE-DELETE ( ^VEC:vector<u8> -- )
   {: v:^ :}
   v NULL-PTR = if exit then
   v 0 VEC:RESIZE
   v VEC:DELETE ;

: BYTES-MAKE ( ^u8 -- )
   >VEC-CELL {: cell:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW cell ! ;

\ Fresh storage is zero, so a cell INIT-BUFFERS has not reached holds NULL.
: BYTES-DELETE ( ^u8 -- ) >VEC-CELL @ VEC:DELETE ;

: MAKE-SERVED-BY ( ^u8 -- )
   >SERVED-BY {: s :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW s SERVED-BY-TIER !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW s SERVED-BY-MODEL ! ;

: DELETE-SERVED-BY ( ^u8 -- )
   >SERVED-BY {: s :}
   s SERVED-BY-TIER @ VEC:DELETE
   s SERVED-BY-MODEL @ VEC:DELETE ;

: INIT-BUFFERS ( ^call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ {: base:^ :}
   base GEN-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base EMBED-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base KEY-SPEC-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base SYSTEM-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base FORM-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base PARTS-AT + BYTES-MAKE
   base JOURNAL-PARTS-AT + BYTES-MAKE
   base ITEMS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base BODY-AT + BYTES-MAKE
   base INPUT-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base URI-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base EFFORT-HDR-AT + CELL-VIEW 32 BUF:N>BLEN BUF:INIT
   base TIER-HDR-AT + CELL-VIEW 32 BUF:N>BLEN BUF:INIT
   base REPLY-AT + MAKE-SERVED-BY
   base ENDPOINT-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base IDENTITY-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base BASE-URL-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base URL-HDR-AT + CELL-VIEW 128 BUF:N>BLEN BUF:INIT
   base GEN-CONFIG-AT + BYTES-MAKE
   base EMBED-CONFIG-AT + BYTES-MAKE
   base JOURNAL-INPUT-AT + BYTES-MAKE
   base JOURNAL-RECORD-AT + BYTES-MAKE
   base JOURNAL-OUT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base REPLAY-CONTENT-HDR-AT + CELL-VIEW 256 BUF:N>BLEN BUF:INIT
   base RESPONSE-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base CONTENT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base ERROR-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW base KEY-LINE-AT + >VEC-CELL !
   base VECTORS-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base PURPOSE-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base ROWS-HDR-AT + CELL-VIEW ROW-BYTES BUF:N>BLEN BUF:INIT
   base ROW-TEXT-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base CACHE-KEY-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base RESPONSE-ID-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base CHUNK-HDR-AT + CELL-VIEW CHUNK-BYTES BUF:N>BLEN BUF:INIT
   base EVENT-ROWS-HDR-AT + CELL-VIEW EVENT-ROW-BYTES BUF:N>BLEN BUF:INIT
   base EVENT-TEXT-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base NOTE-HDR-AT + CELL-VIEW 64 BUF:N>BLEN BUF:INIT
   base OUTCOME-TEXT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT
   base RECORD-TEXT-HDR-AT + CELL-VIEW SCRATCH-INITIAL BUF:N>BLEN BUF:INIT ;

: DISPOSE-BUFFERS ( ^u8 -- )
   {: base:^ :}
   base GEN-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EMBED-HDR-AT + CELL-VIEW BUF:DISPOSE
   base KEY-SPEC-HDR-AT + CELL-VIEW BUF:DISPOSE
   base SYSTEM-HDR-AT + CELL-VIEW BUF:DISPOSE
   base FORM-HDR-AT + CELL-VIEW BUF:DISPOSE
   base PARTS-AT + BYTES-DELETE
   base JOURNAL-PARTS-AT + BYTES-DELETE
   base ITEMS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base BODY-AT + BYTES-DELETE
   base INPUT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base URI-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EFFORT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base TIER-HDR-AT + CELL-VIEW BUF:DISPOSE
   base REPLY-AT + DELETE-SERVED-BY
   base ENDPOINT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base IDENTITY-HDR-AT + CELL-VIEW BUF:DISPOSE
   base BASE-URL-HDR-AT + CELL-VIEW BUF:DISPOSE
   base URL-HDR-AT + CELL-VIEW BUF:DISPOSE
   base GEN-CONFIG-AT + BYTES-DELETE
   base EMBED-CONFIG-AT + BYTES-DELETE
   base JOURNAL-INPUT-AT + BYTES-DELETE
   base JOURNAL-RECORD-AT + BYTES-DELETE
   base JOURNAL-OUT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base REPLAY-CONTENT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base RESPONSE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CONTENT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ERROR-HDR-AT + CELL-VIEW BUF:DISPOSE
   base KEY-LINE-AT + >VEC-CELL @ WIPE-DELETE
   base VECTORS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base PURPOSE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ROWS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base ROW-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CACHE-KEY-HDR-AT + CELL-VIEW BUF:DISPOSE
   base RESPONSE-ID-HDR-AT + CELL-VIEW BUF:DISPOSE
   base CHUNK-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EVENT-ROWS-HDR-AT + CELL-VIEW BUF:DISPOSE
   base EVENT-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base NOTE-HDR-AT + CELL-VIEW BUF:DISPOSE
   base OUTCOME-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE
   base RECORD-TEXT-HDR-AT + CELL-VIEW BUF:DISPOSE ;

: STORAGE ( ^call -- SPAN:span<u8> )
   {: c :}
   c MODEL-CALL:ARENA {: at :}
   at MODEL-ALLOCATION:BASE @ at MODEL-ALLOCATION:SIZE @
   MEM:BYTES-ALLOC-LEN MEM:ALLOCATION>SPAN ;

: REGION ( ^call n n -- SPAN:span<u8> )
   {: c off:n size:n :}
   c STORAGE off size SPAN:SUB ;

: SCRATCH-BASE-CELL ( ^call -- ^n )
   SCRATCH-BASE-AT 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;

: HEADER ( ^call n -- ^n )
   {: c off:n :}
   c off BUF:HDR-BYTES REGION SPAN:$ STR:DATA CELL-VIEW ;

: BYTES-AT ( ^call n -- ^VEC:vector<u8> )
   {: c off:n :}
   c off 1 cells REGION SPAN:$ STR:DATA >VEC-CELL @ ;

: GEN-BUF ( ^call -- ^n ) GEN-HDR-AT HEADER ;
: EMBED-BUF ( ^call -- ^n ) EMBED-HDR-AT HEADER ;
: KEY-SPEC-BUF ( ^call -- ^n ) KEY-SPEC-HDR-AT HEADER ;
: SYSTEM-BUF ( ^call -- ^n ) SYSTEM-HDR-AT HEADER ;
\ The schema or the tools, as the call's form names.
: FORM-BUF ( ^call -- ^n ) FORM-HDR-AT HEADER ;
: PARTS-BYTES ( ^call -- ^VEC:vector<u8> )
   PARTS-AT BYTES-AT ;
\ The parts as the journal input names them: an image by its digest.
: JOURNAL-PARTS-BYTES ( ^call -- ^VEC:vector<u8> )
   JOURNAL-PARTS-AT BYTES-AT ;
\ The history items as given, joined by single commas.
: ITEMS-BUF ( ^call -- ^n ) ITEMS-HDR-AT HEADER ;
: BODY-BYTES ( ^call -- ^VEC:vector<u8> )
   BODY-AT BYTES-AT ;
: INPUT-BUF ( ^call -- ^n ) INPUT-HDR-AT HEADER ;
: URI-BUF ( ^call -- ^n ) URI-HDR-AT HEADER ;
: EFFORT-BUF ( ^call -- ^n ) EFFORT-HDR-AT HEADER ;
\ The service tier generation requests without a purpose row name.
: TIER-BUF ( ^call -- ^n ) TIER-HDR-AT HEADER ;
: ENDPOINT-BUF ( ^call -- ^n ) ENDPOINT-HDR-AT HEADER ;
: IDENTITY-BUF ( ^call -- ^n ) IDENTITY-HDR-AT HEADER ;
: BASE-URL-BUF ( ^call -- ^n ) BASE-URL-HDR-AT HEADER ;
: URL-BUF ( ^call -- ^n ) URL-HDR-AT HEADER ;
: GEN-CONFIG-BYTES ( ^call -- ^VEC:vector<u8> )
   GEN-CONFIG-AT BYTES-AT ;
: EMBED-CONFIG-BYTES ( ^call -- ^VEC:vector<u8> )
   EMBED-CONFIG-AT BYTES-AT ;
: JOURNAL-INPUT-BYTES ( ^call -- ^VEC:vector<u8> )
   JOURNAL-INPUT-AT BYTES-AT ;
: JOURNAL-RECORD-BYTES ( ^call -- ^VEC:vector<u8> )
   JOURNAL-RECORD-AT BYTES-AT ;
: JOURNAL-OUT-BUF ( ^call -- ^n ) JOURNAL-OUT-HDR-AT HEADER ;
: REPLAY-CONTENT-BUF ( ^call -- ^n ) REPLAY-CONTENT-HDR-AT HEADER ;
: RESPONSE-BUF ( ^call -- ^n ) RESPONSE-HDR-AT HEADER ;
: CONTENT-BUF ( ^call -- ^n ) CONTENT-HDR-AT HEADER ;
\ The last failed reply's provider message.
: ERROR-BUF ( ^call -- ^n ) ERROR-HDR-AT HEADER ;
\ The KEY-LINE-AT cell holds the key line's vector, NULL-PTR until it is made.
: KEY-LINE-BUF ( ^call -- ^VEC:vector<u8> )
   KEY-LINE-AT 1 cells REGION SPAN:$ STR:DATA >VEC-CELL @ ;
: VECTORS-BUF ( ^call -- ^n ) VECTORS-HDR-AT HEADER ;
: PURPOSE-BUF ( ^call -- ^n ) PURPOSE-HDR-AT HEADER ;
: ROWS-BUF ( ^call -- ^n ) ROWS-HDR-AT HEADER ;
: ROW-TEXT-BUF ( ^call -- ^n ) ROW-TEXT-HDR-AT HEADER ;
\ The event types the call's stream carried, one row each in first-seen order.
: EVENT-ROWS-BUF ( ^call -- ^n ) EVENT-ROWS-HDR-AT HEADER ;
: EVENT-TEXT-BUF ( ^call -- ^n ) EVENT-TEXT-HDR-AT HEADER ;
: CACHE-KEY-BUF ( ^call -- ^n ) CACHE-KEY-HDR-AT HEADER ;
\ The id of the call's response, empty until its reply names it.
: RESPONSE-ID-BUF ( ^call -- ^n ) RESPONSE-ID-HDR-AT HEADER ;
\ The bytes of the event stream's last read.
: CHUNK-BUF ( ^call -- ^n ) CHUNK-HDR-AT HEADER ;
\ The text LOG-NOTE! names the call's work with.
: NOTE-BUF ( ^call -- ^n ) NOTE-HDR-AT HEADER ;
\ The REASON text of the call's outcome; of its recorder fault.
: OUTCOME-TEXT-BUF ( ^call -- ^n ) OUTCOME-TEXT-HDR-AT HEADER ;
: RECORD-TEXT-BUF ( ^call -- ^n ) RECORD-TEXT-HDR-AT HEADER ;
\ A buffer's whole capacity. Writers that keep their own length write here.
: HELD ( ^n -- SPAN:span<u8> )
   {: buf:^ :}
   buf BUF:SPAN$ drop buf BUF:CAP@ BUF:BLEN>N SPAN:MAKE ;
\ The buffer's capacity, at least need bytes, for writing afresh: the bytes it
\ held are not kept.
: ROOM ( n ^n -- SPAN:span<u8> )
   {: need:n buf:^ :}
   buf BUF:CLEAR
   buf need BUF:N>BLEN BUF:ENSURE
   buf HELD ;
\ The text in place of buf's bytes; the text after them.
: REPLACE$ ( str ^n -- )
   {: s:str buf:^ :}
   s STR:DATA s STR:LEN BUF:N>BLEN buf BUF:REPLACE ;
: APPEND$ ( str ^n -- )
   {: s:str buf:^ :}
   s STR:DATA s STR:LEN BUF:N>BLEN buf BUF:APPEND-SPAN ;
: TOKENS ( ^call -- ^tokens )
   COUNTS-AT TOKENS-BYTES REGION SPAN:$ STR:DATA >TOKENS ;
: LEDGER ( ^call -- ^ledger )
   ACCOUNTS-AT LEDGER-BYTES REGION SPAN:$ STR:DATA >LEDGER ;
: STREAM ( ^call -- ^stream )
   EVENTS-AT STREAM-BYTES REGION SPAN:$ STR:DATA >STREAM ;
: RECEIPT ( ^call -- ^receipt )
   ENDING-AT RECEIPT-BYTES REGION SPAN:$ STR:DATA >RECEIPT ;
: SERVED-BY ( ^call -- ^served-by )
   REPLY-AT SERVED-BY-BYTES REGION SPAN:$ STR:DATA >SERVED-BY ;
\ Keeps in buf the text of n, a code just caught: REASON:TEXT$ holds it only
\ until the task's next REASON:THROW.
: CAUGHT-TEXT! ( n ^n -- )
   {: code:n buf:^ :}
   code REASON:TEXT$ {: t:str :}
   t buf REPLACE$ ;
\ Keeps n as the call's outcome with its text: a code just caught, or
\ E-CANCELLED, which has none.
: OUTCOME! ( n ^call -- )
   {: code:n c :}
   code c RECEIPT RECEIPT-OUTCOME !
   code c OUTCOME-TEXT-BUF CAUGHT-TEXT! ;
\ Throws the call's outcome, if any, through REASON:THROW when it keeps a text.
: OUTCOME-THROW ( ^call -- )
   {: c :}
   c RECEIPT RECEIPT-OUTCOME @ {: fault:n :}
   fault 0= if exit then
   c OUTCOME-TEXT-BUF BUF:SPAN$ BUF:BLEN>N {: a:^ u:n :}
   u 0= if fault throw then
   a u >str fault REASON:THROW ;
\ A sha256 context, then the digest, then its hex.
: HASH-WORK ( ^call -- ^u8 )
   HASH-AT SHA256-CTX-BYTES 96 + REGION SPAN:$ STR:DATA ;
: LABEL$ ( ^call -- str )
   {: c :}
   c PURPOSE-BUF HELD c LEDGER LEDGER-LABEL-U @ SPAN:TAKE SPAN:$ ;
\ The request is leaving for the backend: from here on the call is recorded.
: DISPATCHED ( ^call -- )
   LEDGER {: l :}
   true l LEDGER-DISPATCHED !
   TIME:MONO-NS l LEDGER-STARTED-NS ! ;
: INPUT-OFF ( ^call n -- ^n )
   {: c idx:n :}
   c INPUT-OFF-AT idx cells + 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
: INPUT-LEN ( ^call n -- ^n )
   {: c idx:n :}
   c INPUT-LEN-AT idx cells + 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
: VECTOR-OFF ( ^call n -- ^n )
   {: c idx:n :}
   c VECTOR-OFF-AT idx cells + 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
: VECTOR-LEN ( ^call n -- ^n )
   {: c idx:n :}
   c VECTOR-LEN-AT idx cells + 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;

: ROW-COUNT ( ^call -- n ) ROWS-BUF BUF:LEN@ BUF:BLEN>N ROW-BYTES / ;
\ The row the call in progress uses, plus one; 0 is none.
: PICK-CELL ( ^call -- ^n )
   PICK-AT 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
\ The most output tokens a chat request asks for; 0 asks for no limit.
: MAX-OUTPUT-CELL ( ^call -- ^n )
   MAX-OUTPUT-AT 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
\ Cell k of the row being added.
: ROW-NEW ( n ^call -- ^n )
   {: k:n c :}
   c ROW-NEW-AT k cells + 1 cells REGION SPAN:$ STR:DATA CELL-VIEW ;
\ Cell k of row idx.
: ROW-CELL ( n n ^call -- n )
   {: idx:n k:n c :}
   c ROWS-BUF BUF:SPAN idx ROW-BYTES * k cells + 1 cells SPAN:SUB
   SPAN:$ STR:DATA CELL-VIEW @ ;
\ Field 0, 1, 2 or 3 of row idx: its purpose, model, effort or service tier.
: ROW$ ( n n ^call -- str )
   {: idx:n field:n c :}
   c ROW-TEXT-BUF BUF:SPAN
   idx field 2 * c ROW-CELL idx field 2 * 1+ c ROW-CELL SPAN:SUB SPAN:$ ;
\ Adds field k of the row being added to the row text.
: ROW-TEXT+ ( str n ^call -- )
   {: s:str k:n c :}
   c ROW-TEXT-BUF BUF:LEN@ BUF:BLEN>N k 2 * c ROW-NEW !
   s STR:LEN k 2 * 1+ c ROW-NEW !
   s c ROW-TEXT-BUF APPEND$ ;
\ The row of the purpose, plus one; 0 is none.
: ROW-OF ( str ^call -- n )
   {: s:str c :}
   c ROW-COUNT 0 ?do
      i 0 c ROW$ s STR:= if i 1+ unloop exit then
   loop 0 ;

: EVENT-ROW-COUNT ( ^call -- n )
   EVENT-ROWS-BUF BUF:LEN@ BUF:BLEN>N EVENT-ROW-BYTES / ;
\ Cell k of event row idx.
: EVENT-CELL ( n n ^call -- ^n )
   {: idx:n k:n c :}
   c EVENT-ROWS-BUF BUF:SPAN idx EVENT-ROW-BYTES * k cells + 1 cells SPAN:SUB
   SPAN:$ STR:DATA CELL-VIEW ;
: EVENT-TYPE$ ( n ^call -- str )
   {: idx:n c :}
   c EVENT-TEXT-BUF BUF:SPAN idx 0 c EVENT-CELL @ idx 1 c EVENT-CELL @ SPAN:SUB
   SPAN:$ ;
\ Counts one event of the type, adding its row the first time it is seen.
: COUNT-EVENT ( str ^call -- )
   {: s:str c :}
   c EVENT-ROW-COUNT {: rows:n :}
   rows rows 0 ?do i c EVENT-TYPE$ s STR:= if drop i leave then loop {: idx:n :}
   idx rows = if
      c EVENT-TEXT-BUF BUF:LEN@ BUF:BLEN>N {: off:n :}
      s c EVENT-TEXT-BUF APPEND$
      c EVENT-ROWS-BUF EVENT-ROW-BYTES BUF:N>BLEN BUF:GROW SPAN:$ STR:DATA CELL-VIEW
      {: row:^ :}
      off row !  s STR:LEN row 1 cells + !  0 row 2 cells + !
   then
   1 idx 2 c EVENT-CELL +! ;

: BUFFER$ ( ^n -- str ) BUF:SPAN$ BUF:BLEN>N >str ;
\ The generation model, effort and tier: the purpose row's when the latest call
\ or JOURNAL-INPUT$ selected one, else the call's own.
: GEN$ ( ^call -- str )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 1 c ROW$ exit then
   c GEN-BUF BUFFER$ ;
: EMBED-MODEL$ ( ^call -- str ) EMBED-BUF BUFFER$ ;
: KEY-SPEC$ ( ^call -- str ) KEY-SPEC-BUF BUFFER$ ;
: SYSTEM$ ( ^call -- str ) SYSTEM-BUF BUFFER$ ;
: FORM$ ( ^call -- str ) FORM-BUF BUFFER$ ;
: PARTS$ ( ^call -- str ) PARTS-BYTES VEC:BYTES ;
: JOURNAL-PARTS$ ( ^call -- str ) JOURNAL-PARTS-BYTES VEC:BYTES ;
: ITEMS$ ( ^call -- str ) ITEMS-BUF BUFFER$ ;
: BODY$ ( ^call -- str ) BODY-BYTES VEC:BYTES ;
: EFFORT$ ( ^call -- str )
   {: c :}
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 2 c ROW$ exit then
   c EFFORT-BUF BUFFER$ ;
: CACHE-KEY$ ( ^call -- str ) CACHE-KEY-BUF BUFFER$ ;
: RESPONSE-ID$ ( ^call -- str ) RESPONSE-ID-BUF BUFFER$ ;
: NOTE$ ( ^call -- str ) NOTE-BUF BUFFER$ ;
\ Whether the call's sends name its tier: not once its tier machine sends at
\ the standard tier or is done.
: TIER-NAMED? ( ^call -- bool )
   RECEIPT RECEIPT-TIER @ MATCH tier-step
      preferred OF true ENDOF
      resuming OF true ENDOF
      standard OF false ENDOF
      done OF false ENDOF
   ;MATCH ;
: TIER$ ( ^call -- str )
   {: c :}
   c TIER-NAMED? 0= if s" " exit then
   c PICK-CELL @ {: r:n :}
   r 0<> if r 1- 3 c ROW$ exit then
   c TIER-BUF BUFFER$ ;
: ENDPOINT$ ( ^call -- str ) ENDPOINT-BUF BUFFER$ ;
: IDENTITY$ ( ^call -- str ) IDENTITY-BUF BUFFER$ ;
: BASE-URL$ ( ^call -- str ) BASE-URL-BUF BUFFER$ ;
: GEN-CONFIG$ ( ^call -- str ) GEN-CONFIG-BYTES VEC:BYTES ;
: EMBED-CONFIG$ ( ^call -- str ) EMBED-CONFIG-BYTES VEC:BYTES ;
: JOURNAL-INPUT-SPAN$ ( ^call -- str )
   JOURNAL-INPUT-BYTES VEC:BYTES ;
: JOURNAL-RECORD$ ( ^call -- str ) JOURNAL-RECORD-BYTES VEC:BYTES ;
: JOURNAL-OUT$ ( ^call -- str ) JOURNAL-OUT-BUF BUFFER$ ;
: REPLAY-CONTENT$ ( ^call -- str ) REPLAY-CONTENT-BUF BUFFER$ ;
: RESPONSE$ ( ^call -- str )
   {: c :}
   c RESPONSE-BUF HELD c MODEL-CALL:RESPONSE-U @ SPAN:TAKE SPAN:$ ;

: REQUIRE-OPEN ( ^call -- )
   MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= if E-STATE throw then ;

: IDLE! ( ^call -- ) construct call-state idle swap MODEL-CALL:STATE ! ;


\ The setters that keep the finished request run it.
: REQUIRE-REST ( ^call -- )
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
: REPLAYED? ( call-kind ^call -- bool )
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
: FORGET-GEN-REPLAY ( ^call -- )
   {: c :}
   c MODEL-CALL:REPLAY-KIND @ MATCH option
      none OF ENDOF
      some OF GENERATES? if OPTION:NONE c MODEL-CALL:REPLAY-KIND ! then ENDOF
   ;MATCH ;


: REQUIRE-BUILDING ( ^call -- )
   MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF E-STATE throw ENDOF
      building OF ENDOF
      broken OF E-STATE throw ENDOF
      finished OF E-STATE throw ENDOF
   ;MATCH ;


\ Makes a call idle and answers whether JOURNAL-INPUT$ had finished its
\ request, which stays finished for START to send.
: TAKE-PREPARED ( ^call -- bool )
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
: UNPREPARE ( ^call -- bool )
   {: c :}
   c TAKE-PREPARED dup if
      construct input-state none c MODEL-CALL:CHAT-INPUT !
   then ;


: PUT$ ( ^u8 str -- ^u8 ) {: at:^ s:str :} s STR:DATA at s STR:LEN BYTE-COPY  at s STR:LEN + ;

: EMPTY$ ( -- str ) s" an empty text" ;

\ Writes `, found ` with the found text quoted, an empty one as `an empty text`,
\ then `; ` and the fix, and throws E-CONFIG with the text.
: REJECT-TAIL ( str str -- )
   {: fs:str xs:str :}
   REASON:FOUND
   fs STR:LEN 0= if EMPTY$ REASON:APPEND else fs false REASON:QUOTE then
   REASON:FIX xs REASON:APPEND
   E-CONFIG REASON:STOP ;

\ Throws E-CONFIG with the text `expected <e>, found `<f>`; <fix>`, an empty f
\ found as `an empty text`.
: CONFIG-REJECT ( str str str -- )
   {: es:str fs:str xs:str :}
   REASON:OPEN REASON:EXPECTED es REASON:APPEND
   fs xs REJECT-TAIL ;

\ Throws E-CONFIG with the text `<name> <text>`.
: NAMED-THROW ( str str -- )
   {: ns:str ts:str :}
   REASON:OPEN ns REASON:APPEND s"  " REASON:APPEND ts REASON:APPEND
   E-CONFIG REASON:STOP ;

: REDACTED$ ( -- str ) s" <redacted>" ;

\ A byte a URL scheme may hold (RFC 3986 section 3.1).
: SCHEME-BYTE? ( n -- bool )
   {: b:n :}
   b [char] a >= b [char] z <= and  b [char] A >= b [char] Z <= and or
   b [char] 0 >= b [char] 9 <= and or
   b [char] + = or  b [char] - = or  b [char] . = or ;

\ The index past a leading scheme and its `://`, else 0.
: AUTHORITY-AT ( str -- n )
   {: s:str :}
   s STR:LEN 3 - 0 max 0 ?do
      s STR:DATA i + c@ {: b:n :}
      b [char] : = if
         i 0 > s STR:DATA i + 1+ c@ [char] / = and s STR:DATA i + 2 + c@ [char] / = and
         if i 3 + else 0 then unloop exit
      then
      b SCHEME-BYTE? 0= if 0 unloop exit then
   loop 0 ;

\ The index of the URL's first `?` or `#`, its length when it has neither.
: QUERY-AT ( str -- n )
   {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ {: b:n :}
      b [char] ? = b [char] # = or if i unloop exit then
   loop s STR:LEN ;

\ The index past the URL's last `@`, 0 when it has none.
: USERINFO-END ( str -- n )
   {: s:str :}
   0 s STR:LEN 0 ?do s STR:DATA i + c@ [char] @ = if drop i 1+ then loop ;

: REDACTED-SCOPE ( str str n n n n str ^u8 NUM:alloc-byte-len -- )
   drop {: es:str fs:str at:n end:n q0:n q:n xs:str buf:^ :}
   buf end 0<> if fs STR:DATA at >str PUT$ REDACTED$ PUT$ s" @" PUT$ then
   fs STR:DATA end + q end - >str PUT$
   q fs STR:LEN < if
      q0 end >= if fs STR:DATA q + 1 >str PUT$ then REDACTED$ PUT$
   then
   buf - {: ru:n :}
   es buf ru >str xs CONFIG-REJECT ;

\ CONFIG-REJECT, the found URL's bytes past its scheme to its last `@` and from
\ its first `?` or `#` written `<redacted>`; a negative length found as itself.
: URL-REJECT ( str str str -- )
   {: es:str fs:str xs:str :}
   fs STR:LEN 0< if es fs STR:LEN FMT:INT$ xs CONFIG-REJECT then
   fs AUTHORITY-AT {: at:n :}
   fs USERINFO-END {: end:n :}
   fs QUERY-AT {: q0:n :}
   q0 end max {: q:n :}
   end 0= q fs STR:LEN = and if es fs xs CONFIG-REJECT then
   es fs at end q0 q xs
   fs STR:LEN REDACTED$ STR:LEN 1+ 2 * + MEM:BYTES-ALLOC-LEN
   [: REDACTED-SCOPE ;] MEM:WITH-BYTES ;

\ Rejects a first text of negative length; the second names it as the
\ rejection's expected text.
: REQUIRE-LENGTH ( str str -- )
   {: s:str ns:str :}
   s STR:LEN 0< if
      ns s STR:LEN FMT:INT$ s" pass the text with its length in bytes" CONFIG-REJECT
   then ;

\ Rejects a first text that is empty or of negative length; the second names
\ it as the rejection's expected text.
: REQUIRE-SOME ( str str -- )
   {: s:str ns:str :}
   s ns REQUIRE-LENGTH
   s STR:LEN 0= if
      ns s STR:DATA 0 >str s" pass a text of 1 byte or more" CONFIG-REJECT
   then ;

\ Stores the text REQUIRE-SOME passes in buf.
: REPLACE-SOME ( str str ^n -- )
   {: s:str ns:str buf:^ :}
   s ns REQUIRE-SOME
   s buf REPLACE$ ;

: SCRATCH-TARGET ( n ^call -- n )
   {: used:n c :}
   c SCRATCH-BASE-CELL @
   begin dup used < while 2 * repeat ;

\ Clears buf for a use of used bytes. One whose capacity is above the smallest
\ doubling of the scratch base that holds them is shrunk to that doubling, the
\ room a past use took given back; a shrink that throws leaves buf cleared.
: RESET-SIZED ( ^n n ^call -- )
   {: buf:^ used:n c :}
   used c SCRATCH-TARGET {: target:n :}
   buf BUF:CAP@ BUF:BLEN>N target > if
      buf BUF:CLEAR  buf target BUF:N>BLEN BUF:RESIZE
   else
      buf BUF:CLEAR
   then ;

: RESET-SCRATCH ( ^n ^call -- )
   {: buf:^ c :}
   buf buf BUF:LEN@ BUF:BLEN>N c RESET-SIZED ;

\ Empties v as RESET-SIZED clears a buffer for a use of the bytes v holds.
: RESET-BYTES ( ^VEC:vector<u8> ^call -- )
   {: v:^ c :}
   v VEC:COUNT c SCRATCH-TARGET {: target:n :}
   v 0 VEC:RESIZE
   v VEC:CAP target > if v target VEC:TRIM then ;

\ No reply usage seen yet.
: RESET-USAGE ( ^call -- )
   TOKENS {: t :}
   false t TOKENS-SEEN !
   OPTION:NONE t TOKENS-INPUT !
   OPTION:NONE t TOKENS-CACHED !
   OPTION:NONE t TOKENS-WRITTEN !
   OPTION:NONE t TOKENS-OUTPUT !
   OPTION:NONE t TOKENS-REASONING ! ;

\ No reply has named what served it yet.
: CLEAR-SERVED-BY ( ^call -- )
   SERVED-BY {: s :}
   s SERVED-BY-TIER @ 0 VEC:RESIZE
   s SERVED-BY-MODEL @ 0 VEC:RESIZE
   OPTION:NONE s SERVED-BY-CREATED ! ;

\ A call reports only the counts of its own answered reply.
: CLEAR-COUNTS ( ^call -- )
   {: c :}
   0 c MODEL-CALL:INPUT-TOKENS !
   0 c MODEL-CALL:CACHED-TOKENS !
   0 c MODEL-CALL:OUTPUT-TOKENS !
   0 c MODEL-CALL:REASONING-TOKENS !
   0 c MODEL-CALL:COUNTED ! ;

: OPEN-PREP ( ^call -- ^call )
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
   0 c MODEL-CALL:DEADLINE !
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
   false c MODEL-CALL:TIER-RESEND !
   0 c MODEL-CALL:RESEND-WAIT !
   construct tier-step preferred c RECEIPT RECEIPT-TIER !
   OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   0 c PICK-CELL !
   0 c MAX-OUTPUT-CELL !
   c CLEAR-COUNTS
   c RESET-USAGE
   c ;

: RELEASE-OPEN ( ^call -- )
   {: c :}
   c MODEL-CALL:GATE-READY @ if
      c MODEL-CALL:GATE @ TASK:FREE-SEMAPHORE
      false c MODEL-CALL:GATE-READY !
   then
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ {: base:^ :}
   base DISPOSE-BUFFERS
   base ARENA-BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE !
   0 c MODEL-CALL:ARENA MODEL-ALLOCATION:SIZE ! ;

: SCHEME-LEN ( str -- n )
   {: s:str :}
   s s" https://" STR:STARTS-WITH? if 8 exit then
   s s" http://" STR:STARTS-WITH? if 7 exit then
   0 ;

\ A base byte is printable and no userinfo, query or fragment delimiter, so
\ no credential rides in a URL that producer configs and journals record.
: BASE-BYTE? ( n -- bool )
   {: b:n :}
   b 32 > b 127 < and
   b [char] @ <> and b [char] ? <> and b [char] # <> and ;

\ The provider base every OpenAI-compatible route extends when none is set.
: OPENAI-BASE$ ( -- str ) s" https://api.openai.com/v1" ;

\ The configured base, or OpenAI's when none is set.
: BASE$ ( ^call -- str )
   BASE-URL$ {: base:str :}
   base STR:LEN 0= if OPENAI-BASE$ else base then ;

\ The base followed by the route; valid until the next rendering.
: URL$ ( str ^call -- str )
   {: route:str c :}
   c URL-BUF BUF:CLEAR
   c BASE$ {: base:str :}
   base c URL-BUF APPEND$
   route c URL-BUF APPEND$
   c URL-BUF BUFFER$ ;

: CHAT-URL$ ( ^call -- str )
   {: c :}
   s" /responses" c URL$ ;

: EMBED-URL$ ( ^call -- str )
   {: c :}
   s" /embeddings" c URL$ ;

: COUNT-URL$ ( ^call -- str )
   {: c :}
   s" /responses/input_tokens" c URL$ ;

: COMPACT-URL$ ( ^call -- str )
   {: c :}
   s" /responses/compact" c URL$ ;

: REQUEST-URL$ ( ^call -- str )
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

: DIMS-DEADLINE? ( n n -- )
   {: dims:n deadline:n :}
   dims 0 <= if
      s" an embedding dimension count of 1 or more"
      dims FMT:INT$
      s" pass the length of the model's vectors" CONFIG-REJECT
   then
   deadline 0 <= deadline CURL:MAX-TIMEOUT > or if
      s" an embedding deadline of 1 to CURL:MAX-TIMEOUT ms"
      deadline FMT:INT$
      s" pass the ms that may end each embedding call" CONFIG-REJECT
   then ;

: DIMS-DEADLINE! ( n n ^call -- )
   {: dims:n deadline:n c :}
   dims c MODEL-CALL:DIMS !
   deadline c MODEL-CALL:DEADLINE ! ;

public

: OPEN ( ^call -- )
   {: c :}
   c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE @ 0= 0= if E-STATE throw then
   ARENA-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:^ :}
   \ Fresh anonymous pages are zero; leave unused arena regions untouched.
   base c MODEL-CALL:ARENA MODEL-ALLOCATION:BASE !
   ARENA-BYTES c MODEL-CALL:ARENA MODEL-ALLOCATION:SIZE !
   false c MODEL-CALL:GATE-READY !
   c [: OPEN-PREP ;] catch {: code:n :} drop
   code 0<> if c RELEASE-OPEN code throw then ;


\ Frees the open call, a landed one included; a started one is E-STATE.
: CLOSE ( ^call -- )
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
: JOURNAL! ( n [ n str ^n -- bool ] [ n str str -- ] ^call -- )
   {: ctx:n get put c :}
   c REQUIRE-REST
   ctx c MODEL-CALL:JOURNAL-CONTEXT !
   get c MODEL-CALL:JOURNAL-GET !
   put c MODEL-CALL:JOURNAL-PUT !
   true c MODEL-CALL:JOURNAL-READY ! ;

\ Installs the recorder, which records each request a backend receives through
\ record and each rejected reply through reject, under the header's rules.
: RECORDER! ( n [ n str -- n ] [ n n -- ] ^call -- )
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
: PURPOSE? ( str -- bool )
   {: s:str :}
   s STR:LEN 0= if false exit then
   false s STR:LEN 0 ?do s STR:DATA i + c@ 32 > or loop ;

: PURPOSE! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s s" PURPOSE!'s purpose" REQUIRE-LENGTH
   s PURPOSE? 0= if
      s" a purpose with a byte other than a blank or a control" s
      s" name what the call is for" CONFIG-REJECT
   then
   s c PURPOSE-BUF REPLACE$
   s STR:LEN c LEDGER LEDGER-PENDING-U ! ;

\ Text naming the work the call does, as the caller names it, copied into the
\ call and put into each of its failure lines; empty clears it. It changes no
\ request, journal input, cache key or record. An idle or prepared call is
\ required, else E-STATE; a negative length is E-CONFIG.
: LOG-NOTE! ( str ^call -- )
   {: s:str c :}
   c REQUIRE-REST
   s s" LOG-NOTE!'s note" REQUIRE-LENGTH
   s c NOTE-BUF REPLACE$ ;

: BASE-URL? ( str -- bool )
   {: s:str :}
   s STR:LEN 0< if false exit then
   s SCHEME-LEN {: scheme-u:n :}
   scheme-u 0= s STR:LEN scheme-u <= or if false exit then
   s STR:DATA s STR:LEN + 1- c@ [char] / = if false exit then
   true s STR:LEN 0 ?do s STR:DATA i + c@ BASE-BYTE? 0= if drop false then loop ;

\ The OpenAI-compatible base the chat, count, compaction and embedding routes
\ extend; empty restores OpenAI's.
: BASE-URL! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s STR:LEN 0<> if s BASE-URL? 0= if
      s" BASE-URL!'s http or https base with no trailing slash, userinfo, query or fragment"
      s
      s" pass the provider's base, as https://api.openai.com/v1, or an empty text for OpenAI's"
      URL-REJECT
   then then
   OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   s STR:LEN 0= if c BASE-URL-BUF BUF:CLEAR exit then
   s c BASE-URL-BUF REPLACE$ ;

: GENERATION-MODEL! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s s" GENERATION-MODEL!'s model" c GEN-BUF REPLACE-SOME
   c FORGET-GEN-REPLAY ;

\ The reasoning effort of every chat and count without a purpose row; empty
\ sends none.
: EFFORT! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s STR:LEN 0= if c EFFORT-BUF BUF:CLEAR
   else s s" EFFORT!'s effort" c EFFORT-BUF REPLACE-SOME then
   c FORGET-GEN-REPLAY ;

\ The provider's prompt cache key of every later generation call: requests
\ sharing a key share cache routing. Empty sends none. It leaves the reply,
\ so the journal input and replay, unchanged.
: CACHE-KEY! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s STR:LEN 0= if c CACHE-KEY-BUF BUF:CLEAR
   else s s" CACHE-KEY!'s key" c CACHE-KEY-BUF REPLACE-SOME then ;

\ The most output tokens, reasoning included, of each later chat request alone,
\ 0 none; a chat's journal input names it, so a rejection replays only under it.
: MAX-OUTPUT! ( n ^call -- )
   {: n:n c :}
   c UNPREPARE drop
   n 0< if
      s" MAX-OUTPUT!'s count of 0 or more" n FMT:INT$
      s" pass the most output tokens, or 0 for no limit"
      CONFIG-REJECT
   then
   n c MAX-OUTPUT-CELL ! ;

\ The service tier every generation request without a purpose row names,
\ such as flex; empty sends none, so the provider serves its default.
\ Embedding requests carry none. The tier changes no content, so replay does
\ not depend on it.
: SERVICE-TIER! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s STR:LEN 0= if c TIER-BUF BUF:CLEAR
   else s s" SERVICE-TIER!'s tier" c TIER-BUF REPLACE-SOME then ;

\ Whether a call whose send fails E-TIER is sent again at once, once, with no
\ service tier, so the provider serves its default: each send is recorded,
\ the first tier_unavailable. A second E-TIER, a stopped call or a failed
\ recorder ends it.
: TIER-RESEND! ( bool ^call -- )
   {: on:bool c :}
   c REQUIRE-REST
   on c MODEL-CALL:TIER-RESEND ! ;

\ The most ms a send that ends in RESENDS?'s class rests before the same
\ request is sent again when its reply names no Retry-After: a random time
\ within the ms and within a second doubled with each resend. After a
\ Retry-After, at most 15 minutes of it, the rest adds that random time to
\ it. The resends have no cap: each send is recorded and its line names the
\ rest. A stop or CANCEL during it ends the call E-CANCELLED. 0, OPEN's, is
\ off; below 0 is E-CONFIG.
: RESEND-WAIT! ( n ^call -- )
   {: ms:n c :}
   c REQUIRE-REST
   ms 0< if
      s" RESEND-WAIT!'s ms of 0 or more" ms FMT:INT$
      s" pass the ms to rest before a resend, or 0 for none"
      CONFIG-REJECT
   then
   ms c MODEL-CALL:RESEND-WAIT ! ;

\ The generation model, effort and service tier of every later call for the
\ purpose, in place of the call's own; empty effort or tier sends none, even
\ when SERVICE-TIER! set one. A purpose takes one row: a repeated purpose is
\ E-CONFIG.
: PURPOSE-MODEL! ( str str str str ^call -- )
   {: p:str m:str e:str t:str c :}
   c UNPREPARE drop
   p s" PURPOSE-MODEL!'s purpose" REQUIRE-LENGTH
   p PURPOSE? 0= if
      s" PURPOSE-MODEL!'s purpose with a byte other than a blank or a control"
      p s" name what the purpose's calls are for" CONFIG-REJECT
   then
   m s" PURPOSE-MODEL!'s model" REQUIRE-SOME
   e s" PURPOSE-MODEL!'s effort" REQUIRE-LENGTH
   t s" PURPOSE-MODEL!'s service tier" REQUIRE-LENGTH
   p c ROW-OF 0<> if
      s" a purpose with no row yet" p
      s" give each purpose one PURPOSE-MODEL! row" CONFIG-REJECT
   then
   p 0 c ROW-TEXT+
   m 1 c ROW-TEXT+
   e 2 c ROW-TEXT+
   t 3 c ROW-TEXT+
   c ROW-NEW-AT ROW-BYTES REGION SPAN:$ {: row:str :}
   row c ROWS-BUF APPEND$
   c FORGET-GEN-REPLAY ;

\ The embedding model, its dimensions and the deadline in milliseconds that
\ ends each embedding call, connect included.
: EMBEDDING-MODEL! ( str n n ^call -- )
   {: s:str dims:n deadline:n c :}
   c REQUIRE-REST
   dims deadline DIMS-DEADLINE?
   s s" EMBEDDING-MODEL!'s model" c EMBED-BUF REPLACE-SOME
   dims deadline c DIMS-DEADLINE!
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
: API-KEY! ( str str ^call -- )
   {: svc:str acct:str c :}
   c REQUIRE-REST
   svc s" API-KEY!'s service" REQUIRE-SOME
   acct s" API-KEY!'s account" REQUIRE-SOME
   svc c KEY-SPEC-BUF REPLACE$
   acct c KEY-SPEC-BUF APPEND$
   svc STR:LEN c MODEL-CALL:KEY-SPLIT ! ;

: SYSTEM! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s s" SYSTEM!'s text" c SYSTEM-BUF REPLACE-SOME ;

: SCHEMA! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s s" SCHEMA!'s schema" c FORM-BUF REPLACE-SOME
   construct reply-form schema c MODEL-CALL:FORM ! ;

\ The tools a call offers, a JSON array of OpenAI function tools checked when
\ the request is finished; the last of SCHEMA! and TOOLS! decides its form.
: TOOLS! ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   s s" TOOLS!'s tools" c FORM-BUF REPLACE-SOME
   construct reply-form tools c MODEL-CALL:FORM ! ;

\ Model settings a host files in the native credential store under one
\ service of its choice, one generic-password item per fixed account:
\ "model" names the generation model (required), "base-url" an
\ OpenAI-compatible base BASE-URL! accepts (optional; absent or empty is
\ OpenAI's) and "api-key" holds the key API-KEY! names. A host keeps one in
\ zeroed storage (1 TYPED-BUFFER S MODEL:settings); SETTINGS-OPEN reads the
\ model and base once into bytes the settings own until SETTINGS-CLOSE.
STRUCTURE settings DERIVE addr
   FIELD bufs ^u8
   FIELD ready n
;STRUCTURE

private

0 constant SETTING-SERVICE
1 constant SETTING-MODEL
2 constant SETTING-BASE
3 constant SETTING-BUFFERS

\ BUFS holds SETTING-BUFFERS cells, each a setting's vector once READY counts it.
: SETTING-CELL ( ^settings n -- ^^VEC:vector<u8> )
   {: s idx:n :}
   s MODEL-SETTINGS:BUFS @ idx cells + >VEC-CELL ;

: SETTING-BUF ( ^settings n -- ^VEC:vector<u8> ) SETTING-CELL @ ;

: SETTING-ACCOUNT$ ( n -- str )
   case
      SETTING-MODEL of s" model" endof
      SETTING-BASE of s" base-url" endof
      E-STATE throw
   endcase ;

: SETTING$ ( ^settings n -- str ) SETTING-BUF VEC:BYTES ;


\ Reads the item of setting idx into its vector and returns its length; a
\ missing item reads as empty.
: SETTING-READ ( ^settings n -- n )
   {: s idx:n :}
   s idx SETTING-BUF {: v:^ :}
   s SETTING-SERVICE SETTING$ idx SETTING-ACCOUNT$
   v SECRET:READ if v VEC:COUNT else 0 then ;


: SETTINGS-FILL ( str ^settings -- str ^settings )
   {: svc:str s :}
   SETTING-BUFFERS 0 ?do
      HEAP:ALLOCATOR 0 [type] u8 VEC:NEW s i SETTING-CELL !
      i 1+ s MODEL-SETTINGS:READY !
   loop
   s SETTING-SERVICE SETTING-BUF {: sv:^ :}
   svc sv VEC:APPEND
   s SETTING-MODEL SETTING-READ 0= if
      s" a service whose model item names the generation model" svc
      s" file the model's name in the service's model item" CONFIG-REJECT
   then
   s SETTING-BASE SETTING-READ {: bu:n :}
   bu 0<> if s SETTING-BASE SETTING$ BASE-URL? 0= if
      s" a service whose base-url item holds a base BASE-URL! takes" svc
      s" file an http or https base with no trailing slash, userinfo, query or fragment in the service's base-url item, or erase the item"
      CONFIG-REJECT
   then then
   svc s ;


: SETTINGS-RELEASE ( ^settings -- )
   {: s :}
   s MODEL-SETTINGS:READY @ 0 ?do s i SETTING-BUF WIPE-DELETE loop
   s MODEL-SETTINGS:BUFS @ SETTING-BUFFERS cells
      MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR s MODEL-SETTINGS:BUFS !
   0 s MODEL-SETTINGS:READY ! ;

: SETTINGS-OPEN? ( ^settings -- bool )
   MODEL-SETTINGS:BUFS @ NULL-PTR = 0= ;

public

\ Reads service's model and base-url items into the closed settings s.
\ An open s is E-STATE. An empty service, a missing or empty model item, or a
\ non-empty base-url item BASE-URL! rejects is E-CONFIG; SECRET:E-CONFIG,
\ SECRET:E-ACCESS and SECRET:E-UNAVAILABLE pass through. A failed open
\ leaves s closed.
: SETTINGS-OPEN ( str ^settings -- )
   {: svc:str s :}
   s SETTINGS-OPEN? if E-STATE throw then
   svc s" SETTINGS-OPEN's service" REQUIRE-SOME
   SETTING-BUFFERS cells MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop
      s MODEL-SETTINGS:BUFS !
   0 s MODEL-SETTINGS:READY !
   svc s [: SETTINGS-FILL ;] catch {: code:n :}
   code REASON:TEXT$ {: t:str :}
   drop drop
   code 0<> if
      s SETTINGS-RELEASE
      t STR:LEN 0<> if t code REASON:THROW then
      code throw
   then ;

\ Releases the settings' bytes; closed settings are E-STATE.
: SETTINGS-CLOSE ( ^settings -- )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTINGS-RELEASE ;

\ The open settings' service, model and base URL (empty when its item is
\ absent or empty). The bytes stay owned by the settings until SETTINGS-CLOSE.
\ Closed settings are E-STATE.
: SETTINGS-SERVICE$ ( ^settings -- str )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-SERVICE SETTING$ ;

: SETTINGS-MODEL$ ( ^settings -- str )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-MODEL SETTING$ ;

: SETTINGS-BASE$ ( ^settings -- str )
   {: s :}
   s SETTINGS-OPEN? 0= if E-STATE throw then
   s SETTING-BASE SETTING$ ;

\ Sets the call's generation model, base URL and the service's "api-key" item
\ as its key, copying each into the call: the settings may close after.
\ A busy or unopened call, or closed settings, is E-STATE.
: SETTINGS-APPLY ( ^settings ^call -- )
   {: s c :}
   c REQUIRE-REST
   s SETTINGS-MODEL$ c GENERATION-MODEL!
   s SETTINGS-BASE$ c BASE-URL!
   s SETTINGS-SERVICE$ s" api-key" c API-KEY! ;

private

: WRITER ( ^call -- ^JSON:writer ) MODEL-CALL:BODY-W ;
: PART-WRITER ( ^call -- ^JSON:writer ) MODEL-CALL:PARTS-W ;
: JOURNAL-PART-WRITER ( ^call -- ^JSON:writer ) MODEL-CALL:JOURNAL-PARTS-W ;

\ The flag says whether the configuration carries the effort, when one is set.
: BUILD-GEN-CONFIG ( bool ^call -- )
   {: shaped:bool c :}
   c WRITER c GEN-CONFIG-BYTES JSON:OPEN drop
   c WRITER JSON:WRITE-OBJECT-START
   s" backend" s" openai-responses" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" model" c GEN$ JSON:WRITE-FIELD-S
   shaped c EFFORT$ STR:LEN 0<> and if
      JSON:WRITE-COMMA s" effort" c EFFORT$ JSON:WRITE-FIELD-S
   then
   JSON:WRITE-COMMA s" base_url" c BASE$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:CLOSE ;

: BUILD-EMBED-CONFIG ( ^call -- )
   {: c :}
   c WRITER c EMBED-CONFIG-BYTES JSON:OPEN drop
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
   c WRITER JSON:CLOSE ;

public

: GENERATION-CONFIG$ ( ^call -- str )
   {: c :}
   c REQUIRE-REST
   construct call-kind chat c REPLAYED? 0= if true c BUILD-GEN-CONFIG then
   c GEN-CONFIG$ ;

: EMBEDDING-CONFIG$ ( ^call -- str )
   {: c :}
   c REQUIRE-REST
   construct call-kind embed c REPLAYED? 0= if c BUILD-EMBED-CONFIG then
   c EMBED-CONFIG$ ;

\ Ollama's native /api/embed at endpoint. identity names the model artifact,
\ such as its manifest digest, and is the vector space's identity: a changed
\ artifact needs a new one even under the same tag, and the endpoint is a
\ producer input outside it. Requests send truncate:false because Ollama
\ truncates by default; a context overflow fails the call. dims and the
\ deadline in milliseconds are as EMBEDDING-MODEL! takes them.
: OLLAMA! ( str str str n n ^call -- )
   {: endpoint:str model:str identity:str dims:n deadline:n c :}
   c REQUIRE-REST
   dims deadline DIMS-DEADLINE?
   endpoint BASE-URL? 0= if
      s" OLLAMA!'s http or https endpoint with no trailing slash, userinfo, query or fragment"
      endpoint
      s" pass the Ollama server's embed URL, as http://localhost:11434/api/embed"
      URL-REJECT
   then
   model s" OLLAMA!'s model" REQUIRE-SOME
   identity s" OLLAMA!'s identity" REQUIRE-SOME
   endpoint c ENDPOINT-BUF REPLACE$
   model c EMBED-BUF REPLACE$
   identity c IDENTITY-BUF REPLACE$
   dims deadline c DIMS-DEADLINE!
   OLLAMA c MODEL-CALL:EMBEDDING-KIND !
   construct call-kind embed c REPLAYED? if
      OPTION:NONE c MODEL-CALL:REPLAY-KIND !
   then ;

private

: PART-OPEN ( ^call -- )
   {: c :}
   c PART-WRITER c PARTS-BYTES JSON:OPEN JSON:WRITE-ARRAY-START drop
   c JOURNAL-PART-WRITER c JOURNAL-PARTS-BYTES JSON:OPEN
   JSON:WRITE-ARRAY-START drop ;

: PART-FINISH ( ^call -- )
   {: c :}
   c REQUIRE-BUILDING
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c PART-WRITER JSON:WRITE-ARRAY-END JSON:CLOSE
   c JOURNAL-PART-WRITER JSON:WRITE-ARRAY-END JSON:CLOSE
   construct input-state finished c MODEL-CALL:CHAT-INPUT ! ;

\ Finishes a building input and passes a finished one, as a count or a
\ compaction leaves it.
: FINISH-INPUT ( ^call -- )
   {: c :}
   c MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF E-STATE throw ENDOF
      building OF c PART-FINISH ENDOF
      broken OF E-STATE throw ENDOF
      finished OF ENDOF
   ;MATCH ;

: PART-COMMA ( ^call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 0 > if
      c PART-WRITER JSON:WRITE-COMMA drop
      c JOURNAL-PART-WRITER JSON:WRITE-COMMA drop
   then ;

: PART-ADDED ( ^call -- )
   {: c :}
   c MODEL-CALL:PARTS-COUNT @ 1+ c MODEL-CALL:PARTS-COUNT !
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

: BREAKPOINT-MEMBER ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-COMMA s" prompt_cache_breakpoint" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" mode" s" explicit" JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;

: TEXT-PART ( str bool ^JSON:writer -- )
   {: s:str bp:bool w :}
   w JSON:WRITE-OBJECT-START
   s" type" s" input_text" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" text" s JSON:WRITE-FIELD-S
   bp if BREAKPOINT-MEMBER then
   JSON:WRITE-OBJECT-END drop ;

\ Drops the input's parts, their journal copy and its items.
: EMPTY-INPUT ( ^call -- )
   {: c :}
   c PARTS-BYTES c RESET-BYTES
   c JOURNAL-PARTS-BYTES c RESET-BYTES
   c ITEMS-BUF c RESET-SCRATCH
   0 c MODEL-CALL:PARTS-COUNT ! ;

\ Adds a text part, the request's copy carrying the breakpoint member when the
\ flag holds; the second text names the first in a rejection.
: ADD-TEXT ( str bool str ^call -- )
   {: s:str bp:bool ns:str c :}
   c UNPREPARE drop
   c REQUIRE-BUILDING
   s ns REQUIRE-LENGTH
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c PART-COMMA
   s bp c PART-WRITER TEXT-PART
   s false c JOURNAL-PART-WRITER TEXT-PART
   c PART-ADDED ;

public

: PARTS-RESET ( ^call -- )
   {: c :}
   c UNPREPARE drop
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   0 c MODEL-CALL:HTTP-STATUS !
   c EMPTY-INPUT
   c PART-OPEN
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

: TEXT+ ( str ^call -- )
   {: s:str c :}
   s false s" TEXT+'s text" c ADD-TEXT ;

\ TEXT+, the part asking the provider to cache the prompt up to its end. It
\ leaves the reply, so the journal input and replay, unchanged.
: CACHE-TEXT+ ( str ^call -- )
   {: s:str c :}
   s true s" CACHE-TEXT+'s text" c ADD-TEXT ;

private

: IMAGE-MIME? ( str -- bool )
   {: s:str :}
   s s" image/png" STR:= if true exit then
   s s" image/jpeg" STR:= if true exit then
   s s" image/webp" STR:= if true exit then
   s s" image/gif" STR:= ;

: DIGEST-KEEP ( str ^call -- str ^call )
   {: data:str c :}
   data data STR:LEN 4 / 3 * c URI-BUF ROOM BASE64:DECODE {: n:n :}
   c HASH-WORK {: w:^ :}
   w c URI-BUF BUF:SPAN$ drop n >str w SHA256-CTX-BYTES + SHA256-IN
   w SHA256-CTX-BYTES + w SHA256-CTX-BYTES + 32 + SHA256>HEX
   data c ;

\ The SHA-256 hex of the bytes non-empty base64 data encodes.
: IMAGE-DIGEST ( str ^call -- str )
   {: data:str c :}
   data STR:LEN 0= if
      s" IMAGE+'s image data in base64" s" "
      s" pass the image's bytes as BASE64:ENCODE writes them" CONFIG-REJECT
   then
   data c [: DIGEST-KEEP ;] catch {: code:n :} drop drop
   code REASON:TEXT$ {: t:str :}
   code 0<> if c URI-BUF c RESET-SCRATCH then
   code E-BASE64-FIRST <= code E-BASE64-LAST >= and if
      s" IMAGE+'s image data" t NAMED-THROW
   then
   code 0<> if code throw then
   c HASH-WORK SHA256-CTX-BYTES + 32 + 64 >str ;

public

\ The request carries the image inline; the journal input names it by media
\ type and the SHA-256 of its bytes, so a saved step stays small and the same
\ image replays it.
: IMAGE+ ( str str ^call -- )
   {: mime:str data:str c :}
   c UNPREPARE drop
   c REQUIRE-BUILDING
   mime s" IMAGE+'s media type" REQUIRE-LENGTH
   data s" IMAGE+'s image data" REQUIRE-LENGTH
   mime IMAGE-MIME? 0= if
      s" IMAGE+'s media type image/png, image/jpeg, image/webp or image/gif"
      mime s" pass one of those media types" CONFIG-REJECT
   then
   data c IMAGE-DIGEST {: hex:str :}
   c URI-BUF BUF:CLEAR
   s" data:" c URI-BUF APPEND$
   mime c URI-BUF APPEND$
   s" ;base64," c URI-BUF APPEND$
   data c URI-BUF APPEND$
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
   JSON:WRITE-COMMA s" media_type" mime JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" sha256" hex JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END drop
   c URI-BUF BUF:CLEAR
   c URI-BUF c RESET-SCRATCH
   c PART-ADDED ;

private

: SETTING-THROW ( str str n -- )
   {: ns:str ts:str code:n :}
   code JSON:INPUT-FAULT? if ns ts NAMED-THROW then
   code 0<> if code throw then ;

: OBJECT-FIX$ ( -- str ) s" pass one JSON object" ;

: OBJECT-STEP ( str -- str )
   dup JSON:PARSE {: v :}
   v JSON:OBJECT? 0= if
      v s" an object" OBJECT-FIX$ JSON:REJECT
   then ;

\ Bytes that are not exactly one JSON object with unique member names are
\ E-CONFIG.
: CHECK-OBJECT ( str str -- )
   {: ns:str :}
   [: OBJECT-STEP ;] catch {: code:n :} drop
   code REASON:TEXT$ {: ts:str :}
   ns ts code SETTING-THROW ;

: TOOLS-FIX$ ( -- str ) s" pass one JSON array of one or more tool objects" ;

\ After the first element, `]` may also end the array.
: TOOL-EXPECTED$ ( n -- str )
   0= if s" an object" exit then
   s" an object or `]`" ;

: TOOL-STEP ( n JSON:value -- n )
   {: k:n v :}
   v JSON:OBJECT? 0= if
      v k TOOL-EXPECTED$ TOOLS-FIX$ JSON:REJECT
   then
   k 1+ ;

: TOOLS-STEP ( str -- str )
   dup JSON:PARSE {: v :}
   v JSON:ARRAY? 0= if v s" an array" TOOLS-FIX$ JSON:REJECT then
   0 v [: TOOL-STEP ;] JSON:EACH 0= if
      v s" a non-empty array" TOOLS-FIX$ JSON:REJECT
   then ;

\ Bytes that are not one non-empty JSON array of objects with unique member
\ names are E-CONFIG.
: CHECK-TOOLS ( str str -- )
   {: ns:str :}
   [: TOOLS-STEP ;] catch {: code:n :} drop
   code REASON:TEXT$ {: ts:str :}
   ns ts code SETTING-THROW ;

\ Appends an item's bytes to the items, after a single comma once there are
\ any.
: ITEM-JOIN ( str ^call -- )
   {: s:str c :}
   c ITEMS-BUF BUF:LEN@ BUF:BLEN>N 0<> if
      s" ," c ITEMS-BUF APPEND$
   then
   s c ITEMS-BUF APPEND$ ;

public

\ One OpenAI history item, E-CONFIG unless one JSON object with unique member
\ names; body and journal hold it verbatim after the parts' message, in order.
: ITEM+ ( str ^call -- )
   {: s:str c :}
   c UNPREPARE drop
   c REQUIRE-BUILDING
   s s" ITEM+'s item" REQUIRE-LENGTH
   s s" ITEM+'s item" CHECK-OBJECT
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   s c ITEM-JOIN
   construct input-state building c MODEL-CALL:CHAT-INPUT ! ;

private

\ A request's input: a model, instructions and at least one part or item, else
\ E-CONFIG.
: CHECK-INPUT ( ^call -- )
   {: c :}
   c GEN$ STR:LEN 0= if
      s" a generation model set by GENERATION-MODEL!" c GEN$
      s" pass the model's name to GENERATION-MODEL!" CONFIG-REJECT
   then
   c SYSTEM$ STR:LEN 0= if
      s" instructions set by SYSTEM!" c SYSTEM$
      s" pass the instructions to SYSTEM!" CONFIG-REJECT
   then
   c MODEL-CALL:PARTS-COUNT @ 0= c ITEMS$ STR:LEN 0= and if
      s" at least one part or item" c MODEL-CALL:PARTS-COUNT @ FMT:INT$
      s" add a part with TEXT+ or IMAGE+, or an item with ITEM+" CONFIG-REJECT
   then ;

: NO-FORM ( -- )
   s" a reply form set by SCHEMA! or TOOLS!" s" "
   s" pass the reply's JSON schema to SCHEMA! or its tools to TOOLS!"
   CONFIG-REJECT ;

: CHECK-CHAT ( ^call -- )
   {: c :}
   c CHECK-INPUT
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF NO-FORM ENDOF
      schema OF c FORM$ s" SCHEMA!'s schema" CHECK-OBJECT ENDOF
      tools OF c FORM$ s" TOOLS!'s tools" CHECK-TOOLS ENDOF
   ;MATCH ;

: BODY-OPEN ( ^call -- )
   {: c :}
   c BODY-BYTES c RESET-BYTES
   c WRITER c BODY-BYTES JSON:OPEN drop ;

: BODY-DONE ( ^call -- )
   {: c :}
   c WRITER JSON:CLOSE ;

\ The members after a schema call's input: its strict JSON schema format.
: SCHEMA-TAIL ( ^JSON:writer ^call -- ^JSON:writer )
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
: TOOL-MEMBERS ( ^JSON:writer ^call -- ^JSON:writer )
   {: c :}
   JSON:WRITE-COMMA s" tools" c FORM$ JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" tool_choice" s" required" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" parallel_tool_calls" false JSON:WRITE-FIELD-BOOL ;

\ The effort member, when the call sends one.
: EFFORT-MEMBER ( ^JSON:writer ^call -- ^JSON:writer )
   {: c :}
   c EFFORT$ STR:LEN 0<> if
      JSON:WRITE-COMMA s" reasoning" JSON:WRITE-KEY
      JSON:WRITE-OBJECT-START
      s" effort" c EFFORT$ JSON:WRITE-FIELD-S
      JSON:WRITE-OBJECT-END
   then ;

\ The prompt cache key and the service tier, each when the call sets one.
: SERVICE-MEMBERS ( ^JSON:writer ^call -- ^JSON:writer )
   {: c :}
   c CACHE-KEY$ STR:LEN 0<> if
      JSON:WRITE-COMMA s" prompt_cache_key" c CACHE-KEY$ JSON:WRITE-FIELD-S
   then
   c TIER$ STR:LEN 0<> if
      JSON:WRITE-COMMA s" service_tier" c TIER$ JSON:WRITE-FIELD-S
   then ;

\ The output limit member, when the call sets one.
: OUTPUT-MEMBER ( ^JSON:writer ^call -- ^JSON:writer )
   {: c :}
   c MAX-OUTPUT-CELL @ 0 > if
      JSON:WRITE-COMMA s" max_output_tokens" c MAX-OUTPUT-CELL @ JSON:WRITE-FIELD-U
   then ;

\ The instructions and the input: the parts' message, then the items.
: INPUT-MEMBERS ( ^JSON:writer ^call -- ^JSON:writer )
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
   c ITEMS$ STR:LEN 0<> if
      head if JSON:WRITE-COMMA then
      c ITEMS$ JSON:WRITE-RAW
   then
   JSON:WRITE-ARRAY-END ;

: CHAT-BODY ( ^call -- )
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
      none OF NO-FORM ENDOF
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
: COUNT-BODY ( ^call -- )
   {: c :}
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c GEN$ JSON:WRITE-FIELD-S
   c EFFORT-MEMBER
   c INPUT-MEMBERS
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF NO-FORM ENDOF
      schema OF c SCHEMA-TAIL ENDOF
      tools OF c TOOL-MEMBERS ENDOF
   ;MATCH
   JSON:WRITE-OBJECT-END drop
   c BODY-DONE ;

\ The compact endpoint rejects tools, reasoning, text, store, stream and
\ include.
: COMPACT-BODY ( ^call -- )
   {: c :}
   c BODY-OPEN
   c WRITER
   JSON:WRITE-OBJECT-START
   s" model" c GEN$ JSON:WRITE-FIELD-S
   c SERVICE-MEMBERS
   c INPUT-MEMBERS
   JSON:WRITE-OBJECT-END drop
   c BODY-DONE ;

: INPUT$ ( n ^call -- str )
   {: idx:n c :}
   c INPUT-BUF BUF:SPAN
   c idx INPUT-OFF @ c idx INPUT-LEN @ SPAN:SUB SPAN:$ ;

: CHECK-EMBED ( ^call -- )
   {: c :}
   c EMBED-MODEL$ STR:LEN 0= if
      s" an embedding model set by EMBEDDING-MODEL! or OLLAMA!" c EMBED-MODEL$
      s" pass the model's name to EMBEDDING-MODEL! or OLLAMA!" CONFIG-REJECT
   then
   c MODEL-CALL:DIMS @ 0= if
      s" EMBEDDING-MODEL!'s or OLLAMA!'s dimensions of 1 or more"
      c MODEL-CALL:DIMS @ FMT:INT$
      s" pass the model's dimensions to EMBEDDING-MODEL! or OLLAMA!" CONFIG-REJECT
   then
   c MODEL-CALL:INPUT-COUNT @ 0= if
      s" at least one input" c MODEL-CALL:INPUT-COUNT @ FMT:INT$
      s" add each text to embed with INPUT+" CONFIG-REJECT
   then ;

: EMBED-BODY ( ^call -- )
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
: DROP-VECTORS ( ^call -- )
   {: c :}
   c MODEL-CALL:RESULT @ MATCH call-result
      none OF ENDOF
      content OF ENDOF
      vectors OF construct call-result none c MODEL-CALL:RESULT ! ENDOF
   ;MATCH ;

public

: INPUT-RESET ( ^call -- )
   {: c :}
   c REQUIRE-REST
   0 c MODEL-CALL:HTTP-STATUS !
   c INPUT-BUF BUF:CLEAR
   0 c MODEL-CALL:INPUT-U !
   0 c MODEL-CALL:INPUT-COUNT !
   c DROP-VECTORS ;

: INPUT+ ( str ^call -- )
   {: s:str c :}
   c REQUIRE-REST
   s s" INPUT+'s input" REQUIRE-SOME
   c MODEL-CALL:INPUT-COUNT @ INPUT-MAX >= if E-CAPACITY throw then
   c MODEL-CALL:INPUT-U @ c c MODEL-CALL:INPUT-COUNT @ INPUT-OFF !
   s STR:LEN c c MODEL-CALL:INPUT-COUNT @ INPUT-LEN !
   s c INPUT-BUF APPEND$
   c MODEL-CALL:INPUT-U @ s STR:LEN + c MODEL-CALL:INPUT-U !
   c MODEL-CALL:INPUT-COUNT @ 1+ c MODEL-CALL:INPUT-COUNT !
   c DROP-VECTORS ;

private

\ The member that carries the form's text in a chat's journal input.
: FORM-KEY$ ( ^call -- str )
   MODEL-CALL:FORM @ MATCH reply-form
      none OF NO-FORM ENDOF
      schema OF s" schema" ENDOF
      tools OF s" tools" ENDOF
   ;MATCH ;

\ A generation kind's journal kind and its endpoint; an embedding's is
\ JOURNAL-EMBED-INPUT.
: GEN-INPUT-KIND ( call-kind ^call -- str str )
   {: k:call-kind c :}
   k MATCH call-kind
      chat OF s" generation" c CHAT-URL$ ENDOF
      embed OF E-STATE throw ENDOF
      tally OF s" input_tokens" c COUNT-URL$ ENDOF
      compaction OF s" compaction" c COMPACT-URL$ ENDOF
   ;MATCH ;

\ A generation call's journal input for the kind.
: JOURNAL-GEN-INPUT ( call-kind ^call -- )
   {: k:call-kind c :}
   k c GEN-INPUT-KIND {: kind:str url:str :}
   k SHAPED? {: shaped:bool :}
   c JOURNAL-INPUT-BYTES c RESET-BYTES
   c WRITER c JOURNAL-INPUT-BYTES JSON:OPEN drop
   c WRITER JSON:WRITE-OBJECT-START
   s" kind" kind JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" configuration" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   s" backend" s" openai-responses" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" model" c GEN$ JSON:WRITE-FIELD-S
   shaped c EFFORT$ STR:LEN 0<> and if
      JSON:WRITE-COMMA s" effort" c EFFORT$ JSON:WRITE-FIELD-S
   then
   k LIMITED? if c OUTPUT-MEMBER then
   JSON:WRITE-COMMA s" endpoint" url JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END
   JSON:WRITE-COMMA s" instructions" c SYSTEM$ JSON:WRITE-FIELD-S
   shaped if JSON:WRITE-COMMA c FORM-KEY$ c FORM$ JSON:WRITE-FIELD-RAW then
   JSON:WRITE-COMMA s" parts" c JOURNAL-PARTS$ JSON:WRITE-FIELD-RAW
   c ITEMS$ STR:LEN 0<> if
      JSON:WRITE-COMMA s" items" JSON:WRITE-KEY
      JSON:WRITE-ARRAY-START c ITEMS$ JSON:WRITE-RAW JSON:WRITE-ARRAY-END
   then
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:CLOSE ;

: JOURNAL-EMBED-INPUT ( ^call -- )
   {: c :}
   c JOURNAL-INPUT-BYTES c RESET-BYTES
   c WRITER c JOURNAL-INPUT-BYTES JSON:OPEN drop
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
   c WRITER JSON:CLOSE ;

: TRANSPORT-FAILED ( CURL:code ^call -- )
   LEDGER {: code:CURL:code l :}
   code CURL:CODE>N l LEDGER-TRANSPORT !
   E-TRANSPORT throw ;

: CHECK-STATUS ( CURL:status ^call -- )
   {: s:CURL:status c :}
   s MATCH CURL:status
      ok OF ENDOF
      failed OF c TRANSPORT-FAILED ENDOF
   ;MATCH ;

: OPEN-HANDLE ( ^call -- CURL:handle )
   {: c :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF c TRANSPORT-FAILED ENDOF
   ;MATCH ;

: HEADER-WIPE ( ^call -- ) KEY-LINE-BUF 0 VEC:RESIZE ;

: WIPE-AFTER ( n ^call -- )
   {: fault:n c :}
   c HEADER-WIPE
   fault 0<> if fault throw then ;

: KEY-PREFIX$ ( -- str ) s" Authorization: Bearer " ;

: KEY-FAULT! ( key-fault ^call -- ) RECEIPT RECEIPT-KEY-READ ! ;

\ Keeps how the key read failed and throws E-KEY.
: KEY-REJECT ( key-fault ^call -- ) KEY-FAULT! E-KEY throw ;

\ A key is one or more bytes from ! to ~.
: KEY-CHECK ( str ^call -- )
   {: s:str c :}
   s STR:LEN 0= if construct key-fault empty c KEY-REJECT then
   s STR:LEN 0 ?do
      s STR:DATA i + c@ {: b:n :}
      b 33 < b 126 > or if
         construct key-fault unprintable c KEY-REJECT
      then
   loop ;


\ Appends the store's item to the header line after its prefix; a missing,
\ empty or non-printable item is E-KEY.
: KEY-READ ( ^call -- )
   {: c :}
   c KEY-SPEC$ {: spec:str :}
   c MODEL-CALL:KEY-SPLIT @ {: su:n :}
   c KEY-LINE-BUF {: line:^ :}
   spec STR:DATA su >str spec STR:DATA su + spec STR:LEN su - >str
   line SECRET:READ
   0= if construct key-fault missing c KEY-REJECT then
   line VEC:SPAN KEY-PREFIX$ STR:LEN SPAN:SKIP SPAN:$ c KEY-CHECK ;


\ How the store's rejection failed the key read; none for another fault.
: STORE-FAULT ( n -- key-fault )
   {: code:n :}
   code SECRET:E-ACCESS = if construct key-fault denied exit then
   code SECRET:E-UNAVAILABLE = if construct key-fault unavailable exit then
   code SECRET:E-CONFIG = if construct key-fault config exit then
   construct key-fault none ;


\ The store's own failures pass through, kept as how the key read failed.
: KEY-FETCH ( ^call -- )
   {: c :}
   [: c KEY-READ ;] catch {: fault:n :}
   fault 0= if exit then
   fault E-KEY <> if fault STORE-FAULT c KEY-FAULT! then
   fault throw ;


: KEY-LINE ( ^call -- str )
   {: c :}
   c KEY-SPEC$ {: spec:str :}
   spec STR:LEN 0= if
      s" an API key item named by API-KEY!" spec
      s" pass API-KEY! the service and account of the store item holding the key"
      CONFIG-REJECT
   then
   \ The prefix goes in first: growth for the key comes before the key's first
   \ byte, so a freed block holds only the prefix.
   KEY-PREFIX$ c KEY-LINE-BUF VEC:APPEND
   c KEY-FETCH
   c KEY-LINE-BUF VEC:BYTES ;

: KEY-HEADER-KEEP ( CURL:handle ^call -- CURL:handle ^call )
   2dup KEY-LINE CURL:HEADER+ over CHECK-STATUS ;

\ Whether the call's request carries the API key: all but an Ollama embed.
: KEYED? ( ^call -- bool )
   {: c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF true ENDOF
      embed OF c MODEL-CALL:EMBEDDING-KIND @ OLLAMA <> ENDOF
      tally OF true ENDOF
      compaction OF true ENDOF
   ;MATCH ;

: SET-HEADERS ( CURL:handle ^call -- )
   {: h:CURL:handle c :}
   h s" Content-Type: application/json" CURL:HEADER+ c CHECK-STATUS
   c KEYED? if
      h c [: KEY-HEADER-KEEP ;] catch {: fault:n :} 2drop
      fault c WIPE-AFTER
   then ;

\ Starts an exchange as the call's transport, which CANCEL ends.
: START-LOCKED ( ^call -- ^call )
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

: START-TRANSFER ( ^call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: START-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> if code throw then ;

\ Clears the call's transfer, keeping in the receipt whether a CANCEL cut it:
\ CANCEL-LOCKED clears the transfer it cuts.
: END-TRANSFER ( ^call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:TRANSPORT @ MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH c RECEIPT RECEIPT-CUT !
   OPTION:NONE c MODEL-CALL:TRANSPORT !
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

\ Ollama's error text for an input longer than the model's context.
: OLLAMA-SIZE? ( str -- bool )
   {: s:str :}
   s s" exceeds the context length" STR:CONTAINS?
   s s" exceeds maximum context length" STR:CONTAINS? or ;

\ OpenAI's error codes for a request past the model's size.
: SIZE-CODE? ( JSON:value -- bool )
   {: v :}
   v s" context_length_exceeded" JSON:STRING=
   v s" string_above_max_length" JSON:STRING= or ;

: SIZE-TEXT? ( str -- bool )
   s" This model's maximum context length" STR:STARTS-WITH? ;

: SIZE-MESSAGE? ( JSON:value -- bool )
   {: v :}
   v JSON:STRING? 0= if false exit then
   v [: SIZE-TEXT? ;] JSON:WITH-STRING ;

\ The test's answer for a value present, false for none.
: FOUND? ( option<JSON:value> [ JSON:value -- bool ] -- bool )
   {: test :}
   MATCH option
      none OF false ENDOF
      some OF test execute ENDOF
   ;MATCH ;

\ Whether OpenAI's error object names a rejection for size: by its code, or by
\ its message, as an embeddings rejection with a null code does.
: OPENAI-SIZE? ( JSON:value -- bool )
   {: v :}
   v s" code" JSON:FIND [: SIZE-CODE? ;] FOUND?
   v s" message" JSON:FIND [: SIZE-MESSAGE? ;] FOUND? or ;

\ The reply's error member: Ollama's text, or OpenAI's object.
: SIZE-ERROR? ( JSON:value -- bool )
   {: v :}
   v JSON:STRING? if v [: OLLAMA-SIZE? ;] JSON:WITH-STRING exit then
   v JSON:OBJECT? if v OPENAI-SIZE? exit then
   false ;

\ The provider's message, kept from the reply's error member: the member
\ itself when a string, as Ollama sends it, else its message, as OpenAI and
\ Azure send it.
: ERROR-TEXT ( JSON:value ^call -- )
   {: v c :}
   v JSON:OBJECT? if
      v s" message" JSON:FIND MATCH option
         none OF exit ENDOF
         some OF ENDOF
      ;MATCH
   else v then {: m :}
   m JSON:STRING? 0= if exit then
   m [: c ERROR-BUF REPLACE$ ;] JSON:WITH-STRING ;

\ Keeps that the provider throttled the send.
: THROTTLED! ( ^call -- ) true swap LEDGER LEDGER-THROTTLED ! ;

\ Keeps the longest wait, in seconds, the send's reply asked for.
: RETRY-AFTER! ( n ^call -- )
   RECEIPT RECEIPT-RETRY-AFTER tuck @ max swap ! ;

\ OpenAI's error codes for a send it throttled.
: THROTTLE-CODE? ( JSON:value -- bool )
   {: v :}
   v s" rate_limit_exceeded" JSON:STRING=
   v s" slow_down" JSON:STRING= or ;

\ The whole seconds the text gives, 0 for any other text.
: SECONDS ( str -- n )
   STR:PARSE-POS MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The seconds an error's headers ask in retry-after to wait, 0 for none.
: HEADERS-WAIT ( JSON:value -- n )
   {: v :}
   v JSON:OBJECT? 0= if 0 exit then
   v s" retry-after" JSON:FIND MATCH option
      none OF 0 exit ENDOF
      some OF ENDOF
   ;MATCH {: t :}
   t JSON:STRING? 0= if 0 exit then
   t [: SECONDS ;] JSON:WITH-STRING ;

\ The seconds a failed reply's error object asks to wait, 0 for none: OpenAI
\ sends the reply's HTTP headers before the response fails, so an error event
\ holds its own.
: ERROR-WAIT ( JSON:value -- n )
   s" headers" JSON:FIND MATCH option
      none OF 0 ENDOF
      some OF HEADERS-WAIT ENDOF
   ;MATCH ;

\ Keeps the wait an error object asks for and whether its code says the send
\ was throttled.
: KEEP-BACKOFF ( JSON:value ^call -- )
   {: e c :}
   e ERROR-WAIT c RETRY-AFTER!
   e s" code" JSON:FIND [: THROTTLE-CODE? ;] FOUND? if c THROTTLED! then ;

: ERROR-BODY ( JSON:value ^call -- )
   {: v c :}
   v JSON:OBJECT? 0= if exit then
   v s" error" JSON:FIND MATCH option
      none OF exit ENDOF
      some OF ENDOF
   ;MATCH {: e :}
   e c ERROR-TEXT
   e JSON:OBJECT? if e c KEEP-BACKOFF then ;

\ A failed reply's body, none when it is not JSON.
: FAILED-BODY ( ^call -- option<JSON:value> )
   {: c :}
   [: c RESPONSE$ JSON:PARSE ;] catch {: v code:n :}
   code 0= if v OPTION:SOME exit then
   code JSON:INPUT-FAULT? if OPTION:NONE exit then
   code throw ;

\ Keeps a failed reply's provider message, wait and throttle, when its body
\ names them.
: KEEP-ERROR ( option<JSON:value> ^call -- )
   {: body c :}
   body MATCH option
      none OF ENDOF
      some OF c ERROR-BODY ENDOF
   ;MATCH ;

: SIZE-BODY? ( JSON:value -- bool )
   {: v :}
   v JSON:OBJECT? 0= if false exit then
   v s" error" JSON:FIND [: SIZE-ERROR? ;] FOUND? ;

\ Whether a failed reply rejects the request for its size: HTTP 413, or a 400
\ whose body names the rejection.
: SIZE-REJECTION? ( option<JSON:value> ^call -- bool )
   {: body c :}
   c MODEL-CALL:HTTP-STATUS @ 413 = if true exit then
   c MODEL-CALL:HTTP-STATUS @ 400 <> if false exit then
   body [: SIZE-BODY? ;] FOUND? ;


\ Room in the response buffer for at least one byte past those received.
: GROW-REPLY ( ^call -- ^call )
   {: c :}
   c RESPONSE-BUF {: buf:^ :}
   buf buf BUF:LEN@ BUF:BLEN>N 1+ BUF:N>BLEN BUF:ENSURE
   c ;


\ The fault a rejected reply's status names when its body names no rejection for
\ size.
: STATUS-FAULT ( CURL:http-status -- n )
   {: status:CURL:http-status :}
   status CURL:UNAVAILABLE? if E-UNAVAILABLE exit then
   status CURL:HTTP-STATUS>N {: code:n :}
   code 401 = code 403 = or if E-KEY exit then
   code 404 = if E-CONFIG exit then
   E-HTTP ;


\ Throws STATUS-FAULT's E-CONFIG, the provider's status 404, naming the model
\ the call's kind sent.
: UNKNOWN-MODEL ( ^call -- )
   {: c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF false ENDOF
      embed OF true ENDOF
      tally OF false ENDOF
      compaction OF false ENDOF
   ;MATCH {: embed:bool :}
   embed if
      s" an embedding model the provider serves" c EMBED-MODEL$
      s" pass EMBEDDING-MODEL! or OLLAMA! a model the provider serves: it answered status 404"
      CONFIG-REJECT
   then
   s" a generation model the provider serves" c GEN$
   s" pass GENERATION-MODEL! or PURPOSE-MODEL! a model the provider serves: it answered status 404"
   CONFIG-REJECT ;

\ Throws the fault of a rejected reply's status; a key the provider rejected
\ keeps that it did, and a send answered 429 that it was throttled.
: STATUS-THROW ( CURL:http-status ^call -- )
   {: status:CURL:http-status c :}
   status CURL:HTTP-STATUS>N 429 = if c THROTTLED! then
   status STATUS-FAULT {: fault:n :}
   fault E-KEY = c KEYED? and if
      construct key-fault rejected c KEY-FAULT!
   then
   fault E-CONFIG = if c UNKNOWN-MODEL then
   fault throw ;


: CALL-HANDLE ( ^call -- CURL:handle )
   MODEL-CALL:HANDLE @ MATCH option
      none OF E-STATE throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ Keeps the transfer's status, reply and Retry-After, read before the handle
\ is cleaned up; throws the fault of a cut or failed exchange or a rejected
\ reply.
: RECEIPT-CHECK ( ^call -- ^call )
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
   c CALL-HANDLE CURL:RETRY-AFTER c RETRY-AFTER!
   status CURL:HTTP-STATUS>N 200 <> if
      c FAILED-BODY {: body :}
      body c KEEP-ERROR
      body c SIZE-REJECTION? if E-CAPACITY throw then
      status c STATUS-THROW
   then
   c ;

\ Sets the next exchange: a POST of the body to the url.
: AS-POST ( str str ^call -- )
   {: url:str body:str c :}
   c CALL-HANDLE {: h:CURL:handle :}
   h url CURL:URL! c CHECK-STATUS
   h s" POST" dup STR:DATA swap STR:LEN CURL:METHOD! c CHECK-STATUS
   h body CURL:BODY! c CHECK-STATUS ;

\ The object's member under the name; none is E-RESPONSE.
: MEMBER ( JSON:value str -- JSON:value )
   JSON:FIND MATCH option
      none OF E-RESPONSE throw ENDOF
      some OF ENDOF
   ;MATCH ;

: STRING-MEMBER ( JSON:value str -- JSON:value )
   MEMBER {: s :}
   s JSON:STRING? 0= if E-RESPONSE throw then
   s ;

: ARRAY-MEMBER ( JSON:value str -- JSON:value )
   MEMBER {: a :}
   a JSON:ARRAY? 0= if E-RESPONSE throw then
   a ;

: OBJECT-MEMBER ( JSON:value str -- JSON:value )
   MEMBER {: o :}
   o JSON:OBJECT? 0= if E-RESPONSE throw then
   o ;

\ The string type of an object: an output item, a content part or a stream
\ event; any other value is E-RESPONSE.
: TYPE-OF ( JSON:value -- JSON:value )
   {: v :}
   v JSON:OBJECT? 0= if E-RESPONSE throw then
   v s" type" STRING-MEMBER ;

\ The text of the answer message's output_text part (ANSWER?) is the content;
\ a reply holds one, so a second, in any answer message, is E-RESPONSE.
: READ-TEXT ( JSON:value ^call -- )
   {: p c :}
   c MODEL-CALL:CONTENT-SEEN @ if E-RESPONSE throw then
   p s" text" STRING-MEMBER
   [: c CONTENT-BUF REPLACE$ ;] JSON:WITH-STRING
   c CONTENT-BUF BUF:LEN@ BUF:BLEN>N c MODEL-CALL:CONTENT-U !
   true c MODEL-CALL:CONTENT-SEEN ! ;

: READ-PART ( JSON:value ^call -- )
   {: p c :}
   p TYPE-OF {: t :}
   t s" refusal" JSON:STRING= if E-REFUSED throw then
   t s" output_text" JSON:STRING= if p c READ-TEXT then ;

\ Whether a message's phase marks the answer: null or final_answer does;
\ commentary is a progress note; any other is E-RESPONSE.
: ANSWER-PHASE? ( JSON:value -- bool )
   {: p :}
   p JSON:NULL? if true exit then
   p JSON:STRING? 0= if E-RESPONSE throw then
   p s" final_answer" JSON:STRING= if true exit then
   p s" commentary" JSON:STRING= 0= if E-RESPONSE throw then
   false ;

\ A message with no phase is the answer.
: ANSWER? ( JSON:value -- bool )
   s" phase" JSON:FIND MATCH option
      none OF true ENDOF
      some OF ANSWER-PHASE? ENDOF
   ;MATCH ;

\ Reads the parts of the output item if it is an answer message.
: READ-ITEM ( JSON:value ^call -- )
   {: v c :}
   v TYPE-OF s" message" JSON:STRING= 0= if exit then
   v ANSWER? 0= if exit then
   v s" content" ARRAY-MEMBER [: c READ-PART ;] JSON:EACH ;

: READ-OUTPUT ( JSON:value ^call -- )
   {: v c :}
   false c MODEL-CALL:CONTENT-SEEN !
   0 c MODEL-CALL:CONTENT-U !
   v s" output" ARRAY-MEMBER [: c READ-ITEM ;] JSON:EACH
   c MODEL-CALL:CONTENT-SEEN @ 0= if E-RESPONSE throw then ;

\ A message in a tools reply: E-REFUSED if a part is a refusal, else E-RESPONSE.
: TOOL-MESSAGE ( JSON:value -- )
   s" content" ARRAY-MEMBER
   [: TYPE-OF s" refusal" JSON:STRING= if E-REFUSED throw then ;]
   JSON:EACH
   E-RESPONSE throw ;

\ Counts a function call and skips reasoning; a message is TOOL-MESSAGE's,
\ any other type E-RESPONSE.
: READ-TOOL ( n JSON:value -- n )
   {: count:n v :}
   v TYPE-OF {: t :}
   t s" message" JSON:STRING= if v TOOL-MESSAGE then
   t s" function_call" JSON:STRING= if count 1+ exit then
   t s" reasoning" JSON:STRING= 0= if E-RESPONSE throw then
   count ;

\ A tools reply answers exactly one function call, read whole by OUTPUT$; it
\ leaves no content.
: READ-TOOLS ( JSON:value ^call -- )
   {: v c :}
   0 c MODEL-CALL:CONTENT-U !
   0 v s" output" ARRAY-MEMBER [: READ-TOOL ;] JSON:EACH
   1 <> if E-RESPONSE throw then ;

\ An incomplete reply keeps its reason in the content buffer: it has no
\ content.
: READ-INCOMPLETE ( JSON:value ^call -- )
   {: v c :}
   v s" incomplete_details" OBJECT-MEMBER s" reason" STRING-MEMBER
   [: c CONTENT-BUF REPLACE$ ;] JSON:WITH-STRING
   c CONTENT-BUF BUF:LEN@ BUF:BLEN>N c MODEL-CALL:INCOMPLETE-U !
   E-INCOMPLETE throw ;

\ The fault a failed reply's error object names by its code.
: CODE-FAULT ( JSON:value -- n )
   s" code" JSON:FIND MATCH option
      none OF E-HTTP exit ENDOF
      some OF ENDOF
   ;MATCH {: v :}
   v JSON:STRING? 0= if E-HTTP exit then
   v s" context_length_exceeded" JSON:STRING= if E-CAPACITY exit then
   v THROTTLE-CODE? if E-UNAVAILABLE exit then
   v s" server_error" JSON:STRING= if E-UNAVAILABLE exit then
   v s" server_is_overloaded" JSON:STRING= if E-UNAVAILABLE exit then
   v s" flex_unavailable" JSON:STRING= if E-TIER exit then
   E-HTTP ;

\ Keeps a failed reply's error message as the call's error text, the wait it
\ asks for and whether its code says the send was throttled; answers the
\ fault it names.
: KEEP-FAILURE ( JSON:value ^call -- n )
   {: e c :}
   e c ERROR-TEXT
   e c KEEP-BACKOFF
   e CODE-FAULT ;

: READ-FAILURE ( JSON:value ^call -- )
   {: v c :}
   v s" error" OBJECT-MEMBER c KEEP-FAILURE throw ;

\ Throws the fault of a reply whose status is other than completed.
: REPLY-STATUS ( JSON:value ^call -- )
   {: v c :}
   v s" status" STRING-MEMBER {: st :}
   st s" incomplete" JSON:STRING= if v c READ-INCOMPLETE then
   st s" failed" JSON:STRING= if v c READ-FAILURE then
   st s" completed" JSON:STRING= 0= if E-RESPONSE throw then ;

: ACCEPT-CHAT ( JSON:value ^call -- )
   {: v c :}
   v JSON:OBJECT? 0= if E-RESPONSE throw then
   v c REPLY-STATUS
   c MODEL-CALL:FORM @ MATCH reply-form
      none OF E-RESPONSE throw ENDOF
      schema OF
         v c READ-OUTPUT
         c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
         JSON:PARSE JSON:OBJECT? 0= if E-RESPONSE throw then
      ENDOF
      tools OF v c READ-TOOLS ENDOF
   ;MATCH ;

: VECTORS-U ( ^call -- n ) VECTORS-BUF BUF:LEN@ BUF:BLEN>N ;

\ The vector just read is the one at idx.
: VECTOR-KEEP ( n ^call -- )
   {: idx:n c :}
   c MODEL-CALL:VECTOR-AT @ {: at:n :}
   at c idx VECTOR-OFF !
   c VECTORS-U at - c idx VECTOR-LEN ! ;

\ No vector read yet; the vectors keep the room the last reply's took.
: VECTORS-RESET ( ^call -- )
   {: c :}
   INPUT-MAX 0 ?do 0 c i VECTOR-LEN ! loop
   c VECTORS-BUF c RESET-SCRATCH ;

\ A vector's length in bytes.
: VECTOR-BYTES ( ^call -- n ) MODEL-CALL:DIMS @ 4 * ;

\ Whether binary32 bits have an exponent field of all ones: NaN or infinity.
: NONFINITE? ( n -- bool ) $7F800000 and $7F800000 = ;


\ Grows the vectors by one vector's room, which VECTOR-AT marks, and answers
\ its address.
: VECTOR-ROOM ( ^call -- ^u8 )
   {: c :}
   c VECTORS-U c MODEL-CALL:VECTOR-AT !
   c VECTORS-BUF c VECTOR-BYTES BUF:N>BLEN BUF:GROW SPAN:$ STR:DATA ;


\ A number narrowed to binary32 bits; a NaN or infinity is E-RESPONSE.
: COMPONENT-BITS ( JSON:value -- n )
   {: x :}
   x JSON:NUMBER? 0= if E-RESPONSE throw then
   x JSON:TEXT$ F64-TEXT:PARSE MATCH result
      ok OF F32:NARROW ENDOF
      err OF drop E-RESPONSE throw ENDOF
   ;MATCH
   dup NONFINITE? if E-RESPONSE throw then ;

\ Stores the next component of the vector at v.
: READ-COMPONENT ( JSON:value ^u8 ^call -- )
   {: x v:^ c :}
   c MODEL-CALL:COMPONENT-COUNT @ {: idx:n :}
   idx c MODEL-CALL:DIMS @ >= if E-RESPONSE throw then
   x COMPONENT-BITS v idx 4 * + F32-BUF:STORE
   idx 1+ c MODEL-CALL:COMPONENT-COUNT ! ;

\ A JSON array of DIMS numbers, each narrowed to binary32.
: READ-NUMBERS ( JSON:value ^call -- )
   {: a c :}
   a JSON:ARRAY? 0= if E-RESPONSE throw then
   0 c MODEL-CALL:COMPONENT-COUNT !
   c VECTOR-ROOM {: v:^ :}
   a [: v c READ-COMPONENT ;] JSON:EACH
   c MODEL-CALL:COMPONENT-COUNT @ c MODEL-CALL:DIMS @ <> if
      E-RESPONSE throw
   then ;


\ Decodes the base64 text into the vector's room, answering the decoded
\ length in place of the n under it, so a catch keeps the shape.
: DECODE-VECTOR ( n str ^call -- n str ^call )
   {: s:str c :}
   drop
   s c VECTORS-BUF BUF:SPAN c MODEL-CALL:VECTOR-AT @ c VECTOR-BYTES SPAN:SUB
   BASE64:DECODE s c ;

\ The decoded length of base64 text; text that is not base64 is E-RESPONSE.
: DECODE-LENT ( str ^call -- n )
   {: s:str c :}
   0 s c [: DECODE-VECTOR ;] catch {: code:n :}
   code 0<> if drop drop drop E-RESPONSE throw then
   drop drop ;

\ Base64 of DIMS little-endian binary32 values.
: READ-BASE64 ( JSON:value ^call -- )
   {: s c :}
   c VECTOR-ROOM {: v:^ :}
   s [: c DECODE-LENT ;] JSON:WITH-STRING
   c VECTOR-BYTES <> if E-RESPONSE throw then
   c MODEL-CALL:DIMS @ 0 ?do
      v i 4 * + F32-BUF:LOAD NONFINITE? if E-RESPONSE throw then
   loop ;

\ An input's index: an integer below INPUT-COUNT.
: INPUT-INDEX ( JSON:value ^call -- n )
   {: x c :}
   x JSON:INTEGER? 0= if E-RESPONSE throw then
   x JSON:>N {: idx:n :}
   idx 0 < idx c MODEL-CALL:INPUT-COUNT @ >= or if E-RESPONSE throw then
   idx ;

\ An embedding: a base64 string or an array of numbers.
: READ-VECTOR ( JSON:value ^call -- )
   {: e c :}
   e JSON:STRING? if e c READ-BASE64 else e c READ-NUMBERS then ;

: COUNT-DATUM ( ^call -- )
   {: c :}
   c MODEL-CALL:DATA-COUNT @ 1+ c MODEL-CALL:DATA-COUNT ! ;

: READ-EMBED-OBJECT ( JSON:value ^call -- )
   {: o c :}
   o JSON:OBJECT? 0= if E-RESPONSE throw then
   o s" index" MEMBER c INPUT-INDEX {: idx:n :}
   c idx VECTOR-LEN @ 0<> if E-RESPONSE throw then
   o s" embedding" MEMBER c READ-VECTOR
   idx c VECTOR-KEEP
   c COUNT-DATUM ;

: READ-DATA ( JSON:value ^call -- )
   {: d c :}
   d JSON:ARRAY? 0= if E-RESPONSE throw then
   d [: c READ-EMBED-OBJECT ;] JSON:EACH ;

\ The vector of the next input.
: READ-EMBEDDING ( JSON:value ^call -- )
   {: e c :}
   c MODEL-CALL:DATA-COUNT @ {: idx:n :}
   idx c MODEL-CALL:INPUT-COUNT @ >= if E-RESPONSE throw then
   e c READ-NUMBERS
   idx c VECTOR-KEEP
   c COUNT-DATUM ;

: READ-EMBEDDINGS ( JSON:value ^call -- )
   {: a c :}
   a JSON:ARRAY? 0= if E-RESPONSE throw then
   a [: c READ-EMBEDDING ;] JSON:EACH ;

\ A usage member, when it is an object.
: EMBED-USAGE ( JSON:value -- option<JSON:value> )
   {: u :}
   u JSON:OBJECT? 0= if OPTION:NONE exit then
   u OPTION:SOME ;

\ The reply's usage object, none without one; a reply without one vector per
\ input is E-RESPONSE.
: EMBED-END ( JSON:value ^call -- option<JSON:value> )
   {: r c :}
   c MODEL-CALL:DATA-COUNT @ c MODEL-CALL:INPUT-COUNT @ <> if
      E-RESPONSE throw
   then
   r s" usage" JSON:FIND MATCH option
      none OF OPTION:NONE ENDOF
      some OF EMBED-USAGE ENDOF
   ;MATCH ;

\ An embeddings reply's vectors, and its usage object, none without one.
: ACCEPT-EMBED ( JSON:value ^call -- option<JSON:value> )
   {: r c :}
   r s" data" MEMBER c READ-DATA
   r c EMBED-END ;

: ACCEPT-OLLAMA ( JSON:value ^call -- option<JSON:value> )
   {: r c :}
   r s" embeddings" MEMBER c READ-EMBEDDINGS
   r c EMBED-END ;

: RESPONSE-ERROR ( n -- )
   {: code:n :}
   code JSON:INPUT-FAULT? if E-RESPONSE throw then
   code 0<> if code throw then ;

: PARSE-CHAT ( ^call -- )
   {: c :}
   [: c RESPONSE$ JSON:PARSE c ACCEPT-CHAT ;] catch {: code:n :}
   code RESPONSE-ERROR ;

\ A usage count: an integer, or none for null.
: USAGE-COUNT ( JSON:value -- option<n> )
   {: v :}
   v JSON:NULL? if OPTION:NONE exit then
   v JSON:INTEGER? 0= if E-RESPONSE throw then
   v JSON:>N OPTION:SOME ;

\ A count, 0 when absent.
: OR-ZERO ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The count at the key of an object, such as a usage object: none when the
\ key is absent or null.
: USAGE-AT ( JSON:value str -- option<n> )
   JSON:FIND MATCH option
      none OF OPTION:NONE ENDOF
      some OF USAGE-COUNT ENDOF
   ;MATCH ;

\ The count at the key of a member object of the usage object; none when that
\ member is absent or not an object.
: DETAIL-AT ( JSON:value str str -- option<n> )
   {: v detail:str key:str :}
   v detail JSON:FIND MATCH option
      none OF OPTION:NONE ENDOF
      some OF {: d :}
         d JSON:OBJECT? 0= if OPTION:NONE exit then
         d key USAGE-AT
      ENDOF
   ;MATCH ;

\ The reply's usage object, none when absent or not an object; marks the
\ call's usage seen when there is one.
: USAGE? ( JSON:value ^call -- option<JSON:value> )
   {: v c :}
   v s" usage" JSON:FIND MATCH option
      none OF OPTION:NONE ENDOF
      some OF {: u :}
         u JSON:OBJECT? 0= if OPTION:NONE exit then
         true c TOKENS TOKENS-SEEN !
         u OPTION:SOME
      ENDOF
   ;MATCH ;

\ A usage object's counts, in the Responses protocol's names.
: USAGE-COUNTS ( JSON:value ^call -- )
   {: u c :}
   u s" input_tokens" USAGE-AT c TOKENS TOKENS-INPUT !
   u s" output_tokens" USAGE-AT c TOKENS TOKENS-OUTPUT !
   u s" input_tokens_details" s" cached_tokens" DETAIL-AT
   c TOKENS TOKENS-CACHED !
   u s" input_tokens_details" s" cache_write_tokens" DETAIL-AT
   c TOKENS TOKENS-WRITTEN !
   u s" output_tokens_details" s" reasoning_tokens" DETAIL-AT
   c TOKENS TOKENS-REASONING ! ;

\ A chat or compaction reply's usage counts.
: ACCEPT-USAGE ( JSON:value ^call -- )
   {: v c :}
   v c USAGE? MATCH option
      none OF ENDOF
      some OF c USAGE-COUNTS ENDOF
   ;MATCH ;

\ An embeddings usage object's prompt_tokens.
: PROMPT-TOKENS ( JSON:value ^call -- )
   {: u c :}
   u s" prompt_tokens" USAGE-AT c TOKENS TOKENS-INPUT ! ;

\ A usage object's prompt_tokens, when an integer. It throws nothing, so the
\ walk's fault is the one thrown.
: FAULT-PROMPT ( JSON:value ^call -- )
   {: u c :}
   u s" prompt_tokens" JSON:FIND MATCH option
      none OF ENDOF
      some OF {: n :}
         n JSON:INTEGER? 0= if exit then
         n JSON:>N OPTION:SOME c TOKENS TOKENS-INPUT !
      ENDOF
   ;MATCH ;

\ The prompt_tokens of a reply the walk rejected.
: FAULT-TOKENS ( JSON:value ^call -- )
   {: v c :}
   v c USAGE? MATCH option
      none OF ENDOF
      some OF c FAULT-PROMPT ENDOF
   ;MATCH ;

\ Keeps the text a reply names; an empty one is E-RESPONSE.
: SERVED! ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s STR:LEN 0= if E-RESPONSE throw then
   v 0 VEC:RESIZE
   s v VEC:APPEND ;

\ A served member: a nonempty string, or null for none.
: SERVED-STRING ( JSON:value ^VEC:vector<u8> -- )
   {: m v:^ :}
   m JSON:NULL? if exit then
   m JSON:STRING? 0= if E-RESPONSE throw then
   m [: v SERVED! ;] JSON:WITH-STRING ;

\ The reply's member key, kept in v when the reply names one.
: ACCEPT-SERVED ( JSON:value str ^VEC:vector<u8> -- )
   {: r key:str v:^ :}
   r key JSON:FIND MATCH option
      none OF ENDOF
      some OF v SERVED-STRING ENDOF
   ;MATCH ;

\ The tier the reply says served it, when it names one.
: ACCEPT-TIER ( JSON:value ^call -- )
   {: v c :}
   v s" service_tier" c SERVED-BY SERVED-BY-TIER @ ACCEPT-SERVED ;

\ The model the reply says served it, when it names one.
: ACCEPT-MODEL ( JSON:value ^call -- )
   {: v c :}
   v s" model" c SERVED-BY SERVED-BY-MODEL @ ACCEPT-SERVED ;

\ A creation time: whole seconds since the epoch, or none for null.
: CREATED-SECONDS ( JSON:value -- option<n> )
   {: v :}
   v JSON:NULL? if OPTION:NONE exit then
   v JSON:INTEGER? 0= if E-RESPONSE throw then
   v JSON:>N {: secs:n :}
   secs 0< if E-RESPONSE throw then
   secs OPTION:SOME ;

\ The reply's created_at, none when absent or null.
: CREATED-AT ( JSON:value -- option<n> )
   s" created_at" JSON:FIND MATCH option
      none OF OPTION:NONE ENDOF
      some OF CREATED-SECONDS ENDOF
   ;MATCH ;

\ The model a generation or compaction reply names, and when it says it was
\ created.
: ACCEPT-SERVED-BY ( JSON:value ^call -- )
   {: v c :}
   v c ACCEPT-MODEL
   v CREATED-AT c SERVED-BY SERVED-BY-CREATED ! ;

\ The reply's top object, with no vectors read yet; a reply that is not JSON
\ is E-RESPONSE.
: EMBED-REPLY ( ^call -- JSON:value )
   {: c :}
   c VECTORS-RESET
   0 c MODEL-CALL:DATA-COUNT !
   [: c RESPONSE$ JSON:PARSE ;] catch {: r code:n :}
   code JSON:INPUT-FAULT? if E-RESPONSE throw then
   code 0<> if code throw then
   r JSON:OBJECT? 0= if E-RESPONSE throw then
   r ;

\ The model the reply names, its vectors, and its usage object, none without
\ one.
: PARSE-EMBED ( JSON:value ^call -- option<JSON:value> )
   {: r c :}
   r c ACCEPT-MODEL
   c MODEL-CALL:EMBEDDING-KIND @ OLLAMA = if
      r c ACCEPT-OLLAMA
   else
      r c ACCEPT-EMBED
   then ;

: BEGIN-RESET ( ^call -- ^call )
   {: c :}
   \ The reply buffers keep the room the last call's reply took. They are
   \ resized before the call is started, so a resize that throws leaves the
   \ call unstarted and a later begin runs as normal.
   c CONTENT-BUF c MODEL-CALL:CONTENT-U @ c MODEL-CALL:INCOMPLETE-U @ max c RESET-SIZED
   c RESPONSE-BUF c MODEL-CALL:RESPONSE-U @ c RESET-SIZED
   c ERROR-BUF c RESET-SCRATCH
   c CLEAR-SERVED-BY
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

: BEGIN-LOCKED ( ^call -- ^call )
   {: c :}
   c MODEL-CALL:STOPPED @ if E-CANCELLED throw then
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   c BEGIN-RESET ;

\ Begins a landed call's next send, as BEGIN-LOCKED begins an idle call: a
\ stop or a CANCEL since the send landed is E-CANCELLED, stored as the call's
\ outcome under the gate, so a CANCEL once it is released finds it.
: RESEND-LOCKED ( ^call -- ^call )
   {: c :}
   c MODEL-CALL:STOPPED @ c MODEL-CALL:CANCELLED @ or if
      E-CANCELLED c OUTCOME!
      E-CANCELLED throw
   then
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF E-STATE throw ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF ENDOF
   ;MATCH
   c BEGIN-RESET ;

\ Runs begin under the call's gate.
: BEGIN-GATED ( ^call [ ^call -- ^call ] -- )
   {: c begin :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c begin catch {: code:n :} drop
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   code 0<> if code throw then ;

: BEGIN-CALL ( ^call -- ) [: BEGIN-LOCKED ;] BEGIN-GATED ;

\ A stopped call is E-CANCELLED before its request is checked.
: REQUIRE-GOING ( ^call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:STOPPED @ {: stopped:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   stopped if E-CANCELLED throw then ;

\ Lands the call under its gate, storing no ending: the landing of a settle
\ that threw (SETTLE-CAUGHT). A run that settles lands through END-RUN.
: END-CALL ( ^call -- )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   construct call-state landed c MODEL-CALL:STATE !
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

\ Keeps the reply's response id, which a failure's log line names.
: KEEP-ID ( JSON:value ^call -- )
   {: r c :}
   r s" id" STRING-MEMBER
   [: c RESPONSE-ID-BUF REPLACE$ ;] JSON:WITH-STRING ;

\ Whether the reply's status says the response is still queued or running.
: RUNNING? ( JSON:value -- bool )
   s" status" STRING-MEMBER {: t :}
   t s" queued" JSON:STRING= t s" in_progress" JSON:STRING= or ;


: CURL+ ( n -- )
   {: rc:n :}
   s"  curl " LOG:TEXT+ rc LOG:INT+
   s" : " LOG:TEXT+ rc CURL:>CODE CURL:ERROR$ LOG:TEXT+ ;


\ The text becomes the call's reply.
: KEEP-REPLY ( str ^call -- )
   {: s:str c :}
   s c RESPONSE-BUF REPLACE$
   s STR:LEN c MODEL-CALL:RESPONSE-U ! ;

\ The error event's error object: its error member when an object, else the
\ event.
: ERROR-AT ( JSON:value -- JSON:value )
   {: v :}
   v s" error" JSON:FIND MATCH option
      none OF v ENDOF
      some OF dup JSON:OBJECT? 0= if drop v then ENDOF
   ;MATCH ;

\ An error event fails the response as a failed status does.
: ERROR-EVENT ( JSON:value ^call -- )
   {: v c :}
   v ERROR-AT c KEEP-FAILURE {: fault:n :}
   c STREAM {: s :}
   fault s STREAM-FAULT !
   true s STREAM-DONE ! ;

\ An event carrying the response object makes it the reply; the first names
\ the response's id, and a status other than queued or in_progress ends the
\ stream.
: RESPONSE-EVENT ( JSON:value ^call -- )
   {: v c :}
   v s" response" JSON:FIND MATCH option
      none OF exit ENDOF
      some OF ENDOF
   ;MATCH {: r :}
   r JSON:OBJECT? 0= if E-RESPONSE throw then
   r JSON:TEXT$ c KEEP-REPLY
   c RESPONSE-ID$ STR:LEN 0= if r c KEEP-ID then
   r RUNNING? 0= if true c STREAM STREAM-DONE ! then ;

\ Appends "/" and the object's member to the key when the member is a
\ string; answers whether it was.
: KEY-PART+ ( JSON:value str ^VEC:vector<u8> -- bool )
   {: o name:str key:^ :}
   o name JSON:FIND MATCH option
      none OF false exit ENDOF
      some OF ENDOF
   ;MATCH {: m :}
   m JSON:STRING? 0= if false exit then
   s" /" key VEC:APPEND  m key JSON:APPEND-STRING  true ;

\ Appends an added event's item type to the key, and a message's phase.
: ITEM-KEY+ ( JSON:value ^VEC:vector<u8> -- )
   {: v key:^ :}
   v s" item" JSON:FIND MATCH option
      none OF exit ENDOF
      some OF ENDOF
   ;MATCH {: item :}
   item JSON:OBJECT? 0= if exit then
   item s" type" key KEY-PART+ 0= if exit then
   item s" type" JSON:GET s" message" JSON:STRING= 0= if exit then
   item s" phase" key KEY-PART+ drop ;

\ Counts the event of the type, an added output item under ITEM-KEY+'s key.
: COUNT-KEYED ( str JSON:value ^call -- )
   {: t:str v c :}
   t s" response.output_item.added" STR:= 0= if t c COUNT-EVENT exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: key:^ :}
   [: t key VEC:APPEND  v key ITEM-KEY+  key VEC:BYTES c COUNT-EVENT ;]
   [: key VEC:DELETE ;] finally ;

\ One event, its data a JSON object with a string type, counted by type; the
\ call's first event starts its reply afresh, a keepalive is skipped and an
\ error event is the reply.
: READ-EVENT ( ^call str -- ^call str )
   {: c ev:str :}
   c STREAM {: s :}
   s STREAM-HEARD @ 0= if
      c RESPONSE-BUF BUF:CLEAR
      0 c MODEL-CALL:RESPONSE-U !
      true s STREAM-HEARD !
   then
   ev JSON:PARSE {: v :}
   v TYPE-OF {: t :}
   t [: dup v c COUNT-KEYED s" response.output_" STR:STARTS-WITH? ;]
   JSON:WITH-STRING
   {: output:bool :}
   s STREAM-OUTPUT-NS @ 0= output and if
      TIME:MONO-NS s STREAM-OUTPUT-NS !
   then
   t s" keepalive" JSON:STRING= if c ev exit then
   t s" error" JSON:STRING= if
      ev c KEEP-REPLY
      v c ERROR-EVENT
      c ev exit
   then
   v c RESPONSE-EVENT
   c ev ;


\ Bytes that are not UTF-8 or JSON make an unusable reply.
: READ-FAULT ( n -- n )
   {: code:n :}
   code SSE:E-UTF8 = code JSON:INPUT-FAULT? or if E-RESPONSE exit then
   code ;


\ An event the call cannot read becomes the reply, as an error event does,
\ and fails the stream E-RESPONSE; none is read once the response ended.
: EVENT ( ^call str str -- )
   {: c name:str ev:str :}
   c STREAM STREAM-DONE @ if exit then
   c ev [: READ-EVENT ;] catch {: code:n :} drop drop
   code 0= if exit then
   code READ-FAULT E-RESPONSE = if ev c KEEP-REPLY then
   code throw ;

: KEEP-CHUNK ( ^call -- )
   {: c :}
   c CHUNK-BUF BUF:SPAN$ c RESPONSE-BUF BUF:APPEND-SPAN ;

: FEED-EVENTS ( SSE:decoder ^call -- SSE:decoder ^call )
   {: d:SSE:decoder c :}
   d c CHUNK-BUF BUFFER$ c [: EVENT ;] SSE:FEED
   d c ;


\ The chunk is the reply until the first event, so a 200 that is no event
\ stream is, and so is one whose event the decoder rejects as not UTF-8.
: FEED-CHUNK ( SSE:decoder ^call -- )
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
: KEEP-ENDING ( CURL:http-status n n ^call -- )
   RECEIPT {: status:CURL:http-status rc:n fault:n r :}
   status r RECEIPT-STATUS !
   rc r RECEIPT-CODE !
   fault r RECEIPT-FAULT ! ;


\ The response buffer's room past the bytes received: an embed, count or
\ compaction read lands there, and appending it from where it lies adds it to
\ them.
: REPLY-ROOM ( ^call -- str )
   RESPONSE-BUF {: buf:^ :}
   buf HELD buf BUF:LEN@ BUF:BLEN>N SPAN:SKIP SPAN:$ ;


\ Room for the next read of an embed, count or compaction reply; a fault spoils
\ the reply.
: MAKE-ROOM ( ^call -- answer )
   {: c :}
   c [: GROW-REPLY ;] catch {: fault:n :} drop
   fault 0= if construct answer fed exit then
   0 CURL:>HTTP-STATUS 0 fault c KEEP-ENDING
   construct answer spoiled ;


: CLEAR-REPLY ( ^call -- answer )
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
: CHAT-CHUNK ( CURL:http-status n ^call -- answer )
   {: status:CURL:http-status got:n c :}
   c CHUNK-BUF HELD SPAN:$ STR:DATA {: room:^ :}
   c STREAM STREAM-DECODER @ MATCH option
      none OF E-STATE throw ENDOF
      some OF ENDOF
   ;MATCH {: d:SSE:decoder :}
   c CHUNK-BUF BUF:CLEAR
   room got BUF:N>BLEN c CHUNK-BUF BUF:APPEND-SPAN
   status CURL:HTTP-STATUS>N 200 = if
      [: d c FEED-CHUNK ;] catch
   else
      [: c KEEP-CHUNK ;] catch
   then {: fault:n :}
   fault 0= if construct answer fed exit then
   status 0 fault READ-FAULT c KEEP-ENDING
   construct answer spoiled ;


: EMBED-CHUNK ( n ^call -- answer )
   {: got:n c :}
   c REPLY-ROOM STR:DATA {: room:^ :}
   room got BUF:N>BLEN c RESPONSE-BUF BUF:APPEND-SPAN
   c MAKE-ROOM ;


\ A chat whose response already ended keeps it, however the transfer ends.
: FAILED-END ( n ^call -- )
   {: rc:n c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c STREAM STREAM-DONE @ ENDOF
      embed OF false ENDOF
      tally OF false ENDOF
      compaction OF false ENDOF
   ;MATCH
   if 200 CURL:>HTTP-STATUS 0 0 c KEEP-ENDING exit then
   0 CURL:>HTTP-STATUS rc 0 c KEEP-ENDING ;


: TAKE-WAIT ( ^call -- answer )
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
: DISCARD-WAIT ( ^call -- answer )
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
: TAKE ( ^call -- poll )
   {: c :}
   c CALL-HANDLE CURL:NEXT-READY? 0= if construct poll none exit then
   c TAKE-WAIT POLLED ;


\ As DISCARD-WAIT, but answers none at once while the armed read holds
\ nothing.
: DISCARD ( ^call -- poll )
   {: c :}
   c CALL-HANDLE CURL:NEXT-READY? 0= if construct poll none exit then
   c DISCARD-WAIT POLLED ;


: REARM ( ^call -- )
   {: c :}
   c CALL-HANDLE
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c CHUNK-BUF HELD SPAN:$ ENDOF
      embed OF c REPLY-ROOM ENDOF
      tally OF c REPLY-ROOM ENDOF
      compaction OF c REPLY-ROOM ENDOF
   ;MATCH dup STR:DATA swap STR:LEN >LEN CURL:NEXT-ARM ;


\ Every kind drains into the chunk buffer, so the bytes of a dropped embed,
\ count or compaction reply stay as they arrived.
: REARM-DROP ( ^call -- )
   {: c :}
   c CALL-HANDLE c CHUNK-BUF HELD SPAN:$ dup STR:DATA swap STR:LEN >LEN
   CURL:NEXT-ARM ;


: DROP-REPLY ( ^call -- )
   {: c :}
   c CALL-HANDLE CURL:CANCEL
   c REARM-DROP ;


\ An input the state cannot receive.
: LOST ( ^call -- ) drop E-STATE throw ;


\ Arms a started transfer's first read and keeps how SERVE and AWAIT resume it:
\ a reply the call cannot hold is dropped and drained.
: ARM ( ^call -- )
   {: c :}
   c CLEAR-REPLY MATCH answer
      fed OF c REARM construct receive-rest reading ENDOF
      spoiled OF c DROP-REPLY construct receive-rest dropping ENDOF
      done OF E-STATE throw ENDOF
   ;MATCH c MODEL-CALL:RECEIVE ! ;


\ Keeps where SERVE or AWAIT resumes a parked transfer.
: PARK ( ^call -- )
   construct receive-rest reading swap MODEL-CALL:RECEIVE ! ;
: PARK-DROP ( ^call -- )
   construct receive-rest dropping swap MODEL-CALL:RECEIVE ! ;


\ Ending before the response with no event means no event stream: E-RESPONSE.
: STREAM-CHECK ( ^call -- ^call )
   {: c :}
   c STREAM {: s :}
   s STREAM-DONE @ 0= if
      s STREAM-HEARD @ if E-TRANSPORT throw then
      E-RESPONSE throw
   then
   s STREAM-FAULT @ 0<> if s STREAM-FAULT @ throw then
   c ;

\ Reads a chat reply's usage, service tier, model, creation time and content.
: READ-CHAT ( ^call -- )
   {: c :}
   c RESPONSE$ JSON:PARSE {: v :}
   v c ACCEPT-USAGE
   v c ACCEPT-TIER
   v c ACCEPT-SERVED-BY
   v c ACCEPT-CHAT ;

: CHAT-PARSE ( ^call -- ^call )
   {: c :}
   [: c READ-CHAT ;] catch {: code:n :}
   code RESPONSE-ERROR
   c TOKENS TOKENS-INPUT @ OR-ZERO c MODEL-CALL:INPUT-TOKENS !
   c TOKENS TOKENS-CACHED @ OR-ZERO c MODEL-CALL:CACHED-TOKENS !
   c TOKENS TOKENS-OUTPUT @ OR-ZERO c MODEL-CALL:OUTPUT-TOKENS !
   c TOKENS TOKENS-REASONING @ OR-ZERO c MODEL-CALL:REASONING-TOKENS !
   c ;

\ Reads an embed reply's vectors and usage.
: EMBED-PARSE ( ^call -- ^call )
   {: c :}
   c EMBED-REPLY {: r :}
   [: r c PARSE-EMBED ;] catch {: usage walk:n :}
   walk 0<> if r c FAULT-TOKENS walk throw then
   usage MATCH option
      none OF ENDOF
      some OF
         true c TOKENS TOKENS-SEEN !
         c PROMPT-TOKENS
      ENDOF
   ;MATCH
   c ;

\ A count reply's input_tokens, at its top level: an integer.
: ACCEPT-COUNT ( ^call -- )
   {: c :}
   c RESPONSE$ JSON:PARSE s" input_tokens" USAGE-AT MATCH option
      none OF E-RESPONSE throw ENDOF
      some OF c MODEL-CALL:COUNTED ! ENDOF
   ;MATCH ;

\ Reads a count reply's input tokens into COUNTED.
: COUNT-PARSE ( ^call -- ^call )
   {: c :}
   c [: dup ACCEPT-COUNT ;] catch {: code:n :} drop
   code RESPONSE-ERROR
   c ;

\ The reply's output array; E-RESPONSE unless the reply is an object with
\ one.
: WINDOW-ITEMS ( JSON:value -- JSON:value )
   {: v :}
   v JSON:OBJECT? 0= if E-RESPONSE throw then
   v s" output" ARRAY-MEMBER ;

\ OPTION:NONE's effect is one cell; this one names the item's width.
: NO-ITEM ( -- option<JSON:value> ) OPTION:NONE ;

\ The item, an object, in place of the one before it; else E-RESPONSE.
: LAST-ITEM ( option<JSON:value> JSON:value -- option<JSON:value> )
   {: prev v :}
   v JSON:OBJECT? 0= if E-RESPONSE throw then
   v OPTION:SOME ;

\ Whether the item is a compaction item; E-RESPONSE unless its type is a
\ string.
: COMPACTION? ( JSON:value -- bool )
   s" type" STRING-MEMBER s" compaction" JSON:STRING= ;

\ A compaction reply's output is a nonempty array of objects, the last a
\ compaction item; else E-RESPONSE.
: CHECK-WINDOW ( JSON:value -- )
   {: items :}
   NO-ITEM items [: LAST-ITEM ;] JSON:EACH
   MATCH option
      none OF E-RESPONSE throw ENDOF
      some OF COMPACTION? 0= if E-RESPONSE throw then ENDOF
   ;MATCH ;

\ Replaces the finished input with the reply's window: its output items, in
\ order, and no parts; the input is broken until the window is whole.
: TAKE-WINDOW ( JSON:value ^call -- )
   {: items c :}
   construct input-state broken c MODEL-CALL:CHAT-INPUT !
   c EMPTY-INPUT
   \ ITEM-JOIN writes ITEMS-BUF, so the response text the items borrow stays.
   items [: JSON:TEXT$ c ITEM-JOIN ;] JSON:EACH
   c PART-OPEN
   construct input-state building c MODEL-CALL:CHAT-INPUT !
   c PART-FINISH ;

\ A compaction reply, checked before the input changes: its usage, served
\ tier, model and creation time, then its window as the call's input.
: ACCEPT-COMPACT ( ^call -- )
   {: c :}
   c RESPONSE$ JSON:PARSE {: v :}
   v c ACCEPT-USAGE
   v c ACCEPT-TIER
   v c ACCEPT-SERVED-BY
   v WINDOW-ITEMS {: items :}
   items CHECK-WINDOW
   items c TAKE-WINDOW
   c TOKENS TOKENS-INPUT @ OR-ZERO c MODEL-CALL:INPUT-TOKENS !
   c TOKENS TOKENS-CACHED @ OR-ZERO c MODEL-CALL:CACHED-TOKENS !
   c TOKENS TOKENS-OUTPUT @ OR-ZERO c MODEL-CALL:OUTPUT-TOKENS !
   c TOKENS TOKENS-REASONING @ OR-ZERO c MODEL-CALL:REASONING-TOKENS ! ;

\ Reads a compaction reply into the call's input and usage.
: COMPACT-PARSE ( ^call -- ^call )
   {: c :}
   c [: dup ACCEPT-COMPACT ;] catch {: code:n :} drop
   code RESPONSE-ERROR
   c ;

: JOURNAL-GET? ( ^call -- bool )
   {: c :}
   c JOURNAL-OUT-BUF c RESET-SCRATCH
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-OUT-BUF c MODEL-CALL:JOURNAL-GET @ execute ;

: JOURNAL-STRING-BUF ( JSON:value str ^n ^call -- )
   {: rec key:str buf:^ c :}
   rec key STRING-MEMBER {: f :}
   buf f JSON:TEXT$ STR:LEN c RESET-SIZED
   f [: buf REPLACE$ ;] JSON:WITH-STRING ;

: JOURNAL-RESPONSE ( JSON:value ^call -- )
   {: rec c :}
   rec s" response" STRING-MEMBER
   [: c KEEP-REPLY ;] JSON:WITH-STRING ;

: JOURNAL-ORIGIN ( JSON:value -- )
   s" origin" OBJECT-MEMBER
   s" kind" STRING-MEMBER
   s" " JSON:STRING= if E-RESPONSE throw then ;

\ The recorded call id a journal record carries, 0 when it has none.
: JOURNAL-CALL-ID ( JSON:value -- n )
   s" call_id" JSON:FIND MATCH option
     none OF 0 ENDOF
     some OF {: id :}
        id JSON:INTEGER? 0= if E-RESPONSE throw then
        id JSON:>N
     ENDOF
   ;MATCH ;

\ The fault a journaled rejection records, 0 for a reply.
: JOURNAL-REJECTION-FAULT ( JSON:value -- n )
   s" rejection" JSON:FIND MATCH option
     none OF 0 ENDOF
     some OF {: r :}
        r JSON:STRING? 0= if E-RESPONSE throw then
        r s" capacity" JSON:STRING= if E-CAPACITY exit then
        r s" incomplete" JSON:STRING= if E-INCOMPLETE exit then
        E-RESPONSE throw
     ENDOF
   ;MATCH ;

: JOURNAL-RESTORE ( ^call -- )
   {: c :}
   0 c MODEL-CALL:HTTP-STATUS !
   c JOURNAL-OUT$ JSON:PARSE {: rec :}
   rec JSON:OBJECT? 0= if E-RESPONSE throw then
   rec JOURNAL-REJECTION-FAULT {: rejection:n :}
   rejection 0<> if rejection throw then
   rec JOURNAL-ORIGIN
   rec s" configuration" OBJECT-MEMBER {: config :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN-CONFIG-BYTES ENDOF
      embed OF c EMBED-CONFIG-BYTES ENDOF
      tally OF c GEN-CONFIG-BYTES ENDOF
      compaction OF c GEN-CONFIG-BYTES ENDOF
   ;MATCH {: v:^ :}
   v 0 VEC:RESIZE  config JSON:TEXT$ v VEC:APPEND
   rec c JOURNAL-RESPONSE
   rec s" content" c REPLAY-CONTENT-BUF c JOURNAL-STRING-BUF
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         c PARSE-CHAT
         c CONTENT-BUF HELD c MODEL-CALL:CONTENT-U @ SPAN:TAKE SPAN:$
         c REPLAY-CONTENT$ STR:= 0= if E-RESPONSE throw then
      ENDOF
      embed OF
         c REPLAY-CONTENT$ STR:LEN 0<> if E-RESPONSE throw then
         c EMBED-REPLY c PARSE-EMBED {: usage :}
      ENDOF
      tally OF
         c REPLAY-CONTENT$ STR:LEN 0<> if E-RESPONSE throw then
         c COUNT-PARSE drop
      ENDOF
      compaction OF
         c REPLAY-CONTENT$ STR:LEN 0<> if E-RESPONSE throw then
         c COMPACT-PARSE drop
      ENDOF
   ;MATCH
   rec JOURNAL-CALL-ID c LEDGER LEDGER-CALL-ID !
   c MODEL-CALL:KIND @ OPTION:SOME c MODEL-CALL:REPLAY-KIND ! ;

\ An integer member, null for none.
: COUNT-FIELD ( ^JSON:writer str option<n> -- ^JSON:writer )
   MATCH option
      none OF JSON:WRITE-FIELD-NULL ENDOF
      some OF JSON:WRITE-FIELD-INT ENDOF
   ;MATCH ;

\ A served member's text, null for none.
: SERVED-FIELD ( ^JSON:writer str ^VEC:vector<u8> -- ^JSON:writer )
   VEC:BYTES {: key:str s:str :}
   key s STR:LEN 0<> if s JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then ;

\ The call's kind, configured model and usage counts, when the reply carried
\ usage.
: USAGE-FIELD ( ^JSON:writer ^call -- ^JSON:writer )
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
: RECORD-OPEN ( ^call -- ^JSON:writer )
   {: c :}
   c MODEL-CALL:KIND @ {: k:call-kind :}
   k GENERATES? if k SHAPED? c BUILD-GEN-CONFIG else c BUILD-EMBED-CONFIG then
   c JOURNAL-RECORD-BYTES c RESET-BYTES
   c WRITER c JOURNAL-RECORD-BYTES JSON:OPEN drop
   c WRITER JSON:WRITE-OBJECT-START
   s" configuration" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c GEN-CONFIG$ ENDOF
      embed OF c EMBED-CONFIG$ ENDOF
      tally OF c GEN-CONFIG$ ENDOF
      compaction OF c GEN-CONFIG$ ENDOF
   ;MATCH JSON:WRITE-FIELD-RAW ;

: JOURNAL-RECORD ( ^call -- )
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
   c WRITER JSON:CLOSE ;

\ A nonzero count, else none.
: NONZERO ( n -- option<n> ) dup 0<> if OPTION:SOME else drop OPTION:NONE then ;

: BACKEND$ ( ^call -- str )
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
: PREFIX-HASH$ ( ^call -- str )
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
   w SHA256-CTX-BYTES + 32 + 64 >str ;

\ The call's accounting record, the JSON object RECORDER!'s record receives.
\ outcome is ok, cancelled, tier_unavailable for E-TIER, else failed. A send
\ the provider throttled has throttled true; any other has no such member.
\ served_model is the model the reply names and served_at its created_at, in
\ seconds since the epoch, each null when the reply names none.
: CALL-RECORD ( ^call -- )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-CANCELLED @ if E-CANCELLED else l LEDGER-CODE @ then {: fault:n :}
   c JOURNAL-RECORD-BYTES c RESET-BYTES
   c WRITER c JOURNAL-RECORD-BYTES JSON:OPEN drop
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
      chat OF c EFFORT$ STR:LEN 0<> ENDOF
      embed OF false ENDOF
      tally OF c EFFORT$ STR:LEN 0<> ENDOF
      compaction OF false ENDOF
   ;MATCH if
      c EFFORT$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" service_tier"
   c SERVED-BY SERVED-BY-TIER @ SERVED-FIELD
   JSON:WRITE-COMMA s" requested_tier" c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c TIER$ STR:LEN 0<> ENDOF
      embed OF false ENDOF
      tally OF false ENDOF
      compaction OF c TIER$ STR:LEN 0<> ENDOF
   ;MATCH if
      c TIER$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" served_model"
   c SERVED-BY SERVED-BY-MODEL @ SERVED-FIELD
   JSON:WRITE-COMMA s" served_at"
   c SERVED-BY SERVED-BY-CREATED @ COUNT-FIELD
   JSON:WRITE-COMMA s" purpose" c LABEL$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" outcome" l LEDGER-CANCELLED @ if s" cancelled"
   else fault E-TIER = if s" tier_unavailable"
   else fault 0<> if s" failed" else s" ok" then then then JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" fault" fault NONZERO COUNT-FIELD
   JSON:WRITE-COMMA s" http_status" c MODEL-CALL:HTTP-STATUS @ NONZERO COUNT-FIELD
   l LEDGER-THROTTLED @ if
      JSON:WRITE-COMMA s" throttled" true JSON:WRITE-FIELD-BOOL
   then
   JSON:WRITE-COMMA s" incomplete_reason" c MODEL-CALL:INCOMPLETE-U @ 0<> if
      c CONTENT-BUF HELD c MODEL-CALL:INCOMPLETE-U @ SPAN:TAKE SPAN:$ JSON:WRITE-FIELD-S
   else JSON:WRITE-FIELD-NULL then
   JSON:WRITE-COMMA s" request_bytes" c BODY$ STR:LEN JSON:WRITE-FIELD-INT
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         JSON:WRITE-COMMA s" prefix_bytes"
         c GEN$ STR:LEN c SYSTEM$ STR:LEN + c FORM$ STR:LEN + JSON:WRITE-FIELD-INT
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
   c WRITER JSON:CLOSE ;

: REJECT-RUN ( ^ledger -- ^ledger )
   {: l :}
   l LEDGER-CONTEXT @ l LEDGER-CALL-ID @ l LEDGER-REJECT @ execute
   l ;

: RECORD-RUN ( ^call -- ^call )
   {: c :}
   c CALL-RECORD
   c LEDGER {: l :}
   l LEDGER-CONTEXT @ c JOURNAL-RECORD$ l LEDGER-RECORD @ execute
   l LEDGER-CALL-ID !
   c ;

\ The row of the purpose PURPOSE! left pending, plus one; 0 is none.
: PENDING-ROW ( ^call -- n )
   {: c :}
   c PURPOSE-BUF HELD c LEDGER LEDGER-PENDING-U @ SPAN:TAKE SPAN:$ c ROW-OF ;

\ A call starts: its pending purpose becomes its label, required when a
\ recorder is installed, and selects the purpose's row. A fault before it
\ sends reports no HTTP status, as PARTS-RESET and INPUT-RESET ensure for the
\ builders.
: START-LEDGER ( ^call -- )
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
   c RECORD-TEXT-BUF BUF:CLEAR
   0 l LEDGER-TRANSPORT !
   false l LEDGER-THROTTLED !
   construct tier-step preferred c RECEIPT RECEIPT-TIER !
   construct resend-step sending c RECEIPT RECEIPT-RESEND !
   0 c RECEIPT RECEIPT-RESENDS !
   l LEDGER-READY @ u 0= and if E-STATE throw then ;

\ Keeps a dispatched request's fault and cut for its record.
: END-LEDGER ( n bool ^call -- )
   {: code:n cancelled:bool c :}
   c LEDGER {: l :}
   l LEDGER-READY @ l LEDGER-DISPATCHED @ and 0= if exit then
   code l LEDGER-CODE !
   cancelled l LEDGER-CANCELLED ! ;

: JOURNAL-PUT ( ^call -- )
   {: c :}
   c JOURNAL-RECORD
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

\ Whether the provider's reply rejected the request for its size or did not
\ finish: a fault the caller may answer by asking for less.
: REJECTION? ( n -- bool ) dup E-CAPACITY = swap E-INCOMPLETE = or ;

\ A rejection's record holds the call's configuration, the rejection, capacity or
\ incomplete, the provider's reply as received, a reply cut short with the
\ output it streamed or the size rejection's body, and the recorded call id
\ when there is one. A replay reads only the rejection.
: REJECTION-PUT ( n ^call -- )
   {: code:n c :}
   c RECORD-OPEN
   JSON:WRITE-COMMA s" rejection" code E-CAPACITY = if s" capacity" else
      s" incomplete" then JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" response" c RESPONSE$ JSON:WRITE-FIELD-S
   c LEDGER LEDGER-CALL-ID @ {: id:n :}
   id 0<> if JSON:WRITE-COMMA s" call_id" id JSON:WRITE-FIELD-INT then
   JSON:WRITE-OBJECT-END drop
   c WRITER JSON:CLOSE
   c MODEL-CALL:JOURNAL-CONTEXT @ c JOURNAL-INPUT-SPAN$
   c JOURNAL-RECORD$ c MODEL-CALL:JOURNAL-PUT @ execute ;

: PREP-JOURNAL-CHAT ( ^call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-CHAT
   c FINISH-INPUT
   construct call-kind chat c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-COUNT ( ^call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-CHAT
   c FINISH-INPUT
   construct call-kind tally c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-COMPACT ( ^call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-INPUT
   c FINISH-INPUT
   construct call-kind compaction c JOURNAL-GEN-INPUT ;

: PREP-JOURNAL-EMBED ( ^call -- )
   {: c :}
   c REQUIRE-REST
   c CHECK-EMBED
   c JOURNAL-EMBED-INPUT ;

\ Whether the call's resend machine is sending: its send is out, or has
\ landed and not yet been sensed.
: SENDING? ( ^call -- bool )
   RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF true ENDOF
      begun OF false ENDOF
      resting OF false ENDOF
      resuming OF false ENDOF
      done OF false ENDOF
   ;MATCH ;

public

: INCOMPLETE-REASON$ ( ^call -- str )
   {: c :}
   c CONTENT-BUF HELD c MODEL-CALL:INCOMPLETE-U @ SPAN:TAKE SPAN:$ ;
\ The last call's provider message from its rejected HTTP reply or failed
\ response; empty when none.
: ERROR$ ( ^call -- str ) ERROR-BUF BUFFER$ ;

private

\ STREAM-CHECK's E-TRANSPORT is the only one with a status.
: REPLY+ ( n ^call -- )
   {: code:n c :}
   c MODEL-CALL:HTTP-STATUS @ {: status:n :}
   code E-RESPONSE = code E-REFUSED = or
   code E-TRANSPORT = status 200 = and or if
      s"  status " LOG:TEXT+ status LOG:INT+
      s"  body: " LOG:TEXT+ c RESPONSE$ LOG:TEXT+ exit
   then
   status 0= if exit then
   c STREAM STREAM-FAULT @ 0<> {: event:bool :}
   status 200 = c ERROR$ STR:LEN 0= and event 0= and if exit then
   s"  status " LOG:TEXT+ status LOG:INT+
   c ERROR$ STR:LEN 0<> if
      s" : " LOG:TEXT+ c ERROR$ LOG:TEXT+
      event 0= if exit then
   then
   c RESPONSE$ STR:LEN 0= if exit then
   s"  body: " LOG:TEXT+ c RESPONSE$ LOG:TEXT+ ;


: CAUSE+ ( n ^call -- )
   {: code:n c :}
   code E-INCOMPLETE = c INCOMPLETE-REASON$ STR:LEN 0<> and if
      s"  incomplete " LOG:TEXT+ c INCOMPLETE-REASON$ LOG:TEXT+
   then
   c LEDGER LEDGER-TRANSPORT @ {: rc:n :}
   rc 0<> if rc CURL+ then ;


\ The count of each event type the stream carried, its added output items by
\ type and a message's by phase, and how long after dispatch its first output
\ event came; nothing when it carried none.
: EVENTS+ ( ^call -- )
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


\ The text LOG-NOTE! set, then a colon; nothing when none is set.
: NOTE+ ( ^call -- )
   NOTE$ {: s:str :}
   s STR:LEN 0= if exit then
   s"  " LOG:TEXT+ s LOG:TEXT+ s" :" LOG:TEXT+ ;


: KEY-REASON$ ( key-fault -- str )
   MATCH key-fault
      none OF s" " ENDOF
      missing OF s" not found" ENDOF
      empty OF s" empty" ENDOF
      unprintable OF s" not printable" ENDOF
      denied OF s" access denied" ENDOF
      unavailable OF s" store unavailable" ENDOF
      config OF s" store not set up, or service or account invalid" ENDOF
      rejected OF s" rejected by the provider" ENDOF
   ;MATCH ;

: KEY-FAILED? ( ^call -- bool )
   RECEIPT RECEIPT-KEY-READ @ KEY-REASON$ STR:LEN 0<> ;


\ The key's service and account and why it failed, in its read or at the
\ provider; nothing when it did not.
: KEY+ ( ^call -- )
   {: c :}
   c KEY-FAILED? 0= if exit then
   c KEY-SPEC$ {: spec:str :}
   c MODEL-CALL:KEY-SPLIT @ {: su:n :}
   s"  key service " LOG:TEXT+ spec STR:DATA su >str LOG:TEXT+
   s" , account " LOG:TEXT+ spec STR:DATA su + spec STR:LEN su - >str LOG:TEXT+
   s" : " LOG:TEXT+ c RECEIPT RECEIPT-KEY-READ @ KEY-REASON$ LOG:TEXT+ ;


\ The text of a failed chat, embed, count or compaction call's line: its model
\ and URL, response id once known, the caller's note, the code's name, a failed
\ key's service, account and reason, the code's cause, and a chat stream's
\ events, its added output items counted by type and a message's by phase.
: REPORT+ ( n ^call -- )
   {: code:n c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF s" chat " LOG:TEXT+ c GEN$ ENDOF
      embed OF s" embed " LOG:TEXT+ c EMBED-MODEL$ ENDOF
      tally OF s" count " LOG:TEXT+ c GEN$ ENDOF
      compaction OF s" compact " LOG:TEXT+ c GEN$ ENDOF
   ;MATCH LOG:TEXT+
   s"  " LOG:TEXT+ c REQUEST-URL$ LOG:TEXT+
   c RESPONSE-ID$ STR:LEN 0<> if s"  " LOG:TEXT+ c RESPONSE-ID$ LOG:TEXT+ then
   c NOTE+
   s"  " LOG:TEXT+ code ERROR:NAME$ dup STR:LEN 0= if drop code FMT:INT$ then
   LOG:TEXT+
   c KEY+
   code c REPLY+
   code c CAUSE+
   c EVENTS+ ;

\ One line for a failed call, a warning for a rejection.
: REPORT ( n ^call -- )
   {: code:n c :}
   code REJECTION? if LOG:WARN else LOG:ERROR then s" model" LOG:LINE
   code c REPORT+
   LOG:;LINE ;

\ The line of a send that rests ms, naming the rest and which resend follows
\ it, 1 the first: a warning for a fault that clears without a person (the
\ provider away, a failed transfer, a key store that did not answer), an
\ error for one a person must fix.
: REST-REPORT ( n n ^call -- )
   {: ms:n code:n c :}
   code E-UNAVAILABLE = code E-TRANSPORT = or code SECRET:E-UNAVAILABLE = or
   if LOG:WARN else LOG:ERROR then s" model" LOG:LINE
   code c REPORT+
   s" ; sent again in " LOG:TEXT+
   ms LOG:INT+ s"  ms, resend " LOG:TEXT+
   c RECEIPT RECEIPT-RESENDS @ LOG:INT+
   LOG:;LINE ;

ENUM record-event kept faulted ;ENUM

\ Keeps n as the call's outcome unless one is kept.
: KEEP-FAULT ( n ^call -- )
   {: fault:n c :}
   c RECEIPT RECEIPT-OUTCOME @ 0= if fault c OUTCOME! then ;

: RUN-STEP ( ^call [ ^call -- ^call ] -- ^call [ ^call -- ^call ] )
   {: c step :}
   c step execute drop
   c step ;

\ Runs the step on the call while it keeps no outcome, keeping the step's fault.
: CHECK-STEP ( ^call [ ^call -- ^call ] -- )
   {: c step :}
   c RECEIPT RECEIPT-OUTCOME @ 0<> if exit then
   c step [: RUN-STEP ;] catch {: code:n :} 2drop
   code c KEEP-FAULT ;


\ Whether the send's request left: dispatched, or its transfer failed.
: LEFT? ( ^call -- bool )
   LEDGER {: l :}
   l LEDGER-DISPATCHED @ l LEDGER-TRANSPORT @ 0<> or ;

\ How a run that kept fault n, 0 for none, ended with no CANCEL.
: RUN-ENDING ( n bool ^call -- ending )
   {: fault:n replayed:bool c :}
   replayed if
      fault 0= if construct ending replayed exit then
      construct ending replay-failed exit
   then
   fault 0= if construct ending served exit then
   c LEFT? 0= if construct ending unsent exit then
   fault REJECTION? if construct ending rejected exit then
   construct ending failed ;

\ The ending of a run a CANCEL ended before it landed: after a failure its
\ send met first, whose line it keeps, or alone. A send whose transfer the
\ CANCEL cut met none (E-CANCELLED).
: CUT-ENDING ( n bool ^call -- ending )
   {: fault:n replayed:bool c :}
   fault 0= fault E-CANCELLED = or replayed or if
      construct ending cut exit
   then
   c LEFT? 0= if construct ending cut-unsent exit then
   construct ending cut-failed ;

\ The ending of a run that kept fault n, cut when cancelled, kept in the
\ receipt for REST-OUT? and TIER-OUT?.
: KEPT-ENDING ( n bool bool ^call -- ending )
   {: fault:n replayed:bool cancelled:bool c :}
   cancelled if fault replayed c CUT-ENDING
   else fault replayed c RUN-ENDING then
   dup OPTION:SOME c RECEIPT RECEIPT-ENDING ! ;

\ Lands a run that kept fault n and stores its ending in one hold of the
\ call's gate, so a CANCEL that finds the call landed reads its ending too.
\ Answers whether a CANCEL marked the call, and the ending.
: END-RUN ( n bool ^call -- bool ending )
   {: fault:n replayed:bool c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:CANCELLED @ {: cancelled:bool :}
   construct call-state landed c MODEL-CALL:STATE !
   cancelled  fault replayed cancelled c KEPT-ENDING
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

\ Ends the call's transfer, decoder and handle, wipes its header and parses its
\ reply, keeping the first fault; then lands the call and answers its ending.
: SETTLE-SENSE ( ^call -- ending )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-REPLAYED @ {: replayed:bool :}
   c END-TRANSFER
   replayed 0= if c [: RECEIPT-CHECK ;] CHECK-STEP then
   c STREAM STREAM-DECODER @ MATCH option
      none OF ENDOF
      some OF {: d:SSE:decoder :}
         [: d SSE:CLOSE ;] catch {: closed:n :}
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
      some OF {: h:CURL:handle :}
         [: h CURL:CLEANUP ;] catch {: freed:n :}
         freed c KEEP-FAULT
      ENDOF
   ;MATCH
   OPTION:NONE c MODEL-CALL:HANDLE !
   replayed 0= if
      c HEADER-WIPE
      c MODEL-CALL:KIND @ MATCH call-kind
         chat OF c [: CHAT-PARSE ;] CHECK-STEP ENDOF
         embed OF c [: EMBED-PARSE ;] CHECK-STEP ENDOF
         tally OF c [: COUNT-PARSE ;] CHECK-STEP ENDOF
         compaction OF c [: COMPACT-PARSE ;] CHECK-STEP ENDOF
      ;MATCH
   then
   r RECEIPT-OUTCOME @ {: fault:n :}
   fault replayed c END-RUN {: cancelled:bool e :}
   fault cancelled c END-LEDGER
   e ;

: LAND-CUT ( ^call -- ) E-CANCELLED swap OUTCOME! ;


\ Records a dispatched request, keeping the recorder's fault.
: RECORD-SENSE ( ^call -- record-event )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-READY @ l LEDGER-DISPATCHED @ and 0= if
      construct record-event kept exit
   then
   c [: RECORD-RUN ;] catch {: rc:n :} drop
   rc l LEDGER-FAULT !
   rc c RECORD-TEXT-BUF CAUGHT-TEXT!
   rc 0<> if construct record-event faulted exit then
   construct record-event kept ;

\ A put fault replaces the outcome.
: LAND-SERVED ( ^call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if exit then
   c [: dup JOURNAL-PUT ;] catch {: put:n :} drop
   put 0<> if put c OUTCOME! then ;

\ A served call whose recorder failed keeps the recorder's fault and its text
\ as its outcome and is journaled as served.
: LAND-UNRECORDED ( ^call -- )
   {: c :}
   c LEDGER LEDGER-FAULT @ c RECEIPT RECEIPT-OUTCOME !
   c RECORD-TEXT-BUF BUF:SPAN$ c OUTCOME-TEXT-BUF BUF:REPLACE
   c LAND-SERVED ;

\ Journals a live rejection so a replay meets it at the same step and asks for
\ less with no request. A put fault replaces the outcome.
: LAND-REJECTED ( ^call -- )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if exit then
   c RECEIPT RECEIPT-OUTCOME @ c [: 2dup REJECTION-PUT ;] catch {: put:n :} 2drop
   put 0<> if put c OUTCOME! then ;

: REPORT-OUTCOME ( ^call -- ) dup RECEIPT RECEIPT-OUTCOME @ swap REPORT ;

\ A failed send's line waits for the resend machine, which knows whether the
\ send rests.
: KEEP-REPORT ( ^call -- )
   RECEIPT dup RECEIPT-OUTCOME @ swap RECEIPT-REPORT ! ;

\ Of the faults before a request leaves, only a failed key read was caused
\ outside the program, by the store or the item it holds: its line waits for
\ the resend machine, as a failed send's does.
: KEEP-UNSENT ( ^call -- )
   dup KEY-FAILED? if KEEP-REPORT exit then
   drop ;

\ A send that failed before a CANCEL ended it keeps its failure's line, as it
\ would with no CANCEL, then lands cut.
: CUT-UNSENT ( ^call -- ) dup KEEP-UNSENT LAND-CUT ;
: CUT-FAILED ( ^call -- ) dup KEEP-REPORT LAND-CUT ;

\ Writes the line a failed send keeps, if any, once.
: REPORT-SENT ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-REPORT @ {: code:n :}
   code 0= if exit then
   0 r RECEIPT-REPORT !
   code c REPORT ;

\ Lands a call once its run ends: what each ending reports, records, saves and
\ leaves as the outcome. Settling runs in SETTLE, the rest in ACCOUNT.
FSM:DEFINE LAND-MACHINE ( ^call ) land-step ending
   IN settling  SETTLE-SENSE
      cut            LAND-CUT        >recording
      cut-unsent     CUT-UNSENT      >recording
      cut-failed     CUT-FAILED      >recording
      replayed       -               >landed
      replay-failed  -               >landed
      served         -               >serving
      unsent         KEEP-UNSENT     >landed
      rejected        REPORT-OUTCOME  >rejecting
      failed         KEEP-REPORT     >recording
   IN recording  RECORD-SENSE ( record-event )
      kept     -  >landed
      faulted  -  >landed
   IN serving  RECORD-SENSE ( record-event )
      kept     LAND-SERVED      >landed
      faulted  LAND-UNRECORDED  >landed
   IN rejecting  RECORD-SENSE ( record-event )
      kept     LAND-REJECTED  >landed
      faulted  LAND-REJECTED  >landed
   FINAL landed  -
;DEFINE


ENUM send-step looking sending out ended ;ENUM
ENUM lookup-event missed replayed faulted ;ENUM
ENUM dispatch-event armed faulted ;ENUM

\ Restores the journal record in JOURNAL-OUT-BUF as the call's reply.
: RESTORE ( ^call -- )
   {: c :}
   true c RECEIPT RECEIPT-REPLAYED !
   c [: dup JOURNAL-RESTORE ;] catch {: code:n :} drop
   code JSON:INPUT-FAULT? if E-RESPONSE else code then c KEEP-FAULT ;

: LOOK-UP ( ^call -- lookup-event )
   {: c :}
   c MODEL-CALL:JOURNAL-READY @ 0= if construct lookup-event missed exit then
   [: c JOURNAL-GET? ;] catch {: hit code:n :}
   code 0<> if code c KEEP-FAULT construct lookup-event faulted exit then
   hit if c RESTORE construct lookup-event replayed exit then
   construct lookup-event missed ;

: BOUND ( CURL:handle ^call -- CURL:status )
   {: h:CURL:handle c :}
   c MODEL-CALL:KIND @ MATCH call-kind
      embed OF h c MODEL-CALL:DEADLINE @ >MS CURL:TIMEOUT! ENDOF
      chat OF h 1 STALL-SECONDS CURL:LOW-SPEED! ENDOF
      tally OF h 1 STALL-SECONDS CURL:LOW-SPEED! ENDOF
      compaction OF h 1 STALL-SECONDS CURL:LOW-SPEED! ENDOF
   ;MATCH ;

\ Sends the request on a fresh handle the land machine cleans up; the key is
\ read once and wiped once libcurl copies it.
: POST ( ^call -- ^call )
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
   h c BOUND c CHECK-STATUS
   c REQUEST-URL$ c BODY$ c AS-POST
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF
         c STREAM STREAM-SSE SSE:OPEN OPTION:SOME c STREAM STREAM-DECODER !
      ENDOF
      embed OF ENDOF
      tally OF ENDOF
      compaction OF ENDOF
   ;MATCH
   c START-TRANSFER
   c ARM
   c ;

: DISPATCH ( ^call -- dispatch-event )
   {: c :}
   c [: POST ;] catch {: code:n :} drop
   code 0= if construct dispatch-event armed exit then
   code c KEEP-FAULT
   construct dispatch-event faulted ;

\ The call whose recorder or journal put this task runs (ACCOUNT), for the
\ cleanup a halt's unwind runs, which takes nothing.
TASK:+USER LANDING n
CAST: CALL>N ( ^call -- n )
CAST: N>CALL ( n -- ^call )

: STEP! ( land-step ^call -- ) RECEIPT RECEIPT-STEP ! ;

\ A fault the machine throws becomes the outcome, and the call ends landed.
: LAND-THREW ( n ^call -- )
   {: rc:n c :}
   rc c OUTCOME!
   construct land-step landed c STEP! ;

: SETTLE-RUN ( ^call -- ^call )
   {: c :}
   c construct land-step settling LAND-MACHINE-STEP drop
   c STEP!
   c ;

: SETTLE-CAUGHT ( ^call -- )
   {: c :}
   c [: SETTLE-RUN ;] catch {: rc:n :} drop
   rc 0= if exit then
   rc c [: LAND-THREW ;] [: c END-CALL ;] finally ;

\ Lands a call once its run ends, keeping the next step for ACCOUNT. It runs
\ deferred: a halt while it runs leaves the call landed.
: SETTLE ( ^call -- )
   TASK:DEFER-ENTER
   [: SETTLE-CAUGHT ;] [: TASK:DEFER-LEAVE ;] finally ;

\ Whether the call's recorder or journal put is still to run.
: ACCOUNTING? ( land-step -- bool )
   MATCH land-step
      settling OF false ENDOF
      recording OF true ENDOF
      serving OF true ENDOF
      rejecting OF true ENDOF
      landed OF false ENDOF
   ;MATCH ;

: ACCOUNT-RUN ( ^call -- ^call )
   {: c :}
   c RECEIPT {: r :}
   begin
      c r RECEIPT-STEP @ LAND-MACHINE-STEP
      swap r RECEIPT-STEP !
   until
   c ;

: ACCOUNT-CAUGHT ( ^call -- )
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
   c REPORT-SENT
   c LAND-CUT
   construct land-step landed c STEP! ;

\ Runs a settled call's recorder and journal put after SETTLE's deferred
\ section: a halt served while they wait leaves the call landed E-CANCELLED.
: ACCOUNT ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-STEP @ ACCOUNTING? 0= if exit then
   LANDING @ r RECEIPT-OUTER !
   c CALL>N LANDING !
   c [: ACCOUNT-CAUGHT ;] [: LANDING-END ;] finally ;

\ Settles the call, then runs its recorder and journal put.
: LAND ( ^call -- )
   dup SETTLE ACCOUNT ;

\ Receives the reply, or drops one the call cannot keep and drains its
\ transfer, settling the call when it ends; reading and dropping park, not wait.
FSM:DEFINE RECEIVE-MACHINE ( ^call ) receive-step answer
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


: RECEIVE ( ^call -- )
   {: c :}
   c c MODEL-CALL:RECEIVE @ MATCH receive-rest
      reading OF construct receive-step waiting ENDOF
      dropping OF construct receive-step drop-waiting ENDOF
   ;MATCH RECEIVE-MACHINE ;

: RECEIVE-READY ( ^call -- )
   {: c :}
   c c MODEL-CALL:RECEIVE @ MATCH receive-rest
      reading OF construct receive-step reading ENDOF
      dropping OF construct receive-step dropping ENDOF
   ;MATCH RECEIVE-MACHINE ;

\ Settles a call whose receive threw n before the machine ended; 0: the machine
\ settled or parked it.
: LAND-FAULT ( n ^call -- )
   {: code:n c :}
   code 0= if exit then
   code c KEEP-FAULT
   c SETTLE ;

\ Sends a begun call's request, or restores its journal hit: out with the
\ transfer armed, or landed.
FSM:DEFINE SEND-MACHINE ( ^call ) send-step lookup-event
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

\ A send's or a replay's receipt: nothing replayed, failed, still to report,
\ failing its key, asked to wait or ended yet.
: RECEIPT-RESET ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   false r RECEIPT-REPLAYED !
   0 r RECEIPT-OUTCOME !
   c OUTCOME-TEXT-BUF BUF:CLEAR
   0 r RECEIPT-REPORT !
   construct key-fault none r RECEIPT-KEY-READ !
   0 r RECEIPT-RETRY-AFTER !
   OPTION:NONE r RECEIPT-ENDING ! ;

: SEND-BEGUN ( ^call -- )
   {: c :}
   c RECEIPT-RESET
   c RECEIPT {: r :}
   r RECEIPT-TIER @ r RECEIPT-SENT !
   construct land-step settling c STEP!
   c construct send-step looking SEND-MACHINE ;


: LANDED? ( ^call -- bool )
   MODEL-CALL:STATE @ MATCH call-state
      idle OF false ENDOF
      prepared OF false ENDOF
      started OF false ENDOF
      landed OF true ENDOF
   ;MATCH ;

: STARTED? ( ^call -- bool )
   MODEL-CALL:STATE @ MATCH call-state
      idle OF false ENDOF
      prepared OF false ENDOF
      started OF true ENDOF
      landed OF false ENDOF
   ;MATCH ;

\ Whether the receipt keeps a run that ended failed.
: FAILED-ENDING? ( ^call -- bool )
   RECEIPT RECEIPT-ENDING @ MATCH option
      none OF false ENDOF
      some OF
         MATCH ending
            cut OF false ENDOF
            cut-unsent OF false ENDOF
            cut-failed OF false ENDOF
            replayed OF false ENDOF
            replay-failed OF false ENDOF
            served OF false ENDOF
            unsent OF false ENDOF
            rejected OF false ENDOF
            failed OF true ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

\ Whether the send that landed failed with E-TIER at a tier it named, with
\ TIER-RESEND! on and its record kept. Its ending must be failed: a served or
\ rejected send whose journal put threw E-TIER is not sent again.
: TIER-OUT? ( ^call -- bool )
   {: c :}
   c FAILED-ENDING?
   c RECEIPT RECEIPT-OUTCOME @ E-TIER = and
   c MODEL-CALL:TIER-RESEND @ and
   c TIER$ STR:LEN 0<> and
   c LEDGER LEDGER-FAULT @ 0= and ;

\ A send TIER-OUT? accepts is halted instead while a halt of its starter is
\ pending, a halt served there (TASK:HALT-DUE?), so no request is sent before
\ RECEIVE-STARTED's HALT-UNWIND serves it.
: TIER-SENSE ( ^call -- tier-event )
   TIER-OUT? 0= if construct tier-event settled exit then
   TASK:HALT-DUE? if construct tier-event halted exit then
   construct tier-event tier-out ;

\ A resend's ledger: nothing dispatched, recorded, failed or throttled yet.
: LEDGER-RESET ( ^call -- )
   {: c :}
   c LEDGER {: l :}
   false l LEDGER-DISPATCHED !
   0 l LEDGER-CALL-ID !
   0 l LEDGER-FAULT !
   c RECORD-TEXT-BUF BUF:CLEAR
   0 l LEDGER-TRANSPORT !
   false l LEDGER-THROTTLED ! ;

\ Ends a call a stop or CANCEL kept from being resent, E-CANCELLED stored by
\ RESEND-LOCKED or LAND-CANCELLED; a dispatched one is recorded cancelled.
: CANCEL-RECORD ( ^call -- )
   {: c :}
   0 c MODEL-CALL:HTTP-STATUS !
   false c LEDGER LEDGER-THROTTLED !
   c CLEAR-SERVED-BY
   c RESET-USAGE
   TIME:MONO-NS c LEDGER LEDGER-STARTED-NS !
   E-CANCELLED true c END-LEDGER
   construct land-step recording c STEP!
   c ACCOUNT ;

\ How beginning a landed call's next send went, its fault n, run in the gate
\ hold that begins it: begun; rejected, E-CANCELLED, which RESEND-LOCKED
\ stored as its outcome for a stop or CANCEL since the send landed; else
\ faulted, the call landed with n stored here as its outcome and no ending,
\ so a CANCEL once the gate is released finds it out of REST-OUT?'s and
\ TIER-OUT?'s class.
: BEGIN-EVENT ( n ^call -- begin-event )
   {: code:n c :}
   code 0= if construct begin-event begun exit then
   code E-CANCELLED = if construct begin-event rejected exit then
   code c OUTCOME!
   OPTION:NONE c RECEIPT RECEIPT-ENDING !
   construct begin-event faulted ;

\ Runs begin on a landed call under its gate, as BEGIN-GATED does, and
\ answers its BEGIN-EVENT from the same hold. A closed call is E-STATE: it
\ has no gate or receipt to store in.
: BEGIN-AGAIN ( ^call [ ^call -- ^call ] -- begin-event )
   {: c begin :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c begin catch {: code:n :} drop
   code c BEGIN-EVENT
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

\ Begins a landed call that ended E-TIER again under its gate.
: TIER-STANDARD ( ^call -- begin-event )
   [: RESEND-LOCKED ;] BEGIN-AGAIN ;

\ Sends the begun call's request again under its label and purpose row,
\ naming no tier: the send reads that from the state, so the state moves
\ first.
: SEND-STANDARD ( ^call -- )
   {: c :}
   construct tier-step standard c RECEIPT RECEIPT-TIER !
   c LEDGER-RESET
   c SEND-BEGUN ;

\ Sends a call that ended E-TIER once more with no tier, after the landed
\ send's line. A stop or CANCEL since the send landed lands the call
\ E-CANCELLED with its cancelled record; a halt of its starter due there, or
\ a fault beginning it, leaves it landed with that send's outcome or the
\ fault. Stepped each time a send of the call has landed and been recorded.
FSM:DEFINE TIER-MACHINE ( ^call ) tier-step tier-event
   IN preferred  TIER-SENSE
      settled   -            >done
      tier-out  REPORT-SENT  >resuming
      halted    -            >done
   IN resuming  TIER-STANDARD ( begin-event )
      begun    SEND-STANDARD  >standard
      rejected  CANCEL-RECORD  >done
      faulted  -              >done
   IN standard  TIER-SENSE
      settled   -  >done
      tier-out  -  >done
      halted    -  >done
   FINAL done  -
;DEFINE

: TIER-MOVE ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   c r RECEIPT-TIER @ TIER-MACHINE-STEP drop
   r RECEIPT-TIER ! ;

: TIER-RESUMING? ( ^call -- bool )
   RECEIPT RECEIPT-TIER @ MATCH tier-step
      preferred OF false ENDOF
      resuming OF true ENDOF
      standard OF false ENDOF
      done OF false ENDOF
   ;MATCH ;

\ Steps a landed call's tier machine, and once more when it resumes, keeping
\ its next state until START-LEDGER begins the next call: a resend leaves the
\ call started, or landed on a journal hit or a failed dispatch.
: TIER-STEP ( ^call -- )
   {: c :}
   c LANDED? 0= if exit then
   c TIER-MOVE
   c TIER-RESUMING? if c TIER-MOVE then ;

public

\ Whether fault n of the call is one RESEND-WAIT! rests: the provider away
\ (E-UNAVAILABLE), a failed or stalled transfer (E-TRANSPORT), a rejected key
\ (E-KEY), an unknown model (E-CONFIG with HTTP status 404), or the key
\ store's SECRET:E-CONFIG, E-ACCESS or E-UNAVAILABLE from the key read at
\ dispatch.
: RESENDS? ( n ^call -- bool )
   {: code:n c :}
   code E-UNAVAILABLE = code E-TRANSPORT = or code E-KEY = or
   code E-CONFIG = c MODEL-CALL:HTTP-STATUS @ 404 = and or
   code SECRET:E-CONFIG = or code SECRET:E-ACCESS = or
   code SECRET:E-UNAVAILABLE = or ;

\ The TIME:MONO-NS at which a resting call is sent again, 0 when the call is
\ not resting.
: RESEND-DUE ( ^call -- n )
   {: c :}
   c REQUIRE-OPEN
   c RECEIPT RECEIPT-DUE @ ;

private

\ Whether a stop or CANCEL was asked for, read under the call's gate.
: STOP-ASKED? ( ^call -- bool )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:STOPPED @ c MODEL-CALL:CANCELLED @ or {: asked:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   asked ;

\ Whether a run that ended so may rest: a send that failed once its request
\ left, or went unsent in its key read. A served or rejected send whose journal
\ put failed, a replay, a failed journal lookup and a cut run do not.
: ENDING-RESTS? ( ending ^call -- bool )
   {: e c :}
   e MATCH ending
      cut OF false ENDOF
      cut-unsent OF false ENDOF
      cut-failed OF false ENDOF
      replayed OF false ENDOF
      replay-failed OF false ENDOF
      served OF false ENDOF
      unsent OF c KEY-FAILED? ENDOF
      rejected OF false ENDOF
      failed OF true ENDOF
   ;MATCH ;

: RESTING-ENDING? ( ^call -- bool )
   {: c :}
   c RECEIPT RECEIPT-ENDING @ MATCH option
      none OF false ENDOF
      some OF c ENDING-RESTS? ENDOF
   ;MATCH ;

\ Whether a landed send rests with RESEND-WAIT! on: its ending may rest, its
\ fault is in RESENDS?'s class, and its record is kept. A failed ending has no
\ SECRET fault: the key read and the journal lookup, where one arises, leave
\ the send unsent, and ENDING-RESTS? rests that only with KEY-FAILED?.
: REST-OUT? ( ^call -- bool )
   {: c :}
   c MODEL-CALL:RESEND-WAIT @ 0<>
   c RESTING-ENDING? and
   c RECEIPT RECEIPT-OUTCOME @ c RESENDS? and
   c LEDGER LEDGER-FAULT @ 0= and ;

\ Whether a stop or CANCEL marked the landed send to keep it from being sent
\ again (CANCEL-LOCKED), read under the call's gate. A send a CANCEL cut is
\ E-CANCELLED already, and so is one a cancelled resend began.
: CANCEL-KEPT? ( ^call -- bool )
   {: c :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c MODEL-CALL:CANCELLED @ {: marked:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   marked c RECEIPT RECEIPT-OUTCOME @ E-CANCELLED <> and ;

\ A landed send is stopped when a stop or CANCEL kept it from being sent
\ again, the mark binding even if a later recorder fault takes it out of the
\ class; else outside when it is in REST-OUT?'s class, else settled.
: SENT-SENSE ( ^call -- sent-event )
   {: c :}
   c CANCEL-KEPT? if construct sent-event stopped exit then
   c REST-OUT? if construct sent-event outside exit then
   construct sent-event settled ;

\ The most ms the next rest's random part takes.
: REST-BOUND ( ^call -- n )
   {: c :}
   FIRST-REST-MS c RECEIPT RECEIPT-RESENDS @ DOUBLINGS-MAX min lshift
   c MODEL-CALL:RESEND-WAIT @ min ;

\ A random ms of 0 to the bound; the bound, a legal rest, when the draw
\ fails, which entropy.f has logged.
: JITTER ( n -- n )
   {: bound:n :}
   [: ENTROPY:RANDOM ;] catch {: drawn code:n :}
   code 0= if drawn 1 rshift bound 1+ mod exit then
   code ENTROPY:E-FILL = if bound exit then
   code throw ;

\ Writes the send's line, naming the rest, and starts the call again under
\ its gate with no transfer until it is due.
: BEGIN-REST ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-RETRY-AFTER @ RETRY-SECONDS-MAX min 1000 *
   c REST-BOUND JITTER + {: ms:n :}
   1 r RECEIPT-RESENDS +!
   r RECEIPT-REPORT @ {: code:n :}
   code 0<> if 0 r RECEIPT-REPORT ! ms code c REST-REPORT then
   TIME:MONO-NS ms NS-PER-MS * + {: due:n :}
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   due r RECEIPT-DUE !
   construct call-state started c MODEL-CALL:STATE !
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM ;

: REST-SENSE ( ^call -- rest-event )
   {: c :}
   c STOP-ASKED? if construct rest-event stopped exit then
   TIME:MONO-NS c RECEIPT RECEIPT-DUE @ - 0< if
      construct rest-event early exit
   then
   construct rest-event due ;

\ Ends a rest under the gate: the call lands as its send left it.
: REST-END-LOCKED ( ^call -- ^call )
   {: c :}
   0 c RECEIPT RECEIPT-DUE !
   construct call-state landed c MODEL-CALL:STATE !
   c ;

\ Begins the resend: RESEND-LOCKED makes a stop or CANCEL during the rest
\ E-CANCELLED.
: RESUME-LOCKED ( ^call -- ^call ) REST-END-LOCKED RESEND-LOCKED ;

\ Lands E-CANCELLED a call a stop or CANCEL kept from being sent again,
\ resting or not, after the line its send keeps, if any. The outcome is
\ stored in the gate hold that lands it, so a CANCEL once it is released
\ finds it.
: LAND-CANCELLED ( ^call -- )
   {: c :}
   c REPORT-SENT
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c REST-END-LOCKED LAND-CUT
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   c CANCEL-RECORD ;

\ Serves a halt of the starter pending here, where PAUSE would serve it,
\ before a due rest's resend begins: the throw leaves the call resting.
: SERVE-HALT ( ^call -- ) drop 0 TASK:HALT-UNWIND ;

\ Begins a resting call's resend under its gate: RESUME-LOCKED ends the rest
\ and rejects a stop or CANCEL since the rest was sensed.
: RESEND ( ^call -- begin-event ) [: RESUME-LOCKED ;] BEGIN-AGAIN ;

\ Sends a begun resend's kept request again at the tier its last send went
\ out at, as SEND-STANDARD sends the tier machine's own.
: SEND-AGAIN ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   r RECEIPT-SENT @ r RECEIPT-TIER !
   c LEDGER-RESET
   c SEND-BEGUN ;

\ Rests a send that failed for want of the provider, the transport, the key
\ or its store, and sends it again when due, until it is settled or stopped.
\ A stop or CANCEL that keeps a send from being sent again, before the rest
\ or during it, lands the call E-CANCELLED, with a cancelled record when its
\ request was dispatched. A rest just begun is sensed once more, so one that
\ came after the send was sensed and before the rest's start was published
\ lands it in that step; a due one is sent at the next step, a halt of its
\ starter due there served first. A
\ fault beginning the resend becomes its outcome, sensed as a landed send's.
\ Stepped after ACCOUNT and the tier machine, so it senses a settled send.
FSM:DEFINE RESEND-MACHINE ( ^call ) resend-step sent-event
   IN sending  SENT-SENSE
      settled  REPORT-SENT     >done
      outside  BEGIN-REST      >begun
      stopped  LAND-CANCELLED  >done
   IN begun    REST-SENSE ( rest-event )
      early    -               >resting
      due      -               >resting
      stopped  LAND-CANCELLED  >done
   IN resting  REST-SENSE ( rest-event )
      early    -               >resting
      due      SERVE-HALT      >resuming
      stopped  LAND-CANCELLED  >done
   IN resuming  RESEND ( begin-event )
      begun    SEND-AGAIN      >sending
      rejected  CANCEL-RECORD   >done
      faulted  -               >sending
   FINAL done  -
;DEFINE

: RESTING? ( ^call -- bool )
   RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF false ENDOF
      begun OF true ENDOF
      resting OF true ENDOF
      resuming OF false ENDOF
      done OF false ENDOF
   ;MATCH ;

: BEGUN? ( ^call -- bool )
   RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF false ENDOF
      begun OF true ENDOF
      resting OF false ENDOF
      resuming OF false ENDOF
      done OF false ENDOF
   ;MATCH ;

: RESUMING? ( ^call -- bool )
   RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF false ENDOF
      begun OF false ENDOF
      resting OF false ENDOF
      resuming OF true ENDOF
      done OF false ENDOF
   ;MATCH ;

\ Whether the machine is sending and its send has landed.
: SENT? ( ^call -- bool )
   {: c :}
   c LANDED? 0= if false exit then
   c SENDING? ;

\ Whether the tier machine, at preferred or resuming, is to send the landed
\ send again. A send resuming holds TIER-OUT? until its begin's hold stores
\ another outcome, so it is read there too.
: TIER-NEXT? ( ^call -- bool )
   {: c :}
   c RECEIPT RECEIPT-TIER @ MATCH tier-step
      preferred OF c TIER-OUT? ENDOF
      resuming OF c TIER-OUT? ENDOF
      standard OF false ENDOF
      done OF false ENDOF
   ;MATCH ;

\ Whether a landed send not yet sensed is to be sent again: by a rest, or by
\ the tier machine. A resend that lands at once, or whose begin faulted,
\ leaves the machine resuming until RESEND-MOVE stores sending outside the
\ gate, so it reads REST-OUT? as sending does. Only LAND-CANCELLED lands a
\ call resting, and RESEND's rejected begin one resuming, each E-CANCELLED,
\ which CANCEL-LOCKED's own check marks. Read under the call's gate.
: RESEND-NEXT? ( ^call -- bool )
   {: c :}
   c RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF c REST-OUT? c TIER-NEXT? or ENDOF
      begun OF false ENDOF
      resting OF false ENDOF
      resuming OF c REST-OUT? ENDOF
      done OF false ENDOF
   ;MATCH ;

\ Whether the resend machine has yet to sense the landed send: sending, or
\ resuming while a resend that landed at once waits for RESEND-MOVE.
: UNSENSED? ( ^call -- bool )
   RECEIPT RECEIPT-RESEND @ MATCH resend-step
      sending OF true ENDOF
      begun OF false ENDOF
      resting OF false ENDOF
      resuming OF true ENDOF
      done OF false ENDOF
   ;MATCH ;

\ Marks a started call cancelled and cuts its transfer. A landed send not yet
\ sensed (UNSENSED?) keeps its mark, and one still to be sent again
\ (RESEND-NEXT?, a resend landed at once included) is marked: LAND-CUT makes
\ a send cut as it landed E-CANCELLED, and SENT-SENSE, RESEND-LOCKED or the
\ rest it begins reads any other's mark, landing the call E-CANCELLED. A
\ landed call whose outcome is E-CANCELLED is marked too; CANCEL-KEPT?
\ passes over it, so no second cancelled record follows. Any other call's
\ mark is cleared, so the flag says whether the call ends E-CANCELLED.
: CANCEL-LOCKED ( ^call -- ^call )
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
      landed OF
         c UNSENSED? c MODEL-CALL:CANCELLED @ and
         c RESEND-NEXT? or
         c RECEIPT RECEIPT-OUTCOME @ E-CANCELLED = or
         c MODEL-CALL:CANCELLED !
      ENDOF
   ;MATCH
   c ;

: STOP-LOCKED ( ^call -- ^call )
   {: c :}
   true c MODEL-CALL:STOPPED !
   c CANCEL-LOCKED ;

: RESEND-MOVE ( ^call -- )
   {: c :}
   c RECEIPT {: r :}
   c r RECEIPT-RESEND @ RESEND-MACHINE-STEP drop
   r RECEIPT-RESEND ! ;

\ Moves a call's resend machine where it can: a rest is sensed, a due one's
\ resend begun, then a send that has landed, a resend that landed at once
\ included, then a rest that send began. A rest and a send still out wait
\ for the next SERVE or AWAIT.
: RESEND-STEP ( ^call -- )
   {: c :}
   c RESTING? if c RESEND-MOVE then
   c RESUMING? if c RESEND-MOVE then
   c SENT? if c RESEND-MOVE then
   c BEGUN? if c RESEND-MOVE then ;

\ Parks AWAIT's task until a resting call is due or a hint wakes it.
: REST-PARK ( ^call -- )
   {: c :}
   c RESTING? 0= if exit then
   c STOP-ASKED? if exit then
   c RECEIPT RECEIPT-DUE @ TIME:MONO-NS - {: left:n :}
   left 0 <= if exit then
   left NS-PER-MS 1- + NS-PER-MS / >MS TASK:WAIT-MS drop ;

CAST: N>TASK ( n -- ^n )

\ Wakes the starter of a resting call.
: WAKE-STARTER ( ^call -- )
   MODEL-CALL:STARTER @ N>TASK TASK:WAKE ;

: BEGIN-SEND ( ^call -- ) dup BEGIN-CALL dup SEND-BEGUN RESEND-STEP ;


\ A chat AWAIT ended without a fault has used its finished input; a chat that
\ ended in a fault keeps it for the next START or CHAT to send again.
: USED ( ^call -- )
   {: c :}
   c MODEL-CALL:CHAT-INPUT @ MATCH input-state
      none OF ENDOF
      building OF ENDOF
      broken OF ENDOF
      finished OF construct input-state none c MODEL-CALL:CHAT-INPUT ! ENDOF
   ;MATCH ;


\ The starter is a TCB, as CURL's record owner is: a later task on that TCB
\ passes, and the main thread's null TCB is a starter like any other.
: REQUIRE-STARTER ( ^call -- )
   MODEL-CALL:STARTER @ TASK:SELF-N <> if E-STATE throw then ;


: RECEIVE-CAUGHT ( [ ^call -- ^call ] ^call -- )
   {: receive c :}
   c receive catch {: code:n :} drop code c LAND-FAULT ;


\ Deferred, a halt pending at the receive's take settles the call instead of
\ unwinding out of the take.
: RECEIVE-STARTED ( [ ^call -- ^call ] ^call -- )
   {: receive c :}
   c REQUIRE-STARTER
   c RESTING? 0= if
      TASK:DEFER-ENTER
      receive c [: RECEIVE-CAUGHT ;] [: TASK:DEFER-LEAVE ;] finally
      c ACCOUNT
      c TIER-STEP
   then
   c RESEND-STEP
   0 TASK:HALT-UNWIND ;

public

\ Makes a started call end with E-CANCELLED, cutting any transfer at once, and
\ answers whether the call ends E-CANCELLED; the provider, not told, may bill
\ it. A resting call's starter is woken, so its SERVE or AWAIT lands it at
\ once. A CANCEL that keeps a landed send from being sent again, by a rest or
\ by TIER-RESEND!, lands the call E-CANCELLED, with a cancelled record when
\ its request was dispatched, at its next SERVE or AWAIT step, which does not
\ park, and answers true. A CANCEL of a landed send that would not be sent
\ again leaves its outcome and answers whether it is E-CANCELLED.
\ REQUEST-STOP does the same.
: CANCEL ( ^call -- bool )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: CANCEL-LOCKED ;] catch {: code:n :} drop
   c MODEL-CALL:CANCELLED @ {: selected:bool :}
   c RECEIPT RECEIPT-DUE @ 0<> {: resting:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   resting if c WAKE-STARTER then
   code 0<> code CURL:E-STATE <> and if code throw then
   selected ;

: REQUEST-STOP ( ^call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:GATE @ TASK:WAIT-SEM
   c [: STOP-LOCKED ;] catch {: code:n :} drop
   c RECEIPT RECEIPT-DUE @ 0<> {: resting:bool :}
   c MODEL-CALL:GATE @ TASK:SIGNAL-SEM
   resting if c WAKE-STARTER then
   code 0<> code CURL:E-STATE <> and if code throw then ;

\ Sends the chat request JOURNAL-INPUT$ finished, else the one built since
\ PARTS-RESET; a fault before the call begins is thrown, a later one AWAIT's.
\ A request a failed chat kept is sent again, its body and journal input built
\ from the settings and pending purpose then in force: the first try's when
\ neither changed.
: START ( ^call -- )
   {: c :}
   c TAKE-PREPARED {: ready:bool :}
   construct call-result none c MODEL-CALL:RESULT !
   c CLEAR-COUNTS
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
   construct call-kind chat c MODEL-CALL:KIND !
   c BEGIN-SEND ;

\ Sends the embed request as START sends a chat's.
: START-EMBED ( ^call -- )
   {: c :}
   c UNPREPARE drop
   construct call-result none c MODEL-CALL:RESULT !
   c CLEAR-COUNTS
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if c PREP-JOURNAL-EMBED else c CHECK-EMBED then
   construct call-kind embed c MODEL-CALL:KIND !
   c BEGIN-SEND ;

\ Takes what a started call's transfer holds without waiting for more and
\ answers whether the call landed, as a landed call has; AWAIT ends it. A take
\ that faults gives the transfer up and waits until the loop leaves it. A
\ resting call answers false, and is sent again by the SERVE that finds it due.
: SERVE ( ^call -- bool )
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
\ embed keep theirs, and PURPOSE! then START or CHAT sends it again. It waits
\ through a resend TIER-RESEND! makes, and through each rest RESEND-WAIT!
\ makes, parked until it is due or a CANCEL or stop wakes it.
: AWAIT ( ^call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF E-STATE throw ENDOF
      prepared OF E-STATE throw ENDOF
      started OF ENDOF
      landed OF ENDOF
   ;MATCH
   begin c STARTED? while
      [: dup RECEIVE ;] c RECEIVE-STARTED
      c REST-PARK
   repeat
   c IDLE!
   c OUTCOME-THROW
   c MODEL-CALL:KIND @ MATCH call-kind
      chat OF c USED construct call-result content ENDOF
      embed OF construct call-result vectors ENDOF
      tally OF construct call-result none ENDOF
      compaction OF construct call-result none ENDOF
   ;MATCH c MODEL-CALL:RESULT ! ;

\ The content of the chat AWAIT last ended, read on an idle call and borrowed
\ until the next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED, REPLAY or
\ CLOSE.
: CONTENT$ ( ^call -- str )
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
: OUTPUT$ ( ^call -- str )
   {: c :}
   c CONTENT$ drop
   c RESPONSE$ JSON:PARSE {: v :}
   v JSON:OBJECT? 0= if E-RESPONSE throw then
   v s" output" MEMBER JSON:TEXT$ ;

\ Finishes the chat request built since PARTS-RESET as CHAT does before its
\ journal lookup and answers the journal input a journal's get and put receive
\ for it, valid until the next JOURNAL-INPUT$, CHAT, START, COUNT-INPUT, COMPACT,
\ EMBED, START-EMBED or CLOSE. The pending purpose selects the model and effort
\ as at CHAT, so PURPOSE! comes first. A second call answers the same input.
\ The next CHAT or START sends that request, or REPLAY restores a saved reply
\ for it. Until then a call that changes the chat request (PARTS-RESET, TEXT+,
\ CACHE-TEXT+, IMAGE+, ITEM+, PURPOSE!, PURPOSE-MODEL! and the generation
\ model, base URL, system, schema, tools, effort, service tier, cache key and
\ output limit setters) or EMBED or START-EMBED discards it, and it is built
\ again from PARTS-RESET. Once CHAT, START or REPLAY takes it, one that faults
\ leaves it finished and only PARTS-RESET discards it: PURPOSE! then
\ JOURNAL-INPUT$, CHAT or START answer or send it again under the settings
\ then in force, the same input when none changed.
: JOURNAL-INPUT$ ( ^call -- str )
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

\ A journal hit replays the recorded reply; otherwise the streaming request,
\ and its tier resend when one is due, is read event by event until the
\ response ends. START, AWAIT and CONTENT$.
: CHAT ( ^call -- str )
   {: c :}
   c START
   c AWAIT
   c CONTENT$ ;

\ Counts the input tokens of the chat request built since PARTS-RESET without
\ generating: the request stays finished, so the next CHAT or START sends what
\ was counted. Recorded and journaled as a call; it consumes the pending purpose
\ and leaves no result. A prepared, started or landed call is E-STATE.
: COUNT-INPUT ( ^call -- n )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   construct call-result none c MODEL-CALL:RESULT !
   c CLEAR-COUNTS
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-COUNT
   else
      c CHECK-CHAT
      c FINISH-INPUT
   then
   construct call-kind tally c MODEL-CALL:KIND !
   c BEGIN-SEND
   c AWAIT
   c MODEL-CALL:COUNTED @ ;

\ Replaces the input of the chat request built since PARTS-RESET with the
\ provider's compacted window, kept finished for the next COUNT-INPUT, CHAT or
\ START; it needs no schema or tools and is otherwise as COUNT-INPUT.
: COMPACT ( ^call -- )
   {: c :}
   c REQUIRE-OPEN
   c MODEL-CALL:STATE @ MATCH call-state
      idle OF ENDOF
      prepared OF E-STATE throw ENDOF
      started OF E-STATE throw ENDOF
      landed OF E-STATE throw ENDOF
   ;MATCH
   construct call-result none c MODEL-CALL:RESULT !
   c CLEAR-COUNTS
   c START-LEDGER
   c REQUIRE-GOING
   c MODEL-CALL:JOURNAL-READY @ if
      c PREP-JOURNAL-COMPACT
   else
      c CHECK-INPUT
      c FINISH-INPUT
   then
   construct call-kind compaction c MODEL-CALL:KIND !
   c BEGIN-SEND
   c AWAIT ;

\ Restores, for the request JOURNAL-INPUT$ finished, a record a journal's put
\ received for a chat call, as a journal hit does: its configuration, response,
\ content and the call id its live call recorded; it answers the content as
\ CHAT does and consumes the pending purpose. A saved rejection throws E-CAPACITY
\ or E-INCOMPLETE. It sends nothing and records nothing. Without a request
\ JOURNAL-INPUT$ finished since the last CHAT, START or REPLAY it is E-STATE;
\ one that throws keeps the request as JOURNAL-INPUT$ says.
: REPLAY ( str ^call -- str )
   {: s:str c :}
   s s" REPLAY's record" REQUIRE-LENGTH
   c TAKE-PREPARED 0= if E-STATE throw then
   construct call-result none c MODEL-CALL:RESULT !
   c CLEAR-COUNTS
   c START-LEDGER
   c REQUIRE-GOING
   c JOURNAL-OUT-BUF c RESET-SCRATCH
   s c JOURNAL-OUT-BUF REPLACE$
   construct call-kind chat c MODEL-CALL:KIND !
   c BEGIN-CALL
   c RECEIPT-RESET
   c RESTORE
   c LAND
   c AWAIT
   c CONTENT$ ;

\ START-EMBED and AWAIT.
: EMBED ( ^call -- )
   {: c :}
   c START-EMBED
   c AWAIT ;

private

: INDEX-WANT$ ( -- str ) s" VECTOR@'s index of 0 or more below the input count " ;

\ Throws E-CONFIG for index idx outside a call's count inputs.
: INDEX-REJECT ( n n -- )
   {: idx:n count:n :}
   REASON:OPEN REASON:EXPECTED
   INDEX-WANT$ REASON:APPEND count FMT:INT$ REASON:APPEND
   idx FMT:INT$ s" pass the index of an input INPUT+ added" REJECT-TAIL ;

public

\ Input idx's vector: DIMS little-endian IEEE-754 binary32 values, borrowed
\ until the next CHAT, START, COUNT-INPUT, COMPACT, EMBED, START-EMBED, REPLAY or
\ CLOSE.
: VECTOR@ ( n ^call -- str )
   {: idx:n c :}
   c MODEL-CALL:RESULT @ MATCH call-result
      none OF E-STATE throw ENDOF
      content OF E-STATE throw ENDOF
      vectors OF ENDOF
   ;MATCH
   c MODEL-CALL:INPUT-COUNT @ {: count:n :}
   idx 0 < idx count >= or if idx count INDEX-REJECT then
   c VECTORS-BUF BUF:SPAN c idx VECTOR-OFF @ c idx VECTOR-LEN @ SPAN:SUB SPAN:$ ;

: GENERATION-MODEL$ ( ^call -- str ) GEN$ ;
: EMBEDDING-MODEL$ ( ^call -- str ) EMBED-MODEL$ ;
: HTTP-STATUS ( ^call -- n ) MODEL-CALL:HTTP-STATUS @ ;
: INPUT-TOKENS ( ^call -- n ) MODEL-CALL:INPUT-TOKENS @ ;
\ The last call's recorded id: its own row, or for a replayed reply the row
\ its live call recorded; 0 when none.
: CALL-ID ( ^call -- n ) LEDGER LEDGER-CALL-ID @ ;
\ The recorder's fault from the last call, 0 when recording succeeded or did
\ not run. A caller that rethrows the call's own fault reports this one too.
: RECORD-FAULT ( ^call -- n ) LEDGER LEDGER-FAULT @ ;
\ The library rejected the last call's reply: mark its recorded row. A
\ recorder fault is kept for RECORD-FAULT, unless one already is, and thrown.
: REJECTED ( ^call -- )
   {: c :}
   c LEDGER {: l :}
   l LEDGER-READY @ 0= if exit then
   l LEDGER-CALL-ID @ 0= if exit then
   l [: REJECT-RUN ;] catch {: rc:n :} drop
   rc 0= if exit then
   l LEDGER-FAULT @ 0= if
      rc l LEDGER-FAULT !
   then
   rc throw ;
: CACHED-TOKENS ( ^call -- n ) MODEL-CALL:CACHED-TOKENS @ ;
: OUTPUT-TOKENS ( ^call -- n ) MODEL-CALL:OUTPUT-TOKENS @ ;
\ Reasoning tokens are counted inside OUTPUT-TOKENS, not beside them.
: REASONING-TOKENS ( ^call -- n ) MODEL-CALL:REASONING-TOKENS @ ;

;package
