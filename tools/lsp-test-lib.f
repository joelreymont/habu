\ lsp-test-lib.f - the language server end to end: tools/lsp.f run as a
\ client runs it, one child per conversation.
\ Run: bin/hb --load tools/lsp-test.f
\
\ A conversation is written to the server's stdin whole, and the input then
\ ends; its stdout, stderr and exit code are read after. Every frame the server
\ writes must be `Content-Length: N`, CR LF twice and N bytes, and the replies
\ come in the order of the messages they answer. Two conversations run over
\ pipes the test holds: answers-at-once keeps the input open, stdout-closed
\ closes the output first. The directory the test prints after `artifact:`
\ keeps every conversation byte for byte, NAME.in, NAME.out and NAME.err, and
\ its exit code in `exits`: `bin/hb --load tools/lsp.f < NAME.in` replays one.
\ A conversation whose wait ran out keeps what was read before it.
\
\ How the server could fail, and the conversation that would show it:
\
\ Lifecycle
\ - initialize answers other than its capabilities: positions in utf-16, open
\   and close notifications, Full sync, the name habu ............... lifecycle
\ - shutdown answers other than a null result; exit after it exits other than
\   0 or writes to stderr ............................................ lifecycle
\ - a reply held until more input comes, or the end of input .. answers-at-once
\ - exit without shutdown exits 0, after initialize or as the first message
\   ............................................. exit-without-shutdown, exit-first
\ - input ending before exit exits 0 or says nothing ........ eof-before-exit
\ - input ending after shutdown exits other than 0 or says something
\   ...................................................... eof-after-shutdown
\ - a request before initialize served, or answered without its id; a
\   notification before it applied ........................ before-initialize
\ - initialize served twice ................................. initialize-twice
\ - a request after shutdown served; a notification after it applied
\   .......................................................... after-shutdown
\
\ Envelope
\ - an unknown request answered other than -32601; an unknown notification
\   answered ................................................. unknown-method
\ - an id not echoed byte for byte: 0, -7, 1.5e3, 30 digits, a string with
\   escapes, null ....................................................... ids
\ - a body that is not JSON, or empty, answered other than -32700 with id
\   null, or stopping the server .............................. body-not-json
\ - an array, a scalar, jsonrpc "1.0" or a method that is a number answered
\   other than -32600 with the id the message carries; a client's response
\   answered ................................................ invalid-request
\
\ Document sync
\ - open, change and close of a document: the close publishes anything but an
\   empty list for its URI ................................................ sync
\ - a URI matched or echoed as its JSON text instead of decoded (`\/`), or its
\   percent escapes decoded in the echo ................................... sync
\ - a document opened twice held twice, so two closes publish twice ...... sync
\ - a change or close of a document not open applied or silent; a URI that is
\   not a file URI opened; a change with a range applied under Full sync . sync
\ - params absent, of the wrong kind or with a version past a cell answered,
\   applied or stopping the server; each must log one line ..... malformed-params
\
\ Framing
\ - a header form the transport takes refused: a lower-case name, Content-Type
\   before and after, blanks and tabs around the value, leading zeros
\   ............................................................ frame-forms
\ - a framing fault survived or reported without its code: no Content-Length,
\   a value that is no number, a line ended by LF alone, a line over LINE-CAP,
\   a length over the maximum, end of input in a header and in a body; each is
\   its own conversation ........................................... fault-*
\ - a 700 KB body, written in many pieces, refused or its document lost
\   ............................................................... big-frame
\ - a closed stdout ending the server by SIGPIPE instead of exit 1 and the
\   throw that stopped it .................................... stdout-closed
\
\ Not proven here: the stored text, version and path are first read by the
\ check step, whose cases prove them.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/span.f
require lib/memory.f
require lib/byte-buffer.f
require lib/fd-io.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/content-length.f
require lib/json-rpc.f
require tools/lsp-core.f

package LSP-TEST
using BUF

\ ---- storage ------------------------------------------------------------------

\ What a conversation proves is what the server writes and how it exits, and
\ none of that changes when the host is busy, so the time one run of the server
\ may take is a deadlock guard and nothing else: a server that is merely slow
\ must never reach it. The capture bounds a conversation run through it whole.
\ A conversation over pipes the test holds bounds each wait for output against
\ one deadline CONVERSATION-MS after its reads start; the wait for the server's
\ exit once it has ended stdout and stderr is not bounded. Past its bound,
\ either kind kills the server and fails by name.
\
\ Measured 2026-10-02 on a 12-core machine. The heaviest conversation is
\ big-frame, 705 to 751 ms alone at an ambient load average of 52 to 69 and 915
\ to 1051 ms with six copies of this test running at once at a load of 70;
\ every other conversation stays under 610 ms. The guard is ten times the
\ busiest measurement, and no larger, so that all 25 conversations could reach
\ their bounds and still end inside the gate row's SUITE-TIMEOUT-MS of 360 s
\ (test/gate-stdlib-lib.f), each failing by name.
10000 constant CONVERSATION-MS

$10000 constant OUT-CAP                  \ the most a conversation writes to stdout
$1000 constant ERR-CAP                   \ and to stderr
32000 constant BIG-LINES                 \ big-frame's document, 22 bytes a line escaped

OUT-CAP SPAN-BUFFER: OUT-SPAN
ERR-CAP SPAN-BUFFER: ERR-SPAN
variable OUT-LEN                         \ the bytes the server wrote to stdout,
variable OUT-AT                          \ of them the bytes read as frames,
variable ERR-LEN                         \ and the bytes it wrote to stderr

create LABEL-B HDR-BYTES allot           \ the conversation's name
create IN-B HDR-BYTES allot              \ its input
create WANT-B HDR-BYTES allot            \ the stderr it must write
create MSG-B HDR-BYTES allot             \ a message or a reply being composed
create PAR-B HDR-BYTES allot             \ a notification's params
create DIR-B HDR-BYTES allot             \ the artifact directory

: READY ( ptr a -- )  256 N>BLEN INIT ;

: LABEL$ ( -- ptr u8 n )  LABEL-B SPAN$ BLEN>N ;
: IN$ ( -- ptr u8 n )  IN-B SPAN$ BLEN>N ;
: WANT$ ( -- ptr u8 n )  WANT-B SPAN$ BLEN>N ;
: MSG$ ( -- ptr u8 n )  MSG-B SPAN$ BLEN>N ;
: PAR$ ( -- ptr u8 n )  PAR-B SPAN$ BLEN>N ;
: DIR$ ( -- ptr u8 n )  DIR-B SPAN$ BLEN>N ;
: IN+ ( ptr u8 n -- )  N>BLEN IN-B APPEND-SPAN ;
: WANT+ ( ptr u8 n -- )  N>BLEN WANT-B APPEND-SPAN ;
: MSG+ ( ptr u8 n -- )  N>BLEN MSG-B APPEND-SPAN ;
: PAR+ ( ptr u8 n -- )  N>BLEN PAR-B APPEND-SPAN ;
: OUT$ ( -- ptr u8 n )  OUT-SPAN SPAN:$ drop OUT-LEN @ ;
: ERR$ ( -- ptr u8 n )  ERR-SPAN SPAN:$ drop ERR-LEN @ ;

\ A number's decimal text, in SB.
: INT$ ( n -- ptr u8 n )  SB-RESET FMT:SB-INT SB$ ;

\ ---- the input ----------------------------------------------------------------

\ The header LSP frames a body of N bytes with, in SB.
: HEAD$ ( n -- ptr u8 n )
   SB-RESET
   s" Content-Length: " SB-APPEND
   FMT:SB-U
   s\" \r\n\r\n" SB-APPEND
   SB$ ;

\ A body, framed. It never lives in SB, where the header is built.
: FRAMED ( ptr u8 n -- )
   {: a:ptr u:n :}
   u HEAD$ IN+
   a u IN+ ;

\ A body framed in another header form: BEFORE, the length, AFTER, the body.
: FORM ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr bu:n c:ptr cu:n :}
   b bu IN+
   u INT$ IN+
   c cu IN+
   a u IN+ ;

\ A request with no params, from its id's JSON text and its method.
: ASK$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: i:ptr iu:n m:ptr mu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"" MSG+ m mu MSG+ s\" \"}" MSG+
   MSG$ ;

: ASK ( ptr u8 n ptr u8 n -- )  ASK$ FRAMED ;

\ A notification, from its method and its params' JSON text, none when empty.
: TELL$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: m:ptr mu:n p:ptr pu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"" MSG+ m mu MSG+ s\" \"" MSG+
   pu 0 > if s\" ,\"params\":" MSG+ p pu MSG+ then
   s" }" MSG+
   MSG$ ;

: TELL ( ptr u8 n ptr u8 n -- )  TELL$ FRAMED ;

: INITIALIZE$ ( -- ptr u8 n )
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{}}}" ;

: INITIALIZE ( -- )  INITIALIZE$ FRAMED ;
: SHUTDOWN ( -- )  s" 2" s" shutdown" ASK ;
: EXIT-NOTE ( -- )  s" exit" s" " TELL ;

: SIGN-OFF ( -- )
   SHUTDOWN
   EXIT-NOTE ;

\ The documents, each by its URI's JSON text.
: URI-A ( -- ptr u8 n )  s\" \"file:///habu-lsp-test/a.f\"" ;
: URI-B ( -- ptr u8 n )  s\" \"file:///habu-lsp-test/a%20b.f\"" ;
: URI-BIG ( -- ptr u8 n )  s\" \"file:///habu-lsp-test/big.f\"" ;

\ didOpen params, in PAR, from the JSON text of a URI, a version and a text.
: OPEN-PARAMS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n v:ptr vu:n t:ptr tu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+
   s\" ,\"languageId\":\"habu\",\"version\":" PAR+ v vu PAR+
   s\" ,\"text\":" PAR+ t tu PAR+ s" }}" PAR+ ;

\ didChange params, in PAR, from the JSON text of a URI and of contentChanges.
: CHANGE-PARAMS ( ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n c:ptr cu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+
   s\" ,\"version\":2},\"contentChanges\":" PAR+ c cu PAR+ s" }" PAR+ ;

: OPEN-DOC ( ptr u8 n -- )
   s" 1" s\" \": A ( -- ) ;\\n\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ TELL ;

\ Two whole texts, the last one the document's.
: CHANGE-DOC ( ptr u8 n -- )
   s\" [{\"text\":\"x\"},{\"text\":\": B ( -- ) ;\\n\"}]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ TELL ;

\ A change of a range, which Full sync never sends.
: RANGE-CHANGE ( ptr u8 n -- )
   s\" [{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":1}},\"text\":\"y\"}]"
   CHANGE-PARAMS
   s" textDocument/didChange" PAR$ TELL ;

: CLOSE-DOC ( ptr u8 n -- )
   {: u:ptr uu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+ s" }}" PAR+
   s" textDocument/didClose" PAR$ TELL ;

\ ---- the stderr expected ------------------------------------------------------

\ The line for a notification left unapplied: its method and the throw.
: IGNORED ( ptr u8 n n -- )
   {: m:ptr mu:n code:n :}
   s\" lsp: ignored \"" WANT+ m mu WANT+ s\" \": throw " WANT+
   code INT$ WANT+ s\" \n" WANT+ ;

\ The line the server stops on: the throw that stopped it.
: STOPPED ( n -- )
   s" lsp: stopped by throw " WANT+ INT$ WANT+ s\" \n" WANT+ ;

: CUT-SHORT ( -- )  s\" lsp: input ended before exit\n" WANT+ ;

\ A notification the server must leave unapplied with this code.
: UNAPPLIED ( ptr u8 n ptr u8 n n -- )
   {: m:ptr mu:n p:ptr pu:n code:n :}
   m mu p pu TELL
   m mu code IGNORED ;

\ ---- running the server -------------------------------------------------------

\ Starts a conversation of this name, with no input and no stderr expected.
: CONVERSATION ( ptr u8 n -- )
   N>BLEN LABEL-B REPLACE
   IN-B CLEAR
   WANT-B CLEAR
   0 OUT-LEN !
   0 OUT-AT !
   0 ERR-LEN ! ;

\ The engine to run, with the server's command line and environment staged.
: STAGED ( -- ptr u8 len )
   ENGINE-CANDIDATE:PATH$ >LEN
   s" --load" >LEN PROC-ARGV+
   s" tools/lsp.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING ;

\ The conversation's artifact file with this suffix, in SB.
: ARTIFACT$ ( ptr u8 n -- ptr u8 n )
   {: x:ptr xu:n :}
   SB-RESET
   DIR$ SB-APPEND s" /" SB-APPEND LABEL$ SB-APPEND x xu SB-APPEND
   SB$ ;

\ Keeps the conversation: its input, output and stderr, and its exit code.
: ARCHIVE ( n -- )
   {: rc:n :}
   s" .in" ARTIFACT$ IN$ WRITE-ALL
   s" .out" ARTIFACT$ OUT$ WRITE-ALL
   s" .err" ARTIFACT$ ERR$ WRITE-ALL
   MSG-B CLEAR
   LABEL$ MSG+ s"  " MSG+ rc INT$ MSG+ s\" \n" MSG+
   SB-RESET DIR$ SB-APPEND s" /exits" SB-APPEND
   SB$ MSG$ APPEND-FILE ;

\ The server ended so, and must have exited with this code.
: EXITED ( outcome n -- )
   {: r want:n :}
   r PROC-OUTCOME>RC RC>N ARCHIVE
   LABEL$ T-LABEL r want T-OUTCOME-EXITED= ;

: TOOK ( n -- )
   {: ns:n :}
   SB-RESET
   s" lsp-test: " SB-APPEND LABEL$ SB-APPEND s"  " SB-APPEND
   ns 1000000 / FMT:SB-U s"  ms" SB-APPEND
   SB$ type cr ;

\ Runs the server on the conversation's input and reads all it writes. It must
\ exit with RC.
: CONVERSE ( n -- )
   {: rc:n :}
   mono-ns {: t0:n :}
   STAGED IN$ >LEN OUT-SPAN SPAN:$ >LEN ERR-SPAN SPAN:$ >LEN CONVERSATION-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: o:len e:len r :}
   mono-ns t0 - TOOK
   o LEN>N OUT-LEN !
   e LEN>N ERR-LEN !
   r rc EXITED ;

\ ---- the output ---------------------------------------------------------------

: UNREAD ( -- ptr u8 n )  OUT-SPAN SPAN:$ drop OUT-AT @ +  OUT-LEN @ OUT-AT @ - ;

: ALIKE ( ptr u8 n ptr u8 n -- )  LABEL$ T-LABEL T$= ;

\ The length a frame's header names, from the output at the frame: the number
\ between the name and CR LF twice, or -1.
: FRAME-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   s" Content-Length: " nip {: name:n :}
   a u s\" \r\n\r\n" FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: end:n :}
   end name < if -1 exit then
   a name + end name - STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ Output not framed as LSP frames it fails the conversation and is passed over.
: MISFRAMED ( -- ptr u8 n )
   SB-RESET LABEL$ SB-APPEND s" : output not framed as LSP frames it" SB-APPEND
   SB$ T-LABEL false TTRUE
   OUT-LEN @ OUT-AT !
   OUT-SPAN SPAN:$ drop 0 ;

\ The next frame's body. The server frames a message as `Content-Length: N`, CR
\ LF twice and N bytes, the length without sign or leading zeros.
: NEXT-BODY ( -- ptr u8 n )
   UNREAD {: a:ptr u:n :}
   a u FRAME-LEN {: len:n :}
   len 0 < if MISFRAMED exit then
   len HEAD$ {: h:ptr hu:n :}
   a u h hu STARTS-WITH? 0= if MISFRAMED exit then
   hu len + u > if MISFRAMED exit then
   hu len + OUT-AT +!
   a hu + len ;

\ The next frame is this body.
: HEARD ( ptr u8 n -- )
   {: a:ptr u:n :}
   NEXT-BODY a u ALIKE ;

\ The next frame is an error reply to the request with this id's JSON text,
\ with this code. Its message is prose: only that it is a string is read.
: REFUSED ( ptr u8 n n -- )
   {: i:ptr iu:n code:n :}
   NEXT-BODY {: b:ptr bu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"error\":{\"code\":" MSG+
   code INT$ MSG+ s\" ,\"message\":\"" MSG+
   b bu MSG$ nip min MSG$ ALIKE
   b bu s\" \"}}" ENDS-WITH? LABEL$ T-LABEL TTRUE ;

: CAPABILITIES ( -- )
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{\"positionEncoding\":\"utf-16\"," MSG+
   s\" \"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"habu\"}}}" MSG+
   MSG$ HEARD ;

: NULL-RESULT ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"result\":null}" MSG+
   MSG$ HEARD ;

\ The empty diagnostics list published for a document, by the JSON text of the
\ URI the server must echo.
: PUBLISHED ( ptr u8 n -- )
   {: u:ptr uu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":" MSG+
   u uu MSG+ s\" ,\"diagnostics\":[]}}" MSG+
   MSG$ HEARD ;

\ Nothing is left past the frames read, and stderr is what was expected.
: ENDS ( -- )
   UNREAD s" " ALIKE
   ERR$ WANT$ ALIKE ;

\ ---- conversations over pipes the test holds -----------------------------------

\ Starts the server with these descriptors as its stdin, stdout and stderr.
: SPAWN ( fd fd fd -- pid )
   {: i:fd o:fd e:fd :}
   STAGED i o e PROC-SPAWN-ARGV-ENV-IO ;

\ Waits until the descriptor holds a byte or its end; past the deadline, a
\ mono-ns time, it throws E-PROC-TIMEOUT.
: AWAIT ( fd n -- )
   PROC-LEFT-MS POLL-IN-OR-TIMEOUT drop ;

\ Reads the byte past the span's first N once the descriptor holds it: whether
\ one came before the end. A wait promises one byte, and READ-EXACT reads until
\ its span is full, so each read asks for one.
: BYTE ( n fd SPAN:span<u8> n -- bool )
   {: got:n f:fd dst deadline:n :}
   f deadline AWAIT
   f dst got SPAN:SKIP 1 SPAN:TAKE FD-IO:READ-EXACT MATCH FD-IO:fill
      full OF true ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

\ Reads a descriptor into the span until its end or the span is full, keeping
\ the bytes read so far in the cell. Each wait ends at the deadline.
: DRAIN ( fd SPAN:span<u8> ptr n n -- )
   {: f:fd dst len:ptr deadline:n :}
   0 begin
      dup len !
      dup dst SPAN:LEN < if dup f dst deadline BYTE else false then
   while 1+ repeat drop ;

\ The server's outcome once its output is read. A wait that ran out kills it,
\ and the outcome is a timeout, as a capture's is; any other throw goes on.
: SETTLED ( n pid -- outcome )
   {: code:n pid :}
   code 0= if pid PROC-WAIT-OUTCOME exit then
   code E-PROC-TIMEOUT <> if code throw then
   pid SIGKILL PROC-KILL-RAW drop
   pid PROC-WAIT-STATUS drop
   OUTCOME:TIMEOUT ;

\ The reply's first byte while the input stays open; the input then ends,
\ whether the byte came or the wait ran out.
: FIRST-BYTE ( fd fd n -- )
   {: in-w:fd out-r:fd deadline:n :}
   out-r deadline [: 2dup AWAIT ;] catch {: code:n :}
   2drop
   in-w FD>N close
   code 0<> if code throw then ;

\ answers-at-once's reads, against one deadline: the reply's first byte, then
\ stdout and stderr to their ends.
: HEAR-AT-ONCE ( fd fd fd -- fd fd fd )
   {: in-w:fd out-r:fd err-r:fd :}
   CONVERSATION-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   in-w out-r deadline FIRST-BYTE
   out-r OUT-SPAN OUT-LEN deadline DRAIN
   err-r ERR-SPAN ERR-LEN deadline DRAIN
   in-w out-r err-r ;

\ stdout-closed's read: stderr to its end.
: HEAR-ERR ( fd -- fd )
   dup ERR-SPAN ERR-LEN CONVERSATION-MS >MS PROC-DEADLINE-AT DRAIN ;

\ The reply to initialize starts while the input stays open: a server that
\ waited for more input, or for its end, would never answer a client waiting
\ for that reply.
: TEST-ANSWERS-AT-ONCE ( -- )
   s" answers-at-once" CONVERSATION
   INITIALIZE
   CUT-SHORT
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
   PIPE-PAIR {: err-r:fd err-w:fd :}
   in-w FD-CLOEXEC!
   in-w FD-NOSIGPIPE!
   out-r FD-CLOEXEC!
   err-r FD-CLOEXEC!
   in-r out-w err-w SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   err-w FD>N close
   in-w IN$ FD-IO:WRITE-FULL
   in-w out-r err-r [: HEAR-AT-ONCE ;] catch {: code:n :}
   drop 2drop
   out-r FD>N close
   err-r FD>N close
   code pid SETTLED 1 EXITED
   CAPABILITIES
   ENDS ;

\ The server's stdout has no reader. Its input is in the pipe whole and ended,
\ and a write with no reader fails at once, so nothing can hold it up.
: TEST-STDOUT-CLOSED ( -- )
   s" stdout-closed" CONVERSATION
   INITIALIZE
   SIGN-OFF
   E-FS-IO STOPPED
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
   PIPE-PAIR {: err-r:fd err-w:fd :}
   in-w IN$ FD-IO:WRITE-FULL
   in-w FD>N close
   out-r FD>N close
   err-r FD-CLOEXEC!
   in-r out-w err-w SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   err-w FD>N close
   err-r [: HEAR-ERR ;] catch {: code:n :}
   drop
   err-r FD>N close
   code pid SETTLED 1 EXITED
   ENDS ;

\ ---- conversations ------------------------------------------------------------

: TEST-LIFECYCLE ( -- )
   s" lifecycle" CONVERSATION
   INITIALIZE
   s" initialized" s" {}" TELL
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ exit-first: the initialize after exit is never read.
: TEST-EXIT-WITHOUT-SHUTDOWN ( -- )
   s" exit-without-shutdown" CONVERSATION
   INITIALIZE
   EXIT-NOTE
   1 CONVERSE
   CAPABILITIES
   ENDS
   s" exit-first" CONVERSATION
   EXIT-NOTE
   INITIALIZE
   1 CONVERSE
   ENDS ;

: TEST-EOF-BEFORE-EXIT ( -- )
   s" eof-before-exit" CONVERSATION
   INITIALIZE
   CUT-SHORT
   1 CONVERSE
   CAPABILITIES
   ENDS ;

: TEST-EOF-AFTER-SHUTDOWN ( -- )
   s" eof-after-shutdown" CONVERSATION
   INITIALIZE
   SHUTDOWN
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ The document opened before initialize is not open after it.
: TEST-BEFORE-INITIALIZE ( -- )
   s" before-initialize" CONVERSATION
   s" 7" s" shutdown" ASK
   URI-A OPEN-DOC
   URI-A CLOSE-DOC
   INITIALIZE
   URI-A CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   SIGN-OFF
   0 CONVERSE
   s" 7" LSP:NOT-INITIALIZED REFUSED
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

: TEST-INITIALIZE-TWICE ( -- )
   s" initialize-twice" CONVERSATION
   INITIALIZE
   s" 3" s" initialize" ASK
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ The close of a document open before shutdown publishes nothing after it.
: TEST-AFTER-SHUTDOWN ( -- )
   s" after-shutdown" CONVERSATION
   INITIALIZE
   URI-A OPEN-DOC
   SHUTDOWN
   s" 3" s" initialize" ASK
   s" 4" s" shutdown" ASK
   URI-A CLOSE-DOC
   EXIT-NOTE
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 4" JSON-RPC:INVALID-REQUEST REFUSED
   ENDS ;

: TEST-UNKNOWN-METHOD ( -- )
   s" unknown-method" CONVERSATION
   INITIALIZE
   s" 3" s" habu/unknown" ASK
   s" habu/unknown" s" {}" TELL
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 3" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ A string id with a quote, a backslash, \u and \/ escaped.
: ESCAPED-ID ( -- ptr u8 n )  s\" \"a\\\"b\\\\\\u00e9\\/c\"" ;
: LONG-ID ( -- ptr u8 n )  s" 123456789012345678901234567890" ;

: TEST-IDS ( -- )
   s" ids" CONVERSATION
   INITIALIZE
   s" 0" s" habu/unknown" ASK
   s" -7" s" habu/unknown" ASK
   s" 1.5e3" s" habu/unknown" ASK
   LONG-ID s" habu/unknown" ASK
   ESCAPED-ID s" habu/unknown" ASK
   s" null" s" habu/unknown" ASK
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 0" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" -7" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 1.5e3" JSON-RPC:METHOD-NOT-FOUND REFUSED
   LONG-ID JSON-RPC:METHOD-NOT-FOUND REFUSED
   ESCAPED-ID JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" null" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 2" NULL-RESULT
   ENDS ;

: TEST-BODY-NOT-JSON ( -- )
   s" body-not-json" CONVERSATION
   INITIALIZE
   s\" {\"jsonrpc\":" FRAMED
   s" " FRAMED
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" null" JSON-RPC:PARSE-ERROR REFUSED
   s" null" JSON-RPC:PARSE-ERROR REFUSED
   s" 2" NULL-RESULT
   ENDS ;

: TEST-INVALID-REQUEST ( -- )
   s" invalid-request" CONVERSATION
   INITIALIZE
   s" [1]" FRAMED
   s" 5" FRAMED
   s\" {\"jsonrpc\":\"1.0\",\"id\":3,\"method\":\"habu/unknown\"}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":4,\"method\":7}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"result\":null}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":6,\"error\":{\"code\":1,\"message\":\"m\"}}" FRAMED
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" null" JSON-RPC:INVALID-REQUEST REFUSED
   s" null" JSON-RPC:INVALID-REQUEST REFUSED
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 4" JSON-RPC:INVALID-REQUEST REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ The document opened under its URI escaped is changed and closed under it
\ plain, and the close echoes the URI decoded with its percent escapes kept. A
\ second open replaces the first, so one close publishes and the next is
\ refused.
: TEST-SYNC ( -- )
   s" sync" CONVERSATION
   INITIALIZE
   s\" \"file:\\/\\/\\/habu-lsp-test\\/a.f\"" OPEN-DOC
   URI-A CHANGE-DOC
   URI-A CLOSE-DOC
   URI-B OPEN-DOC
   URI-B CLOSE-DOC
   URI-A OPEN-DOC
   URI-A OPEN-DOC
   URI-A CLOSE-DOC
   URI-A CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   URI-A CHANGE-DOC
   s" textDocument/didChange" LSP:E-LSP-NOT-OPEN IGNORED
   s\" \"untitled:Untitled-1\"" OPEN-DOC
   s" textDocument/didOpen" E-URI-SCHEME IGNORED
   s\" \"untitled:Untitled-1\"" CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   URI-A OPEN-DOC
   URI-A RANGE-CHANGE
   s" textDocument/didChange" LSP:E-LSP-PARAMS IGNORED
   URI-A CLOSE-DOC
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   URI-A PUBLISHED
   URI-B PUBLISHED
   URI-A PUBLISHED
   URI-A PUBLISHED
   s" 2" NULL-RESULT
   ENDS ;

\ didOpen params the server cannot read, each logged.
: BAD-OPENS ( -- )
   s" textDocument/didOpen" s" " LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didOpen" s" [1]" LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didOpen" s" {}" LSP:E-LSP-PARAMS UNAPPLIED
   s" 1" s" 1" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" 1.5" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A LONG-ID s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ E-JR-NUMBER UNAPPLIED
   URI-A s" 1" s" null" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED ;

\ didChange and didClose params the server cannot read, each logged.
: BAD-CHANGES ( -- )
   URI-A s" []" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" [1]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" [{}]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didClose" s\" {\"textDocument\":{}}" LSP:E-LSP-PARAMS UNAPPLIED ;

\ None of them stops the server, and the document open among them stays open.
: TEST-MALFORMED-PARAMS ( -- )
   s" malformed-params" CONVERSATION
   INITIALIZE
   BAD-OPENS
   URI-A OPEN-DOC
   BAD-CHANGES
   URI-A CLOSE-DOC
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   URI-A PUBLISHED
   s" 2" NULL-RESULT
   ENDS ;

\ A lower-case name; Content-Type before, blanks and tabs around the value;
\ leading zeros, Content-Type after.
: TEST-FRAME-FORMS ( -- )
   s" frame-forms" CONVERSATION
   INITIALIZE$ s" content-length: " s\" \r\n\r\n" FORM
   s" 2" s" shutdown" ASK$
   s\" Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: \t " s\" \t \r\n\r\n" FORM
   s" exit" s" " TELL$
   s" Content-Length: 000" s\" \r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n" FORM
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ The server stops on the fault's code, and nothing after the fault is read.
: FAULTED ( n -- )
   STOPPED
   1 CONVERSE
   ENDS ;

\ A header line of LINE-CAP + 1 bytes.
: LONG-LINE ( -- )
   s" X-Pad: " IN+
   CONTENT-LENGTH:LINE-CAP 6 - 0 ?do s" a" IN+ loop
   s\" \r\n\r\n" IN+ ;

: TEST-MALFORMED-FRAMES ( -- )
   s" fault-no-length" CONVERSATION
   s\" Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-not-a-number" CONVERSATION
   s\" Content-Length: abc\r\n\r\n" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-lf-alone" CONVERSATION
   s\" Content-Length: 2\n\n{}" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-long-line" CONVERSATION
   LONG-LINE
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-over-maximum" CONVERSATION
   s" Content-Length: " IN+ LSP:MAX-BODY 1+ INT$ IN+ s\" \r\n\r\n" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED ;

: TEST-TRUNCATED-FRAMES ( -- )
   s" fault-eof-in-header" CONVERSATION
   s\" Content-Length: 5\r\n" IN+
   E-CONTENT-LENGTH-TRUNCATED FAULTED
   s" fault-eof-in-body" CONVERSATION
   s\" Content-Length: 5\r\n\r\n{}" IN+
   E-CONTENT-LENGTH-TRUNCATED FAULTED ;

\ A document of BIG-LINES lines, about 700 KB: the server takes it whole and
\ holds it, so its close publishes.
: TEST-BIG-FRAME ( -- )
   s" big-frame" CONVERSATION
   INITIALIZE
   MSG-B CLEAR
   s\" \"" MSG+
   BIG-LINES 0 ?do s" : F ( n -- n ) 1 + ;\n" MSG+ loop
   s\" \"" MSG+
   URI-BIG s" 1" MSG$ OPEN-PARAMS
   s" textDocument/didOpen" PAR$ TELL
   URI-BIG CLOSE-DOC
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   URI-BIG PUBLISHED
   s" 2" NULL-RESULT
   ENDS ;

public

: TEST ( -- )
   LABEL-B READY
   IN-B READY
   WANT-B READY
   MSG-B READY
   PAR-B READY
   DIR-B READY
   s" habu-lsp-test" HB-TMP-MKDIR N>BLEN DIR-B REPLACE
   T-RESET
   TEST-LIFECYCLE
   TEST-ANSWERS-AT-ONCE
   TEST-EXIT-WITHOUT-SHUTDOWN
   TEST-EOF-BEFORE-EXIT
   TEST-EOF-AFTER-SHUTDOWN
   TEST-BEFORE-INITIALIZE
   TEST-INITIALIZE-TWICE
   TEST-AFTER-SHUTDOWN
   TEST-UNKNOWN-METHOD
   TEST-IDS
   TEST-BODY-NOT-JSON
   TEST-INVALID-REQUEST
   TEST-SYNC
   TEST-MALFORMED-PARAMS
   TEST-FRAME-FORMS
   TEST-MALFORMED-FRAMES
   TEST-TRUNCATED-FRAMES
   TEST-BIG-FRAME
   TEST-STDOUT-CLOSED
   SB-RESET s" artifact: " SB-APPEND DIR$ SB-APPEND SB$ type cr
   T-REPORT ;

;using
;package
