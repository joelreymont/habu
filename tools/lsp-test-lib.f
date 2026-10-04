\ lsp-test-lib.f - the language server end to end: tools/lsp.f run as a
\ client runs it, one child per conversation.
\ Run: bin/hb --load tools/lsp-test.f
\
\ A conversation is written to the server's stdin whole, and the input then
\ ends; its stdout, stderr and exit code are read after. Every frame the server
\ writes must be `Content-Length: N`, CR LF twice and N bytes, and the replies
\ come in the order of the messages they answer. The rest run over pipes the
\ test holds: answers-at-once keeps the input open, stdout-closed closes the
\ output first, and each diagnostics conversation is held turn by turn, the
\ messages of a turn written together once the server has answered the last
\ and every publish read as it comes, since the server checks a document only
\ when no input waits. The directory the test prints after `artifact:` keeps
\ every conversation byte for byte, NAME.in, NAME.out and NAME.err, and its
\ exit code in `exits`: `bin/hb --load tools/lsp.f < NAME.in` replays one,
\ though a held one's turns then come at once. A conversation whose wait ran
\ out keeps what was read before it.
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
\ - params absent, of the wrong kind or with a version past a cell, or a URI
\   naming a path that holds a NUL, answered, applied or stopping the server;
\   each must log one line ................................... malformed-params
\
\ Framing
\ - a header form the transport takes refused: a lower-case name, Content-Type
\   before and after, blanks and tabs around the value, leading zeros
\   ............................................................ frame-forms
\ - a framing fault survived or reported without its code: no Content-Length,
\   a value that is no number, a line ended by LF alone, a line over LINE-CAP,
\   a length over the maximum, end of input in a header and in a body; each is
\   its own conversation ........................................... fault-*
\ - a 700 KB body, written in many pieces, refused, cut or its document lost:
\   its last line's diagnostic proves the whole text checked ...... big-frame
\ - a closed stdout ending the server by SIGPIPE instead of exit 1 and the
\   throw that stopped it .................................... stdout-closed
\
\ Diagnostics, each checked against the test's own CHECK:VERIFY-BYTES of the
\ same text and path, whose packets, in order, must be the diagnostics' data
\ - an opened document's list not published with its version, or a diagnostic
\   whose range is not its packet's byte offsets in lines and UTF-16 units, or
\   whose severity, code, source, message or data is not the packet's
\   ...................................................... diagnostics-open
\ - a refusal after an undefined word not listed, or a definition that uses
\   the undefined one as declared listed ................... diagnostics-open
\ - a require of a file of the tree not resolved, or a changed document not
\   checked again .................................... diagnostics-require
\ - a change that came while another waited checked on its own, or a version
\   other than the newest published ................. diagnostics-supersede
\ - a save leaving an open document unchecked, or documents waiting together
\   not checked in turn ................................... save-dirties-all
\ - a close publishing anything but an empty list without a version, or a
\   document opened and closed in one turn checked ...................... close
\ - a file the engine provides checked, or its list kept from the client
\   ........................................................ engine-provided
\ - a character counted in bytes instead of UTF-16 units ........ utf16-column
\ - a dependency's packets not published at its canonical file URI with
\   positions in its text on disk, its list not withdrawn once its packets are
\   gone, or published over the list of the open document that holds it
\   ............................................................. dependency
\ - a dependency's list not published again from disk once the document that
\   held it open closes ................................... dependency-close
\ - a dependency's list at its canonical URI kept once the client, naming it
\   through a symlink, opens it fixed ..................... dependency-symlink
\ - a dependency two documents require withdrawn while one still does, not
\   published again by that one's check, or kept once neither does
\   ...................................................... dependency-shared
\ - one document's words seen by another's check ................ two-in-turn
\ - a check that ended without a verdict publishing, or not saying how it
\   ended ................................................. incomplete, held
\ - a packet placed at the name of the definition it refuses, a public word
\   whose private twin moves other cells, not published at that name
\   ......................................................... shadowed-arity
\ - a warning, which carries no verdict, published with a severity or not at
\   the name of the definition whose effect it did not record .. not-recorded
\ - a duplicate definition, of the document's own word or of one a file it
\   requires defines, not published at the name defined again
\   ......................................... duplicate, duplicate-of-required
\
\ Not proven here: a packet line that is not JSON, that names no file, or that
\ names its file by a relative path, goes to stderr, but the checker writes
\ none of them - every packet names its file, and the verifier names every
\ file by the canonical absolute path the closure gave it - so no conversation
\ can make one. Nor can one make a check outlast LSP-CHECK's deadline or
\ overflow the verifier's capture.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
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
require lib/uri.f
require lib/content-length.f
require lib/json-read.f
require lib/json-rpc.f
require tools/check-verify-core.f
require tools/lsp-core.f

package LSP-TEST
using BUF

\ ---- storage ------------------------------------------------------------------

\ What a conversation proves is what the server writes and how it exits, and
\ none of that changes when the host is busy, so the time one run of the server
\ may take is a deadlock guard and nothing else: a server that is merely slow
\ must never reach it. The capture bounds a conversation run through it whole.
\ A conversation over pipes the test holds bounds each wait for output against
\ one deadline after its reads start: CONVERSATION-MS, or CHECKED-MS for a
\ diagnostics conversation, whose server checks documents and whose test checks
\ them too inside that bound. The wait for the server's exit once it has ended
\ stdout and stderr is not bounded. Past its bound, any kind kills the server
\ and fails by name.
\
\ Measured 2026-10-02 on a 12-core machine with six copies of this test running
\ at once at a load average of 87 to 91: the diagnostics conversations took 528
\ to 1230 ms, the heaviest big-frame, and every other one timed 418 to 793 ms;
\ answers-at-once and stdout-closed, untimed, carry no more than lifecycle. The
\ conversations added since, dependency-close, dependency-symlink and
\ dependency-shared, took 660 to 1111 ms in single runs at a load average of
\ about 140, when big-frame took up to 1289 ms. CONVERSATION-MS is ten times
\ its busiest measurement and CHECKED-MS about eight times big-frame's 1230 ms,
\ and neither is larger, so that all 24 conversations of the one kind and 16 of
\ the other could reach their bounds and still end inside the gate row's
\ SUITE-TIMEOUT-MS of 360 s (test/gate-stdlib-lib.f), each failing by name.
8000 constant CONVERSATION-MS
10000 constant CHECKED-MS

$10000 constant OUT-CAP                  \ the most a conversation writes to stdout
$1000 constant ERR-CAP                   \ and to stderr
32000 constant BIG-LINES                 \ big-frame's document, 22 bytes a line escaped
512 constant PIPE-ATOM                   \ PIPE_BUF: the room POLLOUT promises a writer

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
create EXP-B HDR-BYTES allot             \ a publish expected
create PATH-B HDR-BYTES allot            \ a document's path
create TXT-B HDR-BYTES allot             \ big-frame's document

TYPED-VARIABLE IN-W fd                   \ a held conversation's ends of the server's stdin,
TYPED-VARIABLE OUT-R fd                  \ stdout
TYPED-VARIABLE ERR-R fd                  \ and stderr,
variable DEADLINE                        \ the mono-ns time its waits run out,
variable SENT                            \ and the bytes of its input written

FS-PATH-CAP 3 * 7 + SPAN-BUFFER: URI-SPAN  \ a file URI: file:// and each byte escaped
create JR-ST JR:STORAGE-BYTES allot      \ JR storage for reading a packet

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
: EXP$ ( -- ptr u8 n )  EXP-B SPAN$ BLEN>N ;
: EXP+ ( ptr u8 n -- )  N>BLEN EXP-B APPEND-SPAN ;
: TXT$ ( -- ptr u8 n )  TXT-B SPAN$ BLEN>N ;
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

\ The diagnostics conversations' documents, each by its path: a file of the
\ artifact directory, on disk only when a case writes it there, or of the tree.
: FIXTURE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   DIR$ N>BLEN PATH-B REPLACE
   s" /" N>BLEN PATH-B APPEND-SPAN
   a u N>BLEN PATH-B APPEND-SPAN
   PATH-B SPAN$ BLEN>N ;

: IN-TREE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ N>BLEN PATH-B REPLACE
   s" /" N>BLEN PATH-B APPEND-SPAN
   a u N>BLEN PATH-B APPEND-SPAN
   PATH-B SPAN$ BLEN>N ;

: A-PATH ( -- ptr u8 n )  s" a.f" FIXTURE ;
: B-PATH ( -- ptr u8 n )  s" b.f" FIXTURE ;

\ A path's file URI, in URI-SPAN.
: URI-OF ( ptr u8 n -- ptr u8 n )
   URI-SPAN URI:PATH>FILE {: n:n :}
   URI-SPAN SPAN:$ drop n ;

\ A path's file URI as a JSON string, in PAR.
: URI+ ( ptr u8 n -- )
   URI-OF {: a:ptr u:n :}
   s\" \"" PAR+ a u PAR+ s\" \"" PAR+ ;

\ The byte at A as JSON string text, in PAR: a quote, a backslash and LF
\ escaped; the texts here hold no other control byte.
: ESCAPED+ ( ptr u8 -- )
   {: a:ptr :}
   a c@ {: c:n :}
   c 10 = if s" \n" PAR+ exit then
   c 34 = if s\" \\\"" PAR+ exit then
   c 92 = if s" \\" PAR+ exit then
   a 1 PAR+ ;

\ A document's text as a JSON string, in PAR.
: TEXT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   s\" \"" PAR+
   u 0 ?do a i + ESCAPED+ loop
   s\" \"" PAR+ ;

\ didOpen of the document at PATH with this text and version.
: OPENS ( ptr u8 n ptr u8 n n -- )
   {: p:ptr pu:n t:ptr tu:n v:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ p pu URI+
   s\" ,\"languageId\":\"habu\",\"version\":" PAR+ v INT$ PAR+
   s\" ,\"text\":" PAR+ t tu TEXT+ s" }}" PAR+
   s" textDocument/didOpen" PAR$ TELL ;

\ didChange of the document at PATH to this whole text and version.
: CHANGES ( ptr u8 n ptr u8 n n -- )
   {: p:ptr pu:n t:ptr tu:n v:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ p pu URI+
   s\" ,\"version\":" PAR+ v INT$ PAR+
   s\" },\"contentChanges\":[{\"text\":" PAR+ t tu TEXT+ s" }]}" PAR+
   s" textDocument/didChange" PAR$ TELL ;

\ Params naming the document at PATH alone, in PAR.
: NAMING ( ptr u8 n -- )
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ URI+ s" }}" PAR+ ;

: SAVES ( ptr u8 n -- )  NAMING s" textDocument/didSave" PAR$ TELL ;
: CLOSES ( ptr u8 n -- )  NAMING s" textDocument/didClose" PAR$ TELL ;

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

\ The line about the check of the document at PATH, these words after its URI,
\ then the prose of the test's own check, ended by LF.
: SAID ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   s" lsp: " WANT+ p pu URI-OF WANT+ s" : " WANT+ w wu WANT+ s\" \n" WANT+
   CHECK:VERIFY-LOG$ {: a:ptr u:n :}
   u 0= if exit then
   a u WANT+
   a u + 1- c@ 10 <> if s\" \n" WANT+ then ;

\ The lines about a check that completed with this verdict: none unless the
\ test's own check wrote prose.
: COMPLETED ( ptr u8 n ptr u8 n -- )
   CHECK:VERIFY-LOG$ nip 0= if 2drop 2drop exit then
   SAID ;

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

\ The server ended so, and must have exited with this code; past its deadline,
\ the conversation and what the server wrote are printed.
: EXITED ( outcome n -- )
   {: r want:n :}
   LABEL$ T-LABEL IN$ OUT$ ERR$ r want T-OUTCOME-EXITED=
   r PROC-OUTCOME>RC RC>N ARCHIVE ;

\ Exercise this adapter's timeout arm without waiting for a slow server.
: TIMEOUT-EXIT-PROBE ( -- )
   s" timed-out conversation" CONVERSATION
   s" timeout input" IN+
   s" timeout stdout" dup OUT-LEN ! OUT-SPAN SPAN:COPY
   s" timeout stderr" dup ERR-LEN ! ERR-SPAN SPAN:COPY
   OUTCOME:TIMEOUT 0 EXITED ;

: TEST-EXIT-TIMEOUT ( -- )
   s" LSP timeout reports the conversation and both streams" T-LABEL
   s" package LSP-TEST TIMEOUT-EXIT-PROBE ;package" {: src:ptr srcu:n :}
   src srcu OUT-SPAN SPAN:$ >LEN ERR-SPAN SPAN:$ >LEN
   CONVERSATION-MS >MS SUBJECT:RUN {: outu:len erru:len r :}
   src srcu OUT-SPAN SPAN:$ drop outu LEN>N
   ERR-SPAN SPAN:$ drop erru LEN>N r UNCAUGHT-RC T-OUTCOME-EXITED=
   OUT-SPAN SPAN:$ drop outu LEN>N s" timed-out conversation" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout input" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout stdout" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout stderr" CONTAINS? TTRUE
   ERR-SPAN SPAN:$ drop erru LEN>N s" uncaught throw code -2502" CONTAINS? TTRUE ;

: TOOK ( n -- )
   {: ns:n :}
   SB-RESET
   s" lsp-test: " SB-APPEND LABEL$ SB-APPEND s"  " SB-APPEND
   ns 1000000 / FMT:SB-U s"  ms" SB-APPEND
   SB$ type cr ;

\ Fails the conversation, under its name and this reason, unless the flag holds.
: HOLDS ( bool ptr u8 n -- )
   {: ok:bool r:ptr ru:n :}
   SB-RESET LABEL$ SB-APPEND s" : " SB-APPEND r ru SB-APPEND
   SB$ T-LABEL ok TTRUE ;

\ Runs the server on the conversation's input and reads all it writes. It must
\ exit with RC. The capture writes the input in pieces of PROC-STDIN-CHUNK-CAP,
\ and one piece lands in the empty pipe whole, so the server reads every
\ message of a conversation no larger before it would look for a document to
\ check: none of these conversations expects a check.
: CONVERSE ( n -- )
   {: rc:n :}
   IN$ nip PROC-STDIN-CHUNK-CAP <= s" input past one write" HOLDS
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
   false s" output not framed as LSP frames it" HOLDS
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
   s\" \"textDocumentSync\":{\"openClose\":true,\"change\":1,\"save\":{\"includeText\":false}}}," MSG+
   s\" \"serverInfo\":{\"name\":\"habu\"}}}" MSG+
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

\ Reads a descriptor into the span, past the bytes the cell counts, until its
\ end or the span is full, keeping the count in the cell. Each wait ends at the
\ deadline.
: DRAIN ( fd SPAN:span<u8> ptr n n -- )
   {: f:fd dst len:ptr deadline:n :}
   len @ begin
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
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   s\" \"file:///habu-lsp-test/a%00.f\"" s" 1" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ E-PATH-RANGE UNAPPLIED ;

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

\ ---- diagnostics conversations, held turn by turn --------------------------------

\ Waits until the server's stdin takes a write; past DEADLINE it throws
\ E-PROC-TIMEOUT.
: WRITABLE ( -- )
   IN-W @ POLLOUT PROC-PFD!
   1 DEADLINE @ PROC-LEFT-MS MS>N DEADLINE @ PROC-POLL-RESTART {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then ;

\ Writes a turn: the input not yet written, in one write. Each turn but the
\ first waits for output the server writes only once it has read all it was
\ sent, so the pipe is empty and takes one piece whole: the turn's messages
\ reach the server together, and no check runs between them.
: SAY ( -- )
   IN$ {: a:ptr u:n :}
   u SENT @ - {: left:n :}
   left PROC-STDIN-CHUNK-CAP <= s" turn past one write" HOLDS
   WRITABLE
   IN-W @ a SENT @ + left FD-IO:WRITE-FULL
   u SENT ! ;

\ Writes the next piece of the input, PIPE-ATOM bytes at most, once the pipe
\ has room for it.
: PIECE ( -- )
   IN$ {: a:ptr u:n :}
   u SENT @ - PIPE-ATOM min {: k:n :}
   WRITABLE
   IN-W @ a SENT @ + k FD-IO:WRITE-FULL
   k SENT +! ;

\ Writes the input not yet written piece by piece, so a server that stopped
\ reading cannot hold the test past DEADLINE.
: STREAM ( -- )
   begin SENT @ IN$ nip < while PIECE repeat ;

\ Whether the output not yet read as frames holds a whole frame.
: WHOLE? ( -- bool )
   UNREAD {: a:ptr u:n :}
   a u FRAME-LEN {: len:n :}
   len 0 < if false exit then
   len HEAD$ nip len + u <= ;

\ Reads stdout until the output not yet read as frames holds a whole frame,
\ stdout ends or the output fills its span.
: HEAR ( -- )
   begin
      WHOLE? 0= OUT-LEN @ OUT-SPAN SPAN:LEN < and if
         OUT-LEN @ OUT-R @ OUT-SPAN DEADLINE @ BYTE
      else false then
   while 1 OUT-LEN +! repeat ;

\ Reads stderr until it holds as many bytes as the conversation must write
\ there, or ends: a check that publishes nothing is over once it has said why.
: LOGGED ( -- )
   begin
      ERR-LEN @ WANT$ nip < ERR-LEN @ ERR-SPAN SPAN:LEN < and if
         ERR-LEN @ ERR-R @ ERR-SPAN DEADLINE @ BYTE
      else false then
   while 1 ERR-LEN +! repeat ;

\ stdout and stderr read on to their ends.
: DRAINED ( -- )
   OUT-R @ OUT-SPAN OUT-LEN DEADLINE @ DRAIN
   ERR-R @ ERR-SPAN ERR-LEN DEADLINE @ DRAIN ;

\ The conversation's turns, then shutdown and exit.
: FINISHED ( [ -- ] -- [ -- ] )
   dup execute
   SIGN-OFF
   SAY ;

\ Holds the conversation of these turns with the server, every wait against
\ one deadline CHECKED-MS from its start. Shutdown and exit end it, stdout and
\ stderr are read to their ends, and the server must exit 0 with the null
\ result to shutdown its last frame. A throw kills the server and fails the
\ conversation by name.
: HELD ( [ -- ] -- )
   {: turns :}
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
   in-w IN-W !
   out-r OUT-R !
   err-r ERR-R !
   0 SENT !
   mono-ns {: t0:n :}
   CHECKED-MS >MS PROC-DEADLINE-AT DEADLINE !
   turns [: FINISHED ;] catch {: code:n :}
   drop
   in-w FD>N close
   code 0= if [: DRAINED ;] catch else code then {: why:n :}
   out-r FD>N close
   err-r FD>N close
   mono-ns t0 - TOOK
   why 0= if 0 else E-PROC-TIMEOUT then pid SETTLED {: r :}
   SB-RESET LABEL$ SB-APPEND s" : throw" SB-APPEND SB$ T-LABEL
   why 0 T=
   r 0 EXITED
   why 0<> if exit then
   s" 2" NULL-RESULT
   ENDS ;

\ Holds the conversation of this name and these turns.
: TALK ( ptr u8 n [ -- ] -- )
   {: a:ptr u:n turns :}
   a u CONVERSATION
   turns HELD ;

\ ---- the diagnostics expected ----------------------------------------------------

variable PACKET-NEXT                     \ where the check's next packet starts

\ The test's own check of the text as the file at PATH, whose packets and prose
\ the server's check of them must publish and write, given what is left of the
\ conversation's time. The words below read it.
: CHECKS ( ptr u8 n ptr u8 n -- )
   0 PACKET-NEXT !
   DEADLINE @ PROC-LEFT-MS CHECK:VERIFY-BYTES MATCH CHECK:verdict
      verified OF ENDOF
      refused OF ENDOF
      engine-provided OF ENDOF
      held OF ENDOF
      incomplete OF PROC-OUTCOME>RC drop ENDOF
   ;MATCH ;

\ The check's packet AT bytes into its output: the line from there, empty
\ past the last.
: PACKET-FROM$ ( n -- ptr u8 n )
   CHECK:VERIFY-OUT$ {: at:n a:ptr u:n :}
   at u min {: from:n :}
   a from + u from - {: p:ptr pu:n :}
   p pu 10 INDEX-OF MATCH option
      none OF p pu ENDOF
      some OF IDX>N p swap ENDOF
   ;MATCH ;

\ The raw text of a top-level member of the packet, a string's between its
\ quotes, and whether it is a string.
: MEMBER$ ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: a:ptr u:n k:ptr ku:n :}
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   k ku JR:FIND-KEY 0= if JR:CLOSE NULL$ false exit then
   JR:TOKEN >r JR:SPAN$ rot JR:CLOSE r> JR:T-STR = ;

\ The message the packet's diagnostic carries: the packet's message, or else
\ its code, its suggestion when not empty, and its expected and actual, each on
\ a line of its own.
: MESSAGE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   s\" \"" EXP+
   a u s" message" MEMBER$ if EXP+ s\" \"" EXP+ exit then
   2drop
   a u s" code" MEMBER$ drop EXP+
   a u s" suggestion" MEMBER$ over 0 > and if s" \n" EXP+ EXP+ else 2drop then
   a u s" expected" MEMBER$ if s" \nexpected: " EXP+ EXP+ else 2drop then
   a u s" actual" MEMBER$ if s" \nactual: " EXP+ EXP+ else 2drop then
   s\" \"" EXP+ ;

\ The publish the next frame must be, for the document or the file at PATH,
\ with this version, or without one for -1: begun, up to its list's bracket.
: EXPECT ( ptr u8 n n -- )
   {: p:ptr pu:n v:n :}
   EXP-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"" EXP+
   p pu URI-OF EXP+ s\" \"," EXP+
   v 0 >= if s\" \"version\":" EXP+ v INT$ EXP+ s" ," EXP+ then
   s\" \"diagnostics\":[" EXP+ ;

\ The diagnostic the list holds next, begun: its range, from line L1,
\ character C1 to line L2, character C2.
: RANGE+ ( n n n n -- )
   {: l1:n c1:n l2:n c2:n :}
   EXP$ s" [" ENDS-WITH? 0= if s" ," EXP+ then
   s\" {\"range\":{\"start\":{\"line\":" EXP+ l1 INT$ EXP+
   s\" ,\"character\":" EXP+ c1 INT$ EXP+
   s\" },\"end\":{\"line\":" EXP+ l2 INT$ EXP+
   s\" ,\"character\":" EXP+ c2 INT$ EXP+
   s\" }}" EXP+ ;

\ The check's next packet for this code, advancing to the following packet.
: PACKET-FOR ( ptr u8 n -- ptr u8 n )
   {: c:ptr cu:n :}
   PACKET-NEXT @ PACKET-FROM$ {: a:ptr u:n :}
   SB-RESET LABEL$ SB-APPEND s" : a packet for " SB-APPEND c cu SB-APPEND SB$ T-LABEL
   u 0 > TTRUE
   u 0= if a u exit then
   PACKET-NEXT @ u + 1+ PACKET-NEXT !
   a u ;

\ The diagnostic ended: this code, then its packet's message and data.
: CODED+ ( ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n a:ptr u:n :}
   s\" ,\"code\":\"" EXP+ c cu EXP+
   s\" \",\"source\":\"habu\",\"message\":" EXP+ a u MESSAGE+
   s\" ,\"data\":" EXP+ a u EXP+ s" }" EXP+ ;

\ The diagnostic the list holds next: the check's next packet, from line L1,
\ character C1 to line L2, character C2, with this severity and code.
: DIAG+ ( n n n n n ptr u8 n -- )
   {: l1:n c1:n l2:n c2:n sev:n c:ptr cu:n :}
   c cu PACKET-FOR {: a:ptr u:n :}
   u 0= if exit then
   l1 c1 l2 c2 RANGE+
   s\" ,\"severity\":" EXP+ sev INT$ EXP+
   c cu a u CODED+ ;

\ DIAG+ for a packet with no verdict, whose diagnostic has no severity.
: UNRATED+ ( n n n n ptr u8 n -- )
   {: l1:n c1:n l2:n c2:n c:ptr cu:n :}
   c cu PACKET-FOR {: a:ptr u:n :}
   u 0= if exit then
   l1 c1 l2 c2 RANGE+
   c cu a u CODED+ ;

\ The next frame is the publish expected.
: PUBLISHES ( -- )
   s" ]}}" EXP+
   HEAR
   EXP$ HEARD ;

\ ---- diagnostics ------------------------------------------------------------------

\ F's drop takes the only cell its effect leaves: E-MISMATCH at bytes 15-19.
: TEXT-F ( -- ptr u8 n )  s" : F ( n -- n ) drop ;" ;

\ The list of the document at PATH holding F's packet, at this version.
: F-LISTED ( ptr u8 n n -- )
   {: p:ptr pu:n v:n :}
   TEXT-F p pu CHECKS
   p pu s" refused" COMPLETED
   p pu v EXPECT 0 15 0 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ The first turn: initialize and A opened as F.
: F-OPENED ( -- )
   INITIALIZE
   A-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   A-PATH 1 F-LISTED ;

\ An undefined word refuses LSPT-U1. LSPT-U2 uses its declared effect and
\ certifies; LSPT-U3 calls it with nothing on the stack and LSPT-U4 leaves a
\ cell short, and each is refused.
: TEXT-U ( -- ptr u8 n )
   s\" : LSPT-U1 ( n -- n ) NOPE ;\n: LSPT-U2 ( n -- n ) LSPT-U1 1 + ;\n: LSPT-U3 ( -- n ) LSPT-U1 ;\n: LSPT-U4 ( n -- n ) drop ;\n" ;

\ A opened as F, then changed to U: every refusal listed, in order.
: OPEN-TURNS ( -- )
   F-OPENED
   A-PATH TEXT-U 2 CHANGES
   SAY
   TEXT-U A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 2 EXPECT
   0 21 0 25 1 s" E-UNDEFINED" DIAG+
   2 19 2 26 1 s" E-INPUT-UNDERFLOW" DIAG+
   3 21 3 25 1 s" E-MISMATCH" DIAG+
   PUBLISHES ;

\ Three changes in one turn: the last alone is checked.
: SUPERSEDE-TURNS ( -- )
   F-OPENED
   A-PATH s" : S ( n -- n ) ;" 2 CHANGES
   A-PATH s" y" 3 CHANGES
   A-PATH s\" ( v4 )\n: S ( n -- n ) drop ;" 4 CHANGES
   SAY
   s\" ( v4 )\n: S ( n -- n ) drop ;" A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 4 EXPECT 1 15 1 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

: REQUIRES-FMT ( -- ptr u8 n )  s\" require lib/fmt.f\n: G ( n -- ) FMT:SB-U ;\n" ;
: UNDERFLOWS-FMT ( -- ptr u8 n )  s\" require lib/fmt.f\n: G ( -- ) FMT:SB-U ;\n" ;

\ A requires a file of the tree: checked in A's load context, it verifies, and
\ changed to call that file's word without its input, it is refused.
: REQUIRE-TURNS ( -- )
   INITIALIZE
   A-PATH REQUIRES-FMT 1 OPENS
   SAY
   HEAR CAPABILITIES
   REQUIRES-FMT A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT PUBLISHES
   A-PATH UNDERFLOWS-FMT 2 CHANGES
   SAY
   UNDERFLOWS-FMT A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 2 EXPECT 1 11 1 19 1 s" E-INPUT-UNDERFLOW" DIAG+ PUBLISHES ;

\ A and B, waiting together, are checked in turn, and a save checks both again.
: SAVE-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-F 1 OPENS
   B-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   A-PATH 1 F-LISTED
   B-PATH 1 F-LISTED
   A-PATH SAVES
   SAY
   A-PATH 1 F-LISTED
   B-PATH 1 F-LISTED ;

\ A close publishes an empty list without a version. A opened and closed in one
\ turn is never checked: that list is all it gets.
: CLOSE-TURNS ( -- )
   F-OPENED
   A-PATH CLOSES
   SAY
   A-PATH -1 EXPECT PUBLISHES
   A-PATH TEXT-F 1 OPENS
   A-PATH CLOSES
   SAY
   A-PATH -1 EXPECT PUBLISHES ;

\ The engine provides lib/string.f, so nothing is checked whatever its text.
: ENGINE-TURNS ( -- )
   INITIALIZE
   s" lib/string.f" IN-TREE TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   s" lib/string.f" IN-TREE 1 EXPECT PUBLISHES ;

\ The emoji before H's drop is four bytes and two UTF-16 units.
: TEXT-H ( -- ptr u8 n )  s" ( 😀 ) : H ( n -- n ) drop ;" ;

: UTF16-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-H 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-H A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 0 22 0 26 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

: BIG-PATH ( -- ptr u8 n )  s" big.f" FIXTURE ;

\ big-frame's document, about 700 KB: BIG-LINES comment lines, then F.
: BIG-TEXT ( -- )
   TXT-B CLEAR
   BIG-LINES 0 ?do s\" ( twenty-byte line )\n" N>BLEN TXT-B APPEND-SPAN loop
   TEXT-F N>BLEN TXT-B APPEND-SPAN ;

\ The document is written in many pieces; F's packet on its last line proves
\ it taken whole and checked.
: BIG-TURNS ( -- )
   INITIALIZE
   SAY
   HEAR CAPABILITIES
   BIG-TEXT
   BIG-PATH TXT$ 1 OPENS
   STREAM
   TXT$ BIG-PATH CHECKS
   BIG-PATH s" refused" COMPLETED
   BIG-PATH 1 EXPECT BIG-LINES 15 BIG-LINES 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

: S-PATH ( -- ptr u8 n )  s" s.f" FIXTURE ;
: DEP-PATH ( -- ptr u8 n )  s" dep.f" FIXTURE ;
: TEXT-S ( -- ptr u8 n )  s\" require dep.f\n: S ( -- ) ;\n" ;
: DEP-BROKEN ( -- ptr u8 n )  s\" : D ( n -- n ) drop ;\n" ;
: DEP-FIXED ( -- ptr u8 n )  s\" : D ( n -- n ) ;\n" ;

\ dep.f on disk holds this text.
: DEP-WRITTEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   DEP-PATH a u WRITE-ALL ;

\ The path the packets name dep.f by: its canonical one.
: DEP-CANON ( -- ptr u8 n )  DEP-PATH SOURCE-ROOT:CANONICAL drop ;

\ The empty list of the document at PATH at this version, after the test's
\ own check of this text there, with this verdict.
: LISTED ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: t:ptr tu:n p:ptr pu:n v:n w:ptr wu:n :}
   t tu p pu CHECKS
   p pu w wu COMPLETED
   p pu v EXPECT PUBLISHES ;

\ The list at PATH, with this version or none for -1, holding the broken D's
\ packet, which the test's last check found first.
: D-LISTED ( ptr u8 n n -- )
   EXPECT 0 15 0 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ S requires dep.f, which is not open. Broken on disk, dep.f gets a list of its
\ own; fixed and saved, S's check withdraws it. Opened with its fix while
\ broken on disk, its own check's list stands, and S's check, which reads the
\ disk, publishes nothing over it: shutdown's reply comes next.
: DEPENDENCY-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   DEP-FIXED DEP-WRITTEN
   S-PATH SAVES
   SAY
   TEXT-S S-PATH 1 s" verified" LISTED
   DEP-CANON -1 EXPECT PUBLISHES
   DEP-BROKEN DEP-WRITTEN
   DEP-PATH DEP-FIXED 1 OPENS
   SAY
   DEP-FIXED DEP-PATH CHECKS
   DEP-PATH s" verified" COMPLETED
   DEP-PATH 1 EXPECT PUBLISHES
   S-PATH TEXT-S 2 CHANGES
   SAY
   TEXT-S S-PATH 2 s" refused" LISTED ;

\ dep.f, broken on disk, opened and then closed: its close publishes its empty
\ list, and S's check, which left dep.f to that list while it was open,
\ publishes dep.f's list from disk again.
: DEP-CLOSE-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   DEP-PATH DEP-BROKEN 1 OPENS
   SAY
   DEP-BROKEN DEP-PATH CHECKS
   DEP-PATH s" refused" COMPLETED
   DEP-PATH 1 D-LISTED
   DEP-PATH CLOSES
   SAY
   DEP-PATH -1 EXPECT PUBLISHES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED ;

: LINK-S ( -- ptr u8 n )  s" link/s.f" FIXTURE ;
: LINK-DEP ( -- ptr u8 n )  s" link/dep.f" FIXTURE ;
: REAL-DEP ( -- ptr u8 n )  s" real/dep.f" FIXTURE ;

\ The path the packets name dep.f under real by.
: REAL-CANON ( -- ptr u8 n )  REAL-DEP SOURCE-ROOT:CANONICAL drop ;

\ The client names S and dep.f through link, a symlink to real, and the
\ packets name dep.f by its canonical path, under real. Broken on disk, dep.f
\ gets a list at that canonical URI. Fixed on disk and opened through link, it
\ has its own list at the link URI, and S's check after a save withdraws the
\ canonical one.
: DEP-SYMLINK-TURNS ( -- )
   s" real" FIXTURE MAKE-DIR
   s" real" s" link" FIXTURE MAKE-SYMLINK
   REAL-DEP DEP-BROKEN WRITE-ALL
   INITIALIZE
   LINK-S TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S LINK-S 1 s" refused" LISTED
   REAL-CANON -1 D-LISTED
   REAL-DEP DEP-FIXED WRITE-ALL
   LINK-DEP DEP-FIXED 1 OPENS
   SAY
   DEP-FIXED LINK-DEP 1 s" verified" LISTED
   LINK-S SAVES
   SAY
   TEXT-S LINK-S 1 s" verified" LISTED
   REAL-CANON -1 EXPECT PUBLISHES
   DEP-FIXED LINK-DEP 1 s" verified" LISTED ;

: T-PATH ( -- ptr u8 n )  s" t.f" FIXTURE ;
: TEXT-T ( -- ptr u8 n )  s\" require dep.f\n: T ( -- ) ;\n" ;
: ALONE-S ( -- ptr u8 n )  s\" : S ( -- ) ;\n" ;
: ALONE-T ( -- ptr u8 n )  s\" : T ( -- ) ;\n" ;

\ S and T both require dep.f, broken on disk, and each one's check publishes
\ dep.f's list. S dropping its require leaves that list to T, whose check
\ publishes it again; T dropping its require too withdraws it.
: DEP-SHARED-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   T-PATH TEXT-T 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   TEXT-T T-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   S-PATH ALONE-S 2 CHANGES
   SAY
   ALONE-S S-PATH 2 s" verified" LISTED
   TEXT-T T-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   T-PATH ALONE-T 2 CHANGES
   SAY
   ALONE-T T-PATH 2 s" verified" LISTED
   DEP-CANON -1 EXPECT PUBLISHES ;

: DEFINES-A ( -- ptr u8 n )  s" : LSPT-ONLY-A ( -- ) ;" ;
: CALLS-A ( -- ptr u8 n )  s" : LSPT-B ( -- ) LSPT-ONLY-A ;" ;

\ B calls the word only A defines: each check sees its own document alone.
: TWO-TURNS ( -- )
   INITIALIZE
   A-PATH DEFINES-A 1 OPENS
   B-PATH CALLS-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   DEFINES-A A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT PUBLISHES
   CALLS-A B-PATH CHECKS
   B-PATH s" refused" COMPLETED
   B-PATH 1 EXPECT 0 16 0 27 1 s" E-UNDEFINED" DIAG+ PUBLISHES ;

\ A definition never ended: the verifier exits without a verdict.
: UNENDED ( -- ptr u8 n )  s" : H6 ( n -- n ) 1" ;

: INCOMPLETE-TURNS ( -- )
   INITIALIZE
   A-PATH UNENDED 1 OPENS
   SAY
   HEAR CAPABILITIES
   UNENDED A-PATH CHECKS
   A-PATH s" not checked: exit 74" SAID
   LOGGED ;

\ The verifier's own image holds tools/check-verify-child.f.
: HELD-PATH ( -- ptr u8 n )  s" tools/check-verify-child.f" IN-TREE ;

: HELD-TURNS ( -- )
   INITIALIZE
   HELD-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-F HELD-PATH CHECKS
   HELD-PATH s" not checked: the verifier's image holds it" SAID
   LOGGED ;

\ The public W moves other cells than the private W, whose tail it shares: the
\ packet is at the public definition's name. The refusal refuses W alone: the
\ check goes on, and M's extra cell on line 7 is published beside it.
: SHADOWS ( -- ptr u8 n )
   s\" package LSPT-Q\nprivate\n: W ( n -- ) drop ;\npublic\n: W ( -- ) ;\n;package\n: LSPT-M ( -- ) 1 ;\n" ;

: SHADOWED-TURNS ( -- )
   INITIALIZE
   A-PATH SHADOWS 1 OPENS
   SAY
   HEAR CAPABILITIES
   SHADOWS A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 4 2 4 3 1 s" E-SHADOWED-ARITY" DIAG+
   6 16 6 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ LSPT-WIDE's inferred effect has more type variables than a record holds: the
\ check verifies it and warns at its name that the effect is not recorded.
: WIDENS ( -- ptr u8 n )
   s\" defer LSPT-V14 ( -- a b c d e g h i j k l m o p )\n: LSPT-WIDE LSPT-V14 LSPT-V14 ;\n" ;

: NOT-RECORDED-TURNS ( -- )
   INITIALIZE
   A-PATH WIDENS 1 OPENS
   SAY
   HEAR CAPABILITIES
   WIDENS A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT 1 2 1 11 s" W-EFFECT-NOT-RECORDED" UNRATED+ PUBLISHES ;

\ U's bare LSPT-W resolves in both used packages: E-USING-AMBIGUOUS, at the
\ token on line 10. The refusal refuses U alone: the check goes on, and M's
\ extra cell on line 11 is published beside it.
: TWO-USED ( -- ptr u8 n )
   s\" package LSPT-UA\npublic\n: LSPT-W ( n -- ) drop ;\n;package\npackage LSPT-UB\npublic\n: LSPT-W ( n -- ) drop ;\n;package\nusing LSPT-UA\nusing LSPT-UB\n: LSPT-U ( -- ) 1 LSPT-W ;\n: LSPT-M ( -- ) 1 ;\n;using\n;using\n" ;

: TWO-USED-TURNS ( -- )
   INITIALIZE
   A-PATH TWO-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   TWO-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 10 18 10 24 1 s" E-USING-AMBIGUOUS" DIAG+
   11 16 11 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ U's bare LSPT-SW names the global and the used public both:
\ E-USING-SHADOW-GLOBAL at the token on line 6, and M's extra cell on line 7.
: GLOBAL-USED ( -- ptr u8 n )
   s\" : LSPT-SW ( n -- ) drop ;\npackage LSPT-SP\npublic\n: LSPT-SW ( n -- ) drop ;\n;package\nusing LSPT-SP\n: LSPT-SU ( -- ) 1 LSPT-SW ;\n: LSPT-M ( -- ) 1 ;\n;using\n" ;

: GLOBAL-USED-TURNS ( -- )
   INITIALIZE
   A-PATH GLOBAL-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   GLOBAL-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 6 19 6 26 1 s" E-USING-SHADOW-GLOBAL" DIAG+
   7 16 7 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ The same refusals in does> clauses each refuse their definer alone:
\ E-USING-SHADOW-GLOBAL at DS's bare LSPT-SW on line 12, E-USING-AMBIGUOUS at
\ DM's bare LSPT-DV on line 13, and M's extra cell on line 14.
: CLAUSE-USED ( -- ptr u8 n )
   s\" : LSPT-SW ( n -- ) drop ;\npackage LSPT-DA\npublic\n: LSPT-SW ( n -- ) drop ;\n: LSPT-DV ( n -- ) drop ;\n;package\npackage LSPT-DB\npublic\n: LSPT-DV ( n -- ) drop ;\n;package\nusing LSPT-DA\nusing LSPT-DB\n: LSPT-DS ( n -- ) create , does> ( -- ) @ LSPT-SW ;\n: LSPT-DM ( n -- ) create , does> ( -- ) @ LSPT-DV ;\n: LSPT-M ( -- ) 1 ;\n;using\n;using\n" ;

: CLAUSE-USED-TURNS ( -- )
   INITIALIZE
   A-PATH CLAUSE-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   CLAUSE-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 12 43 12 50 1 s" E-USING-SHADOW-GLOBAL" DIAG+
   13 43 13 50 1 s" E-USING-AMBIGUOUS" DIAG+
   14 16 14 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ DA defined twice: the packet at the second DA, line 1, characters 2-4.
: TEXT-DUP ( -- ptr u8 n )  s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n" ;

: DUPLICATE-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-DUP 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DUP A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 1 2 1 4 1 s" E-DUPLICATE-DEFINITION" DIAG+ PUBLISHES ;

\ The document defines again the word the file it requires defines: the packet
\ is the document's, at its LSPT-DD, line 1, characters 2-9.
: DD-DEP ( -- ptr u8 n )  s" dd-dep.f" FIXTURE ;
: TEXT-DD ( -- ptr u8 n )  s\" require dd-dep.f\n: LSPT-DD ( -- n ) 2 ;\n" ;

: DUP-REQUIRED-TURNS ( -- )
   DD-DEP s\" : LSPT-DD ( -- n ) 1 ;\n" WRITE-ALL
   INITIALIZE
   A-PATH TEXT-DD 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DD A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 1 2 1 9 1 s" E-DUPLICATE-DEFINITION" DIAG+ PUBLISHES ;

: TEST-DIAGNOSTICS ( -- )
   s" diagnostics-open" [: OPEN-TURNS ;] TALK
   s" diagnostics-require" [: REQUIRE-TURNS ;] TALK
   s" diagnostics-supersede" [: SUPERSEDE-TURNS ;] TALK
   s" save-dirties-all" [: SAVE-TURNS ;] TALK
   s" close" [: CLOSE-TURNS ;] TALK
   s" engine-provided" [: ENGINE-TURNS ;] TALK
   s" utf16-column" [: UTF16-TURNS ;] TALK
   s" big-frame" [: BIG-TURNS ;] TALK
   s" dependency" [: DEPENDENCY-TURNS ;] TALK
   s" dependency-close" [: DEP-CLOSE-TURNS ;] TALK
   s" dependency-symlink" [: DEP-SYMLINK-TURNS ;] TALK
   s" dependency-shared" [: DEP-SHARED-TURNS ;] TALK
   s" two-in-turn" [: TWO-TURNS ;] TALK
   s" incomplete" [: INCOMPLETE-TURNS ;] TALK
   s" held" [: HELD-TURNS ;] TALK
   s" shadowed-arity" [: SHADOWED-TURNS ;] TALK
   s" not-recorded" [: NOT-RECORDED-TURNS ;] TALK
   s" using-ambiguous" [: TWO-USED-TURNS ;] TALK
   s" using-shadow" [: GLOBAL-USED-TURNS ;] TALK
   s" using-clause" [: CLAUSE-USED-TURNS ;] TALK
   s" duplicate" [: DUPLICATE-TURNS ;] TALK
   s" duplicate-of-required" [: DUP-REQUIRED-TURNS ;] TALK ;

public

: TEST ( -- )
   LABEL-B READY
   IN-B READY
   WANT-B READY
   MSG-B READY
   PAR-B READY
   DIR-B READY
   EXP-B READY
   PATH-B READY
   TXT-B READY
   s" habu-lsp-test" HB-TMP-MKDIR N>BLEN DIR-B REPLACE
   T-RESET
   TEST-EXIT-TIMEOUT
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
   TEST-STDOUT-CLOSED
   TEST-DIAGNOSTICS
   SB-RESET s" artifact: " SB-APPEND DIR$ SB-APPEND SB$ type cr
   T-REPORT ;

;using
;package
