\ lsp-core.f - the Habu language server: the Language Server Protocol on stdin
\ and stdout, for any client.
\
\ MAIN reads one framed message at a time (lib/content-length.f), sorts it
\ (lib/json-rpc.f) and acts on it, until the client's exit or the end of input.
\ The server is starting, running or shut down. Starting, it answers initialize
\ with its capabilities and any other request with -32002. Running, it answers
\ shutdown with null, initialize with -32600 and any other request with -32601,
\ and it keeps the documents the client opens, changes and closes: Full sync,
\ each change carrying the whole text, and positions in UTF-16 units. Shut
\ down, it answers every request with -32600. Only a running server takes a
\ notification other than exit; the rest are dropped, as unknown ones always
\ are.
\
\ A running server checks the documents and publishes their diagnostics
\ (tools/lsp-check.f, tools/lsp-diag.f). Opening or changing a document leaves
\ it waiting for a check, and a save leaves every open document waiting, since
\ any of them may require the file saved. Input comes first: only when no
\ message waits, in the reader's buffer or on stdin, is one waiting document
\ checked, the next after the last one checked, and then input is looked at
\ again. So a check always takes a document's newest text, the edits that came
\ while another check ran are all applied before it, and the versions
\ published for a document only rise. Closing a document publishes an empty
\ list for it and leaves every open document waiting: any of them may require
\ the file, whose diagnostics their checks left to it while it was open.
\
\ exit ends the process with 0 after shutdown and 1 before it, and so does the
\ end of input, which before shutdown also says so on stderr. A notification
\ the server cannot apply - params it cannot read, a URI that names no file
\ here, a document not open - changes nothing, and since a notification has no
\ reply, one stderr line says why. A response from the client is dropped: the
\ server asks nothing. Any other throw, a framing fault or a closed stdout
\ among them, stops the server with one stderr line naming it and exit code 1.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the reader, the buffers and the documents
\ belong to the server's one task.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/span.f
require lib/memory.f
require lib/byte-buffer.f
require lib/fd-io.f
require lib/process.f
require lib/adt/option.f
require lib/uri.f
require lib/content-length.f
require lib/json-read.f
require lib/json-write.f
require lib/json-rpc.f
require tools/lsp-docs.f
require tools/lsp-diag.f
require tools/lsp-check.f

package LSP
using SPAN
using MEM
using JR
using JSON-WRITE
using JSON-RPC
using CONTENT-LENGTH
using LSP-DOCS

public

-9400 constant E-LSP-FIRST
-9401 constant E-LSP-LAST
-9400 constant E-LSP-PARAMS    \ a notification's params lack a member the server reads, or hold one of another kind
-9401 constant E-LSP-NOT-OPEN  \ a change or close of a document that is not open

-32002 constant NOT-INITIALIZED          \ LSP's ServerNotInitialized
\ The longest body taken, room for a document's whole text escaped. A header
\ that announces more is refused before any of the body is read.
64 1024 * 1024 * constant MAX-BODY

private

1 constant FULL-SYNC                     \ TextDocumentSyncKind.Full

ENUM state starting running shut-down ;ENUM
1 LAYOUT-BUFFER STATE-BUF state

create JR-ST STORAGE-BYTES allot          \ JR storage for every read of a body
LINE-CAP 2 + SPAN-BUFFER: HEAD-BUF        \ the reader's buffer: a header line and its CR LF
create REPLY-BUF BUF:HDR-BYTES allot      \ a reply's bytes, grown as it is written
TYPED-VARIABLE INPUT CONTENT-LENGTH:reader
TYPED-VARIABLE BODY-SPAN SPAN:span<u8>    \ the message body
TYPED-VARIABLE URI-SPAN SPAN:span<u8>     \ textDocument.uri, decoded
TYPED-VARIABLE PATH-SPAN SPAN:span<u8>    \ the file path it names
TYPED-VARIABLE TEXT-SPAN SPAN:span<u8>    \ a document's text, decoded
TYPED-VARIABLE W JSON-WRITE:writer
variable TEXT-LEN                         \ the last change's text length, -1 before one is read
variable LAST-CHECKED                     \ the slot checked last, -1 before any

: STATE! ( state -- )  0 STATE-BUF ! ;
: STATE@ ( -- state )  0 STATE-BUF @ ;

: RUNNING? ( -- bool )
   STATE@ MATCH state
      starting OF false ENDOF
      running OF true ENDOF
      shut-down OF false ENDOF
   ;MATCH ;

\ The exit code: 0 once the client has asked for shutdown, else 1.
: EXIT-CODE ( -- n )
   STATE@ MATCH state
      starting OF 1 ENDOF
      running OF 1 ENDOF
      shut-down OF 0 ENDOF
   ;MATCH ;

\ The span in V made at least N bytes long, and never empty, since JR:STR
\ refuses a null destination.
: ROOM ( ptr SPAN:span<u8> n -- )
   {: v n:n :}
   n 1 max {: need:n :}
   v @ LEN need >= if exit then
   v @ LEN 0 > if v @ FREE-SPAN then
   need BYTES-ALLOC-LEN ALLOC-SPAN v ! ;

\ ---- replies -----------------------------------------------------------------

\ A writer on the reply bytes.
: WRITER ( -- ptr JSON-WRITE:writer )
   W REPLY-BUF JSON-WRITE:OPEN-BUF ;

: SENT ( ptr JSON-WRITE:writer -- )
   {: w :}
   w JSON-WRITE:$ {: a:ptr u:n :}
   w JSON-WRITE:CLOSE
   1 >FD a u SEND ;

: REPLY-NULL ( JSON-RPC:id -- )
   {: i :}
   WRITER i RESULT NULL END SENT ;

: REPLY-ERROR ( JSON-RPC:id n ptr u8 n -- )
   {: i code:n m:ptr mu:n :}
   WRITER i code m mu ERROR SENT ;

: REPLY-CAPABILITIES ( JSON-RPC:id -- )
   {: i :}
   WRITER i RESULT
   OBJECT-START
      s" capabilities" KEY OBJECT-START
         s" positionEncoding" s" utf-16" FIELD-S COMMA
         s" textDocumentSync" KEY OBJECT-START
            s" openClose" true FIELD-BOOL COMMA
            s" change" FULL-SYNC FIELD-U COMMA
            s" save" KEY OBJECT-START
               s" includeText" false FIELD-BOOL
            OBJECT-END
         OBJECT-END
      OBJECT-END COMMA
      s" serverInfo" KEY OBJECT-START
         s" name" s" habu" FIELD-S
      OBJECT-END
   OBJECT-END
   END SENT ;

\ ---- params ------------------------------------------------------------------

\ A reader on the value of the params' member KEY, and that value's kind.
\ Params absent or null are empty.
: PARAM ( ptr u8 n ptr u8 n -- JR:reader n )
   {: p:ptr pu:n k:ptr ku:n :}
   pu 0= if E-LSP-PARAMS throw then
   JR-ST STORAGE-BYTES p pu INIT
   NEXT T-OBJ <> if E-LSP-PARAMS throw then
   k ku FIND-KEY 0= if E-LSP-PARAMS throw then
   TOKEN ;

\ A reader on the value of params.textDocument's member KEY, and its kind.
: DOC-PARAM ( ptr u8 n ptr u8 n -- JR:reader n )
   {: p:ptr pu:n k:ptr ku:n :}
   p pu s" textDocument" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   k ku FIND-KEY 0= if E-LSP-PARAMS throw then
   TOKEN ;

\ The current string decoded into the span in V, and its length. A string's
\ JSON text is never shorter than what it decodes to.
: DECODE ( JR:reader ptr SPAN:span<u8> -- JR:reader n )
   {: v :}
   SPAN$ nip {: raw:n :}
   v raw ROOM
   v @ SPAN:$ STR ;

\ A string member of params.textDocument, decoded into the span in V.
: DOC-STRING ( ptr u8 n ptr u8 n ptr SPAN:span<u8> -- ptr u8 n )
   {: p:ptr pu:n k:ptr ku:n v :}
   p pu k ku DOC-PARAM T-STR <> if E-LSP-PARAMS throw then
   v DECODE {: n:n :}
   JR:CLOSE
   v @ SPAN:$ drop n ;

: DOC-URI ( ptr u8 n -- ptr u8 n )  s" uri" URI-SPAN DOC-STRING ;

: DOC-VERSION ( ptr u8 n -- n )
   {: p:ptr pu:n :}
   p pu s" version" DOC-PARAM T-INT <> if E-LSP-PARAMS throw then
   JR:INT {: v:n :}
   JR:CLOSE
   v ;

\ The file path a URI names, which is never longer than the URI.
: PATH ( ptr u8 n -- ptr u8 n )
   {: u:ptr uu:n :}
   PATH-SPAN uu ROOM
   u uu PATH-SPAN @ URI:FILE>PATH {: n:n :}
   PATH-SPAN @ SPAN:$ drop n ;

\ One member of a content change, on its key: the text decoded into TEXT-SPAN,
\ any other member passed over. A range is incremental sync, not offered.
: CHANGE-MEMBER ( JR:reader -- JR:reader )
   s" range" STR-EQ? if E-LSP-PARAMS throw then
   s" text" STR-EQ? if
      NEXT T-STR <> if E-LSP-PARAMS throw then
      TEXT-SPAN DECODE TEXT-LEN !
   else
      NEXT drop SKIP-VALUE
   then ;

\ One element of contentChanges, an object with a text.
: CHANGE ( JR:reader -- JR:reader )
   TOKEN T-OBJ <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin NEXT T-KEY = while CHANGE-MEMBER repeat
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then ;

\ contentChanges, at least one: the last one's text, decoded into TEXT-SPAN,
\ and its length.
: CHANGES ( ptr u8 n -- n )
   s" contentChanges" PARAM T-ARR <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin NEXT T-ARR-END <> while CHANGE repeat
   JR:CLOSE
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then
   TEXT-LEN @ ;

: OPEN-SLOT ( ptr u8 n -- n )
   DOC-FIND MATCH option
      none OF E-LSP-NOT-OPEN throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ ---- notifications -----------------------------------------------------------

\ Each handler reads all of its params before it changes a document, so one
\ that throws has changed nothing.
: DID-OPEN ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI {: u:ptr uu:n :}
   u uu PATH {: a:ptr au:n :}
   p pu DOC-VERSION {: v:n :}
   p pu s" text" TEXT-SPAN DOC-STRING {: t:ptr tu:n :}
   u uu v a au t tu DOC-OPEN
   p pu ;

: DID-CHANGE ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT {: slot:n :}
   p pu DOC-VERSION {: v:n :}
   p pu CHANGES {: n:n :}
   slot v TEXT-SPAN @ SPAN:$ drop n DOC-CHANGE
   p pu ;

: DID-CLOSE ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT {: slot:n :}
   slot LSP-DIAG:RETRACT
   slot DOC-CLOSE
   DOC-DIRTY-ALL
   p pu ;

\ The throws that leave a notification unapplied: the server says why and
\ goes on. Any other stops it.
: IGNORABLE? ( n -- bool )
   {: code:n :}
   code E-LSP-PARAMS =  code E-LSP-NOT-OPEN = or  code E-JR-NUMBER = or
   code E-PATH-RANGE = or
   code E-URI-LAST >= code E-URI-FIRST <= and or ;

: GUARDED ( ptr u8 n ptr u8 n [ ptr u8 n -- ptr u8 n ] -- )
   {: m:ptr mu:n p:ptr pu:n q :}
   p pu q catch {: code:n :} 2drop
   code 0= if exit then
   code IGNORABLE? 0= if code throw then
   SB-RESET s" lsp: ignored " SB-APPEND m mu SB-APPEND
   s" : throw " SB-APPEND code FMT:SB-INT 10 SB-APPEND-C
   2 >FD SB$ FD-IO:WRITE-FULL ;

: NAMED? ( ptr u8 n ptr u8 n -- bool )
   {: m:ptr mu:n name:ptr nu:n :}
   JR-ST STORAGE-BYTES m mu name nu METHOD? ;

: NOTIFIED ( ptr u8 n ptr u8 n -- )
   {: m:ptr mu:n p:ptr pu:n :}
   m mu s" exit" NAMED? if s" " EXIT-CODE die then
   RUNNING? 0= if exit then
   m mu s" textDocument/didOpen" NAMED? if m mu p pu [: DID-OPEN ;] GUARDED exit then
   m mu s" textDocument/didChange" NAMED? if m mu p pu [: DID-CHANGE ;] GUARDED exit then
   m mu s" textDocument/didClose" NAMED? if m mu p pu [: DID-CLOSE ;] GUARDED exit then
   m mu s" textDocument/didSave" NAMED? if DOC-DIRTY-ALL then ;

\ ---- requests ----------------------------------------------------------------

: STARTING-REQUEST ( JSON-RPC:id ptr u8 n -- )
   {: i m:ptr mu:n :}
   m mu s" initialize" NAMED? if
      i REPLY-CAPABILITIES
      construct state running STATE!
   else
      i NOT-INITIALIZED s" server not initialized" REPLY-ERROR
   then ;

: RUNNING-REQUEST ( JSON-RPC:id ptr u8 n -- )
   {: i m:ptr mu:n :}
   m mu s" shutdown" NAMED? if
      i REPLY-NULL
      construct state shut-down STATE!
      exit
   then
   m mu s" initialize" NAMED? if
      i INVALID-REQUEST s" server already initialized" REPLY-ERROR
      exit
   then
   i METHOD-NOT-FOUND s" method not found" REPLY-ERROR ;

: REQUESTED ( JSON-RPC:id ptr u8 n -- )
   {: i m:ptr mu:n :}
   STATE@ MATCH state
      starting OF i m mu STARTING-REQUEST ENDOF
      running OF i m mu RUNNING-REQUEST ENDOF
      shut-down OF i INVALID-REQUEST s" server shut down" REPLY-ERROR ENDOF
   ;MATCH ;

\ ---- the loop ----------------------------------------------------------------

: DISPATCH ( JSON-RPC:message -- )
   MATCH JSON-RPC:message
      request OF 2drop REQUESTED ENDOF
      notification OF NOTIFIED ENDOF
      success OF 2drop ID$ 2drop ENDOF
      failure OF 2drop ID$ 2drop ENDOF
      invalid OF {: c:n w:ptr wu:n i :} i c w wu REPLY-ERROR ENDOF
   ;MATCH ;

: RECEIVE ( n -- )
   {: len:n :}
   BODY-SPAN len ROOM
   INPUT BODY-SPAN @ BODY
   JR-ST STORAGE-BYTES BODY-SPAN @ len TAKE SPAN:$ >MESSAGE
   DISPATCH ;

\ The input ended on a message boundary.
: ENDED ( -- )
   EXIT-CODE {: rc:n :}
   rc 0= if s" " 0 die then
   s" lsp: input ended before exit" rc die ;

\ Whether stdin holds input, or its end, a poll that a signal cut short asked
\ again.
: POLLED? ( -- bool )
   begin 0 >FD 0 >MS POLL-IN COUNT>N dup EINTR# negate = while drop repeat
   0<> ;

\ Whether a message waits: begun in the reader's buffer, or on stdin.
: INPUT? ( -- bool )
   INPUT PENDING? if true exit then
   POLLED? ;

\ The document to check now: the next one waiting after the last checked,
\ while the server runs and no message waits.
: DUE ( -- option<n> )
   RUNNING? 0= if OPTION:NONE exit then
   LAST-CHECKED @ DOC-NEXT-DIRTY MATCH option
      none OF OPTION:NONE ENDOF
      some OF INPUT? if drop OPTION:NONE else OPTION:SOME then ENDOF
   ;MATCH ;

\ The next message, or the end of input.
: TAKE-INPUT ( -- )
   INPUT NEXT-LENGTH MATCH option
      none OF ENDED ENDOF
      some OF RECEIVE ENDOF
   ;MATCH ;

: SERVE ( -- )
   1 >FD FD-NOSIGPIPE!
   2 >FD FD-NOSIGPIPE!
   REPLY-BUF 1 BUF:N>BLEN BUF:INIT       \ the smallest buffer: replies grow it
   LSP-DIAG:PREPARE
   -1 LAST-CHECKED !
   construct state starting STATE!
   INPUT 0 >FD HEAD-BUF MAX-BODY BIND
   begin
      DUE MATCH option
         some OF dup LAST-CHECKED ! LSP-CHECK:RUN ENDOF
         none OF TAKE-INPUT ENDOF
      ;MATCH
   again ;

public

: MAIN ( -- )
   [: SERVE ;] catch {: code:n :}
   SB-RESET s" lsp: stopped by throw " SB-APPEND code FMT:SB-INT
   SB$ 1 die ;

;using
;using
;using
;using
;using
;using
;using
;package
