\ lsp-core.f - the Habu language server: the Language Server Protocol on stdin
\ and stdout, for any client.
\
\ MAIN reads framed messages (lib/content-length.f) and holds each one until its
\ turn, then sorts it (lib/json-rpc.f) and acts on it, until the client's exit
\ or the end of input.
\ The server is starting, running or shut down. Starting, it answers initialize
\ with its capabilities and any other request with -32002. Running, it answers
\ shutdown with null, initialize with -32600, workspace/symbol with the open
\ documents' definitions (tools/lsp-symbols.f), textDocument/definition with
\ the declaration the use at the position binds to (tools/lsp-definition.f),
\ textDocument/hover with what the token at the position declares or binds to
\ (tools/lsp-hover.f), as Markdown when the client's initialize listed it in
\ textDocument.hover.contentFormat, else as plain text, textDocument/completion
\ with the spellings that would bind at the position (tools/lsp-completion.f),
\ a request whose params it cannot read, or that names a document not open,
\ with -32602 and any other request with -32601, and it keeps the documents the
\ client opens, changes and closes: Full sync, each change carrying the whole
\ text, and positions in UTF-16 units. Shut down, it answers every request
\ with -32600. Only a running server takes a notification other than exit;
\ the rest are dropped, as unknown ones always are.
\
\ The messages are served in the order they came, and the first one held is
\ served only once no more input waits, so the messages that came behind a
\ request are read before its work starts. INPUT? says only that one has begun
\ to come: reading it waits for the rest. A running server answers a request
\ -32800, doing none of its work, when a $/cancelRequest naming its id was read
\ behind it, an id that is a string by its decoded text and a number by its
\ JSON text, and it answers textDocument/definition, hover or completion -32801
\ when a textDocument/didChange of its document was, since the request asked
\ about the text that change replaces. A cancel is otherwise an unknown
\ notification, so one naming an id answered, unknown or not a request's
\ changes nothing, and a check already running is not stopped. The end of
\ input and a fault in its framing come after the messages held: those are
\ served first.
\
\ A running server checks the documents and publishes their diagnostics
\ (tools/lsp-check.f, tools/lsp-diag.f). Opening or changing a document leaves
\ it waiting for a check, and a save leaves every open document waiting, since
\ any of them may require the file saved. Input comes first: only when no
\ message waits, in the reader's buffer or on stdin, or is held is one waiting
\ document checked, the next after the last one checked, and then input is
\ looked at again. So such a check takes a document's newest text, the edits that came
\ while another check ran are all applied before it, and the versions
\ published for a document only rise. A request about a document waiting for
\ a check, textDocument/definition or textDocument/hover, is the exception: it
\ checks that document
\ first, as its turn, and is answered from that check, which takes the text
\ the client asked about, since LSP orders a request after the changes sent
\ before it. textDocument/completion checks its document as its turn whether
\ it waits or not, with a cursor at the position, and is answered from that
\ check: the spellings it offers belong to that cursor. Closing a document
\ publishes an empty list for it, drops the definitions its last check kept
\ and leaves every open document waiting: any of them may require the file,
\ whose diagnostics their checks left to it while it was open.
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
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-diag.f
require tools/lsp-check.f
require tools/lsp-symbols.f
require tools/lsp-definition.f
require tools/lsp-hover.f
require tools/lsp-completion.f

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
-9401 constant E-LSP-NOT-OPEN  \ a change, close, definition, hover or completion of a document not open

-32002 constant NOT-INITIALIZED          \ LSP's ServerNotInitialized
-32800 constant REQUEST-CANCELLED        \ LSP's RequestCancelled
-32801 constant CONTENT-MODIFIED         \ LSP's ContentModified
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
TYPED-VARIABLE QUERY-SPAN SPAN:span<u8>   \ workspace/symbol's query, decoded,
variable QUERY-U                          \ and its length
variable AT-SLOT                          \ a position's document,
variable AT-LINE                          \ its line
variable AT-CHAR                          \ and character,
variable AT-BYTE                          \ and the byte they name in its text
TYPED-VARIABLE HOVER-MD bool              \ whether the client takes hovers as Markdown
TYPED-VARIABLE W JSON-WRITE:writer
variable TEXT-LEN                         \ the last change's text length, -1 before one is read
variable LAST-CHECKED                     \ the slot checked last, -1 before any
create HELD-B BUF:HDR-BYTES allot         \ the bodies of the messages held, back to back,
DYNAMIC-BUFFER HELD-ENDS n                \ where each one ends there,
variable HELD-COUNT                       \ how many are held,
variable HELD-NEXT                        \ and the first of them not yet served
TYPED-VARIABLE ID-SPAN SPAN:span<u8>      \ a request's string id, decoded
TYPED-VARIABLE AT-END bool                \ whether the input has ended

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
         OBJECT-END COMMA
         s" completionProvider" KEY OBJECT-START OBJECT-END COMMA
         s" definitionProvider" true FIELD-BOOL COMMA
         s" hoverProvider" true FIELD-BOOL COMMA
         s" workspaceSymbolProvider" true FIELD-BOOL
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

\ The value the reader is on, of this kind, a string decoded into the span in
\ V.
: STRING-AT ( JR:reader n ptr SPAN:span<u8> -- ptr u8 n )
   {: v :}
   T-STR <> if E-LSP-PARAMS throw then
   v DECODE {: n:n :}
   JR:CLOSE
   v @ SPAN:$ drop n ;

\ A string member of params.textDocument, decoded into the span in V.
: DOC-STRING ( ptr u8 n ptr u8 n ptr SPAN:span<u8> -- ptr u8 n )
   {: p:ptr pu:n k:ptr ku:n v :}
   p pu k ku DOC-PARAM v STRING-AT ;

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

\ Whether the member KEY of the object the reader is at is an object, the
\ reader then at it.
: INTO? ( JR:reader ptr u8 n -- JR:reader bool )
   FIND-KEY 0= if false exit then
   TOKEN T-OBJ = ;

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
   slot LSP-DEFS:DEFS-DROP
   slot DOC-CLOSE
   DOC-DIRTY-ALL
   p pu ;

\ The throws that come of params the server cannot apply: a notification is
\ left unapplied, the server saying why, and a request is answered -32602.
\ Any other stops the server.
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

\ ---- held messages -----------------------------------------------------------

: HELD? ( -- bool )  HELD-NEXT @ HELD-COUNT @ < ;

\ The body of held message K.
: HELD$ ( n -- ptr u8 n )
   {: k:n :}
   k 0= if 0 else k 1- HELD-ENDS @ then {: at:n :}
   HELD-B BUF:SPAN$ drop at +  k HELD-ENDS @ at - ;

\ Holds a message's body, its turn after every message held before it.
: HOLD ( ptr u8 n -- )
   BUF:N>BLEN HELD-B BUF:APPEND-SPAN
   HELD-COUNT @ 1+ HELD-ENDS-RESERVE
   HELD-B BUF:LEN@ BUF:BLEN>N HELD-COUNT @ HELD-ENDS !
   1 HELD-COUNT +! ;

\ Held message K's params, and whether it is a notification of this method.
: NOTICE? ( n ptr u8 n -- ptr u8 n bool )
   {: k:n name:ptr nu:n :}
   JR-ST STORAGE-BYTES k HELD$ >MESSAGE MATCH JSON-RPC:message
      request OF 2drop 2drop ID$ 2drop s" " false ENDOF
      notification OF {: m:ptr mu:n p:ptr pu:n :} p pu m mu name nu NAMED? ENDOF
      success OF 2drop ID$ 2drop s" " false ENDOF
      failure OF 2drop ID$ 2drop s" " false ENDOF
      invalid OF ID$ 2drop 2drop drop s" " false ENDOF
   ;MATCH ;

\ What a cancel's id is matched against for the request with this id: a
\ string's decoded text, in ID-SPAN, and true; else its JSON text and false.
: ID-KEY ( JSON-RPC:id -- ptr u8 n bool )
   ID$ {: r:ptr ru:n :}
   JR-ST STORAGE-BYTES r ru INIT
   NEXT T-STR = if ID-SPAN DECODE true else 0 false then {: n:n str:bool :}
   JR:CLOSE
   str if ID-SPAN @ SPAN:$ drop n true exit then
   r ru false ;

\ Whether params.id is the id this key gives: a string with this decoded text
\ when STR is true, else a value with this JSON text.
: ID-PARAM? ( ptr u8 n ptr u8 n bool -- bool )
   {: p:ptr pu:n k:ptr ku:n str:bool :}
   pu 0= if false exit then
   JR-ST STORAGE-BYTES p pu INIT
   NEXT T-OBJ = if s" id" FIND-KEY else false then
   if
      TOKEN T-STR = if
         str if k ku STR-EQ? else false then
      else
         str 0= if SPAN$ k ku STR= else false then
      then
   else false then
   {: same:bool :}
   JR:CLOSE
   same ;

\ Whether a cancel of the request with this id was read behind the message
\ being served.
: CANCELLED? ( JSON-RPC:id -- bool )
   ID-KEY {: k:ptr ku:n str:bool :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" $/cancelRequest" NOTICE? if
         k ku str ID-PARAM? if true unloop exit then
      else 2drop then
   loop
   false ;

\ The slot of the open document params.textDocument.uri names, -1 when the
\ params name none.
: URI-SLOT ( ptr u8 n -- n )
   {: p:ptr pu:n :}
   pu 0= if -1 exit then
   JR-ST STORAGE-BYTES p pu INIT
   NEXT T-OBJ =
   if s" textDocument" INTO? else false then
   if s" uri" FIND-KEY else false then
   if TOKEN T-STR = else false then
   {: named:bool :}
   named if URI-SPAN DECODE else 0 then {: n:n :}
   JR:CLOSE
   named 0= if -1 exit then
   URI-SPAN @ SPAN:$ drop n DOC-FIND MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ Whether a change to the open document in this slot was read behind the
\ message being served.
: CHANGED? ( n -- bool )
   {: slot:n :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" textDocument/didChange" NOTICE? if
         URI-SLOT slot = if true unloop exit then
      else 2drop then
   loop
   false ;

\ ---- requests ----------------------------------------------------------------

\ The request's params read by Q. A throw IGNORABLE? names answers the request
\ -32602, and false.
: READ? ( JSON-RPC:id ptr u8 n [ ptr u8 n -- ptr u8 n ] -- bool )
   {: i p:ptr pu:n q :}
   p pu q catch {: code:n :} 2drop
   code 0= if true exit then
   code IGNORABLE? 0= if code throw then
   i INVALID-PARAMS s" invalid params" REPLY-ERROR
   false ;

\ params.query decoded into QUERY-SPAN, its length in QUERY-U.
: QUERY! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu s" query" PARAM QUERY-SPAN STRING-AT QUERY-U ! drop
   p pu ;

: QUERY$ ( -- ptr u8 n )  QUERY-SPAN @ SPAN:$ drop QUERY-U @ ;

\ The definitions of the open documents' last completed checks whose word
\ holds params.query.
: SYMBOLS ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu [: QUERY! ;] READ? 0= if exit then
   WRITER i RESULT QUERY$ LSP-SYMBOLS:ANSWER END SENT ;

\ A member of params.position, a nonnegative integer.
: POSITION-INT ( ptr u8 n ptr u8 n -- n )
   {: p:ptr pu:n k:ptr ku:n :}
   p pu s" position" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   k ku FIND-KEY 0= if E-LSP-PARAMS throw then
   TOKEN T-INT <> if E-LSP-PARAMS throw then
   JR:INT {: v:n :}
   JR:CLOSE
   v 0 < if E-LSP-PARAMS throw then
   v ;

\ The slot of the open document params.textDocument names in AT-SLOT, and
\ params.position in AT-LINE and AT-CHAR.
: POSITION! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT AT-SLOT !
   p pu s" line" POSITION-INT AT-LINE !
   p pu s" character" POSITION-INT AT-CHAR !
   p pu ;

\ Checks the document in this slot now, as its turn: the next check due is of
\ a waiting document after it.
: CHECK-NOW ( n -- )  dup LAST-CHECKED ! LSP-CHECK:RUN ;

\ Checks the document in this slot first if it waits for a check, so that a
\ request about it is answered from a check of its current text.
: CHECK-WAITING ( n -- )
   dup DOC-DIRTY? if CHECK-NOW else drop then ;

\ The byte of the text of the document in AT-SLOT that AT-LINE and AT-CHAR
\ name, in AT-BYTE, positions counting in that text.
: AT-BYTE! ( -- )
   AT-SLOT @ DOC-TEXT$ LSP-TEXT:TEXT!
   AT-LINE @ AT-CHAR @ LSP-TEXT:OFFSET-AT AT-BYTE ! ;

\ The open document params.textDocument names in AT-SLOT, and params.position
\ in AT-LINE and AT-CHAR; false when the request is answered instead: -32602
\ when its params cannot be read, -32801 when a change to that document was
\ read behind it, since it asked about the text the change replaces.
: POSITION? ( JSON-RPC:id ptr u8 n -- bool )
   {: i p:ptr pu:n :}
   i p pu [: POSITION! ;] READ? 0= if false exit then
   AT-SLOT @ CHANGED? 0= if true exit then
   i CONTENT-MODIFIED s" content modified" REPLY-ERROR
   false ;

\ The open document params.textDocument names in AT-SLOT, checked first if it
\ waits for a check, and the byte of its text params.position names in
\ AT-BYTE; false when the request is answered instead.
: AT-READ? ( JSON-RPC:id ptr u8 n -- bool )
   POSITION? 0= if false exit then
   AT-SLOT @ CHECK-WAITING
   AT-BYTE!
   true ;

\ The declaration of the use at params.position in the open document
\ params.textDocument names, as its last completed check bound it.
: DEFINITION ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-DEFINITION:ANSWER END SENT ;

\ What the token at params.position in the open document params.textDocument
\ names declares or binds to, as its last completed check stated it.
: HOVER ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ HOVER-MD @ LSP-HOVER:ANSWER END SENT ;

\ The spellings that would bind at params.position in the open document
\ params.textDocument names: the document checked now, as its turn, with its
\ cursor at that byte of its text, and answered from that check.
: COMPLETION ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu POSITION? 0= if exit then
   AT-BYTE!
   AT-SLOT @ LAST-CHECKED !
   AT-SLOT @ AT-BYTE @ LSP-CHECK:RUN-AT
   WRITER i RESULT AT-SLOT @ LSP-COMPLETION:ANSWER END SENT ;

\ Whether the array the reader is at lists the string markdown.
: LISTS-MARKDOWN? ( JR:reader -- JR:reader bool )
   begin NEXT dup T-ARR-END <> while
      T-STR = if
         s" markdown" STR-EQ? if true exit then
      else
         SKIP-VALUE
      then
   repeat
   drop false ;

\ Whether initialize's params.capabilities.textDocument.hover.contentFormat
\ lists markdown.
: MARKDOWN? ( ptr u8 n -- bool )
   {: p:ptr pu:n :}
   pu 0= if false exit then
   JR-ST STORAGE-BYTES p pu INIT
   NEXT T-OBJ =
   if s" capabilities" INTO? else false then
   if s" textDocument" INTO? else false then
   if s" hover" INTO? else false then
   if s" contentFormat" FIND-KEY else false then
   if TOKEN T-ARR = else false then
   if LISTS-MARKDOWN? else false then
   {: md:bool :}
   JR:CLOSE
   md ;

: STARTING-REQUEST ( JSON-RPC:id ptr u8 n ptr u8 n -- )
   {: i m:ptr mu:n p:ptr pu:n :}
   m mu s" initialize" NAMED? if
      p pu MARKDOWN? HOVER-MD !
      i REPLY-CAPABILITIES
      construct state running STATE!
   else
      i NOT-INITIALIZED s" server not initialized" REPLY-ERROR
   then ;

: RUNNING-REQUEST ( JSON-RPC:id ptr u8 n ptr u8 n -- )
   {: i m:ptr mu:n p:ptr pu:n :}
   i CANCELLED? if
      i REQUEST-CANCELLED s" request cancelled" REPLY-ERROR
      exit
   then
   m mu s" shutdown" NAMED? if
      i REPLY-NULL
      construct state shut-down STATE!
      exit
   then
   m mu s" initialize" NAMED? if
      i INVALID-REQUEST s" server already initialized" REPLY-ERROR
      exit
   then
   m mu s" workspace/symbol" NAMED? if i p pu SYMBOLS exit then
   m mu s" textDocument/definition" NAMED? if i p pu DEFINITION exit then
   m mu s" textDocument/hover" NAMED? if i p pu HOVER exit then
   m mu s" textDocument/completion" NAMED? if i p pu COMPLETION exit then
   i METHOD-NOT-FOUND s" method not found" REPLY-ERROR ;

: REQUESTED ( JSON-RPC:id ptr u8 n ptr u8 n -- )
   {: i m:ptr mu:n p:ptr pu:n :}
   STATE@ MATCH state
      starting OF i m mu p pu STARTING-REQUEST ENDOF
      running OF i m mu p pu RUNNING-REQUEST ENDOF
      shut-down OF i INVALID-REQUEST s" server shut down" REPLY-ERROR ENDOF
   ;MATCH ;

\ ---- the loop ----------------------------------------------------------------

: DISPATCH ( JSON-RPC:message -- )
   MATCH JSON-RPC:message
      request OF REQUESTED ENDOF
      notification OF NOTIFIED ENDOF
      success OF 2drop ID$ 2drop ENDOF
      failure OF 2drop ID$ 2drop ENDOF
      invalid OF {: c:n w:ptr wu:n i :} i c w wu REPLY-ERROR ENDOF
   ;MATCH ;

\ The body of the message the reader is at, read and held.
: RECEIVE ( n -- )
   {: len:n :}
   BODY-SPAN len ROOM
   INPUT BODY-SPAN @ BODY
   BODY-SPAN @ len TAKE SPAN:$ HOLD ;

\ The first held message served. It is passed before it is dispatched, so the
\ messages held behind it are the ones from HELD-NEXT on; once none is left,
\ their room is reused.
: TURN ( -- )
   HELD-NEXT @ {: k:n :}
   1 HELD-NEXT +!
   JR-ST STORAGE-BYTES k HELD$ >MESSAGE DISPATCH
   HELD? if exit then
   0 HELD-NEXT !
   0 HELD-COUNT !
   HELD-B BUF:CLEAR ;

\ Every held message served, in turn.
: TURNS ( -- )  begin HELD? while TURN repeat ;

\ The input ended on a message boundary: the messages held are served, and
\ then the server ends.
: ENDED ( -- )
   TURNS
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

\ The next message read and held, or AT-END set at the end of input.
: READ-ON ( -- )
   INPUT NEXT-LENGTH MATCH option
      none OF true AT-END ! ENDOF
      some OF RECEIVE ENDOF
   ;MATCH ;

\ The next message, held, or the end of input, which ends the server. A fault
\ in the framing stops the server, but only once the messages held before it
\ are served, as they are before the end.
: TAKE-INPUT ( -- )
   [: READ-ON ;] catch {: code:n :}
   code 0<> if TURNS code throw then
   AT-END @ if ENDED then ;

: SERVE ( -- )
   1 >FD FD-NOSIGPIPE!
   2 >FD FD-NOSIGPIPE!
   REPLY-BUF 1 BUF:N>BLEN BUF:INIT       \ the smallest buffers: replies grow one,
   HELD-B 1 BUF:N>BLEN BUF:INIT          \ held bodies the other
   0 HELD-COUNT !
   0 HELD-NEXT !
   false AT-END !
   LSP-DIAG:PREPARE
   LSP-DEFS:DEFS-PREPARE
   LSP-HOVER:PREPARE
   LSP-COMPLETION:PREPARE
   false HOVER-MD !
   -1 LAST-CHECKED !
   construct state starting STATE!
   INPUT 0 >FD HEAD-BUF MAX-BODY BIND
   begin
      HELD? if
         INPUT? if TAKE-INPUT else TURN then
      else
         DUE MATCH option
            some OF CHECK-NOW ENDOF
            none OF TAKE-INPUT ENDOF
         ;MATCH
      then
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
