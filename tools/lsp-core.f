\ lsp-core.f - the Habu language server: the Language Server Protocol on stdin
\ and stdout, for any client. MAIN reads framed messages
\ (src/lib/content-length.f), holds each until its turn, sorts it
\ (src/lib/json-rpc.f) and acts on it, until the client's exit or the end of
\ input. Full sync, positions in UTF-16 units. PROCESS-GLOBAL: the reader, the
\ buffers and the documents belong to the server's one task.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/adt/option.f
require src/lib/uri.f
require src/lib/content-length.f
require src/lib/json-read.f
require src/lib/json-write.f
require src/lib/json-rpc.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-diag.f
require tools/lsp-check.f
require tools/lsp-symbols.f
require tools/lsp-outline.f
require tools/lsp-definition.f
require tools/lsp-hover.f
require tools/lsp-highlight.f
require tools/lsp-tokens.f
require tools/lsp-completion.f
require tools/lsp-links.f
require tools/lsp-workspace.f
require tools/lsp-references.f
require tools/lsp-rename.f

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
-9401 constant E-LSP-NOT-OPEN  \ a change, close, definition, references, prepareRename, rename, hover, highlight, semantic tokens, completion, document symbols or links of a document not open

-32002 constant NOT-INITIALIZED          \ LSP's ServerNotInitialized
-32800 constant REQUEST-CANCELLED        \ LSP's RequestCancelled
-32801 constant CONTENT-MODIFIED         \ LSP's ContentModified
-32803 constant REQUEST-FAILED           \ LSP's RequestFailed
\ The longest body taken, room for a document's whole text escaped. A header
\ that announces more is refused before any of the body is read.
64 1024 * 1024 * constant MAX-BODY

private

1 constant FULL-SYNC                     \ TextDocumentSyncKind.Full

ENUM state starting running shut-down ;ENUM
1 LAYOUT-BUFFER STATE-BUF state

TYPED-VARIABLE JR-ST JR:cursor            \ JR storage for every read of a body
LINE-CAP 2 + SPAN-BUFFER: HEAD-BUF        \ the reader's buffer: a header line and its CR LF
create REPLY-BUF BUF:HDR-BYTES allot      \ a reply's bytes, grown as it is written
TYPED-VARIABLE INPUT CONTENT-LENGTH:reader
TYPED-VARIABLE BODY-SPAN SPAN:span<u8>    \ the message body
TYPED-VARIABLE URI-SPAN SPAN:span<u8>     \ textDocument.uri, decoded
TYPED-VARIABLE PATH-SPAN SPAN:span<u8>    \ the file path it names
TYPED-VARIABLE TEXT-SPAN SPAN:span<u8>    \ a document's text, decoded
TYPED-VARIABLE QUERY-SPAN SPAN:span<u8>   \ workspace/symbol's query, decoded,
variable QUERY-U                          \ and its length
TYPED-VARIABLE NAME-SPAN SPAN:span<u8>    \ a rename's newName, decoded,
variable NAME-U                           \ and its length
variable AT-SLOT                         \ a request's document,
variable AT-LINE                          \ its line
variable AT-CHAR                          \ and character,
variable AT-BYTE                          \ and the byte they name in its text
TYPED-VARIABLE HOVER-MD bool              \ whether the client takes hovers as Markdown
TYPED-VARIABLE WITH-DECL bool             \ whether a references request includes the declaration
TYPED-VARIABLE W JSON-WRITE:writer
variable TEXT-LEN                         \ the last change's text length, -1 before one is read
variable LAST-CHECKED                     \ the slot checked last, -1 before any
create HELD-B BUF:HDR-BYTES allot         \ the bodies of the messages held, back to back,
DYNAMIC-BUFFER HELD-ENDS n                \ where each one ends there,
variable HELD-COUNT                       \ how many are held,
variable HELD-NEXT                        \ and the first of them not yet served
TYPED-VARIABLE ID-SPAN SPAN:span<u8>      \ a request's string id, decoded
TYPED-VARIABLE KEPT-ID SPAN:span<u8>      \ a request's id, copied out of the held bodies
TYPED-VARIABLE AT-END bool                \ whether the input has ended,
variable FAULT                            \ and the throw a read of it took, 0 while none has

: STATE! ( state -- )  0 STATE-BUF ! ;
: STATE@ ( -- state )  0 STATE-BUF @ ;

: RUNNING? ( -- bool )
   STATE@ MATCH state
      starting OF false ENDOF
      running OF true ENDOF
      shut-down OF false ENDOF
   ;MATCH ;

\ The exit code, for exit and the end of input alike: 0 once the client has
\ asked for shutdown, else 1; the end of input before shutdown also says so on
\ stderr. Any other throw, a framing fault or a closed stdout among them, stops
\ the server with one stderr line naming it and exit code 1.
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

: WRITER ( -- ptr JSON-WRITE:writer )
   W REPLY-BUF JSON-WRITE:OPEN-BUF ;

: SENT ( ptr JSON-WRITE:writer -- )
   {: w :}
   w JSON-WRITE:$ {: a:ptr u:n :}
   w JSON-WRITE:CLOSE
   FD:STDOUT a u SEND ;

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
         s" declarationProvider" true FIELD-BOOL COMMA
         s" referencesProvider" true FIELD-BOOL COMMA
         s" renameProvider" KEY OBJECT-START
            s" prepareProvider" true FIELD-BOOL
         OBJECT-END COMMA
         s" hoverProvider" true FIELD-BOOL COMMA
         s" documentHighlightProvider" true FIELD-BOOL COMMA
         s" documentSymbolProvider" true FIELD-BOOL COMMA
         s" documentLinkProvider" KEY OBJECT-START OBJECT-END COMMA
         LSP-TOKENS:LEGEND COMMA
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
   JR-ST p pu INIT
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

\ An editor event changes checks that consumed this path. A watched disk
\ event also changes checks stopped at this path before they could read it.
\ A document holding the path itself still supplies its editor text.
: DIRTY-DEPENDENTS ( ptr u8 n bool -- )
   {: path:ptr pathu:n include-unread:bool :}
   DOC-SLOTS 0 ?do
      i DOC-LIVE? if
         i DOC-CANON$ path pathu STR= 0= if
            include-unread if
               i path pathu LSP-DEFS:DEFS-DEPENDS?
            else
               i path pathu LSP-DEFS:DEFS-READ?
            then if i DOC-DIRTY then
         then
      then
   loop ;

: READERS-DIRTY-PATH ( ptr u8 n -- )  false DIRTY-DEPENDENTS ;

: READERS-DIRTY ( n -- )  DOC-CANON$ READERS-DIRTY-PATH ;

\ Each handler reads all of its params before it changes a document, so one
\ that throws has changed nothing.
: DID-OPEN ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI {: u:ptr uu:n :}
   u uu PATH {: a:ptr au:n :}
   p pu DOC-VERSION {: v:n :}
   p pu s" text" TEXT-SPAN DOC-STRING {: t:ptr tu:n :}
   u uu v a au t tu DOC-OPEN
   u uu OPEN-SLOT READERS-DIRTY
   p pu ;

: DID-CHANGE ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT {: slot:n :}
   p pu DOC-VERSION {: v:n :}
   p pu CHANGES {: n:n :}
   slot v TEXT-SPAN @ SPAN:$ drop n DOC-CHANGE
   slot READERS-DIRTY
   p pu ;

: DID-CLOSE ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT {: slot:n :}
   slot LSP-DIAG:RETRACT
   slot READERS-DIRTY
   slot LSP-DEFS:DEFS-DROP
   slot DOC-CLOSE
   p pu ;

\ A watched disk path affects open checks that read it or failed to read it.
\ An editor buffer at that path remains the check's source while it stays open.
: WATCH-CHANGE ( JR:reader -- JR:reader )
   JR:TOKEN JR:T-OBJ <> if E-LSP-PARAMS throw then
   begin JR:NEXT JR:T-KEY = while
      s" uri" JR:STR-EQ? if
         JR:NEXT JR:T-STR <> if E-LSP-PARAMS throw then
         URI-SPAN DECODE {: n:n :}
         URI-SPAN @ SPAN:$ drop n PATH SOURCE-ROOT:CANONICAL drop
         true DIRTY-DEPENDENTS
      else JR:NEXT drop JR:SKIP-VALUE then
   repeat ;

: DID-WATCH ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu s" changes" PARAM JR:T-ARR <> if E-LSP-PARAMS throw then
   begin JR:NEXT JR:T-ARR-END <> while WATCH-CHANGE repeat
   JR:CLOSE
   p pu ;

\ The throws that come of params the server cannot apply: a notification is
\ left unapplied, the server saying why, and a request is answered -32602.
\ Any other stops the server.
: IGNORABLE? ( n -- bool )
   {: code:n :}
   code E-LSP-PARAMS =  code E-LSP-NOT-OPEN = or  code E-JR-NUMBER = or
   code E-PATH-RANGE = or  code E-PATH-NUL = or
   code E-URI-LAST >= code E-URI-FIRST <= and or ;

: GUARDED ( ptr u8 n ptr u8 n [ ptr u8 n -- ptr u8 n ] -- )
   {: m:ptr mu:n p:ptr pu:n q :}
   p pu q catch {: code:n :} 2drop
   code 0= if exit then
   code IGNORABLE? 0= if code throw then
   SB-RESET s" lsp: ignored " SB-APPEND m mu SB-APPEND
   s" : throw " SB-APPEND code FMT:SB-INT 10 SB-APPEND-C
   FD:STDERR SB$ FD:WRITE-FULL ;

: NAMED? ( ptr u8 n ptr u8 n -- bool )
   {: m:ptr mu:n name:ptr nu:n :}
   JR-ST m mu name nu METHOD? ;

\ Only a running server takes a notification other than exit; the rest are
\ dropped, as unknown ones always are. One the server cannot apply (params it
\ cannot read, a URI that names no file here, a document not open) changes
\ nothing, and one stderr line says why, since a notification has no reply. A
\ response from the client is dropped: the server asks nothing.
: NOTIFIED ( ptr u8 n ptr u8 n -- )
   {: m:ptr mu:n p:ptr pu:n :}
   m mu s" exit" NAMED? if s" " EXIT-CODE die then
   RUNNING? 0= if exit then
   m mu s" textDocument/didOpen" NAMED? if m mu p pu [: DID-OPEN ;] GUARDED exit then
   m mu s" textDocument/didChange" NAMED? if m mu p pu [: DID-CHANGE ;] GUARDED exit then
   m mu s" textDocument/didClose" NAMED? if m mu p pu [: DID-CLOSE ;] GUARDED exit then
   m mu s" textDocument/didSave" NAMED? if DOC-DIRTY-ALL exit then
   m mu s" workspace/didChangeWatchedFiles" NAMED? if m mu p pu [: DID-WATCH ;] GUARDED then ;

\ ---- held messages -----------------------------------------------------------

\ Messages are served in the order they came, and the first one held is served
\ only once no more input waits, so the messages behind a request are read
\ before its work starts. A running server answers a request -32800, doing none
\ of its work, when a $/cancelRequest naming its id was read behind it (a string
\ id by its decoded text, a number by its JSON text), and a request about a
\ document's text -32801 when a didChange of that document was. References and
\ rename read the waiting input again before each file check and before the
\ answer, serving none: -32800 for a cancel of them, else -32801 for a change of
\ any open document, since each check reads them all. The check under way
\ finishes and a message begun is read whole, so how soon such a request stops
\ has no bound. A cancel is otherwise an unknown notification and stops no
\ running check. The end of input and a framing fault come after the messages
\ held, even when a request read them: it is answered and those served first.
: HELD? ( -- bool )  HELD-NEXT @ HELD-COUNT @ < ;

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
   JR-ST k HELD$ >MESSAGE MATCH JSON-RPC:message
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
   JR-ST r ru INIT
   NEXT T-STR = if ID-SPAN DECODE true else 0 false then {: n:n str:bool :}
   JR:CLOSE
   str if ID-SPAN @ SPAN:$ drop n true exit then
   r ru false ;

\ Whether params.id is the id this key gives: a string with this decoded text
\ when STR is true, else a value with this JSON text.
: ID-PARAM? ( ptr u8 n ptr u8 n bool -- bool )
   {: p:ptr pu:n k:ptr ku:n str:bool :}
   pu 0= if false exit then
   JR-ST p pu INIT
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
   JR-ST p pu INIT
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

: EDIT-NOTICE? ( n ptr u8 n [ ptr u8 n -- bool ] -- bool )
   {: k:n name:ptr nu:n affected :}
   k name nu NOTICE? if
      2dup [: DOC-URI PATH SOURCE-ROOT:CANONICAL drop ;] catch {: code:n :}
      code 0<> if
         2drop 2drop
         code IGNORABLE? if false exit then
         code throw
      then
      affected execute -rot 2drop
   else 2drop false then ;

\ A queued editor notification whose path the request actually uses.
: HELD-EDITED? ( [ ptr u8 n -- bool ] -- bool )
   {: affected :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" textDocument/didOpen" affected EDIT-NOTICE? if true unloop exit then
      i s" textDocument/didChange" affected EDIT-NOTICE? if true unloop exit then
      i s" textDocument/didClose" affected EDIT-NOTICE? if true unloop exit then
      i s" textDocument/didSave" affected EDIT-NOTICE? if true unloop exit then
   loop
   false ;

: REQUEST-DEPENDS? ( ptr u8 n -- bool )
   {: path:ptr pathu:n :}
   AT-SLOT @ DOC-CANON$ path pathu STR= if true exit then
   AT-SLOT @ path pathu LSP-DEFS:DEFS-DEPENDS? ;

\ ---- input -------------------------------------------------------------------

: SETTLED? ( -- bool )  FAULT @ 0<> AT-END @ or ;

\ The body of the message the reader is at, read and held.
: RECEIVE ( n -- )
   {: len:n :}
   BODY-SPAN len ROOM
   INPUT BODY-SPAN @ BODY
   BODY-SPAN @ len TAKE SPAN:$ HOLD ;

\ Whether stdin holds input, or its end, a poll that a signal cut short asked
\ again.
: POLLED? ( -- bool )
   begin FD:STDIN 0 >MS POLL-IN COUNT>N dup EINTR# negate = while drop repeat
   0<> ;

\ Whether a message waits: begun in the reader's buffer, or on stdin; or
\ reading is over, which the loop then acts on.
: INPUT? ( -- bool )
   SETTLED? if true exit then
   INPUT PENDING? if true exit then
   POLLED? ;

\ The next message read and held, or AT-END set at the end of input.
: READ-NEXT ( -- )
   INPUT NEXT-LENGTH MATCH option
      none OF true AT-END ! ENDOF
      some OF RECEIVE ENDOF
   ;MATCH ;

\ The next message read and held, unless reading is over. A throw the read
\ takes, a fault in the framing among them, is kept in FAULT, and reading is
\ then over.
: READ-ON ( -- )
   SETTLED? if exit then
   [: READ-NEXT ;] catch FAULT ! ;

\ Every message that waits read and held, none of them served, until none
\ waits or reading is over. A message begun is read whole, which waits for
\ the rest of it.
: READ-WAITING ( -- )
   begin INPUT? SETTLED? 0= and while READ-ON repeat ;

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

\ The definitions of the open documents' last completed checks that
\ params.query lists, as tools/lsp-symbols.f states.
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

\ The slot of the open document params.textDocument names in AT-SLOT.
: SLOT! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-URI OPEN-SLOT AT-SLOT !
   p pu ;

\ The slot of the open document params.textDocument names in AT-SLOT, and
\ params.position in AT-LINE and AT-CHAR.
: POSITION! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu SLOT! s" line" POSITION-INT AT-LINE !
   p pu s" character" POSITION-INT AT-CHAR !
   p pu ;

\ Checks the document in this slot now, as its turn: the next check due is of
\ a waiting document after it.
: CHECK-NOW ( n -- )  dup LAST-CHECKED ! LSP-CHECK:RUN ;

\ Checks the document in this slot first if it waits for a check, so a request
\ about it is answered from the text the client asked about: LSP orders a
\ request after the changes sent before it.
: CHECK-WAITING ( n -- )
   dup DOC-DIRTY? if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-INPUTS-FRESH? 0= if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-UNREAD-RETRY? if CHECK-NOW else drop then ;

\ The byte of the text of the document in AT-SLOT that AT-LINE and AT-CHAR
\ name, in AT-BYTE, positions counting in that text.
: AT-BYTE! ( -- )
   AT-SLOT @ DOC-TEXT$ LSP-TEXT:TEXT!
   AT-LINE @ AT-CHAR @ LSP-TEXT:OFFSET-AT AT-BYTE ! ;

\ Whether no change to the document in AT-SLOT was read behind the request;
\ else it is answered -32801, since it asked about the text the change
\ replaces, and false.
: UNCHANGED? ( JSON-RPC:id -- bool )
   {: i :}
   AT-SLOT @ CHANGED? 0= [: REQUEST-DEPENDS? ;] HELD-EDITED? 0= and if true exit then
   i CONTENT-MODIFIED s" content modified" REPLY-ERROR
   false ;

\ Only a check that still owns positions can have stale inputs for a request.
: CURRENT-INPUTS-FRESH? ( n -- bool )
   dup LSP-DEFS:DEFS-CURRENT? 0= if drop true exit then
   LSP-DEFS:DEFS-INPUTS-FRESH? ;

\ A request rechecks a stale root once. A completed check's positions require
\ fresh inputs; an unfinished check kept no current positions to claim.
: CHECK-CURRENT? ( JSON-RPC:id -- bool )
   {: i :}
   AT-SLOT @ CHECK-WAITING
   READ-WAITING
   i UNCHANGED? 0= if false exit then
   AT-SLOT @ CURRENT-INPUTS-FRESH? if true exit then
   i CONTENT-MODIFIED s" content modified" REPLY-ERROR
   false ;

\ The open document params.textDocument names in AT-SLOT; false when the
\ request is answered instead: -32602 when its params cannot be read, -32801
\ when a change to that document was read behind it.
: DOCUMENT? ( JSON-RPC:id ptr u8 n -- bool )
   {: i p:ptr pu:n :}
   i p pu [: SLOT! ;] READ? 0= if false exit then
   i UNCHANGED? ;

\ As DOCUMENT?, with params.position in AT-LINE and AT-CHAR.
: POSITION? ( JSON-RPC:id ptr u8 n -- bool )
   {: i p:ptr pu:n :}
   i p pu [: POSITION! ;] READ? 0= if false exit then
   i UNCHANGED? ;

\ The open document params.textDocument names in AT-SLOT, checked first if it
\ waits for a check, and the byte of its text params.position names in
\ AT-BYTE; false when the request is answered instead.
: AT-READ? ( JSON-RPC:id ptr u8 n -- bool )
   {: i p:ptr pu:n :}
   i p pu POSITION? 0= if false exit then
   i CHECK-CURRENT? 0= if false exit then
   AT-BYTE!
   true ;

\ The declaration of the use at params.position in the open document
\ params.textDocument names, as its last completed check bound it.
: DEFINITION ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-DEFINITION:ANSWER END SENT ;

\ The symbols of the open document params.textDocument names, a flat
\ SymbolInformation list named by word alone (tools/lsp-outline.f): the
\ definitions its last completed check retained, the document checked first if
\ it waits for a check.
: DOCUMENT-SYMBOLS ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-OUTLINE:ANSWER END SENT ;

\ What the token at params.position in the open document params.textDocument
\ names declares or binds to, as its last completed check stated it: Markdown
\ when initialize listed it in textDocument.hover.contentFormat, else plain
\ text.
: HOVER ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ HOVER-MD @ LSP-HOVER:ANSWER END SENT ;

\ The token that declares the word at params.position in the open document
\ params.textDocument names and that word's uses in it, as its last completed
\ check stated them once that check was verified.
: HIGHLIGHT ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-HIGHLIGHT:ANSWER END SENT ;

\ The tokens of the words the last completed check of the open document
\ params.textDocument names bound in it, the document checked first if it
\ waits for a check.
: SEMANTIC-TOKENS ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-TOKENS:ANSWER END SENT ;

\ params.context.includeDeclaration, true or false, in WITH-DECL.
: WITH-DECL! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu s" context" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   s" includeDeclaration" FIND-KEY 0= if E-LSP-PARAMS throw then
   TOKEN {: t:n :}
   t T-TRUE <> t T-FALSE <> and if E-LSP-PARAMS throw then
   t T-TRUE = WITH-DECL !
   JR:CLOSE
   p pu ;

: REFS! ( ptr u8 n -- ptr u8 n )  POSITION! WITH-DECL! ;

\ params.newName decoded into NAME-SPAN, its length in NAME-U.
: NEW-NAME! ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu s" newName" PARAM NAME-SPAN STRING-AT NAME-U ! drop
   p pu ;

: NEW-NAME$ ( -- ptr u8 n )  NAME-SPAN @ SPAN:$ drop NAME-U @ ;

: RENAME! ( ptr u8 n -- ptr u8 n )  POSITION! NEW-NAME! ;

\ The request's id copied out of the held bodies, which reading input during
\ a check can grow and move.
: KEEP-ID ( JSON-RPC:id -- JSON-RPC:id )
   ID$ {: a:ptr u:n :}
   KEPT-ID u ROOM
   a u KEPT-ID @ SPAN:COPY
   KEPT-ID @ SPAN:$ drop u JSON--RPC-ID:MAKE ;

\ Whether a queued edit affects a subject or file a gathered check read.
: GATHER-CHANGED? ( -- bool )
   [: LSP-REFERENCES:TOUCHES? ;] HELD-EDITED? ;

\ Whether the request with this id is answered once the input that waits is
\ read: -32800 when a cancel of it was read behind it, else -32801 when a
\ change GATHER-CHANGED? names was.
: HALTED? ( JSON-RPC:id -- bool )
   {: id :}
   READ-WAITING
   id CANCELLED? if
      id REQUEST-CANCELLED s" request cancelled" REPLY-ERROR
      true exit
   then
   GATHER-CHANGED? AT-SLOT @ CURRENT-INPUTS-FRESH? 0= or if
      id CONTENT-MODIFIED s" content modified" REPLY-ERROR
      true exit
   then
   false ;

\ The uses of each subject from the K-th on gathered for the request with
\ this id; before each, the request is answered instead when HALTED? says,
\ and false.
: GATHERED-FROM? ( n JSON-RPC:id -- bool )
   {: k:n id :}
   LSP-REFERENCES:PRIMITIVE? if
      LSP-REFERENCES:SUBJECT-N k ?do
         id HALTED? if false unloop exit then
         LSP-REFERENCES:SUBJECT-N i - LSP-REFERENCES:WAVE-COUNT
         i swap LSP-REFERENCES:PREPARE-WAVE
         dup LSP-REFERENCES:RUN-WAVE
         id HALTED? if drop false unloop exit then
         LSP-REFERENCES:APPLY-WAVE
      LSP-REFERENCES:WAVE-SIZE +loop
      true exit
   then
   LSP-REFERENCES:SUBJECT-N k ?do
      id HALTED? if false unloop exit then
      i LSP-REFERENCES:GATHER
   loop
   true ;

\ Each subject's uses gathered, again while pass 2 (LSP-REFERENCES:MORE?)
\ gives subjects, then settled (LSP-REFERENCES:SETTLE); before each and once
\ all are, the request is answered instead when HALTED? says, and false.
: GATHERED? ( JSON-RPC:id -- bool )
   {: id :}
   0 begin
      dup id GATHERED-FROM? 0= if drop false exit then
      drop LSP-REFERENCES:SUBJECT-N
   LSP-REFERENCES:MORE? 0= until
   drop
   id HALTED? if false exit then
   LSP-REFERENCES:SETTLE
   true ;

\ Every use of the declaration at params.position in the open document
\ params.textDocument names, and the declaration first when
\ params.context.includeDeclaration is true, as tools/lsp-references.f states:
\ -32803 when the request is refused, and a warning before the answer when a
\ file is not covered.
: REFERENCES ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu [: REFS! ;] READ? 0= if exit then
   i UNCHANGED? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   AT-BYTE!
   AT-SLOT @ AT-BYTE @ WITH-DECL @ LSP-REFERENCES:SELECT
   LSP-REFERENCES:REFUSED? if
      i REQUEST-FAILED LSP-REFERENCES:BLOCKED$ REPLY-ERROR
      exit
   then
   i GATHERED? 0= if exit then
   LSP-REFERENCES:UNCOVERED? if WRITER LSP-REFERENCES:WARNING END SENT then
   WRITER i RESULT LSP-REFERENCES:ANSWER END SENT ;

\ Whether a rename of the declaration LSP-REFERENCES selected may go on; else
\ the request is answered, and false: null when it selects none, -32803 when
\ the selection is refused, a token selecting the declaration does not spell
\ its tail, or the declaration is an EXPORT of another word.
: RENAMEABLE? ( JSON-RPC:id -- bool )
   {: i :}
   AT-SLOT @ LSP-REFERENCES:RENAME-VERIFIED? 0= if
      i REQUEST-FAILED LSP-REFERENCES:BLOCKED$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:REFUSED? if
      i REQUEST-FAILED LSP-REFERENCES:BLOCKED$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:FOUND? 0= if i REPLY-NULL false exit then
   LSP-REFERENCES:PRIMITIVE? if
      i REQUEST-FAILED LSP-REFERENCES:PRIMITIVE-RENAME$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:CONSTRUCTOR? if
      i REQUEST-FAILED LSP-REFERENCES:CONSTRUCTOR-RENAME$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:SPELT? 0= if
      i REQUEST-FAILED LSP-REFERENCES:UNSPELT$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:EXPORT? if
      i REQUEST-FAILED LSP-REFERENCES:EXPORT$ REPLY-ERROR
      false exit
   then
   true ;

\ Whether every location gathered may be edited; else the request is answered
\ -32803, and false, since a rename applies every edit or none: a token
\ naming the declaration does not spell its tail, a location's token also
\ declares another word, a file is not covered, or the new name's colon
\ would follow a location's qualifier.
: APPLIES? ( JSON-RPC:id -- bool )
   {: i :}
   LSP-REFERENCES:SPELT? 0= if
      i REQUEST-FAILED LSP-REFERENCES:UNSPELT$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:RE-EXPORTED? if
      i REQUEST-FAILED LSP-REFERENCES:RE-EXPORT$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:UNCOVERED? if
      i REQUEST-FAILED s" Rename not applied" LSP-REFERENCES:UNCOVERED$ REPLY-ERROR
      false exit
   then
   NEW-NAME$ LSP-RENAME:QUALIFIED-WHY$ {: w:ptr wu:n :}
   wu 0<> if i REQUEST-FAILED w wu REPLY-ERROR false exit then
   true ;

\ The range a rename of the declaration at params.position in the open
\ document params.textDocument names would edit there, as
\ tools/lsp-rename.f states: null when the position selects none, and
\ -32803 when the selection is refused, a token selecting the declaration
\ does not spell its tail, or the declaration is an EXPORT of another word.
: PREPARE-RENAME ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu AT-READ? 0= if exit then
   AT-SLOT @ AT-BYTE @ LSP-REFERENCES:PLACE
   i RENAMEABLE? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-RENAME:PREPARED END SENT ;

\ The changes that write params.newName over the tail of the declaration at
\ params.position in the open document params.textDocument names and of
\ every use of it, as tools/lsp-rename.f states: -32803 before any check
\ when newName is empty or holds a byte that ends a token, a colon, a quote or a
\ backslash, null when the position selects none, and else -32803, applying no
\ edit, when prepareRename would refuse or APPLIES? does.
: RENAME ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu [: RENAME! ;] READ? 0= if exit then
   i UNCHANGED? 0= if exit then
   NEW-NAME$ LSP-RENAME:WHY$ {: w:ptr wu:n :}
   wu 0<> if i REQUEST-FAILED w wu REPLY-ERROR exit then
   i CHECK-CURRENT? 0= if exit then
   AT-BYTE!
   AT-SLOT @ AT-BYTE @ true LSP-REFERENCES:SELECT
   i RENAMEABLE? 0= if exit then
   i GATHERED? 0= if exit then
   i APPLIES? 0= if exit then
   WRITER i RESULT NEW-NAME$ LSP-RENAME:ANSWER END SENT ;

\ The spellings that would bind at params.position in the open document
\ params.textDocument names: the document checked now, as its turn, with its
\ cursor at that byte of its text, and answered from that check.
: COMPLETION ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu POSITION? 0= if exit then
   AT-BYTE!
   AT-SLOT @ LAST-CHECKED !
   AT-SLOT @ AT-BYTE @ LSP-CHECK:RUN-AT
   READ-WAITING
   i UNCHANGED? 0= if exit then
   AT-SLOT @ CURRENT-INPUTS-FRESH? 0= if
      i CONTENT-MODIFIED s" content modified" REPLY-ERROR exit
   then
   WRITER i RESULT AT-SLOT @ LSP-COMPLETION:ANSWER END SENT ;

\ The links of the open document params.textDocument names, over the operands
\ of the top-level loaders its last completed check reported, the document
\ checked first if it waits for a check.
: DOCUMENT-LINKS ( JSON-RPC:id ptr u8 n -- )
   {: i p:ptr pu:n :}
   i p pu DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-LINKS:ANSWER END SENT ;

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
   JR-ST p pu INIT
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

\ A workspace folder by the URI string the reader is at; one that names no
\ file here is passed over.
: ROOT-AT ( JR:reader -- JR:reader )
   URI-SPAN DECODE {: n:n :}
   URI-SPAN @ SPAN:$ drop n
   [: 2dup PATH LSP-WORKSPACE:ROOT+ ;] catch {: code:n :} 2drop
   code 0= if exit then
   code E-PATH-RANGE =  code E-PATH-NUL = or
   code E-URI-LAST >= code E-URI-FIRST <= and  or 0= if code throw then ;

\ One member of a workspace folder: its uri a root.
: FOLDER-MEMBER ( JR:reader -- JR:reader )
   s" uri" STR-EQ? if
      NEXT T-STR = if ROOT-AT else SKIP-VALUE then
   else
      NEXT drop SKIP-VALUE
   then ;

: FOLDER ( JR:reader -- JR:reader )
   TOKEN T-OBJ = if
      begin NEXT T-KEY = while FOLDER-MEMBER repeat
   else
      SKIP-VALUE
   then ;

\ The roots references walks (tools/lsp-workspace.f): initialize's
\ params.workspaceFolders, each folder's uri, an empty array naming none; else
\ params.rootUri.
: WORKSPACE! ( ptr u8 n -- )
   {: p:ptr pu:n :}
   pu 0= if exit then
   JR-ST p pu INIT
   NEXT T-OBJ =
   if s" workspaceFolders" FIND-KEY else false then
   if TOKEN T-ARR = else false then
   {: folders:bool :}
   folders if begin NEXT T-ARR-END <> while FOLDER repeat then
   JR:CLOSE
   folders if exit then
   JR-ST p pu INIT
   NEXT T-OBJ =
   if s" rootUri" FIND-KEY else false then
   if TOKEN T-STR = else false then
   if ROOT-AT then
   JR:CLOSE ;

\ Starting, the server answers initialize with its capabilities and any other
\ request -32002. Running, it answers shutdown with null, initialize -32600, a
\ request whose params it cannot read or that names a document not open -32602,
\ and an unknown one -32601. Shut down, it answers every request -32600.
: STARTING-REQUEST ( JSON-RPC:id ptr u8 n ptr u8 n -- )
   {: i m:ptr mu:n p:ptr pu:n :}
   m mu s" initialize" NAMED? if
      p pu MARKDOWN? HOVER-MD !
      p pu WORKSPACE!
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
   m mu s" textDocument/documentSymbol" NAMED? if i p pu DOCUMENT-SYMBOLS exit then
   m mu s" textDocument/definition" NAMED? if i p pu DEFINITION exit then
   m mu s" textDocument/declaration" NAMED? if i p pu DEFINITION exit then
   m mu s" textDocument/references" NAMED? if i p pu REFERENCES exit then
   m mu s" textDocument/prepareRename" NAMED? if i p pu PREPARE-RENAME exit then
   m mu s" textDocument/rename" NAMED? if i p pu RENAME exit then
   m mu s" textDocument/hover" NAMED? if i p pu HOVER exit then
   m mu s" textDocument/documentHighlight" NAMED? if i p pu HIGHLIGHT exit then
   m mu s" textDocument/semanticTokens/full" NAMED? if i p pu SEMANTIC-TOKENS exit then
   m mu s" textDocument/completion" NAMED? if i p pu COMPLETION exit then
   m mu s" textDocument/documentLink" NAMED? if i p pu DOCUMENT-LINKS exit then
   i METHOD-NOT-FOUND s" method not found" REPLY-ERROR ;

: REQUESTED ( JSON-RPC:id ptr u8 n ptr u8 n -- )
   {: held m:ptr mu:n p:ptr pu:n :}
   held KEEP-ID {: i :}
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

\ The first held message served. It is passed before it is dispatched, so the
\ messages held behind it are the ones from HELD-NEXT on; once none is left,
\ their room is reused.
: TURN ( -- )
   HELD-NEXT @ {: k:n :}
   1 HELD-NEXT +!
   JR-ST k HELD$ >MESSAGE DISPATCH
   HELD? if exit then
   0 HELD-NEXT !
   0 HELD-COUNT !
   HELD-B BUF:CLEAR ;

: TURNS ( -- )  begin HELD? while TURN repeat ;

\ The input ended on a message boundary: the messages held are served, and
\ then the server ends.
: ENDED ( -- )
   TURNS
   EXIT-CODE {: rc:n :}
   rc 0= if s" " 0 die then
   s" lsp: input ended before exit" rc die ;

\ A running server checks the documents and publishes their diagnostics
\ (tools/lsp-check.f, tools/lsp-diag.f); each check reads every other open
\ document's text in place of its file. Opening or changing a document leaves it
\ waiting, with every open document whose last completed check read its file
\ (LSP-DEFS:DEFS-READ?). A save leaves every open document waiting: the loader
\ finds files on disk (tools/check-verify-child.f), and a saved file may be one
\ a check could not find or one that now precedes the file it found. Closing
\ one publishes an empty list, drops its definitions and leaves waiting each
\ document whose last check read its file.
\ The document to check now: the next one waiting after the last checked, while
\ the server runs, no message waits or is held and reading is not over. A check
\ then takes a document's newest text, and the versions published only rise.
: DUE ( -- option<n> )
   RUNNING? 0= if OPTION:NONE exit then
   LAST-CHECKED @ DOC-NEXT-DIRTY MATCH option
      none OF OPTION:NONE ENDOF
      some OF INPUT? if drop OPTION:NONE else OPTION:SOME then ENDOF
   ;MATCH ;

\ When reading is over: the messages held are served, and then a fault stops
\ the server with its throw, or the end of input ends it.
: SETTLE ( -- )
   FAULT @ {: code:n :}
   code 0<> if TURNS code throw then
   AT-END @ if ENDED then ;

\ The next message, held, or the end of input, which ends the server. A fault
\ in the framing stops the server, but only once the messages held before it
\ are served, as they are before the end. Either, read by a request between
\ its checks, is acted on here, once that request is answered.
: TAKE-INPUT ( -- )  READ-ON SETTLE ;

: SERVE ( -- )
   FD:STDOUT FD-NOSIGPIPE!
   FD:STDERR FD-NOSIGPIPE!
   REPLY-BUF 1 BUF:N>BLEN BUF:INIT       \ the smallest buffers: replies grow one,
   HELD-B 1 BUF:N>BLEN BUF:INIT          \ held bodies the other
   0 HELD-COUNT !
   0 HELD-NEXT !
   false AT-END !
   0 FAULT !
   LSP-DIAG:PREPARE
   LSP-DEFS:DEFS-PREPARE
   LSP-HOVER:PREPARE
   LSP-COMPLETION:PREPARE
   LSP-LINKS:PREPARE
   false HOVER-MD !
   -1 LAST-CHECKED !
   construct state starting STATE!
   INPUT FD:STDIN HEAD-BUF MAX-BODY BIND
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
