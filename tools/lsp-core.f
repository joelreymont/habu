\ lsp-core.f - the Habu language server: the Language Server Protocol on stdin
\ and stdout, for any client. MAIN reads framed messages, holds each until its
\ turn, sorts it and acts on it, until the client's exit or the end of input.
\ Full sync, positions in UTF-16 units. PROCESS-GLOBAL: the reader, the
\ buffers and the documents belong to the server's one task.
\
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
\
\ A running server checks the documents and publishes their diagnostics; each
\ check reads every other open document's text in place of its file. Opening
\ or changing a document leaves it waiting, with every open document whose
\ last completed check read its file. A save leaves every open document
\ waiting: the loader finds files on disk, and a saved file may be one a check
\ could not find or one that now precedes the file it found. Closing one
\ publishes an empty list, drops its definitions and leaves waiting each
\ document whose last check read its file. The document to check now is the
\ next one waiting after the last checked, while the server runs, no message
\ waits or is held and reading is not over. A check then takes a document's
\ newest text, and the versions published only rise.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/adt/option.f
require src/lib/uri.f
require src/lib/content-length.f
require src/lib/json.f
require src/lib/json-rpc.f
require src/lib/reason.f
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
require src/core/exit-status.f

package LSP
using SPAN
using MEM
using JSON
using JSON-RPC
using CONTENT-LENGTH
using LSP-DOCS

public

-9400 constant E-LSP-FIRST
-9401 constant E-LSP-LAST
-9400 s" a notification's params lack a member the server reads, or hold one of another kind"
   ERROR:CODE E-LSP-PARAMS
-9401 s" a request or notification names a document that is not open: send didOpen for it first"
   ERROR:CODE E-LSP-NOT-OPEN

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

TYPED-VARIABLE JSON-ST JSON:cursor
LINE-CAP 2 + SPAN-BUFFER: HEAD-BUF        \ the reader's buffer: a header line and its CR LF
TYPED-VARIABLE REPLY-OUT ^VEC:vector<u8>  \ a reply's bytes, grown as it is written
TYPED-VARIABLE INPUT CONTENT-LENGTH:reader
TYPED-VARIABLE BODY-SPAN SPAN:span<u8>
TYPED-VARIABLE URI-SPAN SPAN:span<u8>     \ textDocument.uri, decoded
TYPED-VARIABLE PATH-SPAN SPAN:span<u8>
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
TYPED-VARIABLE W JSON:writer
variable TEXT-LEN                         \ the last change's text length, -1 before one is read
variable LAST-CHECKED                     \ the slot checked last, -1 before any
TYPED-VARIABLE HELD-B ^VEC:vector<u8> SCRATCH-ONLY \ the bodies of the messages held, back to back,
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
\ the server with one stderr line naming it and EXIT-STATUS:UNCAUGHT.
: EXIT-CODE ( -- n )
   STATE@ MATCH state
      starting OF 1 ENDOF
      running OF 1 ENDOF
      shut-down OF 0 ENDOF
   ;MATCH ;

\ The span in V made at least N bytes long, and never empty, since JSON:READ-STR
\ refuses a null destination.
: ROOM ( ^SPAN:span<u8> n -- )
   {: v n:n :}
   n 1 max {: need:n :}
   v @ LEN need >= if exit then
   v @ FREE-SPAN
   need BYTES-ALLOC-LEN ALLOC-SPAN v ! ;

\ ---- replies -----------------------------------------------------------------

: WRITER ( -- ^JSON:writer )
   W REPLY-OUT @ JSON:OPEN ;

: SENT ( ^JSON:writer -- )
   {: w :}
   w WRITTEN$ {: s:str :}
   w JSON:CLOSE
   FD:STDOUT s SEND ;

: REPLY-NULL ( JSON-RPC:id -- )
   {: i :}
   WRITER i RESULT WRITE-NULL END SENT ;

: REPLY-ERROR ( JSON-RPC:id n str -- )
   {: i code:n m :}
   WRITER i code m ERROR SENT ;

: REPLY-CAPABILITIES ( JSON-RPC:id -- )
   {: i :}
   WRITER i RESULT
   WRITE-OBJECT-START
      s" capabilities" WRITE-KEY WRITE-OBJECT-START
         s" positionEncoding" s" utf-16" WRITE-FIELD-S WRITE-COMMA
         s" textDocumentSync" WRITE-KEY WRITE-OBJECT-START
            s" openClose" true WRITE-FIELD-BOOL WRITE-COMMA
            s" change" FULL-SYNC WRITE-FIELD-U WRITE-COMMA
            s" save" WRITE-KEY WRITE-OBJECT-START
               s" includeText" false WRITE-FIELD-BOOL
            WRITE-OBJECT-END
         WRITE-OBJECT-END WRITE-COMMA
         s" completionProvider" WRITE-KEY WRITE-OBJECT-START WRITE-OBJECT-END WRITE-COMMA
         s" definitionProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" declarationProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" referencesProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" renameProvider" WRITE-KEY WRITE-OBJECT-START
            s" prepareProvider" true WRITE-FIELD-BOOL
         WRITE-OBJECT-END WRITE-COMMA
         s" hoverProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" documentHighlightProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" documentSymbolProvider" true WRITE-FIELD-BOOL WRITE-COMMA
         s" documentLinkProvider" WRITE-KEY WRITE-OBJECT-START WRITE-OBJECT-END WRITE-COMMA
         LSP-TOKENS:LEGEND WRITE-COMMA
         s" workspaceSymbolProvider" true WRITE-FIELD-BOOL
      WRITE-OBJECT-END WRITE-COMMA
      s" serverInfo" WRITE-KEY WRITE-OBJECT-START
         s" name" s" habu" WRITE-FIELD-S
      WRITE-OBJECT-END
   WRITE-OBJECT-END
   END SENT ;

\ ---- params ------------------------------------------------------------------

\ A reader on the value of the params' member KEY, and that value's kind.
\ Params absent or null are empty.
: PARAM ( str str -- JSON:reader n )
   {: p k :}
   p str> nip 0= if E-LSP-PARAMS throw then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ <> if E-LSP-PARAMS throw then
   k READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   READ-TOKEN ;

\ A reader on the value of params.textDocument's member KEY, and its kind.
: DOC-PARAM ( str str -- JSON:reader n )
   {: p k :}
   p s" textDocument" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   k READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   READ-TOKEN ;

\ A string's JSON text is never shorter than what it decodes to.
: DECODE ( JSON:reader ^SPAN:span<u8> -- JSON:reader n )
   {: v :}
   READ-SPAN$ nip {: raw:n :}
   v raw ROOM
   v @ SPAN:$ str> READ-STR ;

: STRING-AT ( JSON:reader n ^SPAN:span<u8> -- str )
   {: v :}
   T-STR <> if E-LSP-PARAMS throw then
   v DECODE {: n:n :}
   READ-CLOSE
   v @ SPAN:$ str> drop n >str ;

: DOC-STRING ( str str ^SPAN:span<u8> -- str )
   {: p k v :}
   p k DOC-PARAM v STRING-AT ;

: DOC-URI ( str -- str )  s" uri" URI-SPAN DOC-STRING ;

: DOC-VERSION ( str -- n )
   {: p :}
   p s" version" DOC-PARAM T-INT <> if E-LSP-PARAMS throw then
   READ-INT {: v:n :}
   READ-CLOSE
   v ;

\ The file path a URI names, which is never longer than the URI.
: PATH ( str -- str )
   {: u :}
   PATH-SPAN u str> nip ROOM
   u PATH-SPAN @ URI:FILE>PATH {: n:n :}
   PATH-SPAN @ SPAN:$ str> drop n >str ;

\ A range is incremental sync, not offered.
: CHANGE-MEMBER ( JSON:reader -- JSON:reader )
   s" range" READ-STR-EQ? if E-LSP-PARAMS throw then
   s" text" READ-STR-EQ? if
      READ-NEXT T-STR <> if E-LSP-PARAMS throw then
      TEXT-SPAN DECODE TEXT-LEN !
   else
      READ-NEXT drop READ-SKIP-VALUE
   then ;

: CHANGE ( JSON:reader -- JSON:reader )
   READ-TOKEN T-OBJ <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin READ-NEXT T-KEY = while CHANGE-MEMBER repeat
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then ;

: CHANGES ( str -- n )
   s" contentChanges" PARAM T-ARR <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin READ-NEXT T-ARR-END <> while CHANGE repeat
   READ-CLOSE
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then
   TEXT-LEN @ ;

: OPEN-SLOT ( str -- n )
   DOC-FIND MATCH option
      none OF E-LSP-NOT-OPEN throw ENDOF
      some OF ENDOF
   ;MATCH ;

: INTO? ( JSON:reader str -- JSON:reader bool )
   READ-FIND-KEY 0= if false exit then
   READ-TOKEN T-OBJ = ;

\ ---- notifications -----------------------------------------------------------

\ An editor event changes checks that consumed this path. A watched disk
\ event also changes checks stopped at this path before they could read it.
\ A document holding the path itself still supplies its editor text.
: DIRTY-DEPENDENTS ( str bool -- )
   {: path include-unread:bool :}
   DOC-SLOTS 0 ?do
      i DOC-LIVE? if
         i DOC-CANON$ path STR= 0= if
            include-unread if
               i path LSP-DEFS:DEFS-DEPENDS?
            else
               i path LSP-DEFS:DEFS-READ?
            then if i DOC-DIRTY then
         then
      then
   loop ;

: READERS-DIRTY-PATH ( str -- )  false DIRTY-DEPENDENTS ;

: READERS-DIRTY ( n -- )  DOC-CANON$ READERS-DIRTY-PATH ;

\ Each handler reads all of its params before it changes a document, so one
\ that throws has changed nothing.
: DID-OPEN ( str -- str )
   {: p :}
   p DOC-URI {: u :}
   u PATH {: a :}
   p DOC-VERSION {: v:n :}
   p s" text" TEXT-SPAN DOC-STRING {: t :}
   u v a t DOC-OPEN
   u OPEN-SLOT READERS-DIRTY
   p ;

: DID-CHANGE ( str -- str )
   {: p :}
   p DOC-URI OPEN-SLOT {: slot:n :}
   p DOC-VERSION {: v:n :}
   p CHANGES {: n:n :}
   slot v TEXT-SPAN @ SPAN:$ str> drop n >str DOC-CHANGE
   slot READERS-DIRTY
   p ;

: DID-CLOSE ( str -- str )
   {: p :}
   p DOC-URI OPEN-SLOT {: slot:n :}
   slot LSP-DIAG:RETRACT
   slot READERS-DIRTY
   slot LSP-DEFS:DEFS-DROP
   slot DOC-CLOSE
   p ;

: WATCH-CHANGE ( JSON:reader -- JSON:reader )
   READ-TOKEN T-OBJ <> if E-LSP-PARAMS throw then
   begin READ-NEXT T-KEY = while
      s" uri" READ-STR-EQ? if
         READ-NEXT T-STR <> if E-LSP-PARAMS throw then
         URI-SPAN DECODE {: n:n :}
         URI-SPAN @ SPAN:$ str> drop n >str PATH
         SOURCE-ROOT:CANONICAL drop
         true DIRTY-DEPENDENTS
      else READ-NEXT drop READ-SKIP-VALUE then
   repeat ;

: DID-WATCH ( str -- str )
   {: p :}
   p s" changes" PARAM T-ARR <> if E-LSP-PARAMS throw then
   begin READ-NEXT T-ARR-END <> while WATCH-CHANGE repeat
   READ-CLOSE
   p ;

\ The throws that come of params the server cannot apply: a notification is
\ left unapplied, the server's stderr line naming the method, the code and the
\ reader's text when it has one, and a request is answered -32602. Any other
\ stops the server.
: IGNORABLE? ( n -- bool )
   {: code:n :}
   code E-LSP-PARAMS =  code E-LSP-NOT-OPEN = or  code E-JSON-READ-NUMBER = or
   code E-PATH-RANGE = or  code E-PATH-NUL = or
   code E-URI-LAST >= code E-URI-FIRST <= and or ;

: GUARDED ( str str [ str -- str ] -- )
   {: m p q :}
   p q catch {: code:n :} drop
   code REASON:TEXT$ {: why :}
   code 0= if exit then
   code IGNORABLE? 0= if code throw then
   SB-RESET s" lsp: ignored " SB-APPEND m SB-APPEND
   s" : throw " SB-APPEND code FMT:SB-INT
   why str> nip 0<> if s" : " SB-APPEND why SB-APPEND then
   10 SB-APPEND-C
   FD:STDERR SB$ FD:WRITE-FULL ;

\ Only a running server takes a notification other than exit; the rest are
\ dropped, as unknown ones always are. One the server cannot apply (params it
\ cannot read, a URI that names no file here, a document not open) changes
\ nothing, and one stderr line says why, since a notification has no reply. A
\ response from the client is dropped: the server asks nothing.
: NOTIFIED ( str str -- )
   {: ms p :}
   ms s" exit" METHOD? if s" " EXIT-CODE die then
   RUNNING? 0= if exit then
   ms s" textDocument/didOpen" METHOD? if
      ms p [: DID-OPEN ;] GUARDED exit
   then
   ms s" textDocument/didChange" METHOD? if
      ms p [: DID-CHANGE ;] GUARDED exit
   then
   ms s" textDocument/didClose" METHOD? if
      ms p [: DID-CLOSE ;] GUARDED exit
   then
   ms s" textDocument/didSave" METHOD? if DOC-DIRTY-ALL exit then
   ms s" workspace/didChangeWatchedFiles" METHOD? if
      ms p [: DID-WATCH ;] GUARDED
   then ;

\ ---- held messages -----------------------------------------------------------

: HELD? ( -- bool )  HELD-NEXT @ HELD-COUNT @ < ;

: HELD$ ( n -- str )
   {: k:n :}
   k 0= if 0 else k 1- HELD-ENDS @ then {: at:n :}
   HELD-B @ VEC:SPAN SPAN:$ str> drop at +
   k HELD-ENDS @ at - >str ;

: HOLD ( str -- )
   str> HELD-B @ VEC:APPEND
   HELD-COUNT @ 1+ HELD-ENDS-RESERVE
   HELD-B @ VEC:COUNT HELD-COUNT @ HELD-ENDS !
   1 HELD-COUNT +! ;

: NOTICE? ( n str -- str bool )
   {: k:n name :}
   k HELD$ >MESSAGE MATCH JSON-RPC:message
      request OF 2drop 2drop ID$ drop s" " false ENDOF
      notification OF
         {: m:^ mu:n p:^ pu:n :} p pu >str m mu >str name METHOD?
      ENDOF
      success OF 2drop ID$ drop s" " false ENDOF
      failure OF 2drop ID$ drop s" " false ENDOF
      invalid OF ID$ drop 2drop drop s" " false ENDOF
   ;MATCH ;

\ What a cancel's id is matched against for the request with this id: a
\ string's decoded text, in ID-SPAN, and true; else its JSON text and false.
: ID-KEY ( JSON-RPC:id -- str bool )
   ID$ {: r :}
   JSON-ST r str> READ-INIT
   READ-NEXT T-STR = if ID-SPAN DECODE true else 0 false then {: n:n str:bool :}
   READ-CLOSE
   str if ID-SPAN @ SPAN:$ str> drop n >str true exit then
   r false ;

\ Whether params.id is the id this key gives: a string with this decoded text
\ when STR is true, else a value with this JSON text.
: ID-PARAM? ( str str bool -- bool )
   {: p k str:bool :}
   p str> nip 0= if false exit then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ = if s" id" READ-FIND-KEY else false then
   if
      READ-TOKEN T-STR = if
         str if k READ-STR-EQ? else false then
      else
         str 0= if READ-SPAN$ >str k STR= else false then
      then
   else false then
   {: same:bool :}
   READ-CLOSE
   same ;

: CANCELLED? ( JSON-RPC:id -- bool )
   ID-KEY {: k str:bool :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" $/cancelRequest" NOTICE? if
         k str ID-PARAM? if true unloop exit then
      else drop then
   loop
   false ;

: URI-SLOT ( str -- n )
   {: p :}
   p str> nip 0= if -1 exit then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ =
   if s" textDocument" INTO? else false then
   if s" uri" READ-FIND-KEY else false then
   if READ-TOKEN T-STR = else false then
   {: named:bool :}
   named if URI-SPAN DECODE else 0 then {: n:n :}
   READ-CLOSE
   named 0= if -1 exit then
   URI-SPAN @ SPAN:$ str> drop n >str DOC-FIND MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: CHANGED? ( n -- bool )
   {: slot:n :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" textDocument/didChange" NOTICE? if
         URI-SLOT slot = if true unloop exit then
      else drop then
   loop
   false ;

: EDIT-NOTICE? ( n str [ str -- bool ] -- bool )
   {: k:n name affected :}
   k name NOTICE? if
      [: DOC-URI PATH SOURCE-ROOT:CANONICAL drop ;]
      catch {: code:n :}
      code 0<> if
         drop
         code IGNORABLE? if false exit then
         code throw
      then
      affected execute
   else drop false then ;

\ A queued editor notification whose path the request actually uses.
: HELD-EDITED? ( [ str -- bool ] -- bool )
   {: affected :}
   HELD-COUNT @ HELD-NEXT @ ?do
      i s" textDocument/didOpen" affected EDIT-NOTICE?
      if true unloop exit then
      i s" textDocument/didChange" affected EDIT-NOTICE?
      if true unloop exit then
      i s" textDocument/didClose" affected EDIT-NOTICE?
      if true unloop exit then
      i s" textDocument/didSave" affected EDIT-NOTICE?
      if true unloop exit then
   loop
   false ;

: REQUEST-DEPENDS? ( str -- bool )
   {: path :}
   AT-SLOT @ DOC-CANON$ path STR= if true exit then
   AT-SLOT @ path LSP-DEFS:DEFS-DEPENDS? ;

\ ---- input -------------------------------------------------------------------

: SETTLED? ( -- bool )  FAULT @ 0<> AT-END @ or ;

: RECEIVE ( n -- )
   {: len:n :}
   BODY-SPAN len ROOM
   INPUT BODY-SPAN @ BODY
   BODY-SPAN @ len TAKE SPAN:$ HOLD ;

\ Whether stdin holds input, or its end, a poll that a signal cut short asked
\ again.
: POLLED? ( -- bool )
   begin FD:STDIN 0 >MS PROCESS:POLL-IN COUNT>N dup FD:EINTR# negate = while drop repeat
   0<> ;

\ Whether a message waits: begun in the reader's buffer, or on stdin; or
\ reading is over, which the loop then acts on.
: INPUT? ( -- bool )
   SETTLED? if true exit then
   INPUT PENDING? if true exit then
   POLLED? ;

: READ-MESSAGE ( -- )
   INPUT NEXT-LENGTH MATCH option
      none OF true AT-END ! ENDOF
      some OF RECEIVE ENDOF
   ;MATCH ;

\ SETTLE's throw of FAULT is a new throw, so a record the caught one carried is sent here.
: READ-ON ( -- )
   SETTLED? if exit then
   [: READ-MESSAGE ;] catch {: code:n :}
   code 0<> REJECTION:CARRIED? and if REJECTION:SEND-SAVED then
   code FAULT ! ;

\ A message begun is read whole, which waits for the rest of it.
: READ-WAITING ( -- )
   begin INPUT? SETTLED? 0= and while READ-ON repeat ;

\ ---- requests ----------------------------------------------------------------

: READ? ( JSON-RPC:id str [ str -- str ] -- bool )
   {: i p q :}
   p q catch {: code:n :} drop
   code REASON:TEXT$ {: why :}
   code 0= if true exit then
   code IGNORABLE? 0= if code throw then
   why str> nip 0= if s" invalid params" else why then {: msg :}
   i INVALID-PARAMS msg REPLY-ERROR
   false ;

: QUERY! ( str -- str )
   {: p :}
   p s" query" PARAM QUERY-SPAN STRING-AT str> QUERY-U ! drop
   p ;

: QUERY$ ( -- str )
   QUERY-SPAN @ SPAN:$ str> drop QUERY-U @ >str ;

: SYMBOLS ( JSON-RPC:id str -- )
   {: i p :}
   i p [: QUERY! ;] READ? 0= if exit then
   WRITER i RESULT QUERY$ LSP-SYMBOLS:ANSWER END SENT ;

: POSITION-INT ( str str -- n )
   {: p k :}
   p s" position" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   k READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   READ-TOKEN T-INT <> if E-LSP-PARAMS throw then
   READ-INT {: v:n :}
   READ-CLOSE
   v 0 < if E-LSP-PARAMS throw then
   v ;

: SLOT! ( str -- str )
   {: p :}
   p DOC-URI OPEN-SLOT AT-SLOT !
   p ;

: POSITION! ( str -- str )
   {: p :}
   p SLOT! s" line" POSITION-INT AT-LINE !
   p s" character" POSITION-INT AT-CHAR !
   p ;

\ Checks the document in this slot now, as its turn: the next check due is of
\ a waiting document after it.
: CHECK-NOW ( n -- )  dup LAST-CHECKED ! -1 LSP-CHECK:RUN-AT ;

\ Checks the document in this slot first if it waits for a check, so a request
\ about it is answered from the text the client asked about: LSP orders a
\ request after the changes sent before it.
: CHECK-WAITING ( n -- )
   dup DOC-DIRTY? if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-INPUTS-FRESH? 0= if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-UNREAD-RETRY? if CHECK-NOW else drop then ;

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
\ fresh inputs.
: CHECK-CURRENT? ( JSON-RPC:id -- bool )
   {: i :}
   AT-SLOT @ CHECK-WAITING
   READ-WAITING
   i UNCHANGED? 0= if false exit then
   AT-SLOT @ CURRENT-INPUTS-FRESH? if true exit then
   i CONTENT-MODIFIED s" content modified" REPLY-ERROR
   false ;

: DOCUMENT? ( JSON-RPC:id str -- bool )
   {: i p :}
   i p [: SLOT! ;] READ? 0= if false exit then
   i UNCHANGED? ;

: POSITION? ( JSON-RPC:id str -- bool )
   {: i p :}
   i p [: POSITION! ;] READ? 0= if false exit then
   i UNCHANGED? ;

: AT-READ? ( JSON-RPC:id str -- bool )
   {: i p :}
   i p POSITION? 0= if false exit then
   i CHECK-CURRENT? 0= if false exit then
   AT-BYTE!
   true ;

: DEFINITION ( JSON-RPC:id str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-DEFINITION:ANSWER END SENT ;

: DOCUMENT-SYMBOLS ( JSON-RPC:id str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-OUTLINE:ANSWER END SENT ;

: HOVER ( JSON-RPC:id str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ HOVER-MD @ LSP-HOVER:ANSWER END SENT ;

: HIGHLIGHT ( JSON-RPC:id str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-HIGHLIGHT:ANSWER END SENT ;

: SEMANTIC-TOKENS ( JSON-RPC:id str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-TOKENS:ANSWER END SENT ;

: WITH-DECL! ( str -- str )
   {: p :}
   p s" context" PARAM T-OBJ <> if E-LSP-PARAMS throw then
   s" includeDeclaration" READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   READ-TOKEN {: t:n :}
   t T-TRUE <> t T-FALSE <> and if E-LSP-PARAMS throw then
   t T-TRUE = WITH-DECL !
   READ-CLOSE
   p ;

: REFS! ( str -- str )  POSITION! WITH-DECL! ;

: NEW-NAME! ( str -- str )
   {: p :}
   p s" newName" PARAM NAME-SPAN STRING-AT str> NAME-U ! drop
   p ;

: NEW-NAME$ ( -- str )
   NAME-SPAN @ SPAN:$ str> drop NAME-U @ >str ;

: RENAME! ( str -- str )  POSITION! NEW-NAME! ;

\ The request's id copied out of the held bodies, which reading input during
\ a check can grow and move.
: KEEP-ID ( JSON-RPC:id -- JSON-RPC:id )
   ID$ str> {: a:^ u:n :}
   KEPT-ID u ROOM
   a u >str KEPT-ID @ SPAN:COPY
   KEPT-ID @ SPAN:$ str> drop u JSON--RPC-ID:MAKE ;

: GATHER-CHANGED? ( -- bool )
   [: LSP-REFERENCES:TOUCHES? ;] HELD-EDITED? ;

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

: REFERENCES ( JSON-RPC:id str -- )
   {: i p :}
   i p [: REFS! ;] READ? 0= if exit then
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
\ -32803, and false, since a rename applies every edit or none.
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
      i REQUEST-FAILED
      s" Rename not applied" LSP-REFERENCES:UNCOVERED$
      REPLY-ERROR
      false exit
   then
   NEW-NAME$ LSP-RENAME:QUALIFIED-WHY$ {: w :}
   w str> nip 0<> if i REQUEST-FAILED w REPLY-ERROR false exit then
   true ;

: PREPARE-RENAME ( JSON-RPC:id str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   AT-SLOT @ AT-BYTE @ LSP-REFERENCES:PLACE
   i RENAMEABLE? 0= if exit then
   WRITER i RESULT AT-SLOT @ AT-BYTE @ LSP-RENAME:PREPARED END SENT ;

: RENAME ( JSON-RPC:id str -- )
   {: i p :}
   i p [: RENAME! ;] READ? 0= if exit then
   i UNCHANGED? 0= if exit then
   NEW-NAME$ LSP-RENAME:WHY$ {: w :}
   w str> nip 0<> if i REQUEST-FAILED w REPLY-ERROR exit then
   i CHECK-CURRENT? 0= if exit then
   AT-BYTE!
   AT-SLOT @ AT-BYTE @ true LSP-REFERENCES:SELECT
   i RENAMEABLE? 0= if exit then
   i GATHERED? 0= if exit then
   i APPLIES? 0= if exit then
   WRITER i RESULT NEW-NAME$ LSP-RENAME:ANSWER END SENT ;

: COMPLETION ( JSON-RPC:id str -- )
   {: i p :}
   i p POSITION? 0= if exit then
   AT-BYTE!
   AT-SLOT @ LAST-CHECKED !
   AT-SLOT @ AT-BYTE @ LSP-CHECK:RUN-AT
   READ-WAITING
   i UNCHANGED? 0= if exit then
   AT-SLOT @ CURRENT-INPUTS-FRESH? 0= if
      i CONTENT-MODIFIED s" content modified" REPLY-ERROR exit
   then
   WRITER i RESULT AT-SLOT @ LSP-COMPLETION:ANSWER END SENT ;

: DOCUMENT-LINKS ( JSON-RPC:id str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i RESULT AT-SLOT @ LSP-LINKS:ANSWER END SENT ;

: LISTS-MARKDOWN? ( JSON:reader -- JSON:reader bool )
   begin READ-NEXT dup T-ARR-END <> while
      T-STR = if
         s" markdown" READ-STR-EQ? if true exit then
      else
         READ-SKIP-VALUE
      then
   repeat
   drop false ;

: MARKDOWN? ( str -- bool )
   {: p :}
   p str> nip 0= if false exit then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ =
   if s" capabilities" INTO? else false then
   if s" textDocument" INTO? else false then
   if s" hover" INTO? else false then
   if s" contentFormat" READ-FIND-KEY else false then
   if READ-TOKEN T-ARR = else false then
   if LISTS-MARKDOWN? else false then
   {: md:bool :}
   READ-CLOSE
   md ;

\ A workspace folder by the URI string the reader is at; one that names no
\ file here is passed over.
: ROOT-AT ( JSON:reader -- JSON:reader )
   URI-SPAN DECODE {: n:n :}
   URI-SPAN @ SPAN:$ str> drop n >str {: u :}
   [: u PATH LSP-WORKSPACE:ROOT+ ;] catch {: code:n :}
   code 0= if exit then
   code E-PATH-RANGE =  code E-PATH-NUL = or
   code E-URI-LAST >= code E-URI-FIRST <= and  or 0= if code throw then ;

: FOLDER-MEMBER ( JSON:reader -- JSON:reader )
   s" uri" READ-STR-EQ? if
      READ-NEXT T-STR = if ROOT-AT else READ-SKIP-VALUE then
   else
      READ-NEXT drop READ-SKIP-VALUE
   then ;

: FOLDER ( JSON:reader -- JSON:reader )
   READ-TOKEN T-OBJ = if
      begin READ-NEXT T-KEY = while FOLDER-MEMBER repeat
   else
      READ-SKIP-VALUE
   then ;

: WORKSPACE! ( str -- )
   {: p :}
   p str> nip 0= if exit then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ =
   if s" workspaceFolders" READ-FIND-KEY else false then
   if READ-TOKEN T-ARR = else false then
   {: folders:bool :}
   folders if begin READ-NEXT T-ARR-END <> while FOLDER repeat then
   READ-CLOSE
   folders if exit then
   JSON-ST p str> READ-INIT
   READ-NEXT T-OBJ =
   if s" rootUri" READ-FIND-KEY else false then
   if READ-TOKEN T-STR = else false then
   if ROOT-AT then
   READ-CLOSE ;

: STARTING-REQUEST ( JSON-RPC:id str str -- )
   {: i m p :}
   m s" initialize" METHOD? if
      p MARKDOWN? HOVER-MD !
      p WORKSPACE!
      i REPLY-CAPABILITIES
      construct state running STATE!
   else
      i NOT-INITIALIZED s" server not initialized" REPLY-ERROR
   then ;

: RUNNING-REQUEST ( JSON-RPC:id str str -- )
   {: i m p :}
   i CANCELLED? if
      i REQUEST-CANCELLED s" request cancelled" REPLY-ERROR
      exit
   then
   m s" shutdown" METHOD? if
      i REPLY-NULL
      construct state shut-down STATE!
      exit
   then
   m s" initialize" METHOD? if
      i INVALID-REQUEST s" server already initialized" REPLY-ERROR
      exit
   then
   m s" workspace/symbol" METHOD? if i p SYMBOLS exit then
   m s" textDocument/documentSymbol" METHOD? if i p DOCUMENT-SYMBOLS exit then
   m s" textDocument/definition" METHOD? if i p DEFINITION exit then
   m s" textDocument/declaration" METHOD? if i p DEFINITION exit then
   m s" textDocument/references" METHOD? if i p REFERENCES exit then
   m s" textDocument/prepareRename" METHOD? if i p PREPARE-RENAME exit then
   m s" textDocument/rename" METHOD? if i p RENAME exit then
   m s" textDocument/hover" METHOD? if i p HOVER exit then
   m s" textDocument/documentHighlight" METHOD? if i p HIGHLIGHT exit then
   m s" textDocument/semanticTokens/full" METHOD? if i p SEMANTIC-TOKENS exit then
   m s" textDocument/completion" METHOD? if i p COMPLETION exit then
   m s" textDocument/documentLink" METHOD? if i p DOCUMENT-LINKS exit then
   i METHOD-NOT-FOUND s" method not found" REPLY-ERROR ;

: REQUESTED ( JSON-RPC:id str str -- )
   {: held m p :}
   held KEEP-ID {: i :}
   STATE@ MATCH state
      starting OF i m p STARTING-REQUEST ENDOF
      running OF i m p RUNNING-REQUEST ENDOF
      shut-down OF i INVALID-REQUEST s" server shut down" REPLY-ERROR ENDOF
   ;MATCH ;

\ ---- the loop ----------------------------------------------------------------

: DISPATCH ( JSON-RPC:message -- )
   MATCH JSON-RPC:message
      request OF
         {: i m:^ mu:n p:^ pu:n :} i m mu >str p pu >str REQUESTED
      ENDOF
      notification OF
         {: m:^ mu:n p:^ pu:n :} m mu >str p pu >str NOTIFIED
      ENDOF
      success OF 2drop ID$ drop ENDOF
      failure OF 2drop ID$ drop ENDOF
      invalid OF {: c:n w:^ wu:n i :} i c w wu >str REPLY-ERROR ENDOF
   ;MATCH ;

\ The first held message served. It is passed before it is dispatched, so the
\ messages held behind it are the ones from HELD-NEXT on; once none is left,
\ their room is reused.
: TURN ( -- )
   HELD-NEXT @ {: k:n :}
   1 HELD-NEXT +!
   k HELD$ >MESSAGE DISPATCH
   HELD? if exit then
   0 HELD-NEXT !
   0 HELD-COUNT !
   HELD-B @ 0 VEC:RESIZE ;

: TURNS ( -- )  begin HELD? while TURN repeat ;

: ENDED ( -- )
   TURNS
   EXIT-CODE {: rc:n :}
   rc 0= if s" " EXIT-STATUS:SUCCESS die then
   s" lsp: input ended before exit" rc die ;

: DUE ( -- option<n> )
   RUNNING? 0= if OPTION:NONE exit then
   LAST-CHECKED @ DOC-NEXT-DIRTY MATCH option
      none OF OPTION:NONE ENDOF
      some OF INPUT? if drop OPTION:NONE else OPTION:SOME then ENDOF
   ;MATCH ;

: SETTLE ( -- )
   FAULT @ {: code:n :}
   code 0<> if TURNS code throw then
   AT-END @ if ENDED then ;

\ A fault in the framing stops the server, but only once the messages held
\ before it are served, as they are before the end. Either, read by a request
\ between its checks, is acted on here, once that request is answered.
: TAKE-INPUT ( -- )  READ-ON SETTLE ;

: SERVE ( -- )
   FD:STDOUT FD:FD-NOSIGPIPE!
   FD:STDERR FD:FD-NOSIGPIPE!
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW REPLY-OUT !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW HELD-B !
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
   REJECTION:CARRIED? if REJECTION:SEND-SAVED then
   SB-RESET s" lsp: stopped by throw " SB-APPEND code FMT:SB-INT
   SB$ code ERROR:STATUS die ;

;using
;using
;using
;using
;using
;using
;package
