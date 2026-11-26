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
require src/lib/json/write.f
require src/lib/json/read.f
require src/lib/json/value.f

package LSP

public

-9400 constant E-LSP-FIRST
-9401 constant E-LSP-LAST
-9400 s" params lack a member the server reads, hold one of another kind or one it does not take"
   ERROR:CODE E-LSP-PARAMS
-9401 s" a request or notification names a document that is not open: send didOpen for it first"
   ERROR:CODE E-LSP-NOT-OPEN

-32002 constant NOT-INITIALIZED          \ LSP's ServerNotInitialized
-32800 constant REQUEST-CANCELLED        \ LSP's RequestCancelled
-32801 constant CONTENT-MODIFIED         \ LSP's ContentModified
-32803 constant REQUEST-FAILED           \ LSP's RequestFailed
\ The longest body taken, room for a document's whole text escaped. A header
\ that announces more is rejected before any of the body is read.
64 1024 * 1024 * constant MAX-BODY

private

1 constant FULL-SYNC                     \ TextDocumentSyncKind.Full

ENUM state starting running shut-down ;ENUM
1 LAYOUT-BUFFER STATE-BUF state

TYPED-VARIABLE JSON-ST JSON:cursor
CONTENT-LENGTH:LINE-CAP 2 + constant HEAD-BYTES          \ a header line and its CR LF
create HEAD-ROW HEAD-BYTES SCRATCH-ALLOT
\ The reader's buffer.
: HEAD-BUF ( -- SPAN:span<u8> ) HEAD-ROW HEAD-BYTES SPAN:MAKE ;
TYPED-VARIABLE REPLY-OUT ^VEC:vector<u8>  \ a reply's bytes, grown as it is written
TYPED-VARIABLE INPUT CONTENT-LENGTH:reader
TYPED-VARIABLE BODY-SPAN SPAN:span<u8>
TYPED-VARIABLE URI-B ^VEC:vector<u8> SCRATCH-ONLY \ a URI the params name, decoded
TYPED-VARIABLE PATH-SPAN SPAN:span<u8>
TYPED-VARIABLE TEXT-B ^VEC:vector<u8> SCRATCH-ONLY \ a document's text, decoded
TYPED-VARIABLE QUERY-B ^VEC:vector<u8> SCRATCH-ONLY \ workspace/symbol's query, decoded
TYPED-VARIABLE NAME-B ^VEC:vector<u8> SCRATCH-ONLY \ a rename's newName, decoded
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
TYPED-VARIABLE ID-B ^VEC:vector<u8> SCRATCH-ONLY \ a request's string id, decoded
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

: ROOM ( ^SPAN:span<u8> n -- )
   {: v n:n :}
   v @ SPAN:LEN n >= if exit then
   v @ MEM:FREE-SPAN
   n MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN v ! ;

\ ---- replies -----------------------------------------------------------------

: WRITER ( -- ^JSON:writer )
   W REPLY-OUT @ JSON:OPEN ;

: SENT ( ^JSON:writer -- )
   {: w :}
   w JSON:WRITTEN$ {: s:str :}
   w JSON:CLOSE
   FD:STDOUT s CONTENT-LENGTH:SEND ;

: REPLY-NULL ( JSON:value -- )
   {: i :}
   WRITER i JSON-RPC:RESULT JSON:WRITE-NULL JSON-RPC:END SENT ;

: REPLY-ERROR ( JSON:value n str -- )
   {: i code:n m :}
   WRITER i code m JSON-RPC:ERROR SENT ;

: REPLY-CAPABILITIES ( JSON:value -- )
   {: i :}
   WRITER i JSON-RPC:RESULT
   JSON:WRITE-OBJECT-START
      s" capabilities" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
         s" positionEncoding" s" utf-16" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
         s" textDocumentSync" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
            s" openClose" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
            s" change" FULL-SYNC JSON:WRITE-FIELD-U JSON:WRITE-COMMA
            s" save" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
               s" includeText" false JSON:WRITE-FIELD-BOOL
            JSON:WRITE-OBJECT-END
         JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
         s" completionProvider" JSON:WRITE-KEY JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
         s" definitionProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" declarationProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" referencesProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" renameProvider" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
            s" prepareProvider" true JSON:WRITE-FIELD-BOOL
         JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
         s" hoverProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" documentHighlightProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" documentSymbolProvider" true JSON:WRITE-FIELD-BOOL JSON:WRITE-COMMA
         s" documentLinkProvider" JSON:WRITE-KEY JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
         LSP-TOKENS:LEGEND JSON:WRITE-COMMA
         s" workspaceSymbolProvider" true JSON:WRITE-FIELD-BOOL
      JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
      s" serverInfo" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
         s" name" s" habu" JSON:WRITE-FIELD-S
      JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END
   JSON-RPC:END SENT ;

\ ---- params ------------------------------------------------------------------

\ A reader on the value of the params' member KEY, and that value's kind.
\ Params absent or null are empty.
: PARAM ( str str -- JSON:reader n )
   {: p k :}
   p str> nip 0= if E-LSP-PARAMS throw then
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if E-LSP-PARAMS throw then
   k JSON:READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   JSON:READ-TOKEN ;

\ A reader on the value of params.textDocument's member KEY, and its kind.
: DOC-PARAM ( str str -- JSON:reader n )
   {: p k :}
   p s" textDocument" PARAM JSON:T-OBJ <> if E-LSP-PARAMS throw then
   k JSON:READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   JSON:READ-TOKEN ;

: STRING-AT ( JSON:reader n ^VEC:vector<u8> -- str )
   {: v:^ :}
   JSON:T-STR <> if E-LSP-PARAMS throw then
   v JSON:READ-STR {: s:str :}
   JSON:READ-CLOSE
   s ;

: DOC-STRING ( str str ^VEC:vector<u8> -- str )
   {: p k v :}
   p k DOC-PARAM v STRING-AT ;

: DOC-URI ( str -- str )  s" uri" URI-B @ DOC-STRING ;

: DOC-VERSION ( str -- n )
   {: p :}
   p s" version" DOC-PARAM JSON:T-INT <> if E-LSP-PARAMS throw then
   JSON:READ-INT {: v:n :}
   JSON:READ-CLOSE
   v ;

\ The file path a URI names, which is never longer than the URI.
: PATH ( str -- str )
   {: u :}
   PATH-SPAN u str> nip ROOM
   u PATH-SPAN @ URI:FILE>PATH {: n:n :}
   PATH-SPAN @ SPAN:$ str> drop n >str ;

\ A range is incremental sync, not offered.
: CHANGE-MEMBER ( JSON:reader -- JSON:reader )
   s" range" JSON:READ-STR-EQ? if E-LSP-PARAMS throw then
   s" text" JSON:READ-STR-EQ? if
      JSON:READ-NEXT JSON:T-STR <> if E-LSP-PARAMS throw then
      TEXT-B @ JSON:READ-STR str> nip TEXT-LEN !
   else
      JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   then ;

: CHANGE ( JSON:reader -- JSON:reader )
   JSON:READ-TOKEN JSON:T-OBJ <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin JSON:READ-NEXT JSON:T-KEY = while CHANGE-MEMBER repeat
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then ;

: CHANGES ( str -- )
   s" contentChanges" PARAM JSON:T-ARR <> if E-LSP-PARAMS throw then
   -1 TEXT-LEN !
   begin JSON:READ-NEXT JSON:T-ARR-END <> while CHANGE repeat
   JSON:READ-CLOSE
   TEXT-LEN @ 0 < if E-LSP-PARAMS throw then ;

: OPEN-SLOT ( str -- n )
   LSP-DOCS:DOC-FIND MATCH option
      none OF E-LSP-NOT-OPEN throw ENDOF
      some OF ENDOF
   ;MATCH ;

: INTO? ( JSON:reader str -- JSON:reader bool )
   JSON:READ-FIND-KEY 0= if false exit then
   JSON:READ-TOKEN JSON:T-OBJ = ;

\ ---- notifications -----------------------------------------------------------

\ An editor event changes checks that consumed this path. A watched disk
\ event also changes checks stopped at this path before they could read it.
\ A document holding the path itself still supplies its editor text.
: DIRTY-DEPENDENTS ( str bool -- )
   {: path include-unread:bool :}
   LSP-DOCS:DOC-SLOTS 0 ?do
      i LSP-DOCS:DOC-LIVE? if
         i LSP-DOCS:DOC-CANON$ path STR:= 0= if
            include-unread if
               i path LSP-DEFS:DEFS-DEPENDS?
            else
               i path LSP-DEFS:DEFS-READ?
            then if i LSP-DOCS:DOC-DIRTY then
         then
      then
   loop ;

: READERS-DIRTY-PATH ( str -- )  false DIRTY-DEPENDENTS ;

: READERS-DIRTY ( n -- )  LSP-DOCS:DOC-CANON$ READERS-DIRTY-PATH ;

\ Each handler reads all of its params before it changes a document, so one
\ that throws has changed nothing.
: DID-OPEN ( str -- str )
   {: p :}
   p DOC-URI {: u :}
   u PATH {: a :}
   p DOC-VERSION {: v:n :}
   p s" text" TEXT-B @ DOC-STRING {: t :}
   u v a t LSP-DOCS:DOC-OPEN
   u OPEN-SLOT READERS-DIRTY
   p ;

: DID-CHANGE ( str -- str )
   {: p :}
   p DOC-URI OPEN-SLOT {: slot:n :}
   p DOC-VERSION {: v:n :}
   p CHANGES
   slot v TEXT-B @ VEC:BYTES LSP-DOCS:DOC-CHANGE
   slot READERS-DIRTY
   p ;

: DID-CLOSE ( str -- str )
   {: p :}
   p DOC-URI OPEN-SLOT {: slot:n :}
   slot LSP-DIAG:RETRACT
   slot READERS-DIRTY
   slot LSP-DEFS:DEFS-DROP
   slot LSP-DOCS:DOC-CLOSE
   p ;

: WATCH-CHANGE ( JSON:reader -- JSON:reader )
   JSON:READ-TOKEN JSON:T-OBJ <> if E-LSP-PARAMS throw then
   begin JSON:READ-NEXT JSON:T-KEY = while
      s" uri" JSON:READ-STR-EQ? if
         JSON:READ-NEXT JSON:T-STR <> if E-LSP-PARAMS throw then
         URI-B @ JSON:READ-STR PATH
         SOURCE-ROOT:CANONICAL drop
         true DIRTY-DEPENDENTS
      else JSON:READ-NEXT drop JSON:READ-SKIP-VALUE then
   repeat ;

: DID-WATCH ( str -- str )
   {: p :}
   p s" changes" PARAM JSON:T-ARR <> if E-LSP-PARAMS throw then
   begin JSON:READ-NEXT JSON:T-ARR-END <> while WATCH-CHANGE repeat
   JSON:READ-CLOSE
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
: NOTIFIED ( JSON:value str -- )
   {: ms p :}
   ms s" exit" JSON:STRING= if s" " EXIT-CODE die then
   RUNNING? 0= if exit then
   ms s" textDocument/didOpen" JSON:STRING= if
      ms JSON:TEXT$ p [: DID-OPEN ;] GUARDED exit
   then
   ms s" textDocument/didChange" JSON:STRING= if
      ms JSON:TEXT$ p [: DID-CHANGE ;] GUARDED exit
   then
   ms s" textDocument/didClose" JSON:STRING= if
      ms JSON:TEXT$ p [: DID-CLOSE ;] GUARDED exit
   then
   ms s" textDocument/didSave" JSON:STRING= if LSP-DOCS:DOC-DIRTY-ALL exit then
   ms s" workspace/didChangeWatchedFiles" JSON:STRING= if
      ms JSON:TEXT$ p [: DID-WATCH ;] GUARDED
   then ;

\ ---- held messages -----------------------------------------------------------

: HELD? ( -- bool )  HELD-NEXT @ HELD-COUNT @ < ;

: HELD$ ( n -- str )
   {: k:n :}
   k 0= if 0 else k 1- HELD-ENDS @ then {: at:n :}
   HELD-B @ VEC:BYTES str> drop at +
   k HELD-ENDS @ at - >str ;

: HOLD ( str -- )
   HELD-B @ VEC:APPEND
   HELD-COUNT @ 1+ HELD-ENDS-RESERVE
   HELD-B @ VEC:COUNT HELD-COUNT @ HELD-ENDS !
   1 HELD-COUNT +! ;

\ The text of params a message carries, empty for none.
: GIVEN$ ( option<JSON:value> -- str )
   MATCH option
      none OF s" " ENDOF
      some OF JSON:TEXT$ ENDOF
   ;MATCH ;

: NOTICE? ( n str -- str bool )
   {: k:n name :}
   k HELD$ JSON-RPC:>MESSAGE MATCH JSON-RPC:message
      request OF drop drop drop s" " false ENDOF
      notification OF
         {: m p :} p GIVEN$ m name JSON:STRING=
      ENDOF
      success OF drop drop s" " false ENDOF
      failure OF drop drop s" " false ENDOF
      invalid OF drop 2drop drop s" " false ENDOF
   ;MATCH ;

\ What a cancel's id is matched against for the request with this id: a
\ string's decoded text, in ID-B, and true; else its JSON text and false.
: ID-KEY ( JSON:value -- str bool )
   JSON:TEXT$ {: r :}
   JSON-ST r JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR = if ID-B @ JSON:READ-STR str> 2drop true else false then
   {: str:bool :}
   JSON:READ-CLOSE
   str if ID-B @ VEC:BYTES true exit then
   r false ;

\ Whether params.id is the id this key gives: a string with this decoded text
\ when STR is true, else a value with this JSON text.
: ID-PARAM? ( str str bool -- bool )
   {: p k str:bool :}
   p str> nip 0= if false exit then
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if s" id" JSON:READ-FIND-KEY else false then
   if
      JSON:READ-TOKEN JSON:T-STR = if
         str if k JSON:READ-STR-EQ? else false then
      else
         str 0= if JSON:READ-SPAN$ k STR:= else false then
      then
   else false then
   {: same:bool :}
   JSON:READ-CLOSE
   same ;

: CANCELLED? ( JSON:value -- bool )
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
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ =
   if s" textDocument" INTO? else false then
   if s" uri" JSON:READ-FIND-KEY else false then
   if JSON:READ-TOKEN JSON:T-STR = else false then
   {: named:bool :}
   named if URI-B @ JSON:READ-STR str> 2drop then
   JSON:READ-CLOSE
   named 0= if -1 exit then
   URI-B @ VEC:BYTES LSP-DOCS:DOC-FIND MATCH option
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
   AT-SLOT @ LSP-DOCS:DOC-CANON$ path STR:= if true exit then
   AT-SLOT @ path LSP-DEFS:DEFS-DEPENDS? ;

\ ---- input -------------------------------------------------------------------

: SETTLED? ( -- bool )  FAULT @ 0<> AT-END @ or ;

: RECEIVE ( n -- )
   {: len:n :}
   BODY-SPAN len ROOM
   INPUT BODY-SPAN @ CONTENT-LENGTH:BODY
   BODY-SPAN @ len SPAN:TAKE SPAN:$ HOLD ;

\ Whether stdin holds input, or its end, a poll that a signal cut short asked
\ again.
: POLLED? ( -- bool )
   begin FD:STDIN 0 >MS PROCESS:POLL-IN COUNT>N dup FD:EINTR# negate = while drop repeat
   0<> ;

\ Whether a message waits: begun in the reader's buffer, or on stdin; or
\ reading is over, which the loop then acts on.
: INPUT? ( -- bool )
   SETTLED? if true exit then
   INPUT CONTENT-LENGTH:PENDING? if true exit then
   POLLED? ;

: READ-MESSAGE ( -- )
   INPUT CONTENT-LENGTH:NEXT-LENGTH MATCH option
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

: READ? ( JSON:value str [ str -- str ] -- bool )
   {: i p q :}
   p q catch {: code:n :} drop
   code REASON:TEXT$ {: why :}
   code 0= if true exit then
   code IGNORABLE? 0= if code throw then
   why str> nip 0= if s" invalid params" else why then {: msg :}
   i JSON-RPC:INVALID-PARAMS msg REPLY-ERROR
   false ;

: QUERY! ( str -- str )
   {: p :}
   p s" query" PARAM QUERY-B @ STRING-AT str> 2drop
   p ;

: QUERY$ ( -- str )  QUERY-B @ VEC:BYTES ;

: SYMBOLS ( JSON:value str -- )
   {: i p :}
   i p [: QUERY! ;] READ? 0= if exit then
   WRITER i JSON-RPC:RESULT QUERY$ LSP-SYMBOLS:ANSWER JSON-RPC:END SENT ;

: POSITION-INT ( str str -- n )
   {: p k :}
   p s" position" PARAM JSON:T-OBJ <> if E-LSP-PARAMS throw then
   k JSON:READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   JSON:READ-TOKEN JSON:T-INT <> if E-LSP-PARAMS throw then
   JSON:READ-INT {: v:n :}
   JSON:READ-CLOSE
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
   dup LSP-DOCS:DOC-DIRTY? if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-INPUTS-FRESH? 0= if CHECK-NOW exit then
   dup LSP-DEFS:DEFS-UNREAD-RETRY? if CHECK-NOW else drop then ;

: AT-BYTE! ( -- )
   AT-SLOT @ LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   AT-LINE @ AT-CHAR @ LSP-TEXT:OFFSET-AT AT-BYTE ! ;

\ Whether no change to the document in AT-SLOT was read behind the request;
\ else it is answered -32801, since it asked about the text the change
\ replaces, and false.
: UNCHANGED? ( JSON:value -- bool )
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
: CHECK-CURRENT? ( JSON:value -- bool )
   {: i :}
   AT-SLOT @ CHECK-WAITING
   READ-WAITING
   i UNCHANGED? 0= if false exit then
   AT-SLOT @ CURRENT-INPUTS-FRESH? if true exit then
   i CONTENT-MODIFIED s" content modified" REPLY-ERROR
   false ;

: DOCUMENT? ( JSON:value str -- bool )
   {: i p :}
   i p [: SLOT! ;] READ? 0= if false exit then
   i UNCHANGED? ;

: POSITION? ( JSON:value str -- bool )
   {: i p :}
   i p [: POSITION! ;] READ? 0= if false exit then
   i UNCHANGED? ;

: AT-READ? ( JSON:value str -- bool )
   {: i p :}
   i p POSITION? 0= if false exit then
   i CHECK-CURRENT? 0= if false exit then
   AT-BYTE!
   true ;

: DEFINITION ( JSON:value str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ AT-BYTE @ LSP-DEFINITION:ANSWER JSON-RPC:END SENT ;

: DOCUMENT-SYMBOLS ( JSON:value str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ LSP-OUTLINE:ANSWER JSON-RPC:END SENT ;

: HOVER ( JSON:value str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ AT-BYTE @ HOVER-MD @ LSP-HOVER:ANSWER JSON-RPC:END SENT ;

: HIGHLIGHT ( JSON:value str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ AT-BYTE @ LSP-HIGHLIGHT:ANSWER JSON-RPC:END SENT ;

: SEMANTIC-TOKENS ( JSON:value str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ LSP-TOKENS:ANSWER JSON-RPC:END SENT ;

: WITH-DECL! ( str -- str )
   {: p :}
   p s" context" PARAM JSON:T-OBJ <> if E-LSP-PARAMS throw then
   s" includeDeclaration" JSON:READ-FIND-KEY 0= if E-LSP-PARAMS throw then
   JSON:READ-TOKEN {: t:n :}
   t JSON:T-TRUE <> t JSON:T-FALSE <> and if E-LSP-PARAMS throw then
   t JSON:T-TRUE = WITH-DECL !
   JSON:READ-CLOSE
   p ;

: REFS! ( str -- str )  POSITION! WITH-DECL! ;

: NEW-NAME! ( str -- str )
   {: p :}
   p s" newName" PARAM NAME-B @ STRING-AT str> 2drop
   p ;

: NEW-NAME$ ( -- str )  NAME-B @ VEC:BYTES ;

: RENAME! ( str -- str )  POSITION! NEW-NAME! ;

\ The request's id copied out of the held bodies, which reading input during
\ a check can grow and move.
: KEEP-ID ( JSON:value -- JSON:value )
   JSON:TEXT$ str> {: a:^ u:n :}
   KEPT-ID u ROOM
   a u >str KEPT-ID @ SPAN:COPY
   KEPT-ID @ SPAN:$ str> drop u >str JSON:PARSE ;

: GATHER-CHANGED? ( -- bool )
   [: LSP-REFERENCES:TOUCHES? ;] HELD-EDITED? ;

: HALTED? ( JSON:value -- bool )
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

: GATHERED-FROM? ( n JSON:value -- bool )
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

: GATHERED? ( JSON:value -- bool )
   {: id :}
   0 begin
      dup id GATHERED-FROM? 0= if drop false exit then
      drop LSP-REFERENCES:SUBJECT-N
   LSP-REFERENCES:MORE? 0= until
   drop
   id HALTED? if false exit then
   LSP-REFERENCES:SETTLE
   true ;

: REFERENCES ( JSON:value str -- )
   {: i p :}
   i p [: REFS! ;] READ? 0= if exit then
   i UNCHANGED? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   AT-BYTE!
   AT-SLOT @ AT-BYTE @ WITH-DECL @ LSP-REFERENCES:SELECT
   LSP-REFERENCES:REJECTED? if
      i REQUEST-FAILED LSP-REFERENCES:BLOCKED$ REPLY-ERROR
      exit
   then
   i GATHERED? 0= if exit then
   LSP-REFERENCES:UNCOVERED? if WRITER LSP-REFERENCES:WARNING JSON-RPC:END SENT then
   WRITER i JSON-RPC:RESULT LSP-REFERENCES:ANSWER JSON-RPC:END SENT ;

: RENAMEABLE? ( JSON:value -- bool )
   {: i :}
   AT-SLOT @ LSP-REFERENCES:RENAME-VERIFIED? 0= if
      i REQUEST-FAILED LSP-REFERENCES:BLOCKED$ REPLY-ERROR
      false exit
   then
   LSP-REFERENCES:REJECTED? if
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
: APPLIES? ( JSON:value -- bool )
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

: PREPARE-RENAME ( JSON:value str -- )
   {: i p :}
   i p AT-READ? 0= if exit then
   AT-SLOT @ AT-BYTE @ LSP-REFERENCES:PLACE
   i RENAMEABLE? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ AT-BYTE @ LSP-RENAME:PREPARED JSON-RPC:END SENT ;

: RENAME ( JSON:value str -- )
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
   WRITER i JSON-RPC:RESULT NEW-NAME$ LSP-RENAME:ANSWER JSON-RPC:END SENT ;

: COMPLETION ( JSON:value str -- )
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
   WRITER i JSON-RPC:RESULT AT-SLOT @ LSP-COMPLETION:ANSWER JSON-RPC:END SENT ;

: DOCUMENT-LINKS ( JSON:value str -- )
   {: i p :}
   i p DOCUMENT? 0= if exit then
   i CHECK-CURRENT? 0= if exit then
   WRITER i JSON-RPC:RESULT AT-SLOT @ LSP-LINKS:ANSWER JSON-RPC:END SENT ;

: LISTS-MARKDOWN? ( JSON:reader -- JSON:reader bool )
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-STR = if
         s" markdown" JSON:READ-STR-EQ? if true exit then
      else
         JSON:READ-SKIP-VALUE
      then
   repeat
   drop false ;

: MARKDOWN? ( str -- bool )
   {: p :}
   p str> nip 0= if false exit then
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ =
   if s" capabilities" INTO? else false then
   if s" textDocument" INTO? else false then
   if s" hover" INTO? else false then
   if s" contentFormat" JSON:READ-FIND-KEY else false then
   if JSON:READ-TOKEN JSON:T-ARR = else false then
   if LISTS-MARKDOWN? else false then
   {: md:bool :}
   JSON:READ-CLOSE
   md ;

\ A workspace folder by the URI string the reader is at; one that names no
\ file here is passed over.
: ROOT-AT ( JSON:reader -- JSON:reader )
   URI-B @ JSON:READ-STR {: u :}
   [: u PATH LSP-WORKSPACE:ROOT+ ;] catch {: code:n :}
   code 0= if exit then
   code E-PATH-RANGE =  code E-PATH-NUL = or
   code E-URI-LAST >= code E-URI-FIRST <= and  or 0= if code throw then ;

: FOLDER-MEMBER ( JSON:reader -- JSON:reader )
   s" uri" JSON:READ-STR-EQ? if
      JSON:READ-NEXT JSON:T-STR = if ROOT-AT else JSON:READ-SKIP-VALUE then
   else
      JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   then ;

: FOLDER ( JSON:reader -- JSON:reader )
   JSON:READ-TOKEN JSON:T-OBJ = if
      begin JSON:READ-NEXT JSON:T-KEY = while FOLDER-MEMBER repeat
   else
      JSON:READ-SKIP-VALUE
   then ;

: WORKSPACE! ( str -- )
   {: p :}
   p str> nip 0= if exit then
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ =
   if s" workspaceFolders" JSON:READ-FIND-KEY else false then
   if JSON:READ-TOKEN JSON:T-ARR = else false then
   {: folders:bool :}
   folders if begin JSON:READ-NEXT JSON:T-ARR-END <> while FOLDER repeat then
   JSON:READ-CLOSE
   folders if exit then
   JSON-ST p JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ =
   if s" rootUri" JSON:READ-FIND-KEY else false then
   if JSON:READ-TOKEN JSON:T-STR = else false then
   if ROOT-AT then
   JSON:READ-CLOSE ;

: INITIALIZE! ( str -- str )
   {: p :}
   p MARKDOWN? HOVER-MD !
   p WORKSPACE!
   p ;

: STARTING-REQUEST ( JSON:value JSON:value str -- )
   {: i m p :}
   m s" initialize" JSON:STRING= if
      i p [: INITIALIZE! ;] READ? 0= if exit then
      i REPLY-CAPABILITIES
      construct state running STATE!
   else
      i NOT-INITIALIZED s" server not initialized" REPLY-ERROR
   then ;

: RUNNING-REQUEST ( JSON:value JSON:value str -- )
   {: i m p :}
   i CANCELLED? if
      i REQUEST-CANCELLED s" request cancelled" REPLY-ERROR
      exit
   then
   m s" shutdown" JSON:STRING= if
      i REPLY-NULL
      construct state shut-down STATE!
      exit
   then
   m s" initialize" JSON:STRING= if
      i JSON-RPC:INVALID-REQUEST s" server already initialized" REPLY-ERROR
      exit
   then
   m s" workspace/symbol" JSON:STRING= if i p SYMBOLS exit then
   m s" textDocument/documentSymbol" JSON:STRING= if i p DOCUMENT-SYMBOLS exit then
   m s" textDocument/definition" JSON:STRING= if i p DEFINITION exit then
   m s" textDocument/declaration" JSON:STRING= if i p DEFINITION exit then
   m s" textDocument/references" JSON:STRING= if i p REFERENCES exit then
   m s" textDocument/prepareRename" JSON:STRING= if i p PREPARE-RENAME exit then
   m s" textDocument/rename" JSON:STRING= if i p RENAME exit then
   m s" textDocument/hover" JSON:STRING= if i p HOVER exit then
   m s" textDocument/documentHighlight" JSON:STRING= if i p HIGHLIGHT exit then
   m s" textDocument/semanticTokens/full" JSON:STRING= if i p SEMANTIC-TOKENS exit then
   m s" textDocument/completion" JSON:STRING= if i p COMPLETION exit then
   m s" textDocument/documentLink" JSON:STRING= if i p DOCUMENT-LINKS exit then
   i JSON-RPC:METHOD-NOT-FOUND s" method not found" REPLY-ERROR ;

: REQUESTED ( JSON:value JSON:value str -- )
   {: held m p :}
   held KEEP-ID {: i :}
   STATE@ MATCH state
      starting OF i m p STARTING-REQUEST ENDOF
      running OF i m p RUNNING-REQUEST ENDOF
      shut-down OF i JSON-RPC:INVALID-REQUEST s" server shut down" REPLY-ERROR ENDOF
   ;MATCH ;

\ ---- the loop ----------------------------------------------------------------

: DISPATCH ( JSON-RPC:message -- )
   MATCH JSON-RPC:message
      request OF {: i m p :} i m p GIVEN$ REQUESTED ENDOF
      notification OF {: m p :} m p GIVEN$ NOTIFIED ENDOF
      success OF drop drop ENDOF
      failure OF drop drop ENDOF
      invalid OF {: c:n w:^ wu:n i :} i c w wu >str REPLY-ERROR ENDOF
   ;MATCH ;

\ The first held message served. It is passed before it is dispatched, so the
\ messages held behind it are the ones from HELD-NEXT on; once none is left,
\ their room is reused.
: TURN ( -- )
   HELD-NEXT @ {: k:n :}
   1 HELD-NEXT +!
   k HELD$ JSON-RPC:>MESSAGE DISPATCH
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
   LAST-CHECKED @ LSP-DOCS:DOC-NEXT-DIRTY MATCH option
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
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW URI-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW TEXT-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW QUERY-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW NAME-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ID-B !
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
   INPUT FD:STDIN HEAD-BUF MAX-BODY CONTENT-LENGTH:BIND
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

;package
