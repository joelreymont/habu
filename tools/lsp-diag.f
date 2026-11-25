\ lsp-diag.f - the checker's packets (tools/check-verify-core.f schema-1 JSON,
\ one per line, each naming its file by canonical absolute path) published as LSP
\ diagnostics for the checked document and each other file they name. A line
\ that is not an object with a string absolute `file` goes to stderr after
\ `lsp: `, as it came. Strings are copied as the packet escapes them, never
\ decoded. PROCESS-GLOBAL: the buffers belong to the server's one task.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/uri.f
require src/lib/content-length.f
require src/lib/json.f
require src/lib/json-rpc.f
require tools/lsp-docs.f
require tools/lsp-line.f
require tools/lsp-text.f

package LSP-DIAG
using JSON
using JSON-RPC
using LSP-DOCS
using LSP-LINE
using LSP-TEXT

private

47 constant SLASH
1 constant ERROR-SEVERITY                \ LSP DiagnosticSeverity.Error
2 constant WARNING-SEVERITY              \ LSP DiagnosticSeverity.Warning

TYPED-VARIABLE JSON-ST JSON:cursor       \ JSON reader storage for reading a packet line
TYPED-VARIABLE PUB-OUT ^VEC:vector<u8>   \ a notification's bytes, grown as written
TYPED-VARIABLE NEW-B ^VEC:vector<u8> SCRATCH-ONLY \ the files this check published lists for
TYPED-VARIABLE W JSON:writer
TYPED-VARIABLE FILE-B ^VEC:vector<u8> SCRATCH-ONLY \ a packet's file, decoded
PATH-CAP 3 * 7 + constant URI-BYTES    \ file:// and each byte of a path escaped
create URI-ROW URI-BYTES SCRATCH-ALLOT
: URI-SPAN ( -- SPAN:span<u8> ) URI-ROW URI-BYTES SPAN:MAKE ;

TYPED-VARIABLE LINE-A ^u8             \ the packet line being screened
variable LINE-U
TYPED-VARIABLE PKTS-A ^u8             \ the packets being published
variable PKTS-U
variable SELF                            \ the slot whose lists are published
TYPED-VARIABLE LIST-A ^u8             \ the file the list being written is for,
variable LIST-U
variable ITEMS                           \ and the diagnostics in it so far

: NEW$ ( -- str )  NEW-B @ VEC:BYTES ;
: FILE$ ( -- str )  FILE-B @ VEC:BYTES ;

: ERR ( str -- )
   {: s:str :}
   FD:STDERR s FD:WRITE-FULL ;

\ ---- packets -------------------------------------------------------------------

\ FILE-B the line's file, decoded, or empty for a line not an object with a
\ string file; any line that is not one JSON value throws.
: SCAN ( -- )
   FILE-B @ 0 VEC:RESIZE
   JSON-ST LINE-A @ LINE-U @ >str READ-INIT
   READ-NEXT T-OBJ <> if READ-CLOSE exit then
   s" file" READ-FIND-KEY 0= if READ-CLOSE exit then
   READ-TOKEN T-STR <> if READ-CLOSE exit then
   FILE-B @ READ-STR drop
   READ-FINISH ;

\ Whether the line is a diagnostic: an object whose file names an absolute
\ path, which FILE$ then holds.
: SCREENED ( str -- bool )
   {: s:str :}
   s STR:DATA LINE-A !
   s STR:LEN LINE-U !
   [: SCAN ;] catch {: code:n :}
   code 0<> if
      code JSON:INPUT-FAULT? 0= if code throw then
      FILE-B @ 0 VEC:RESIZE
   then
   FILE$ {: f:str :}
   f STR:LEN 0 > if f STR:DATA c@ SLASH = exit then
   false ;

\ ---- diagnostics ---------------------------------------------------------------
\ One diagnostic per packet, fields verbatim. range: byte_start and byte_end in
\ LSP counting (lines by LF, characters in UTF-16 units); a missing start is 0, a
\ missing end the start, both held to the text and the end to no less than the
\ start; the packet's numbers stay in data. severity: 1 for an error, 2 for a
\ warning, else the client's. code, and source "habu". message: the packet's,
\ then `evaluated: ` and the evaluated place's source line when the packet has
\ one, then a `key: value` line for each string note in the packet's order, then
\ `help: ` and its help when non-empty. data: the whole packet.

: QUOTE ( ^JSON:writer -- ^JSON:writer )  s\" \"" WRITE-RAW ;

: QUOTED ( ^JSON:writer str -- ^JSON:writer )
   {: s:str :}
   QUOTE s WRITE-RAW QUOTE ;

: SEVERITY ( ^JSON:writer str -- ^JSON:writer )
   s" severity" STRING-MEMBER 0= if drop exit then
   {: v:str :}
   v s" error" STR:= if WRITE-COMMA s" severity" ERROR-SEVERITY WRITE-FIELD-U exit then
   v s" warning" STR:= if WRITE-COMMA s" severity" WARNING-SEVERITY WRITE-FIELD-U then ;

: CODE-FIELD ( ^JSON:writer str -- ^JSON:writer )
   s" code" STRING-MEMBER 0= if drop exit then
   {: c:str :}
   WRITE-COMMA s" code" WRITE-KEY c QUOTED ;

: HELP-LINE ( ^JSON:writer str -- ^JSON:writer )
   s" help" STRING-MEMBER over STR:LEN 0 > and 0= if drop exit then
   {: s:str :}
   s" \nhelp: " WRITE-RAW s WRITE-RAW ;

\ A note with a string value as its line; any other value is passed over.
: NOTE-LINE ( JSON:reader ^JSON:writer -- JSON:reader )
   {: w :}
   READ-SPAN$ {: k:str :}
   READ-NEXT T-STR <> if READ-SKIP-VALUE exit then
   READ-SPAN$ {: v:str :}
   w s" \n" WRITE-RAW k WRITE-RAW s" : " WRITE-RAW
   v WRITE-RAW drop ;

: NOTES ( ^JSON:writer str -- ^JSON:writer )
   {: w s:str :}
   JSON-ST s READ-INIT
   READ-NEXT drop
   s" notes" READ-FIND-KEY 0= if READ-CLOSE w exit then
   READ-TOKEN T-OBJ <> if READ-CLOSE w exit then
   begin READ-NEXT T-KEY = while w NOTE-LINE repeat
   READ-CLOSE w ;

: EVALUATED-LINE ( ^JSON:writer str -- ^JSON:writer )
   {: w s:str :}
   JSON-ST s READ-INIT
   READ-NEXT drop
   s" evaluated" READ-FIND-KEY 0= if READ-CLOSE w exit then
   READ-TOKEN T-OBJ <> if READ-CLOSE w exit then
   s" source_line" READ-FIND-KEY 0= if READ-CLOSE w exit then
   READ-TOKEN T-STR <> if READ-CLOSE w exit then
   READ-SPAN$ {: v:str :}
   READ-CLOSE
   w s" \nevaluated: " WRITE-RAW v WRITE-RAW ;

: MESSAGE ( ^JSON:writer str -- ^JSON:writer )
   {: s:str :}
   QUOTE
   s s" message" STRING-MEMBER if WRITE-RAW else drop then
   s EVALUATED-LINE
   s NOTES
   s HELP-LINE
   QUOTE ;

: DIAGNOSTIC ( ^JSON:writer str -- ^JSON:writer )
   {: s:str :}
   WRITE-OBJECT-START
   s RANGE
   s SEVERITY
   s CODE-FIELD
   WRITE-COMMA s" source" s" habu" WRITE-FIELD-S
   WRITE-COMMA s" message" WRITE-KEY s MESSAGE
   WRITE-COMMA s" data" s WRITE-FIELD-RAW
   WRITE-OBJECT-END ;

\ ---- lists ---------------------------------------------------------------------

\ A publishDiagnostics notification begun for the URI, with the version when
\ there is one, up to its diagnostics' opening bracket.
: OPENING ( str option<n> -- ^JSON:writer )
   {: u:str v :}
   W PUB-OUT @ JSON:OPEN
   s" textDocument/publishDiagnostics" NOTIFY
   WRITE-OBJECT-START
   s" uri" u WRITE-FIELD-S WRITE-COMMA
   v MATCH option
      some OF >r s" version" r> WRITE-FIELD-INT WRITE-COMMA ENDOF
      none OF ENDOF
   ;MATCH
   s" diagnostics" WRITE-KEY WRITE-ARRAY-START ;

: CLOSING ( ^JSON:writer -- )
   WRITE-ARRAY-END WRITE-OBJECT-END END
   {: w :}
   w WRITTEN$ {: s:str :}
   w JSON:CLOSE
   FD:STDOUT s CONTENT-LENGTH:SEND ;

: ITEM ( str -- )
   {: s:str :}
   s SCREENED 0= if exit then
   FILE$ LIST-A @ LIST-U @ >str STR:= 0= if exit then
   ITEMS @ 0 > if W WRITE-COMMA drop then
   W s DIAGNOSTIC drop
   1 ITEMS +! ;

\ The list for the file in LIST-A, its positions counted in the text TEXT!
\ named, published at the URI.
: LIST ( str option<n> -- )
   OPENING drop
   0 ITEMS !
   PKTS-A @ PKTS-U @ >str [: ITEM ;] EACH-LINE
   W CLOSING ;

: LIST-URI ( -- str )
   LIST-A @ LIST-U @ >str URI-SPAN URI:PATH>FILE {: n:n :}
   URI-SPAN SPAN:$ STR:DATA n >str ;

\ ---- the files besides the document ---------------------------------------------
\ The document's own list always goes out, with its version, empty when no packet
\ names it. Each other named file gets an unversioned list at its file URI, its
\ positions counted in its text on disk, unless an open document holds it. A file
\ the earlier check listed and this one does not is withdrawn with an empty list,
\ unless an open document holds it or another document's last check published
\ it: that document is checked again, so only the check that would publish a
\ list withdraws it. RETRACT does the same for a closing document, its own list
\ first.
: SLOT? ( option<n> -- bool )
   MATCH option
      some OF drop true ENDOF
      none OF false ENDOF
   ;MATCH ;

\ Whether an open document is the file the checker names by this path.
: HELD-OPEN? ( str -- bool )  DOC-HOLDING SLOT? ;

\ Whether an open document's own list is at this path's file URI: the client
\ opened it by this path.
: SHOWN? ( str -- bool )  DOC-OPENED-AS SLOT? ;

\ Whether another open document's last check published a list for this path;
\ each such document waits for a check again.
: CLAIMED? ( str -- bool )
   {: f:str :}
   false
   DOC-SLOTS 0 ?do
      i SELF @ <> i DOC-LIVE? and if
         i DOC-DEPS$ f HAS-LINE? if i DOC-DIRTY drop true then
      then
   loop ;

\ Appends the path, LF-ended, to the files published; answers that copy, which
\ outlives FILE-B's bytes.
: KEEP-NEW ( str -- str )
   {: f:str :}
   NEW$ STR:LEN {: at:n :}
   f NEW-B @ VEC:APPEND
   s\" \n" NEW-B @ VEC:APPEND
   NEW$ STR:DATA at + f STR:LEN >str ;

: DEP ( str -- )
   {: s:str :}
   s SCREENED 0= if s" lsp: " ERR s ERR s\" \n" ERR exit then
   FILE$ SELF @ DOC-CANON$ STR:= if exit then
   NEW$ FILE$ HAS-LINE? if exit then
   FILE$ HELD-OPEN? if exit then
   FILE$ KEEP-NEW dup STR:LEN LIST-U ! STR:DATA LIST-A !
   LIST-A @ LIST-U @ >str FILE-SNAPSHOT!
   LIST-URI OPTION:NONE LIST ;

: WITHDRAW ( str -- )
   {: f:str :}
   NEW$ f HAS-LINE? if exit then
   f SHOWN? if exit then
   f CLAIMED? if exit then
   f STR:DATA LIST-A !
   f STR:LEN LIST-U !
   LIST-URI OPTION:NONE OPENING CLOSING ;

public

\ Readies the buffers; the server calls it once, before any other word here.
: PREPARE ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW FILE-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW PUB-OUT !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW NEW-B ! ;

: PUBLISH ( n str -- )
   {: slot:n p:str :}
   p STR:DATA PKTS-A !
   p STR:LEN PKTS-U !
   slot SELF !
   NEW-B @ 0 VEC:RESIZE
   slot DOC-CANON$ dup STR:LEN LIST-U ! STR:DATA LIST-A !
   slot DOC-TEXT$ TEXT!
   slot DOC-URI$ slot DOC-VERSION@ OPTION:SOME LIST
   p [: DEP ;] EACH-LINE
   slot DOC-DEPS$ [: WITHDRAW ;] EACH-LINE
   NEW$ slot DOC-DEPS! ;

\ The document in this slot is closing: an empty list for it, without a
\ version, and its check's lists for other files withdrawn.
: RETRACT ( n -- )
   {: slot:n :}
   slot SELF !
   NEW-B @ 0 VEC:RESIZE
   slot DOC-URI$ OPTION:NONE OPENING CLOSING
   slot DOC-DEPS$ [: WITHDRAW ;] EACH-LINE ;

;using
;using
;using
;using
;using
;package
