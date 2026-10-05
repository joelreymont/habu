\ lsp-diag.f - the checker's packets published as LSP diagnostics, for the
\ document checked and for each file besides it that they name.
\
\ PUBLISH takes the packets of a check that completed: tools/check-verify-core.f's
\ schema-1 JSON objects, one per line, each naming the file it is about by its
\ canonical absolute path. A line that is not a JSON object with a string
\ `file` naming an absolute path is no diagnostic: it is written to stderr after
\ `lsp: `, as it came, and published nowhere.
\
\ The document's own list always goes out, with its version: the packets whose
\ file is the path its check named it by, an empty list when there are none.
\ Each other file the packets name gets its list too, without a version, at the
\ file URI of its path, its positions counted in its text on disk - unless an
\ open document holds that file, whose own check owns its list. Each file a
\ document's earlier check published a list for and this one names no more is
\ withdrawn with an empty list, unless an open document holds it, or another
\ document's last check published it: that document is checked again instead,
\ so a list is only ever withdrawn by the check that would have published it.
\ RETRACT does the same for a document closing, its own list emptied first.
\
\ Each packet becomes one diagnostic, its fields taken verbatim:
\ - range: byte_start and byte_end, counted through the text as LSP counts, the
\   line by LF and the character in UTF-16 units. A missing start is 0, a
\   missing end the start; both are held to the text and the end to no less
\   than the start, so the range is valid whatever the packet says. The packet's
\   own numbers stay in data.
\ - severity: 1 (Error) for verdict rejected or uncheckable, 3 (Information)
\   for deferred; any other verdict leaves the severity to the client.
\ - code, and source "habu".
\ - message: the packet's message; without one, its code, then its suggestion
\   when not empty, then a line `expected: ` and a line `actual: ` with those
\   members when present.
\ - data: the packet, whole.
\ Strings are copied as the packet escapes them, so nothing is decoded or
\ re-encoded on the way.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the buffers and the list being written belong
\ to the server's one task.

require lib/errors.f
require lib/string.f
require lib/span.f
require lib/byte-buffer.f
require lib/adt/option.f
require lib/fd-io.f
require lib/fs.f
require lib/uri.f
require lib/content-length.f
require lib/json-read.f
require lib/json-write.f
require lib/json-rpc.f
require tools/lsp-docs.f
require tools/lsp-line.f
require tools/lsp-text.f

package LSP-DIAG
using JSON-WRITE
using JSON-RPC
using LSP-DOCS
using LSP-LINE
using LSP-TEXT

private

47 constant SLASH
1 constant ERROR-SEVERITY                \ LSP DiagnosticSeverity.Error
3 constant INFORMATION-SEVERITY          \ LSP DiagnosticSeverity.Information

create JR-ST JR:STORAGE-BYTES allot      \ JR storage for screening a packet line
create PUB-B BUF:HDR-BYTES allot         \ a notification's bytes, grown as written
create NEW-B BUF:HDR-BYTES allot         \ the files this check published lists for
TYPED-VARIABLE W JSON-WRITE:writer
create FILE-BUF FS-PATH-CAP allot        \ a packet's file, decoded
variable FILE-U
FS-PATH-CAP 3 * 7 + SPAN-BUFFER: URI-SPAN  \ a file's URI: file:// and each byte escaped

TYPED-VARIABLE LINE-A ptr u8             \ the packet line being screened
variable LINE-U
TYPED-VARIABLE PKTS-A ptr u8             \ the packets being published
variable PKTS-U
variable SELF                            \ the slot whose lists are published
TYPED-VARIABLE LIST-A ptr u8             \ the file the list being written is for,
variable LIST-U
variable ITEMS                           \ and the diagnostics in it so far

: NEW$ ( -- ptr u8 n )  NEW-B BUF:SPAN$ BUF:BLEN>N ;
: FILE$ ( -- ptr u8 n )  FILE-BUF FILE-U @ ;

\ Writes these bytes to stderr.
: ERR ( ptr u8 n -- )
   {: a:ptr u:n :}
   2 >FD a u FD-IO:WRITE-FULL ;

\ ---- packets -------------------------------------------------------------------

\ The line read as a packet: FILE-U the length of its file, decoded into
\ FILE-BUF, or -1 for a line that is not an object with a string file. The
\ rest of the line is read to its end, so JR throws for any line that is not
\ one JSON value.
: SCAN ( -- )
   -1 FILE-U !
   JR-ST JR:STORAGE-BYTES LINE-A @ LINE-U @ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE exit then
   s" file" JR:FIND-KEY 0= if JR:CLOSE exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE exit then
   FILE-BUF FS-PATH-CAP JR:STR >r
   begin JR:NEXT JR:T-END = until
   JR:CLOSE
   r> FILE-U ! ;

\ Whether the line is a diagnostic: an object whose file names an absolute
\ path, which FILE$ then holds.
: SCREENED ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a LINE-A !
   u LINE-U !
   [: SCAN ;] catch {: code:n :}
   code 0<> if
      code E-JR-LAST >= code E-JR-FIRST <= and 0= if code throw then
      -1 FILE-U !
   then
   FILE-U @ 0 > if FILE-BUF c@ SLASH = exit then
   false ;

\ ---- diagnostics ---------------------------------------------------------------

: QUOTE ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )  s\" \"" RAW ;

\ A string from its raw text, as it was escaped.
: QUOTED ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n :}
   QUOTE a u RAW QUOTE ;

: SEVERITY ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   s" verdict" STRING-MEMBER 0= if 2drop exit then
   {: v:ptr vu:n :}
   v vu s" rejected" STR=  v vu s" uncheckable" STR= or if
      COMMA s" severity" ERROR-SEVERITY FIELD-U exit
   then
   v vu s" deferred" STR= if COMMA s" severity" INFORMATION-SEVERITY FIELD-U then ;

: CODE-FIELD ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   s" code" STRING-MEMBER 0= if 2drop exit then
   {: c:ptr cu:n :}
   COMMA s" code" KEY c cu QUOTED ;

\ The suggestion on a line of its own, when the packet has one not empty.
: SUGGESTION ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   s" suggestion" STRING-MEMBER over 0 > and 0= if 2drop exit then
   {: s:ptr su:n :}
   s" \n" RAW s su RAW ;

\ The member K on a line of its own, `K: ` then its text, when present.
: PIECE ( ptr JSON-WRITE:writer ptr u8 n ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n k:ptr ku:n :}
   a u k ku STRING-MEMBER 0= if 2drop exit then
   {: v:ptr vu:n :}
   s" \n" RAW k ku RAW s" : " RAW v vu RAW ;

: MESSAGE ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n :}
   a u s" message" STRING-MEMBER if QUOTED exit then
   2drop
   QUOTE
   a u s" code" STRING-MEMBER if RAW else 2drop then
   a u SUGGESTION
   a u s" expected" PIECE
   a u s" actual" PIECE
   QUOTE ;

: DIAGNOSTIC ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n :}
   OBJECT-START
   a u RANGE
   a u SEVERITY
   a u CODE-FIELD
   COMMA s" source" s" habu" FIELD-S
   COMMA s" message" KEY a u MESSAGE
   COMMA s" data" a u FIELD-RAW
   OBJECT-END ;

\ ---- lists ---------------------------------------------------------------------

\ A publishDiagnostics notification begun for the URI, with the version when
\ there is one, up to its diagnostics' opening bracket.
: OPENING ( ptr u8 n option<n> -- ptr JSON-WRITE:writer )
   {: u:ptr uu:n v :}
   W PUB-B JSON-WRITE:OPEN-BUF
   s" textDocument/publishDiagnostics" NOTIFY
   OBJECT-START
   s" uri" u uu FIELD-S COMMA
   v MATCH option
      some OF >r s" version" r> FIELD-INT COMMA ENDOF
      none OF ENDOF
   ;MATCH
   s" diagnostics" KEY ARRAY-START ;

\ The notification ended and sent.
: CLOSING ( ptr JSON-WRITE:writer -- )
   ARRAY-END OBJECT-END END
   {: w :}
   w JSON-WRITE:$ {: a:ptr u:n :}
   w JSON-WRITE:CLOSE
   1 >FD a u CONTENT-LENGTH:SEND ;

: ITEM ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u SCREENED 0= if exit then
   FILE$ LIST-A @ LIST-U @ STR= 0= if exit then
   ITEMS @ 0 > if W COMMA drop then
   W a u DIAGNOSTIC drop
   1 ITEMS +! ;

\ The list for the file in LIST-A, its positions counted in the text TEXT!
\ named, published at the URI.
: LIST ( ptr u8 n option<n> -- )
   OPENING drop
   0 ITEMS !
   PKTS-A @ PKTS-U @ [: ITEM ;] EACH-LINE
   W CLOSING ;

\ The file URI of the path in LIST-A.
: LIST-URI ( -- ptr u8 n )
   LIST-A @ LIST-U @ URI-SPAN URI:PATH>FILE {: n:n :}
   URI-SPAN SPAN:$ drop n ;

\ ---- the files besides the document ---------------------------------------------

\ Whether the slot is there.
: SLOT? ( option<n> -- bool )
   MATCH option
      some OF drop true ENDOF
      none OF false ENDOF
   ;MATCH ;

\ Whether an open document is the file the checker names by this path.
: HELD-OPEN? ( ptr u8 n -- bool )  DOC-HOLDING SLOT? ;

\ Whether an open document's own list is at this path's file URI: the client
\ opened it by this path.
: SHOWN? ( ptr u8 n -- bool )  DOC-OPENED-AS SLOT? ;

\ Whether another open document's last check published a list for this path;
\ each such document waits for a check again.
: CLAIMED? ( ptr u8 n -- bool )
   {: f:ptr fu:n :}
   false
   DOC-SLOTS 0 ?do
      i SELF @ <> i DOC-LIVE? and if
         i DOC-DEPS$ f fu HAS-LINE? if i DOC-DIRTY drop true then
      then
   loop ;

\ Appends the path, LF-ended, to the files published; answers that copy, which
\ outlives FILE-BUF.
: KEEP-NEW ( ptr u8 n -- ptr u8 n )
   {: f:ptr fu:n :}
   NEW$ nip {: at:n :}
   f fu BUF:N>BLEN NEW-B BUF:APPEND-SPAN
   s\" \n" BUF:N>BLEN NEW-B BUF:APPEND-SPAN
   NEW$ drop at + fu ;

\ A packet line in the pass over all of them: not a diagnostic, it goes to
\ stderr; of a file besides the document that nothing else lists, that file's
\ list is published.
: DEP ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u SCREENED 0= if s" lsp: " ERR a u ERR s\" \n" ERR exit then
   FILE$ SELF @ DOC-CANON$ STR= if exit then
   NEW$ FILE$ HAS-LINE? if exit then
   FILE$ HELD-OPEN? if exit then
   FILE$ KEEP-NEW LIST-U ! LIST-A !
   LIST-A @ LIST-U @ FILE-TEXT!
   LIST-URI OPTION:NONE LIST ;

\ A file the document's earlier check published a list for, withdrawn unless
\ this check published it again, an open document's own list is at its URI or
\ another document's check claims it.
: WITHDRAW ( ptr u8 n -- )
   {: f:ptr fu:n :}
   NEW$ f fu HAS-LINE? if exit then
   f fu SHOWN? if exit then
   f fu CLAIMED? if exit then
   f LIST-A !
   fu LIST-U !
   LIST-URI OPTION:NONE OPENING CLOSING ;

public

\ Readies the buffers; the server calls it once, before any other word here.
: PREPARE ( -- )
   PUB-B 1 BUF:N>BLEN BUF:INIT
   NEW-B 1 BUF:N>BLEN BUF:INIT ;

\ Publishes the packets of the check of the document in this slot.
: PUBLISH ( n ptr u8 n -- )
   {: slot:n p:ptr pu:n :}
   p PKTS-A !
   pu PKTS-U !
   slot SELF !
   NEW-B BUF:CLEAR
   slot DOC-CANON$ LIST-U ! LIST-A !
   slot DOC-TEXT$ TEXT!
   slot DOC-URI$ slot DOC-VERSION@ OPTION:SOME LIST
   p pu [: DEP ;] EACH-LINE
   slot DOC-DEPS$ [: WITHDRAW ;] EACH-LINE
   NEW$ slot DOC-DEPS! ;

\ The document in this slot is closing: an empty list for it, without a
\ version, and its check's lists for other files withdrawn.
: RETRACT ( n -- )
   {: slot:n :}
   slot SELF !
   NEW-B BUF:CLEAR
   slot DOC-URI$ OPTION:NONE OPENING CLOSING
   slot DOC-DEPS$ [: WITHDRAW ;] EACH-LINE ;

;using
;using
;using
;using
;using
;package
