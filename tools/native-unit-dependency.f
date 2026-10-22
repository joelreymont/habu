\ Observed inputs to one native definition and to one source file. Section 14
\ carries declaration identities, checked contracts, frozen primitive schemas
\ and fixed values; anything it cannot re-verify is an ORDERED row. A nested
\ definition keeps a separate span in the definition arena. A file frame holds
\ one row per kind and key, only about records the file did not make itself:
\ those before its first ordinal or made by a child file it loaded.

require src/lib/errors.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require src/core/checker-owner-abi.f
require src/core/checker-owner-extension.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/host.f
require src/compiler/native/checker-owner.f
require src/compiler/native/feed.f
require tools/native-file-reference.f
require tools/native-file-scope.f

package NUNIT-DEP
private

1 constant BINDING
2 constant VALUE
4 constant ORDERED
5 constant CHECKER-ROLE
6 constant SCOPE
7 constant RESOLUTION
8 constant PRIMITIVE
9 constant COMPILER-ROLE
10 constant READER-FLAGS
16 constant HEADER-BYTES
24 constant ROW-BYTES
48 constant RES-ROW-BYTES
32 constant RES-KEY-BYTES
32 constant PRIMITIVE-HEAD
\ A file frame's cells: its row offset, its first record ordinal, where its
\ children's record ranges start, and where its first ORDERED reason starts.
0 constant F-OFF
1 constant F-FIRST
2 constant F-RBASE
3 constant F-WHY
4 constant FRAME-CELLS
1024 constant SLOTS-MIN
$cbf29ce484222325 constant FNV-BASIS
$100000001b3 constant FNV-PRIME

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER FRAMES n
DYNAMIC-BUFFER RES-KEY u8
DYNAMIC-BUFFER PRIMITIVE-DATA u8
\ File frames: rows, frame cells, closed child ranges (first, end) of every
\ open frame, first ORDERED reasons, and the slots indexing the open rows.
DYNAMIC-BUFFER FBYTES u8
DYNAMIC-BUFFER FFRAMES n
DYNAMIC-BUFFER FRANGES n
DYNAMIC-BUFFER FWHY u8
DYNAMIC-BUFFER FSLOTS n
0 CWORK-VARIABLE USED n
0 CWORK-VARIABLE DEPTH n
0 CWORK-VARIABLE FUSED n
0 CWORK-VARIABLE FDEPTH n
0 CWORK-VARIABLE FRN n
0 CWORK-VARIABLE WHYU n
0 CWORK-VARIABLE FCAP n
0 CWORK-VARIABLE FLIVE n
0 CWORK-VARIABLE CHECK-OFF n
0 CWORK-VARIABLE CHECK-OK bool
0 CWORK-VARIABLE MISS-P ptr u8
0 CWORK-VARIABLE MISS-U n
0 CWORK-VARIABLE SCAN-OFF n
create CELL-BYTES 8 allot

\ The checker owner installs an exact-occurrence query at its publication
\ boundary. An older owner has none, and that FILE is explicitly ordered.
defer CONTRACT$ ( n -- ptr u8 n bool )
: NO-CONTRACT ( n -- ptr u8 n bool ) drop NULL-PTR 0 false ;
defer CHECKER-ROLE$ ( -- ptr u8 n bool )
: NO-ROLE ( -- ptr u8 n bool ) NULL-PTR 0 false ;
defer COMPILER-ROLE$ ( -- ptr u8 n bool )
defer PRIMITIVE-STATE ( n n -- n bool )
: NO-PRIMITIVE ( n n -- n bool ) 2drop 0 false ;
defer SYM-IDENTITY ( n -- ptr u8 n ptr u8 n n )
: NO-IDENTITY ( n -- ptr u8 n ptr u8 n n )
   drop NULL-PTR 0 NULL-PTR 0 0 ;
defer ORIGIN-SYM ( ptr u8 n n ptr u8 n -- n )
: NO-ORIGIN ( ptr u8 n n ptr u8 n -- n )
   2drop drop 2drop 0 ;
: INSTALL-DEFAULT ( -- )
   ['] NO-CONTRACT is CONTRACT$
   ['] NO-ROLE is CHECKER-ROLE$
   ['] NO-ROLE is COMPILER-ROLE$
   ['] NO-PRIMITIVE is PRIMITIVE-STATE
   ['] NO-IDENTITY is SYM-IDENTITY
   ['] NO-ORIGIN is ORIGIN-SYM ;
INSTALL-DEFAULT

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;

: FRAME ( -- n )
   DEPTH @ 0 <= if BAD then
   DEPTH @ 1- FRAMES @ ;

\ Field n of the innermost file frame.
: FFIELD ( n -- ptr n )
   FDEPTH @ 0 <= if BAD then
   FDEPTH @ 1- FRAME-CELLS * + FFRAMES ;

: OPEN? ( -- bool ) DEPTH @ 0 > FDEPTH @ 0 > or ;

\ A record the innermost file made itself: at or after its first ordinal and
\ outside every child file it loaded. Rows about it are internal to the file.
: OWN? ( n -- bool )
   {: ord:n :}
   FDEPTH @ 0= if false exit then
   ord F-FIRST FFIELD @ < if false exit then
   FRN @ F-RBASE FFIELD @ ?do
      ord i FRANGES @ >= ord i 1+ FRANGES @ < and if false unloop exit then
   2 +loop
   true ;

\ No open frame keeps a row about this record.
: SKIP? ( n -- bool )
   DEPTH @ 0 > if drop false exit then
   OWN? FDEPTH @ 0= or ;

: ROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N USED @ - > or if BAD then
   USED @ need + BYTES-RESERVE
   USED @ dup need + USED ! ;

: FROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N FUSED @ - > or if BAD then
   FUSED @ need + FBYTES-RESERVE
   FUSED @ dup need + FUSED ! ;

: ROW-SIZE ( n n n -- n )
   {: kind:n keyu:n datau:n :}
   keyu 0 < datau 0 < or if BAD then
   kind ORDERED <> kind CHECKER-ROLE <> and kind COMPILER-ROLE <> and
      kind SCOPE <> and
      keyu 0= and if BAD then
   keyu MEM-MAX-N ROW-BYTES - > if BAD then
   datau MEM-MAX-N ROW-BYTES - keyu - > if BAD then
   ROW-BYTES keyu + datau + ;

\ A zero-length field ends exactly at the reserved bound for a trailing
\ ORDERED row; nothing is copied there.
: ROW! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: row:ptr kind:n key:ptr keyu:n data:ptr datau:n :}
   kind row LE:U64!
   keyu row 8 + LE:U64!
   datau row 16 + LE:U64!
   keyu 0 > if key row ROW-BYTES + keyu BYTE-COPY then
   datau 0 > if data row ROW-BYTES keyu + + datau BYTE-COPY then ;

: COUNT+ ( ptr u8 -- ) 8 + dup LE:U64@ 1+ swap LE:U64! ;

: DEF-ROW ( n ptr u8 n ptr u8 n -- )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   kind keyu datau ROW-SIZE ROOM {: off:n :}
   off BYTES kind key keyu data datau ROW!
   FRAME BYTES COUNT+ ;

: KEY-HASH ( n ptr u8 n -- n )
   {: kind:n key:ptr keyu:n :}
   FNV-BASIS kind xor FNV-PRIME *
   keyu 0 ?do key i + c@ xor FNV-PRIME * loop ;

: ROW-SPAN ( ptr u8 -- n )
   dup 8 + LE:U64@ swap 16 + LE:U64@ + ROW-BYTES + ;

: SAME-ROW? ( n n ptr u8 n -- bool )
   {: off:n kind:n key:ptr keyu:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ kind <> if false exit then
   row 8 + LE:U64@ keyu <> if false exit then
   keyu 0= if true exit then
   row ROW-BYTES + keyu key keyu STR= ;

\ Whether slot idx holds kind and key, and whether it ends the probe. A slot
\ below the innermost frame belongs to an outer frame and never matches.
: SLOT-END ( n n ptr u8 n -- bool bool )
   {: idx:n kind:n key:ptr keyu:n :}
   idx FSLOTS @ {: v:n :}
   v 0= if false true exit then
   v 1- F-OFF FFIELD @ < if false false exit then
   v 1- kind key keyu SAME-ROW? dup ;

\ The slot holding the innermost frame's row for kind and key, else the empty
\ slot that ends its probe.
: PROBE ( n ptr u8 n -- n bool )
   {: kind:n key:ptr keyu:n :}
   kind key keyu KEY-HASH FCAP @ 1- and
   begin dup kind key keyu SLOT-END 0= while
      drop 1+ FCAP @ 1- and
   repeat ;

\ Index the row at off in the first empty slot of its probe; the next row.
: PLACE ( n -- n )
   {: off:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ row ROW-BYTES + row 8 + LE:U64@ KEY-HASH FCAP @ 1- and
   begin dup FSLOTS @ 0<> while 1+ FCAP @ 1- and repeat
   off 1+ swap FSLOTS !
   off row ROW-SPAN + ;

: INDEX-FRAME ( n -- )
   FRAME-CELLS * F-OFF + FFRAMES @ {: off:n :}
   off HEADER-BYTES +
   off FBYTES 8 + LE:U64@ 0 ?do PLACE loop
   drop ;

\ Outer frames first, so an outer row's probe never passes an inner slot and
\ closing a frame can empty its slots alone.
: REINDEX ( n -- )
   {: cap:n :}
   cap FSLOTS-RESERVE
   cap FCAP !
   cap 0 ?do 0 i FSLOTS ! loop
   FDEPTH @ 0 ?do i INDEX-FRAME loop ;

: FILE-ROW ( n ptr u8 n ptr u8 n -- )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   FLIVE @ 1+ 2 * FCAP @ > if FCAP @ 2 * SLOTS-MIN max REINDEX then
   kind key keyu PROBE {: idx:n found:bool :}
   found if exit then
   kind keyu datau ROW-SIZE FROOM {: off:n :}
   off FBYTES kind key keyu data datau ROW!
   off 1+ idx FSLOTS !
   1 FLIVE +!
   F-OFF FFIELD @ FBYTES COUNT+ ;

\ The definition frame takes each observation; the file frame one row per
\ kind and key about a record it did not make.
: ADD-ROW ( n n ptr u8 n ptr u8 n -- )
   {: ord:n kind:n key:ptr keyu:n data:ptr datau:n :}
   OPEN? 0= if BAD then
   DEPTH @ 0 > if kind key keyu data datau DEF-ROW then
   FDEPTH @ 0 > ord OWN? 0= and if kind key keyu data datau FILE-ROW then ;

: BINDING-ROW ( n ptr u8 n ptr u8 n -- )
   {: ord:n key:ptr keyu:n contract:ptr contractu:n :}
   contractu 0= if BAD then
   ord BINDING key keyu contract contractu ADD-ROW ;

: VALUE-ROW ( n ptr u8 n ptr u8 n -- )
   {: ord:n key:ptr keyu:n result:ptr resultu:n :}
   resultu 8 <> if BAD then
   ord VALUE key keyu result resultu ADD-ROW ;

: REMEMBER-MISS ( ptr u8 n -- )
   MISS-P @ NULL-PTR = if
      MISS-U ! MISS-P !
   else 2drop then ;

: CURRENT ( ptr u8 n -- ptr n n bool )
   NFILE-REF:KEY-RECORD {: rec:ptr ord:n :}
   ord 0 < if rec ord false exit then
   ord NFILE-REF:RECORD-RETIRED? if rec ord false exit then
   rec ord true ;

\ The owner retained this exact fixed result at publication. CHECK reads only
\ its record fact; it never enters a definition to reconstruct the value.
: FIXED-RAW? ( n -- n bool )
   CONTRACT$ {: contract:ptr size:n known:bool :}
   known 0= if 0 false exit then
   size CHECKER-OWNER-EXT:MIN-BYTES < if 0 false exit then
   contract LE:U64@ CHECKER-OWNER-EXT:CONTRACT-MAGIC <> if
      0 false exit
   then
   contract CHECKER-OWNER-EXT:FIXED-KIND-OFF + LE:U64@
      CHECKER-OWNER-EXT:FIXED-VAL <> if 0 false exit then
   contract CHECKER-OWNER-EXT:FIXED-VALUE-OFF + LE:U64@ true ;

: CHECK-BINDING ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" binding absent" REMEMBER-MISS false exit then
   ord CONTRACT$ {: got:ptr gotu:n known:bool :}
   known 0= if s" contract unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR= dup 0= if s" contract changed" REMEMBER-MISS then ;

: CHECK-READER-FLAGS ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   ord drop
   found 0= if s" reader binding absent" REMEMBER-MISS false exit then
   rec XREF-FLAGS DNAME-IMM and CELL-BYTES LE:U64!
   CELL-BYTES 8 want wantu STR=
   dup 0= if s" reader flags changed" REMEMBER-MISS then ;

: CHECK-VALUE ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" value binding absent" REMEMBER-MISS false exit then
   ord FIXED-RAW? {: value:n known:bool :}
   known 0= if s" value fact unknown" REMEMBER-MISS false exit then
   value CELL-BYTES LE:U64!
   CELL-BYTES 8 want wantu STR=
   dup 0= if s" value changed" REMEMBER-MISS then ;

: CHECK-CHECKER-ROLE ( ptr u8 n -- bool )
   {: want:ptr wantu:n :}
   CHECKER-ROLE$ {: got:ptr gotu:n known:bool :}
   known 0= if s" checker producer unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR=
   dup 0= if s" checker producer changed" REMEMBER-MISS then ;

: CHECK-COMPILER-ROLE ( ptr u8 n -- bool )
   {: want:ptr wantu:n :}
   COMPILER-ROLE$ {: got:ptr gotu:n known:bool :}
   known 0= if s" emitting compiler unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR=
   dup 0= if s" emitting compiler changed" REMEMBER-MISS then ;

: CHECK-PRIMITIVE ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n data:ptr datau:n :}
   datau PRIMITIVE-HEAD < if MALFORMED then
   data 8 + LE:U64@ {: pkgu:n :}
   data 16 + LE:U64@ {: vis:n :}
   data 24 + LE:U64@ {: tailu:n :}
   pkgu 0 < tailu 0 <= or vis 0 < or vis 2 > or if MALFORMED then
   pkgu datau PRIMITIVE-HEAD - > if MALFORMED then
   tailu datau PRIMITIVE-HEAD pkgu + - <> if MALFORMED then
   vis 0= if pkgu 0<> if MALFORMED then
   else pkgu 0= if MALFORMED then then
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" primitive binding absent" REMEMBER-MISS false exit then
   data PRIMITIVE-HEAD + pkgu + {: tail:ptr :}
   data PRIMITIVE-HEAD + pkgu vis tail tailu ORIGIN-SYM
      {: sym:n :}
   sym 0= if s" primitive symbol absent" REMEMBER-MISS false exit then
   ord sym PRIMITIVE-STATE {: control:n known:bool :}
   known 0= if s" primitive state changed" REMEMBER-MISS false exit then
   control data LE:U64@ = dup 0= if
      s" primitive control changed" REMEMBER-MISS
   then ;

: CHECK-RESOLUTION ( ptr u8 n ptr u8 n -- bool )
   {: row:ptr rowu:n want:ptr wantu:n :}
   rowu RES-KEY-BYTES < if MALFORMED then
   row LE:U64@ {: role:n :}
   row 8 + LE:U64@ {: shape:n :}
   row 16 + LE:U64@ {: spellingu:n :}
   row 24 + LE:U64@ {: namespaceu:n :}
   role 0 < role 9 > or shape 0 < shape 3 > or or
      spellingu 0 < namespaceu 0 < or or if MALFORMED then
   spellingu rowu RES-KEY-BYTES - > if MALFORMED then
   namespaceu rowu RES-KEY-BYTES spellingu + - <> if MALFORMED then
   role row RES-KEY-BYTES + spellingu
      row RES-KEY-BYTES spellingu + + namespaceu
      NFILE-REF:RESOLVE-CANDIDATE {: ord:n got-shape:n :}
   ord -2 = if s" ambiguous resolution" REMEMBER-MISS false exit then
   got-shape shape <> if s" namespace shape changed" REMEMBER-MISS false exit then
   ord -1 = if
      wantu 0= exit
   then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n known:bool :}
   known 0= if s" candidate identity unknown" REMEMBER-MISS false exit then
   key keyu want wantu STR= dup 0= if
      s" candidate changed" REMEMBER-MISS
   then ;

: CHECK-ROW ( n ptr u8 n ptr u8 n -- bool )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   kind BINDING = if
      keyu 0= datau 0= or if MALFORMED then
      key keyu data datau CHECK-BINDING exit
   then
   kind READER-FLAGS = if
      keyu 0= datau 8 <> or if MALFORMED then
      key keyu data datau CHECK-READER-FLAGS exit
   then
   kind VALUE = if
      keyu 0= datau 8 <> or if MALFORMED then
      key keyu data datau CHECK-VALUE exit
   then
   kind CHECKER-ROLE = if
      keyu 0<> datau 32 <> or if MALFORMED then
      data datau CHECK-CHECKER-ROLE exit
   then
   kind COMPILER-ROLE = if
      keyu 0<> datau 32 <> or if MALFORMED then
      data datau CHECK-COMPILER-ROLE exit
   then
   kind PRIMITIVE = if
      keyu 0= if MALFORMED then
      key keyu data datau CHECK-PRIMITIVE exit
   then
   kind SCOPE = if
      keyu 0<> datau 0= or if MALFORMED then
      data datau NFILE-SCOPE:SCOPE-MATCH? exit
   then
   kind RESOLUTION = if
      keyu RES-KEY-BYTES < if MALFORMED then
      key keyu data datau CHECK-RESOLUTION exit
   then
   kind ORDERED = if
      keyu 0<> datau 0<> or if MALFORMED then
      s" ordered source action" REMEMBER-MISS false exit
   then
   MALFORMED ;

\ The row at CHECK-OFF, its lengths checked against the bytes; CHECK-OFF
\ moves past it.
: ROW-AT ( ptr u8 n -- n ptr u8 n ptr u8 n )
   {: bytes:ptr size:n :}
   size CHECK-OFF @ - ROW-BYTES < if MALFORMED then
   bytes CHECK-OFF @ + {: row:ptr :}
   row 8 + LE:U64@ {: keyu:n :}
   row 16 + LE:U64@ {: datau:n :}
   keyu 0 < datau 0 < or if MALFORMED then
   keyu size CHECK-OFF @ - ROW-BYTES - > if MALFORMED then
   datau size CHECK-OFF @ - ROW-BYTES - keyu - > if MALFORMED then
   CHECK-OFF @ ROW-BYTES keyu + datau + + CHECK-OFF !
   row LE:U64@ row ROW-BYTES + keyu
      row ROW-BYTES keyu + + datau ;

: CHECK-NEXT ( ptr u8 n -- )
   ROW-AT CHECK-ROW 0= if false CHECK-OK ! then ;

\ The header's row count, bounded by the bytes; CHECK-OFF starts at row 0.
: HEAD ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   size HEADER-BYTES < if MALFORMED then
   bytes 8 s" HBFDEP02" STR= 0= if MALFORMED then
   bytes 8 + LE:U64@ {: count:n :}
   count 0 < count size HEADER-BYTES - ROW-BYTES / > or if MALFORMED then
   HEADER-BYTES CHECK-OFF !
   count ;

: HAS-ORDERED? ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   HEADER-BYTES SCAN-OFF !
   begin SCAN-OFF @ size < while
      bytes SCAN-OFF @ + {: row:ptr :}
      row LE:U64@ ORDERED = if true exit then
      row 8 + LE:U64@ row 16 + LE:U64@ + ROW-BYTES +
         SCAN-OFF +!
   repeat
   false ;

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL DIAG-PATH

: DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

: REPORT-MISS ( -- )
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= 0= if exit then
   MISS-P @ NULL-PTR = if exit then
   s" native-file: dependency miss: " DIAG-W2
   MISS-P @ MISS-U @ DIAG-W2
   S\" \n" DIAG-W2 ;

: DIAG-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 DIAG-W2 ;

: DIAG-PACKAGE ( n -- )
   {: wid:n :}
   wid 0= if exit then
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC wid = rec XREF-PKG-PRIVATE wid = or if
            s"  package=" DIAG-W2 rec XREF-NAME$ DIAG-W2
            unloop exit
         then
      then
   loop ;

\ Report before the first ORDERED row loses the dependency's identity.
\ The existing frame supplies the first-row test; diagnostics keep no state.
: DIAG-ORDERED-NAME ( ptr u8 n ptr u8 n n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n ord:n :}
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= 0= if exit then
   FRAME {: off:n :}
   off BYTES USED @ off - HAS-ORDERED? if exit then
   s" native-cache: ordered-dependency reason=" DIAG-W2
   reason reasonu DIAG-W2
   nameu 0 > if s"  dependency=" DIAG-W2 name nameu DIAG-W2 then
   ord 0 >= if
      s"  ord=" DIAG-W2 ord DIAG-N
      ord XREF-REC XREF-WORDLIST DIAG-PACKAGE
   then
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   pathu 0 > if s"  path=" DIAG-W2 DIAG-PATH @ pathu DIAG-W2 then
   S\" \n" DIAG-W2 ;

: WHY+ ( ptr u8 n -- )
   {: text:ptr textu:n :}
   textu 0= if exit then
   WHYU @ textu + FWHY-RESERVE
   text WHYU @ FWHY textu BYTE-COPY
   textu WHYU +! ;

: FILE-ORDERED? ( -- bool ) WHYU @ F-WHY FFIELD @ > ;

\ The definition frame reports its first ORDERED row now; the file frame keeps
\ its first reason for FILE-REPORT.
: ORDERED-AT ( ptr u8 n ptr u8 n n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n ord:n :}
   DEPTH @ 0 > if reason reasonu name nameu ord DIAG-ORDERED-NAME then
   FDEPTH @ 0 > ord OWN? 0= and if
      FILE-ORDERED? 0= if
         s"  reason=" WHY+ reason reasonu WHY+
         nameu 0 > if s"  word=" WHY+ name nameu WHY+ then
      then
   then
   ord ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW ;

: ORDERED-BY ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   reason reasonu
   ord 0 >= if ord XREF-REC XREF-NAME$ else NULL-PTR 0 then
   ord ORDERED-AT ;

: CAPTURE-BIND ( n -- )
   {: ord:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" binding-key-unknown" ord ORDERED-BY exit then
   ord CONTRACT$ {: contract:ptr contractu:n known:bool :}
   known 0= if s" binding-contract-unknown" ord ORDERED-BY exit then
   ord key keyu contract contractu BINDING-ROW ;

: CAPTURE-READER-FLAGS ( n n -- )
   {: ord:n flags:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" reader-key-unknown" ord ORDERED-BY exit then
   flags DNAME-IMM and CELL-BYTES LE:U64!
   ord READER-FLAGS key keyu CELL-BYTES 8 ADD-ROW ;

: CAPTURE-VALUE ( n -- )
   {: ord:n :}
   ord SKIP? if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" value-key-unknown" ord ORDERED-BY exit then
   ord FIXED-RAW? {: value:n known:bool :}
   known 0= if s" fixed-value-unknown" ord ORDERED-BY exit then
   value CELL-BYTES LE:U64!
   ord key keyu CELL-BYTES 8 VALUE-ROW ;

: ADD-RESOLUTION ( n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ord:n role:n shape:n spelling:ptr spellingu:n namespace:ptr namespaceu:n
      key:ptr keyu:n :}
   spellingu 0 < namespaceu 0 < or if BAD then
   spellingu MEM-MAX-N RES-KEY-BYTES - > if BAD then
   namespaceu MEM-MAX-N RES-KEY-BYTES spellingu + - > if BAD then
   RES-KEY-BYTES spellingu + namespaceu + RES-KEY-RESERVE
   role 0 RES-KEY LE:U64!
   shape 8 RES-KEY LE:U64!
   spellingu 16 RES-KEY LE:U64!
   namespaceu 24 RES-KEY LE:U64!
   spellingu 0<> if
      spelling 32 RES-KEY spellingu BYTE-COPY
   then
   namespaceu 0<> if
      namespace 32 spellingu + RES-KEY namespaceu BYTE-COPY
   then
   ord RESOLUTION 0 RES-KEY RES-KEY-BYTES spellingu + namespaceu +
      key keyu ADD-ROW ;

: OBSERVE-RESOLUTION ( ptr u8 n -- )
   {: row:ptr rowu:n :}
   OPEN? 0= if exit then
   rowu RES-ROW-BYTES < if BAD then
   row LE:U64@ drop
   row 8 + LE:U64@ {: role:n :}
   row 16 + LE:U64@ {: ord:n :}
   row 24 + LE:U64@ {: shape:n :}
   row 32 + LE:U64@ {: spellingu:n :}
   row 40 + LE:U64@ {: namespaceu:n :}
   role 0 < role 9 > or shape 0 < shape 3 > or or
      spellingu 0 < namespaceu 0 < or or if BAD then
   spellingu rowu RES-ROW-BYTES - > if BAD then
   namespaceu rowu RES-ROW-BYTES spellingu + - <> if BAD then
   ord -1 < if
      s" resolution-ambiguous" row RES-ROW-BYTES + spellingu -1 ORDERED-AT
      exit
   then
   ord 0 >= if
      ord SKIP? if exit then
      ord NFILE-REF:CURRENT-RECORD? if exit then
      ord NFILE-REF:DECL-KEY? 0= if
         2drop s" resolution-key-unknown" ord ORDERED-BY exit
      then 2drop
   then
   ord -1 = if
      -1 role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         NULL-PTR 0 ADD-RESOLUTION
   else
      ord NFILE-REF:DECL-KEY$ {: key:ptr keyu:n :}
      ord role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         key keyu ADD-RESOLUTION
   then ;

: CAPTURE-CHECKER-ROLE ( -- )
   OPEN? 0= if exit then
   CHECKER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if s" checker-producer-unknown" -1 ORDERED-BY exit then
   digestu 32 <> if BAD then
   -1 CHECKER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

: CAPTURE-COMPILER-ROLE ( -- )
   OPEN? 0= if exit then
   COMPILER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if s" compiler-producer-unknown" -1 ORDERED-BY exit then
   digestu 32 <> if BAD then
   -1 COMPILER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

: BOUND@ ( ptr u8 n -- n )
   {: row:ptr field:n :}
   row field cells + CELL-VIEW @ ;

: PRIMITIVE-DATA$ ( n n -- ptr u8 n )
   {: sym:n control:n :}
   sym SYM-IDENTITY {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   pkgu 0 < tailu 0 <= or vis 0 < or vis 2 > or if BAD then
   pkgu MEM-MAX-N PRIMITIVE-HEAD - > if BAD then
   tailu MEM-MAX-N PRIMITIVE-HEAD pkgu + - > if BAD then
   PRIMITIVE-HEAD pkgu + tailu + PRIMITIVE-DATA-RESERVE
   control 0 PRIMITIVE-DATA LE:U64!
   pkgu 8 PRIMITIVE-DATA LE:U64!
   vis 16 PRIMITIVE-DATA LE:U64!
   tailu 24 PRIMITIVE-DATA LE:U64!
   pkg PRIMITIVE-HEAD PRIMITIVE-DATA pkgu BYTE-COPY
   tail PRIMITIVE-HEAD pkgu + PRIMITIVE-DATA tailu BYTE-COPY
   0 PRIMITIVE-DATA PRIMITIVE-HEAD pkgu + tailu + ;

: CAPTURE-PRIMITIVE ( ptr u8 n -- )
   {: row:ptr ord:n :}
   row CHECKER-OWNER-ABI:BOUND-SYM BOUND@ {: sym:n :}
   ord sym PRIMITIVE-STATE {: control:n known:bool :}
   known 0= if s" primitive-state-unknown" ord ORDERED-BY exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if s" primitive-key-unknown" ord ORDERED-BY exit then
   sym control PRIMITIVE-DATA$ {: data:ptr datau:n :}
   ord PRIMITIVE key keyu data datau ADD-ROW
   CAPTURE-CHECKER-ROLE ;

\ This row is the checker's exact resolution from the successful source scan.
\ The tape's symbol interner is only a name; it is never a declaration key.
: OBSERVE-BOUND ( ptr u8 n -- )
   {: row:ptr size:n :}
   OPEN? 0= if exit then
   size CHECKER-OWNER-ABI:BOUND-CELLS cells <> if
      CHECKER-OWNER-ABI:BINDING-RC throw
   then
   row CHECKER-OWNER-ABI:BOUND-KIND BOUND@ {: kind:n :}
   kind CHECKER-OWNER-ABI:BOUND-DICT = if
      row CHECKER-OWNER-ABI:BOUND-RECORD BOUND@ {: ord:n :}
      ord 0 < if CHECKER-OWNER-ABI:BINDING-RC throw then
      ord SKIP? if exit then
      row CHECKER-OWNER-ABI:BOUND-CTL BOUND@
         CHECKER-OWNER-ABI:BOUND-SEEDED and 0<> if
         row ord CAPTURE-PRIMITIVE
      else ord CAPTURE-BIND then
      row CHECKER-OWNER-ABI:BOUND-FLAGS BOUND@ {: flags:n :}
      ord flags CAPTURE-READER-FLAGS
      flags DKIND:MASK and DKIND:VAL = if ord CAPTURE-VALUE then
      \ What an immediate ran while compiling cannot be re-verified.
      flags DNAME-IMM and 0<> if s" immediate-binding" ord ORDERED-BY then
      exit
   then
   kind CHECKER-OWNER-ABI:BOUND-INTRINSIC = if
      CAPTURE-CHECKER-ROLE
   then ;

\ The feed reads the source owner's candidates only after the scan. Enable
\ their producer while a frame is open; top-level namespace actions still run.
: OBSERVE ( -- )
   ['] OBSERVE-BOUND NFEED:USE-BINDINGS
   ['] OBSERVE-RESOLUTION NFEED:USE-RESOLUTIONS
   [: 2drop ;] CHECKER-OWNER:RESOLUTION-NS-INSTALL ;

: UNOBSERVE ( -- )
   OPEN? if exit then
   NFEED:BINDINGS-DEFAULT
   NFEED:RESOLUTIONS-DEFAULT
   CHECKER-OWNER:RESOLUTION-NS-DEFAULT ;

: HEADER! ( ptr u8 -- )
   {: head:ptr :}
   s" HBFDEP02" drop head 8 BYTE-COPY
   0 head 8 + LE:U64! ;

: RANGE+ ( n n -- )
   {: first:n end:n :}
   FRN @ 2 + FRANGES-RESERVE
   first FRN @ FRANGES !
   end FRN @ 1+ FRANGES !
   2 FRN +! ;

: TALLY ( n n n -- n n )
   {: tally:n off:n kind:n :}
   off FBYTES {: row:ptr :}
   row LE:U64@ kind = if tally 1+ else tally then
   off row ROW-SPAN + ;

: KIND-ROWS ( n -- n )
   {: kind:n :}
   F-OFF FFIELD @ {: off:n :}
   0 off HEADER-BYTES +
   off FBYTES 8 + LE:U64@ 0 ?do kind TALLY loop
   drop ;

public

\ Open a file frame at the file's first record ordinal.
: BEGIN-UNIT ( -- )
   FDEPTH @ 1+ FRAME-CELLS * FFRAMES-RESERVE
   FDEPTH @ FRAME-CELLS * {: base:n :}
   FUSED @ base F-OFF + FFRAMES !
   ndict@ base F-FIRST + FFRAMES !
   FRN @ base F-RBASE + FFRAMES !
   WHYU @ base F-WHY + FFRAMES !
   1 FDEPTH +!
   HEADER-BYTES FROOM FBYTES HEADER!
   NFILE-SCOPE:SCOPE-INPUT$ {: scope:ptr scopeu:n :}
   SCOPE NULL-PTR 0 scope scopeu FILE-ROW
   OBSERVE ;

\ Close the innermost file frame. Its records become a child range of the
\ enclosing file frame, whose rows may name them.
: CLOSE-UNIT ( -- )
   F-OFF FFIELD @ {: off:n :}
   F-FIRST FFIELD @ {: first:n :}
   off FBYTES 8 + LE:U64@ negate FLIVE +!
   FCAP @ 0 ?do
      i FSLOTS @ off > if 0 i FSLOTS ! then
   loop
   off FUSED !
   F-RBASE FFIELD @ FRN !
   F-WHY FFIELD @ WHYU !
   -1 FDEPTH +!
   FDEPTH @ 0 > if first ndict@ RANGE+ then
   UNOBSERVE ;

\ The innermost file frame's cache diagnostic line, on stderr.
: FILE-REPORT ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   s" native-file: frame path=" DIAG-W2 path pathu DIAG-W2
   s"  rows=" DIAG-W2 F-OFF FFIELD @ FBYTES 8 + LE:U64@ DIAG-N
   s"  ordered=" DIAG-W2 FILE-ORDERED? if 1 else 0 then DIAG-N
   s"  binding=" DIAG-W2 BINDING KIND-ROWS DIAG-N
   s"  value=" DIAG-W2 VALUE KIND-ROWS DIAG-N
   FILE-ORDERED? if
      F-WHY FFIELD @ {: why:n :}
      why FWHY WHYU @ why - DIAG-W2
   then
   S\" \n" DIAG-W2 ;

: BEGIN-DEFINITION ( n -- )
   {: ord:n :}
   ord NFILE-REF:BEGIN-DEFINITION
   DEPTH @ 1+ FRAMES-RESERVE
   USED @ DEPTH @ FRAMES !
   1 DEPTH +!
   HEADER-BYTES ROOM BYTES HEADER!
   NFILE-SCOPE:SCOPE-INPUT$ {: scope:ptr scopeu:n :}
   SCOPE NULL-PTR 0 scope scopeu DEF-ROW
   OBSERVE ;

: COMPILED! ( -- ) CAPTURE-COMPILER-ROLE ;

\ The frame's rows, and whether the definition may be stored: an ORDERED row
\ names a fact no later build can re-verify.
: EXPORT ( -- ptr u8 n bool )
   FRAME {: off:n :}
   off BYTES USED @ off - {: deps:ptr depsu:n :}
   deps depsu deps depsu HAS-ORDERED? 0= ;

\ The innermost file frame's rows, and whether its file may be stored: no
\ ORDERED row and no ORDERED reason.
: FILE-EXPORT ( -- ptr u8 n bool )
   F-OFF FFIELD @ {: off:n :}
   off FBYTES FUSED @ off - {: deps:ptr depsu:n :}
   deps depsu
   deps depsu HAS-ORDERED? 0= FILE-ORDERED? 0= and ;

: CHECK ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   bytes size HEAD {: count:n :}
   true CHECK-OK !
   NULL-PTR MISS-P ! 0 MISS-U !
   count 0 ?do bytes size CHECK-NEXT loop
   CHECK-OFF @ size <> if MALFORMED then
   CHECK-OK @ 0= if REPORT-MISS then
   CHECK-OK @ ;

\ The row count of well-framed dependency bytes, without checking any row
\ against this session.
: FORMAT ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   bytes size HEAD {: count:n :}
   count 0 ?do bytes size ROW-AT 2drop 2drop drop loop
   CHECK-OFF @ size <> if MALFORMED then
   count ;

: RESET ( -- )
   NFILE-REF:END-DEFINITION
   0 DEPTH !
   0 USED !
   0 FDEPTH !
   0 FUSED !
   0 FRN !
   0 WHYU !
   0 FCAP !
   0 FLIVE !
   UNOBSERVE
   BYTES-RELEASE FRAMES-RELEASE
   FBYTES-RELEASE FFRAMES-RELEASE FRANGES-RELEASE FWHY-RELEASE FSLOTS-RELEASE
   RES-KEY-RELEASE PRIMITIVE-DATA-RELEASE
   8 0 ?do 0 CELL-BYTES i + c! loop ;

\ Close the innermost definition frame.
: CLOSE ( -- )
   FRAME {: off:n :}
   NFILE-REF:END-DEFINITION
   off USED !
   -1 DEPTH +!
   UNOBSERVE ;

\ Installation is an explicit boundary: no owner field means no reusable
\ checked binding, while an actual old object with no section is malformed.
: USE-CONTRACT ( [ n -- ptr u8 n bool ] -- ) is CONTRACT$ ;
: USE-CHECKER-ROLE ( [ -- ptr u8 n bool ] -- ) is CHECKER-ROLE$ ;
: USE-COMPILER-ROLE ( [ -- ptr u8 n bool ] -- ) is COMPILER-ROLE$ ;
: USE-PRIMITIVE-STATE ( [ n n -- n bool ] -- ) is PRIMITIVE-STATE ;
: USE-SYM-IDENTITY ( [ n -- ptr u8 n ptr u8 n n ] -- ) is SYM-IDENTITY ;
: USE-ORIGIN-SYM ( [ ptr u8 n n ptr u8 n -- n ] -- ) is ORIGIN-SYM ;
: UNBIND ( -- ) INSTALL-DEFAULT ;

\ The scanner supplies the exact resolved record ordinal, not a spelling or
\ an interned name. A missing contract is visible as an ordered FILE.
: BIND-ORD ( n -- )
   CAPTURE-BIND ;

;package
