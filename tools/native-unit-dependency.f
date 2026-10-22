\ Observed inputs to one native definition. Section 14 carries declaration identities,
\ checked contracts, frozen primitive schemas, fixed values and implementations
\ actually run while the FILE was built. A nested child keeps a separate span
\ in the same byte arena.

require src/lib/errors.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require src/core/sha256.f
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
3 constant EXECUTED
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

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER FRAMES n
DYNAMIC-BUFFER FRAME-KINDS n
DYNAMIC-BUFFER RES-KEY u8
DYNAMIC-BUFFER PRIMITIVE-DATA u8
0 CWORK-VARIABLE USED n
0 CWORK-VARIABLE DEPTH n
0 CWORK-VARIABLE CHECK-OFF n
0 CWORK-VARIABLE CHECK-OK bool
0 CWORK-VARIABLE MISS-P ptr u8
0 CWORK-VARIABLE MISS-U n
0 CWORK-VARIABLE SCAN-OFF n
create CELL-BYTES 8 allot
create PRODUCER-CTX SHA256-CTX-BYTES allot
create PRODUCER-DIGEST 32 allot

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
defer CLOSURE-EACH ( n [ n -- ] -- bool )
: NO-CLOSURE ( n [ n -- ] -- bool ) 2drop false ;
: INSTALL-DEFAULT ( -- )
   ['] NO-CONTRACT is CONTRACT$
   ['] NO-ROLE is CHECKER-ROLE$
   ['] NO-ROLE is COMPILER-ROLE$
   ['] NO-PRIMITIVE is PRIMITIVE-STATE
   ['] NO-IDENTITY is SYM-IDENTITY
   ['] NO-ORIGIN is ORIGIN-SYM
   ['] NO-CLOSURE is CLOSURE-EACH ;
INSTALL-DEFAULT

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;

: FRAME ( -- n )
   DEPTH @ 0 <= if BAD then
   DEPTH @ 1- FRAMES @ ;

: ROOM ( n -- n )
   {: need:n :}
   need 0 < need MEM-MAX-N USED @ - > or if BAD then
   USED @ need + BYTES-RESERVE
   USED @ dup need + USED ! ;

: ADD-ROW ( n ptr u8 n ptr u8 n -- )
   {: kind:n key:ptr keyu:n data:ptr datau:n :}
   DEPTH @ 0= if BAD then
   keyu 0 < datau 0 < or if BAD then
   kind ORDERED <> kind CHECKER-ROLE <> and kind COMPILER-ROLE <> and
      kind SCOPE <> and
      keyu 0= and if BAD then
   keyu MEM-MAX-N ROW-BYTES - > if BAD then
   datau MEM-MAX-N ROW-BYTES - keyu - > if BAD then
   ROW-BYTES keyu + datau + ROOM {: off:n :}
   kind off BYTES LE:U64!
   keyu off 8 + BYTES LE:U64!
   datau off 16 + BYTES LE:U64!
   \ A zero-length field ends exactly at the reserved bound for a trailing
   \ ORDERED row; forming BYTES at that one-past-end offset is invalid.
   keyu 0 > if key off ROW-BYTES + BYTES keyu BYTE-COPY then
   datau 0 > if data off ROW-BYTES keyu + + BYTES datau BYTE-COPY then
   FRAME 8 + BYTES dup LE:U64@ 1+ swap LE:U64! ;

: BINDING-ROW ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n contract:ptr contractu:n :}
   contractu 0= if BAD then
   BINDING key keyu contract contractu ADD-ROW ;

: VALUE-ROW ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n result:ptr resultu:n :}
   resultu 8 <> if BAD then
   VALUE key keyu result resultu ADD-ROW ;

: EXECUTED-ROW ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n producer:ptr produceru:n :}
   produceru 32 <> if BAD then
   EXECUTED key keyu producer produceru ADD-ROW ;

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

: CHECK-EXECUTED ( ptr u8 n ptr u8 n -- bool )
   {: key:ptr keyu:n want:ptr wantu:n :}
   key keyu CURRENT {: rec:ptr ord:n found:bool :}
   rec drop
   found 0= if s" executed binding absent" REMEMBER-MISS false exit then
   ord NFILE-REF:PRODUCER$ {: got:ptr gotu:n known:bool :}
   known 0= if s" executed producer unknown" REMEMBER-MISS false exit then
   got gotu want wantu STR=
   dup 0= if s" executed producer changed" REMEMBER-MISS then ;

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
   kind EXECUTED = if
      keyu 0= datau 32 <> or if MALFORMED then
      key keyu data datau CHECK-EXECUTED exit
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

: CHECK-NEXT ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size CHECK-OFF @ - ROW-BYTES < if MALFORMED then
   bytes CHECK-OFF @ + {: row:ptr :}
   row 8 + LE:U64@ {: keyu:n :}
   row 16 + LE:U64@ {: datau:n :}
   keyu 0 < datau 0 < or if MALFORMED then
   keyu size CHECK-OFF @ - ROW-BYTES - > if MALFORMED then
   datau size CHECK-OFF @ - ROW-BYTES - keyu - > if MALFORMED then
   row LE:U64@ row ROW-BYTES + keyu
      row ROW-BYTES keyu + + datau CHECK-ROW
   0= if false CHECK-OK ! then
   CHECK-OFF @ ROW-BYTES keyu + datau + + CHECK-OFF ! ;

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

: DIAG-ORDERED ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   reason reasonu
   ord 0 >= if ord XREF-REC XREF-NAME$ else NULL-PTR 0 then
   ord DIAG-ORDERED-NAME ;

: CAPTURE-BIND ( n -- )
   {: ord:n :}
   DEPTH @ 0= if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if
      s" binding-key-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord CONTRACT$ {: contract:ptr contractu:n known:bool :}
   known 0= if
      s" binding-contract-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   key keyu contract contractu BINDING-ROW ;

: CAPTURE-READER-FLAGS ( n n -- )
   {: ord:n flags:n :}
   DEPTH @ 0= if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if
      s" reader-key-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   flags DNAME-IMM and CELL-BYTES LE:U64!
   READER-FLAGS key keyu CELL-BYTES 8 ADD-ROW ;

: CAPTURE-VALUE ( n -- )
   {: ord:n :}
   DEPTH @ 0= if exit then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if
      s" value-key-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord FIXED-RAW? {: value:n known:bool :}
   known 0= if
      s" fixed-value-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   value CELL-BYTES LE:U64!
   key keyu CELL-BYTES 8 VALUE-ROW ;

: CAPTURE-EXEC-ENTRY ( n -- )
   NFILE-REF:IMPL-OWNER {: ord:n owned:bool :}
   owned 0= if
      s" executed-entry-unowned" -1 DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord NFILE-REF:CURRENT-RECORD? if exit then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if
      s" executed-key-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord NFILE-REF:PRODUCER$ {: producer:ptr produceru:n known:bool :}
   known 0= if
      s" executed-producer-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   key keyu producer produceru EXECUTED-ROW ;

: CAPTURE-EXEC ( n -- )
   {: ord:n :}
   DEPTH @ 0= if exit then
   ord XREF-REC XREF-START ['] CAPTURE-EXEC-ENTRY CLOSURE-EACH
   0= if
      s" execution-closure-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW
   then ;

: ADD-RESOLUTION ( n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: role:n shape:n spelling:ptr spellingu:n namespace:ptr namespaceu:n
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
   RESOLUTION 0 RES-KEY RES-KEY-BYTES spellingu + namespaceu +
      key keyu ADD-ROW ;

: OBSERVE-RESOLUTION ( ptr u8 n -- )
   {: row:ptr rowu:n :}
   DEPTH @ 0= if exit then
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
      s" resolution-ambiguous" row RES-ROW-BYTES + spellingu -1
         DIAG-ORDERED-NAME
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord 0 >= if
      ord NFILE-REF:CURRENT-RECORD? if exit then
      ord NFILE-REF:DECL-KEY? 0= if
         2drop s" resolution-key-unknown" ord DIAG-ORDERED
         ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
      then 2drop
   then
   ord -1 = if
      role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         NULL-PTR 0 ADD-RESOLUTION
   else
      ord NFILE-REF:DECL-KEY$ {: key:ptr keyu:n :}
      role shape row RES-ROW-BYTES + spellingu
         row RES-ROW-BYTES spellingu + + namespaceu
         key keyu ADD-RESOLUTION
   then ;

: ABSENCE-DECL ( n -- ) {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if
      rec XREF-NAME$ {: name:ptr nameu:n :}
      8 name nameu name nameu NFILE-REF:RESOLVE-CANDIDATE
         {: got:n shape:n :}
      got -1 <> if
         s" namespace-present" name nameu got DIAG-ORDERED-NAME
         ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
      then
      8 shape name nameu name nameu NULL-PTR 0 ADD-RESOLUTION
      exit
   then
   rec NFILE-REF:PACKAGE-OF {: namespace:ptr namespaceu:n vis:n :}
   vis 3 = if exit then
   rec XREF-NAME$ {: name:ptr nameu:n :}
   vis 0= if 0 else vis then {: role:n :}
   role name nameu namespace namespaceu
      NFILE-REF:RESOLVE-CANDIDATE {: got:n shape:n :}
   got -1 <> if
      s" declaration-present" name nameu got DIAG-ORDERED-NAME
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   role shape name nameu namespace namespaceu NULL-PTR 0
      ADD-RESOLUTION ;

: CAPTURE-CHECKER-ROLE ( -- )
   DEPTH @ 0= if exit then
   CHECKER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if
      s" checker-producer-unknown" -1 DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   digestu 32 <> if BAD then
   CHECKER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

: CAPTURE-COMPILER-ROLE ( -- )
   DEPTH @ 0= if exit then
   COMPILER-ROLE$ {: digest:ptr digestu:n known:bool :}
   known 0= if
      s" compiler-producer-unknown" -1 DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   digestu 32 <> if BAD then
   COMPILER-ROLE NULL-PTR 0 digest digestu ADD-ROW ;

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
   known 0= if
      s" primitive-state-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   ord NFILE-REF:DECL-KEY? {: key:ptr keyu:n keyed:bool :}
   keyed 0= if
      s" primitive-key-unknown" ord DIAG-ORDERED
      ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW exit
   then
   sym control PRIMITIVE-DATA$ {: data:ptr datau:n :}
   PRIMITIVE key keyu data datau ADD-ROW
   CAPTURE-CHECKER-ROLE ;

\ This row is the checker's exact resolution from the successful source scan.
\ The tape's symbol interner is only a name; it is never a declaration key.
: OBSERVE-BOUND ( ptr u8 n -- )
   {: row:ptr size:n :}
   DEPTH @ 0= if exit then
   size CHECKER-OWNER-ABI:BOUND-CELLS cells <> if
      CHECKER-OWNER-ABI:BINDING-RC throw
   then
   row CHECKER-OWNER-ABI:BOUND-KIND BOUND@ {: kind:n :}
   kind CHECKER-OWNER-ABI:BOUND-DICT = if
      row CHECKER-OWNER-ABI:BOUND-RECORD BOUND@ {: ord:n :}
      ord 0 < if CHECKER-OWNER-ABI:BINDING-RC throw then
      row CHECKER-OWNER-ABI:BOUND-CTL BOUND@
         CHECKER-OWNER-ABI:BOUND-SEEDED and 0<> if
         row ord CAPTURE-PRIMITIVE
      else ord CAPTURE-BIND then
      row CHECKER-OWNER-ABI:BOUND-FLAGS BOUND@ {: flags:n :}
      ord flags CAPTURE-READER-FLAGS
      flags DKIND:MASK and DKIND:VAL = if ord CAPTURE-VALUE then
      flags DNAME-IMM and 0<> if ord CAPTURE-EXEC then
      exit
   then
   kind CHECKER-OWNER-ABI:BOUND-INTRINSIC = if
      CAPTURE-CHECKER-ROLE
   then ;

public

: BEGIN-UNIT ( -- )
   DEPTH @ 1+ FRAMES-RESERVE
   DEPTH @ 1+ FRAME-KINDS-RESERVE
   USED @ DEPTH @ FRAMES !
   0 DEPTH @ FRAME-KINDS !
   1 DEPTH +!
   HEADER-BYTES ROOM {: off:n :}
   off FRAME <> if BAD then
   s" HBFDEP02" drop off BYTES 8 BYTE-COPY
   0 off 8 + BYTES LE:U64!
   NFILE-SCOPE:SCOPE-INPUT$ {: scope:ptr scopeu:n :}
   SCOPE NULL-PTR 0 scope scopeu ADD-ROW
   ['] OBSERVE-BOUND NFEED:USE-BINDINGS
   ['] OBSERVE-RESOLUTION NFEED:USE-RESOLUTIONS ;

: BEGIN-DEFINITION ( n -- )
   {: ord:n :}
   ord NFILE-REF:BEGIN-DEFINITION
   BEGIN-UNIT
   \ The feed reads the source owner's candidates only after the scan. Enable
   \ their producer for this definition; top-level namespace actions still run.
   [: 2drop ;] CHECKER-OWNER:RESOLUTION-NS-INSTALL
   1 DEPTH @ 1- FRAME-KINDS ! ;

: BINDING+ ( ptr u8 n ptr u8 n -- ) BINDING-ROW ;
: VALUE+ ( ptr u8 n ptr u8 n -- ) VALUE-ROW ;
: EXECUTED+ ( ptr u8 n ptr u8 n -- ) EXECUTED-ROW ;
: ORDERED! ( -- )
   s" ordered-source-action" -1 DIAG-ORDERED
   ORDERED NULL-PTR 0 NULL-PTR 0 ADD-ROW ;
: COMPILED! ( -- ) CAPTURE-COMPILER-ROLE ;
: TOP-BOUND ( ptr u8 n -- ) OBSERVE-BOUND ;
: TOP-RESOLUTION ( ptr u8 n -- ) OBSERVE-RESOLUTION ;
: ABSENT-NEW ( n n -- ) {: first:n last:n :}
   first 0 < last first < or last ndict@ > or if BAD then
   last first ?do i ABSENCE-DECL loop ;

: EXPORT ( -- ptr u8 n )
   FRAME {: off:n :}
   off BYTES USED @ off - ;

\ A declaration's producer is the source bytes and the observations needed
\ to construct it. The FILE owner can publish this digest for declarations it
\ owns after all its source actions have run. An unproved action leaves no
\ portable producer for downstream FILEs to rely on.
: FINAL-PRODUCER$ ( ptr u8 n -- ptr u8 n bool )
   {: source:ptr sourceu:n :}
   sourceu 32 <> if BAD then
   EXPORT {: deps:ptr depsu:n :}
   deps depsu HAS-ORDERED? if NULL-PTR 0 false exit then
   PRODUCER-CTX SHA256-BEGIN
   PRODUCER-CTX s" native-definition-construction-v1" SHA256-FEED
   PRODUCER-CTX source 32 SHA256-FEED
   PRODUCER-CTX deps depsu SHA256-FEED
   PRODUCER-CTX PRODUCER-DIGEST SHA256-END
   PRODUCER-DIGEST 32 true ;

: CHECK ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   size HEADER-BYTES < if MALFORMED then
   bytes 8 s" HBFDEP02" STR= 0= if MALFORMED then
   bytes 8 + LE:U64@ {: count:n :}
   count 0 < count size HEADER-BYTES - ROW-BYTES / > or if MALFORMED then
   HEADER-BYTES CHECK-OFF !
   true CHECK-OK !
   NULL-PTR MISS-P ! 0 MISS-U !
   count 0 ?do bytes size CHECK-NEXT loop
   CHECK-OFF @ size <> if MALFORMED then
   CHECK-OK @ 0= if REPORT-MISS then
   CHECK-OK @ ;

: RESET ( -- )
   NFILE-REF:END-DEFINITION
   CHECKER-OWNER:RESOLUTION-NS-DEFAULT
   0 DEPTH !
   0 USED !
   NFEED:BINDINGS-DEFAULT
   NFEED:RESOLUTIONS-DEFAULT
   BYTES-RELEASE FRAMES-RELEASE FRAME-KINDS-RELEASE
   RES-KEY-RELEASE PRIMITIVE-DATA-RELEASE
   8 0 ?do 0 CELL-BYTES i + c! loop
   SHA256-CTX-BYTES 0 ?do 0 PRODUCER-CTX i + c! loop
   32 0 ?do 0 PRODUCER-DIGEST i + c! loop ;

: CLOSE ( -- )
   FRAME {: off:n :}
   DEPTH @ 1- FRAME-KINDS @ 0<> if NFILE-REF:END-DEFINITION then
   off USED !
   -1 DEPTH +!
   DEPTH @ 0= if
      NFEED:BINDINGS-DEFAULT
      NFEED:RESOLUTIONS-DEFAULT
      CHECKER-OWNER:RESOLUTION-NS-DEFAULT
   then ;

\ Installation is an explicit boundary: no owner field means no reusable
\ checked binding, while an actual old object with no section is malformed.
: USE-CONTRACT ( [ n -- ptr u8 n bool ] -- ) is CONTRACT$ ;
: USE-CHECKER-ROLE ( [ -- ptr u8 n bool ] -- ) is CHECKER-ROLE$ ;
: USE-COMPILER-ROLE ( [ -- ptr u8 n bool ] -- ) is COMPILER-ROLE$ ;
: USE-PRIMITIVE-STATE ( [ n n -- n bool ] -- ) is PRIMITIVE-STATE ;
: USE-SYM-IDENTITY ( [ n -- ptr u8 n ptr u8 n n ] -- ) is SYM-IDENTITY ;
: USE-ORIGIN-SYM ( [ ptr u8 n n ptr u8 n -- n ] -- ) is ORIGIN-SYM ;
: USE-CLOSURE ( [ n [ n -- ] -- bool ] -- ) is CLOSURE-EACH ;
: UNBIND ( -- ) INSTALL-DEFAULT ;

\ The scanner supplies the exact resolved record ordinal, not a spelling or
\ an interned name. A missing contract is visible as an ordered FILE.
: BIND-ORD ( n -- )
   CAPTURE-BIND ;

: VALUE-ORD ( n -- )
   CAPTURE-VALUE ;

: EXEC-ORD ( n -- )
   CAPTURE-EXEC ;

;package
