\ External attribution for a stripped link. The linker has already selected
\ disjoint members and their final offsets; this file only records that plan.
require src/lib/fs.f
require src/lib/string.f
require src/core/sha256.f
require src/os/script-argv.f
require src/habu/code-span.f
require tools/image-names.f

package AOT-LINK

DYNAMIC-BUFFER MAP-TEXT u8
variable MAP-U
create MAP-PATH PATH-CAP allot
variable MAP-PATH-U
create MAP-IMAGE PATH-CAP allot
variable MAP-IMAGE-U
create MAP-HASH-CTX SHA256-FILE-CTX-BYTES allot
create MAP-HASH-HEX 64 allot
create MAP-ONE 1 allot
create MAP-NUM-BUF 20 allot
variable MAP-NUM-I
variable MAP-ROW-N
variable MAP-DONOR-USED

: MAP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   MAP-U @ u + MAP-TEXT-RESERVE
   a 0 MAP-TEXT BYTE-VIEW MAP-U @ + u BYTE-COPY
   MAP-U @ u + MAP-U ! ;

: MAP-CHAR+ ( n -- ) MAP-ONE c! MAP-ONE 1 MAP+ ;

: MAP-DIGITS+ ( n -- ) {: value:n :}
   value 0= if s" 0" MAP+ exit then
   20 MAP-NUM-I !
   value begin dup 0 > while
      dup 10 mod 48 +
      MAP-NUM-I @ 1- MAP-NUM-I !
      MAP-NUM-BUF MAP-NUM-I @ + c!
      10 /
   repeat drop
   MAP-NUM-BUF MAP-NUM-I @ + 20 MAP-NUM-I @ - MAP+ ;

: MAP-INT+ ( n -- ) {: value:n :}
   value 0 < if s" -" MAP+ 0 value - MAP-DIGITS+ exit then
   value MAP-DIGITS+ ;

: MAP-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 4 + PATH-CAP > if E-FS-CAPACITY throw then
   a MAP-PATH u BYTE-COPY
   s" .map" drop MAP-PATH u + 4 BYTE-COPY
   u 4 + MAP-PATH-U ! ;

: MAP-DONOR$ ( -- ptr u8 n bool )
   SCRIPT-ARGC 4 < if NULL$ false exit then
   SCRIPT-ARGC 2 - SCRIPT-ARGV$ s" --donor" STR= if
      SCRIPT-ARGC 1- SCRIPT-ARGV$ true exit
   then NULL$ false ;

: MAP-OWNER ( n -- ptr u8 n n ) {: wid:n :}
   wid 0= if s" global" IMAGE-NAMES:OWNER-GLOBAL exit then
   wid 0 < if s" -" IMAGE-NAMES:OWNER-UNKNOWN exit then
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC wid = if
            rec XREF-NAME$ IMAGE-NAMES:OWNER-PUBLIC unloop exit then
         rec XREF-PKG-PRIVATE wid = if
            rec XREF-NAME$ IMAGE-NAMES:OWNER-PRIVATE unloop exit then
      then
   loop s" -" IMAGE-NAMES:OWNER-UNKNOWN ;

: MAP-SPACE ( -- ) 32 MAP-CHAR+ ;

: MAP-HEAD ( n n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: named:n start:n bytes:n wid:n name:ptr nameu:n kind:ptr kindu:n origin:ptr originu:n :}
   MAP-ROW-N @ MAP-INT+ MAP-SPACE
   named MAP-INT+ MAP-SPACE
   start MAP-INT+ MAP-SPACE
   bytes CODE-SPAN:FULL or MAP-INT+ MAP-SPACE
   wid MAP-INT+ MAP-SPACE
   nameu 0 > if name nameu else s" -" then MAP+ MAP-SPACE
   kind kindu MAP+ MAP-SPACE
   origin originu MAP+ MAP-SPACE ;

: MAP-TAIL ( ptr u8 n -- )
   {: source:ptr sourceu:n :}
   sourceu 0 > if source sourceu else s" -" then MAP+
   10 MAP-CHAR+
   MAP-ROW-N @ 1+ MAP-ROW-N ! ;

: MAP-RUNTIME ( n n -- ) {: start:n bytes:n :}
   bytes 0 <= if exit then
   0 start bytes -1 s" runtime" s" runtime" s" generated" MAP-HEAD
   s" -" MAP-TAIL ;

: MAP-ANON-DONOR ( n -- ptr u8 n bool ) {: k:n :}
   k CLO-AT-N SPAN-OWNER {: span:n :}
   span 0 < if NULL$ false exit then
   k CLO-AT-N span SPAN-START-N =
   k CLO-BYTES span SPAN-BYTES = and 0= if NULL$ false exit then
   k CLO-AT-N SPAN-BASE-N - k CLO-BYTES IMAGE-NAMES:DONOR-ANON-ROW ;

: MAP-ANON ( n -- ) {: k:n :}
   k CLO-AT-N WINDOW-CODE-START @ < if
      k MAP-ANON-DONOR if
         {: donor:ptr donoru:n :}
         donor donoru IMAGE-NAMES:DONOR-ROW-WID IMAGE-NAMES:DONOR-WID
         {: wid:n :}
         wid 0 < wid -1 <> and if true MAP-DONOR-USED ! then
         donor donoru IMAGE-NAMES:DONOR-ROW-NAME$ {: name:ptr nameu:n :}
         0 k MEMBER-NEWOFF k BLEN @ wid name nameu s" word" s" baked" MAP-HEAD
         donor donoru IMAGE-NAMES:DONOR-ROW-SOURCE$ MAP-TAIL
         exit
      then 2drop
   then
   k CLO-AT-N WINDOW-CODE-START @ >=
      if s" loaded" else s" unknown" then {: origin:ptr originu:n :}
   0 k MEMBER-NEWOFF k BLEN @ -1 s" -" s" word" origin originu MAP-HEAD
   s" -" MAP-TAIL ;

: MAP-REC-BAKED? ( n ptr n -- bool ) {: k:n rec:ptr :}
   k CLO-AT-N WINDOW-CODE-START @ >= if false exit then
   k CLO-AT-N SPAN-BASE-N - k CLO-BYTES rec REC-NAME@
   rec REC-WID@ MAP-OWNER IMAGE-NAMES:DONOR-RECORD? ;

: MAP-RECORD ( n ptr n -- ) {: k:n rec:ptr :}
   rec REC-NAME@ {: name:ptr nameu:n :}
   rec REC-WID@ {: wid:n :}
   k rec MAP-REC-BAKED? {: baked:bool :}
   baked if s" baked" else
      k CLO-AT-N WINDOW-CODE-START @ >= if s" loaded" else s" unknown" then
   then {: origin:ptr originu:n :}
   0 k MEMBER-NEWOFF k BLEN @ wid name nameu s" word" origin originu MAP-HEAD
   baked if
      k CLO-AT-N SPAN-BASE-N - k CLO-BYTES name nameu
      wid MAP-OWNER IMAGE-NAMES:DONOR-SOURCE$
      if MAP-TAIL else 2drop s" -" MAP-TAIL then
   else s" -" MAP-TAIL then ;

: MAP-MEMBER ( n -- ) {: k:n :}
   k CLO-REC@ dup XREF-FOUND? if k swap MAP-RECORD else drop k MAP-ANON then ;

: MAP-NS-USED? ( n n -- bool ) {: pub:n priv:n :}
   NCLO @ 0 ?do
      i CLO-REC@ dup XREF-FOUND? if
         REC-WID@ dup pub = swap priv = or if true unloop exit then
      else drop then
   loop false ;

: MAP-NAMESPACE ( ptr n -- ) {: rec:ptr :}
   MAP-ROW-N @ MAP-INT+ MAP-SPACE
   0 MAP-INT+ MAP-SPACE
   rec XREF-PKG-PUBLIC MAP-INT+ MAP-SPACE
   rec XREF-PKG-PRIVATE MAP-INT+ MAP-SPACE
   -1 MAP-INT+ MAP-SPACE
   rec XREF-NAME$ MAP+ MAP-SPACE
   s" namespace unknown -" MAP+ 10 MAP-CHAR+
   MAP-ROW-N @ 1+ MAP-ROW-N ! ;

: MAP-DONOR-NAMESPACE ( n n ptr u8 n -- )
   {: pub:n priv:n name:ptr nameu:n :}
   MAP-ROW-N @ MAP-INT+ MAP-SPACE
   0 MAP-INT+ MAP-SPACE
   pub MAP-INT+ MAP-SPACE
   priv MAP-INT+ MAP-SPACE
   -1 MAP-INT+ MAP-SPACE
   name nameu MAP+ MAP-SPACE
   s" namespace baked -" MAP+ 10 MAP-CHAR+
   MAP-ROW-N @ 1+ MAP-ROW-N ! ;

: MAP-NAMESPACES ( -- )
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC rec XREF-PKG-PRIVATE MAP-NS-USED? if
            rec MAP-NAMESPACE
         then
      then
   loop
   MAP-DONOR-USED @ if
      [: MAP-DONOR-NAMESPACE ;] IMAGE-NAMES:DONOR-NAMESPACES-EACH
   then ;

: MAP-ROWS ( -- )
   0 MAP-ROW-N !  false MAP-DONOR-USED !
   0 0 MEMBER-NEWOFF MAP-RUNTIME
   NCLO @ 0 ?do i MAP-MEMBER loop
   NCLO @ 1- {: last:n :}
   last MEMBER-NEWOFF last BLEN @ + {: end:n :}
   end CODE-END @ end - MAP-RUNTIME
   MAP-NAMESPACES ;

: MAP-WRITE ( -- )
   AOT-OUT {: image:ptr imageu:n :}
   imageu PATH-CAP > if E-FS-CAPACITY throw then
   image MAP-IMAGE imageu BYTE-COPY
   imageu MAP-IMAGE-U !
   image imageu MAP-PATH!
   MAP-DONOR$ if IMAGE-NAMES:LOAD else 2drop then
   0 MAP-U !
   S\" habu-names 1\n" MAP+
   S\" columns rec named start len wid name kind origin source\n" MAP+
   MAP-HASH-CTX MAP-IMAGE MAP-IMAGE-U @ MAP-HASH-HEX SHA256-FILE-HEX-IN
      dup 0<> if throw then drop
   s" image-sha256 " MAP+ MAP-HASH-HEX 64 MAP+ 10 MAP-CHAR+
   MAP-ROWS
   MAP-PATH MAP-PATH-U @ 0 MAP-TEXT BYTE-VIEW MAP-U @ FS:WRITE-ALL ;

;package
