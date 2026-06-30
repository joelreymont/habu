\ suite.f - TEST package suite/group/test implementation.
\
\ Loaded by lib/test.f. A project adapter configures setup, teardown, argument
\ construction, selection, and runner words; suite files define named
\ groups/tests, then call TEST:RUN once.

require lib/errors.f
require lib/string.f

package TEST

128 constant NAME-CAP
1024 constant STDIN-CAP
128 constant ITEM-MAX
32 constant GROUP-MAX
$10000 constant ARG-CAP
0 constant GROUP-PARALLEL
1 constant GROUP-SEQUENTIAL
0 constant ITEM-FILE
1 constant ITEM-STDIN

create ITEM-NAMES ITEM-MAX NAME-CAP * allot
create ITEM-NAME-US ITEM-MAX cells allot
create ITEM-KINDS ITEM-MAX cells allot
create ITEM-GROUPS ITEM-MAX cells allot
create ITEM-ARG-OFFS ITEM-MAX cells allot
create ITEM-ARG-COUNTS ITEM-MAX cells allot
create ITEM-STDINS ITEM-MAX STDIN-CAP * allot
create ITEM-STDIN-US ITEM-MAX cells allot
create GROUP-NAMES GROUP-MAX NAME-CAP * allot
create GROUP-NAME-US GROUP-MAX cells allot
create GROUP-MODES GROUP-MAX cells allot
create ARGS ARG-CAP allot
create CUR-LABEL-BUF NAME-CAP allot

variable ITEM-N
variable GROUP-N
variable GROUP-CUR
variable ARG-U
variable ARG-SCAN
variable DEF-ID
variable LAST-GROUP
variable CUR-LABEL-U

defer SETUP ( -- )
defer TEARDOWN ( -- )
defer DRAIN ( -- )
defer ARGS-BEGIN ( -- )
defer ARG+ ( ptr u8 n -- )
defer SELECT? ( -- bool )
defer RUNNER ( ptr u8 n -- )
defer STDIN-RUNNER ( ptr u8 n ptr u8 n -- )

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   0 0= 0= ;

: NOOP ( -- )
;

: ARG-DROP ( ptr u8 n -- )
   2drop ;

: YES ( -- bool )
   TRUE ;

: RUN-MISSING ( ptr u8 n -- )
   2drop E-FS-OPEN throw ;

: STDIN-RUN-MISSING ( ptr u8 n ptr u8 n -- )
   2drop 2drop E-FS-OPEN throw ;

: CAP-CHECK ( n n -- ) {: u:n cap:n :}
   u 0 < if E-STR-BOUNDS throw then
   u cap > if E-STR-CAPACITY throw then ;

: ITEM-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id ITEM-MAX >= if E-TBL-BOUNDS throw then ;

: GROUP-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id GROUP-MAX >= if E-TBL-BOUNDS throw then ;

: ITEM-NAME-BUF ( n -- ptr u8 ) {: id:n :}
   id ITEM-CHECK
   ITEM-NAMES id NAME-CAP * + ;

: ITEM-STDIN-BUF ( n -- ptr u8 ) {: id:n :}
   id ITEM-CHECK
   ITEM-STDINS id STDIN-CAP * + ;

: GROUP-NAME-BUF ( n -- ptr u8 ) {: id:n :}
   id GROUP-CHECK
   GROUP-NAMES id NAME-CAP * + ;

: ITEM-CELL ( ptr a n -- ptr a ) {: base:ptr id:n :}
   id ITEM-CHECK
   base id cells + ;

: GROUP-CELL ( ptr a n -- ptr a ) {: base:ptr id:n :}
   id GROUP-CHECK
   base id cells + ;

: ITEM-NAME! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   u NAME-CAP CAP-CHECK
   a id ITEM-NAME-BUF u BYTE-COPY
   u ITEM-NAME-US id ITEM-CELL ! ;

: CUR-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-CAP CAP-CHECK
   a CUR-LABEL-BUF u BYTE-COPY
   u CUR-LABEL-U ! ;

: GROUP-NAME! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   u NAME-CAP CAP-CHECK
   a id GROUP-NAME-BUF u BYTE-COPY
   u GROUP-NAME-US id GROUP-CELL ! ;

: ITEM-STDIN! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   u STDIN-CAP CAP-CHECK
   a id ITEM-STDIN-BUF u BYTE-COPY
   u ITEM-STDIN-US id ITEM-CELL ! ;

: ITEM-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id ITEM-NAME-BUF
   ITEM-NAME-US id ITEM-CELL @ ;

: GROUP-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id GROUP-NAME-BUF
   GROUP-NAME-US id GROUP-CELL @ ;

: ITEM-STDIN$ ( n -- ptr u8 n ) {: id:n :}
   id ITEM-STDIN-BUF
   ITEM-STDIN-US id ITEM-CELL @ ;

: ITEM-GROUP@ ( n -- n )
   ITEM-GROUPS swap ITEM-CELL @ ;

: ITEM-KIND@ ( n -- n )
   ITEM-KINDS swap ITEM-CELL @ ;

: ITEM-ARG-OFF@ ( n -- n )
   ITEM-ARG-OFFS swap ITEM-CELL @ ;

: ITEM-ARG-COUNT@ ( n -- n )
   ITEM-ARG-COUNTS swap ITEM-CELL @ ;

: GROUP-MODE@ ( n -- n )
   GROUP-MODES swap GROUP-CELL @ ;

: PARSE-NAME ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-STR-BOUNDS throw then ;

: END-SUITE? ( ptr u8 n -- bool )
   s" TEST:END-SUITE" STR= ;

: ITEM-ALLOC ( -- n )
   ITEM-N @ ITEM-MAX >= if E-TBL-BOUNDS throw then
   ITEM-N @ {: id:n :}
   id 1+ ITEM-N !
   id ;

: GROUP-ALLOC ( -- n )
   GROUP-N @ GROUP-MAX >= if E-TBL-BOUNDS throw then
   GROUP-N @ {: id:n :}
   id 1+ GROUP-N !
   id ;

: GROUP-ADD ( ptr u8 n n -- n ) {: a:ptr u:n mode:n :}
   GROUP-ALLOC {: id:n :}
   a u id GROUP-NAME!
   mode GROUP-MODES id GROUP-CELL !
   id ;

: DEFAULT-GROUP ( -- )
   s" default" GROUP-PARALLEL GROUP-ADD GROUP-CUR ! ;

: ARG-ROOM ( n -- )
   ARG-U @ + ARG-CAP > if E-STR-CAPACITY throw then ;

: ARG-WRITE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u cell + ARG-ROOM
   ARG-U @ {: off:n :}
   u ARGS off + !
   a ARGS off + cell + u BYTE-COPY
   off cell + u + ARG-U ! ;

: ITEM-ADD-ARG ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   a u ARG-WRITE
   ITEM-ARG-COUNTS id ITEM-CELL @ 1+ ITEM-ARG-COUNTS id ITEM-CELL ! ;

: ITEM-ARGS-BEGIN ( n -- ) {: id:n :}
   ARG-U @ ITEM-ARG-OFFS id ITEM-CELL !
   0 ITEM-ARG-COUNTS id ITEM-CELL ! ;

: PARSE-ARGS ( n -- ) {: id:n :}
   0 DEF-ID !
   begin DEF-ID @ 0= while
      parse-name dup 0= if 2drop E-FS-CAPACITY throw then
      2dup END-SUITE? if
         2drop -1 DEF-ID !
      else
         id ITEM-ADD-ARG
      then
   repeat ;

: ITEM-ADD ( n ptr u8 n ptr u8 n -- ) {: kind:n name:ptr nameu:n inptr:ptr inu:n :}
   ITEM-ALLOC {: id:n :}
   name nameu id ITEM-NAME!
   kind ITEM-KINDS id ITEM-CELL !
   GROUP-CUR @ ITEM-GROUPS id ITEM-CELL !
   inptr inu id ITEM-STDIN!
   id ITEM-ARGS-BEGIN
   id PARSE-ARGS ;

: LABEL-CURRENT ( n -- ) {: id:n :}
   id ITEM-NAME$ CUR-LABEL! ;

: GROUP-HEADER? ( n -- bool ) {: gid:n :}
   gid LAST-GROUP @ <> ;

: GROUP-HEADER ( n -- ) {: gid:n :}
   gid GROUP-HEADER? if
      gid GROUP-MODE@ GROUP-SEQUENTIAL = if DRAIN then
      s" GROUP: " type gid GROUP-NAME$ type
      gid GROUP-MODE@ GROUP-SEQUENTIAL = if s"  (sequential)" else s"  (parallel)" then
      type cr
      gid LAST-GROUP !
   then ;

: SEQUENTIAL? ( n -- bool )
   ITEM-GROUP@ GROUP-MODE@ GROUP-SEQUENTIAL = ;

: ITEM-ARGS-FEED ( n -- ) {: id:n :}
   id ITEM-ARG-OFF@ ARG-SCAN !
   id ITEM-ARG-COUNT@ 0 ?do
      ARGS ARG-SCAN @ + @ {: u:n :}
      ARGS ARG-SCAN @ + cell + u ARG+
      ARG-SCAN @ cell + u + ARG-SCAN !
   loop ;

: ITEM-RUN-FILE ( n -- ) {: id:n :}
   ARGS-BEGIN
   id ITEM-ARGS-FEED
   id ITEM-NAME$ RUNNER ;

: ITEM-RUN-STDIN ( n -- ) {: id:n :}
   DRAIN
   ARGS-BEGIN
   id ITEM-ARGS-FEED
   id ITEM-STDIN$ id ITEM-NAME$ STDIN-RUNNER
   DRAIN ;

: ITEM-RUN ( n -- ) {: id:n :}
   id LABEL-CURRENT
   SELECT? 0= if exit then
   id ITEM-GROUP@ GROUP-HEADER
   id SEQUENTIAL? if DRAIN then
   id ITEM-KIND@
   case
      ITEM-FILE of id ITEM-RUN-FILE endof
      ITEM-STDIN of id ITEM-RUN-STDIN endof
      E-TBL-FIELD throw
   endcase
   id SEQUENTIAL? if DRAIN then ;

: RUN-BODY ( -- )
   -1 LAST-GROUP !
   ITEM-N @ 0 ?do i ITEM-RUN loop
   DRAIN ;

: RUN-ACT ( -- )
   SETUP
   [: RUN-BODY ;] catch {: rc:n :}
   TEARDOWN
   rc 0 <> if rc throw then ;

public

: SETUP! ( [ -- ] -- )
   is SETUP ;

: TEARDOWN! ( [ -- ] -- )
   is TEARDOWN ;

: DRAIN! ( [ -- ] -- )
   is DRAIN ;

: ARGS-BEGIN! ( [ -- ] -- )
   is ARGS-BEGIN ;

: ARG+! ( [ ptr u8 n -- ] -- )
   is ARG+ ;

: SELECT?! ( [ -- bool ] -- )
   is SELECT? ;

: RUNNER! ( [ ptr u8 n -- ] -- )
   is RUNNER ;

: STDIN-RUNNER! ( [ ptr u8 n ptr u8 n -- ] -- )
   is STDIN-RUNNER ;

: DEFAULTS ( -- )
   [: NOOP ;] SETUP!
   [: NOOP ;] TEARDOWN!
   [: NOOP ;] DRAIN!
   [: NOOP ;] ARGS-BEGIN!
   [: ARG-DROP ;] ARG+!
   [: YES ;] SELECT?!
   [: RUN-MISSING ;] RUNNER!
   [: STDIN-RUN-MISSING ;] STDIN-RUNNER! ;

: RESET ( -- )
   0 ITEM-N !
   0 GROUP-N !
   0 ARG-U !
   DEFAULT-GROUP ;

: LABEL$ ( -- ptr u8 n )
   CUR-LABEL-BUF CUR-LABEL-U @ ;

: LABEL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CUR-LABEL-BUF CUR-LABEL-U @ a u STR= ;

: GROUP-PARALLEL ( -- )
   PARSE-NAME GROUP-PARALLEL GROUP-ADD GROUP-CUR ! ;

: GROUP-SEQUENTIAL ( -- )
   PARSE-NAME GROUP-SEQUENTIAL GROUP-ADD GROUP-CUR ! ;

: END-GROUP ( -- )
   0 GROUP-CUR ! ;

: SUITE ( -- )
   PARSE-NAME {: name:ptr nameu:n :}
   ITEM-FILE name nameu s" " ITEM-ADD ;

: SUITE-STDIN ( -- )
   PARSE-NAME {: name:ptr nameu:n :}
   PARSE-NAME {: inptr:ptr inu:n :}
   ITEM-STDIN name nameu inptr inu ITEM-ADD ;

: END-SUITE ( -- )
;

: RUN ( -- )
   RUN-ACT ;

DEFAULTS
RESET

end-package
