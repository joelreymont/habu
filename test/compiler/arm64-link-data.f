\ arm64-link-data.f - LINKER's link of a stripped build's DATA. Inside one
\ build, after the start-up and the kernel the tree's kernel object holds,
\ LINK-STRIPPED roots the closure at the entry and at each xt cell's value in
\ the data the entry reaches, here all the build body's, and appends a copy of
\ the space as its non-zero runs and the DATA list after the routine band: an xt
\ cell's copy holds its routine's link address and is listed, a quotation's
\ included; a scratch cell's copy is 0 and a DATA pointer's DATA-VA plus its
\ target's offset, neither listed; a plain cell's is its bytes; a placed data
\ address site gives DATA-VA plus the variable's offset, and a variable reached
\ only by data addresses is not placed. The header's DP and its bounds are the
\ build's at DATA-VA; its heap start, namespace marker and signal fd pointer are
\ hb's own; its code cells are listed once, and the entry a build stores with
\ xt! into APP-ENTRY:XT-CELL holds its link address. Every listed cell holds an
\ address in the product's code. A second link gives the same bytes. One
\ process: the boundary under test is in-process; only the refusals
\ run in children.
\ Fixture words are LD- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/test.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/address-carrier.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-data.f
require tools/hb-build-core.f

package LINKER
private
package ARM64-LINK-DATA

CAST: CELL-XT ( n -- [ -- ptr n ] )
CAST: ENTRY-XT ( n -- [ n -- n ] )

DYNAMIC-BUFFER FIRST-LINK u8         \ the first link's code buffer


\ LD-LEAF reaches LD-ADDR by a call and LD-LEAF2 only from the quotation LD-MAKE
\ stores; no code calls LD-LEAF or LD-MAKE.
: FIXTURE ( -- )
   s" variable LD-V  11 LD-V !" evaluate-closed
   s" variable LD-P  LD-V NULL-PTR - LD-P !  LD-P ptr-cell-mark" evaluate-closed
   s" : LD-ADDR ( -- ptr n ) LD-V ;" evaluate-closed
   s" : LD-LEAF ( n -- n ) LD-ADDR @ + ;" evaluate-closed
   s" : LD-LEAF2 ( n -- n ) 5 + ;" evaluate-closed
   s" variable LD-XT  ' LD-LEAF LD-XT xt!" evaluate-closed
   s" variable LD-SCR  7 LD-SCR !  LD-SCR scratch-cell-mark" evaluate-closed
   s" variable LD-Q" evaluate-closed
   s" : LD-MAKE ( -- ) [: LD-LEAF2 ;] LD-Q xt! ;" evaluate-closed
   s" LD-MAKE" evaluate-closed ;


: HOST-NAMED ( ptr u8 n -- n ) XREF-FIND-INDEX XREF-REC XREF-START ;

: EMISSION-NAMED ( ptr u8 n -- n )
   HOST-NAMED REC-AT {: r:n :}
   r 0< if s" arm64-link-data: a fixture word has no shadow record" 1 die then
   r NSHADOW:EMISSION@ ;

\ A fixture variable's offset in the space and the value it holds there.
: CELL-OFF ( ptr u8 n -- n ) HOST-NAMED CELL-XT execute NULL-PTR - BASE @ - ;
: SPACE-VALUE ( ptr u8 n -- n ) CELL-OFF SPACE-AT @ ;

\ The image copy of a fixture variable's cell.
: COPY-VALUE ( ptr u8 n -- n ) CELL-OFF COPY-AT LE:U64@ ;

\ How many times the DATA list holds offset off.
: LIST-COUNT ( n -- n )
   {: off:n :}
   0  DATA-LIST-N 0 ?do
      A64ICODE:CODE DATA-LIST-AT + i 8 * + LE:U64@
      off = if 1 + then
   loop ;

: LISTED? ( n -- bool ) LIST-COUNT 0<> ;

: LISTED-NAMED? ( ptr u8 n -- bool ) CELL-OFF LISTED? ;

: BAND-LINK ( n -- n ) OFFSET@ ARM64:TEXT-VA + ;
: PLACED? ( ptr u8 n -- bool ) EMISSION-NAMED PLACED @ 0<> ;


: XT-CASE ( -- )
   s" LD-LEAF" EMISSION-NAMED {: leaf:n :}
   s" xt cell: its copy holds LD-LEAF's link address" T-LABEL
   s" LD-XT" COPY-VALUE  leaf BAND-LINK T=
   s" xt cell: it is in the DATA list" T-LABEL  s" LD-XT" LISTED-NAMED? TTRUE ;


\ The quotation's code lies in LD-MAKE's emission, so LD-MAKE is placed whole.
: QUOTE-CASE ( -- )
   s" LD-MAKE" EMISSION-NAMED {: make:n :}
   s" LD-Q" COPY-VALUE {: q:n :}
   s" quotation cell: its copy holds the quotation's link address" T-LABEL
   q make BAND-LINK >  q make BAND-LINK make NSHADOW:SIZE + <  and TTRUE
   s" quotation cell: it is in the DATA list" T-LABEL
   s" LD-Q" LISTED-NAMED? TTRUE ;


: STORE-ENTRY ( -- )
   s" LD-LEAF" HOST-NAMED ENTRY-XT  data-base APP-ENTRY:XT-CELL + xt! ;


: DATA-CASE ( -- )
   s" scratch cell: its copy is 0" T-LABEL
   s" LD-SCR" SPACE-VALUE 7 T=  s" LD-SCR" COPY-VALUE 0 T=
   s" plain cell: its copy is its bytes" T-LABEL  s" LD-V" COPY-VALUE 11 T=
   s" scratch cell: not in the DATA list" T-LABEL
   s" LD-SCR" LISTED-NAMED? TFALSE
   s" DATA pointer: its copy is DATA-VA plus LD-V's offset" T-LABEL
   s" LD-P" COPY-VALUE  ARM64:DATA-VA s" LD-V" CELL-OFF + T=
   s" DATA pointer: not in the DATA list" T-LABEL
   s" LD-P" LISTED-NAMED? TFALSE ;


\ The value of emission e's first data address chain in its placed copy.
: DATA-CHAIN ( n -- n )
   {: e:n :}
   A64ICODE:CODE e OFFSET@ + {: band:ptr :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         band e i NSHADOW:ADDR-SITE@ + {: at:ptr :}
         at  band e NSHADOW:SIZE +  ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
         at size ADDRESS-CARRIER:CHAIN-VALUE unloop exit
      then
   loop
   -1 ;

\ Header cell off's image copy holds v.
: HEADER-IS ( ptr u8 n n n -- )
   {: v:n off:n :}
   T-LABEL  off COPY-AT LE:U64@ v T= ;

: LISTED-ONCE ( ptr u8 n n -- )
   {: off:n :}
   T-LABEL  off LIST-COUNT 1 T= ;


: HEADER-CASE ( -- )
   s" header: DP is the space's"
      DP-CELL HOST@ DATA-ADDRESS  DP-CELL HEADER-IS
   s" header: the DP floor is the build's"
      BOOT:DP-FLOOR-CELL HOST@ DATA-ADDRESS  BOOT:DP-FLOOR-CELL HEADER-IS
   s" header: the DP ceiling is the build's"
      BOOT:DP-CEILING-CELL HOST@ DATA-ADDRESS  BOOT:DP-CEILING-CELL HEADER-IS
   s" header: the heap start is hb's"
      BOOT:HEAP-START-CELL HOST@  BOOT:HEAP-START-CELL HEADER-IS
   s" header: the namespace marker is hb's"
      SAVED-NAMESPACE-WL @  BOOT:NAMESPACE-WL-CELL HEADER-IS
   s" header: the signal fd pointer is hb's"
      SIGNAL-ABI:FD-PTR-CELL HOST@  SIGNAL-ABI:FD-PTR-CELL HEADER-IS
   s" header: the code base is listed once"  RBASE-CELL LISTED-ONCE
   s" header: the uncaught reporter is listed once"  UNCGH-CELL LISTED-ONCE
   s" header: the signal stub is listed once"  SIGNAL-ABI:STUB-CELL LISTED-ONCE
   s" header: the code end is listed once"  CODE-END-CELL LISTED-ONCE
   s" header: the entry is LD-LEAF's link address"
      s" LD-LEAF" EMISSION-NAMED BAND-LINK  APP-ENTRY:XT-CELL HEADER-IS
   s" header: the entry is listed once"  APP-ENTRY:XT-CELL LISTED-ONCE ;


\ Whether listed cell off's copy lies in the product's code, its end included.
: IN-CODE? ( n -- bool )
   {: off:n :}
   off COPY-AT LE:U64@ {: v:n :}
   v ARM64:TEXT-VA >=  v ARM64:TEXT-VA DATA-IMAGE-AT + <=  and ;

: LIST-CASE ( -- )
   s" list: every listed cell holds an address in the product's code" T-LABEL
   true  DATA-LIST-N 0 ?do
      A64ICODE:CODE DATA-LIST-AT + i 8 * + LE:U64@ IN-CODE? and
   loop
   TTRUE ;


: CHAIN-CASE ( -- )
   s" data address: LD-ADDR's chain is DATA-VA plus LD-V's offset" T-LABEL
   s" LD-ADDR" EMISSION-NAMED DATA-CHAIN  ARM64:DATA-VA s" LD-V" CELL-OFF + T=
   s" data address: its variable LD-V is not placed" T-LABEL
   s" LD-V" PLACED? TFALSE ;


\ A link again from an empty code buffer.
: SECOND-CASE ( -- )
   A64ICODE:ASM-LEN {: len:n :}
   len FIRST-LINK-RESERVE
   A64ICODE:CODE  0 FIRST-LINK  len BYTE-COPY
   LINK-STRIPPED
   s" a second link gives the same code buffer bytes" T-LABEL
   A64ICODE:ASM-LEN len T=
   A64ICODE:CODE len  0 FIRST-LINK len  STR= TTRUE
   FIRST-LINK-RELEASE ;


: BODY ( -- )
   FIXTURE
   STORE-ENTRY
   LINK-STRIPPED
   XT-CASE QUOTE-CASE DATA-CASE HEADER-CASE LIST-CASE CHAIN-CASE
   SECOND-CASE ;


: CHILD-LABEL ( ptr u8 n ptr u8 n -- )
   {: name:ptr name-n:n what:ptr what-n:n :}
   SB-RESET  s" refusal, " SB-APPEND  name name-n SB-APPEND
   s" : " SB-APPEND  what what-n SB-APPEND  SB$ T-LABEL ;

: CHILD-RC ( CMD:command ptr u8 n -- CMD:command n )
   {: name:ptr name-n:n :}
   s" --load" CMD:ARG+  s" test/compiler/arm64-link-data-bad.f" CMD:ARG+
   s" --" CMD:ARG+  name name-n CMD:ARG+  HB-BUILD-CORE:KERNEL$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

: BAD-RUN ( CMD:command ptr u8 n ptr u8 n n ptr u8 n -- CMD:command )
   {: name:ptr name-n:n pre:ptr pre-n:n v:n err:ptr err-n:n :}
   name name-n CHILD-RC {: rc:n :}
   name name-n s" the child arms before the link" CHILD-LABEL
   CMD:OUT$ s\" ARM64-LINK-DATA-BAD-ARMED\n" T$=
   name name-n s" the child names the value" CHILD-LABEL
   SB-RESET  pre pre-n SB-APPEND  v FMT:SB-INT  10 SB-APPEND-C
   CMD:ERR$ SB$ CONTAINS? TTRUE
   name name-n s" the child's reason" CHILD-LABEL
   CMD:ERR$ err err-n CONTAINS? TTRUE
   name name-n s" the child exits 74" CHILD-LABEL  rc 74 T= ;

: BAD-DATA ( CMD:command -- CMD:command )
   s" data"  s" link: a DATA pointer outside the build's space: " 1
   s" link: a DATA pointer outside the build's space"
   BAD-RUN ;

: BAD-HEADER ( CMD:command -- CMD:command )
   s" header"
   s" link: the build stored into the header cell at offset " UNCGH-CELL
   s" link: a build stored into a header cell the link writes"
   BAD-RUN ;

\ The eval child names its caller and no number, so it is checked apart from
\ BAD-RUN.
: BAD-EVAL ( CMD:command -- CMD:command )
   s" eval" CHILD-RC {: rc:n :}
   s" eval" s" the child arms before the link" CHILD-LABEL
   CMD:OUT$ s\" ARM64-LINK-DATA-BAD-ARMED\n" T$=
   s" eval" s" the child names the caller and evaluate-closed" CHILD-LABEL
   CMD:ERR$ s" link: LD-EV calls evaluate-closed, which a stripped program cannot run" CONTAINS? TTRUE
   s" eval" s" the child's reason" CHILD-LABEL
   CMD:ERR$ s" link: a stripped program reaches a word it cannot run" CONTAINS? TTRUE
   s" eval" s" the child exits 74" CHILD-LABEL  rc 74 T= ;

\ Each refusal dies, so each runs in its own child process.
: REFUSAL-CASE ( -- )
   [: BAD-DATA ;] CMD:WITH-COMMAND
   [: BAD-HEADER ;] CMD:WITH-COMMAND
   [: BAD-EVAL ;] CMD:WITH-COMMAND ;


public

: RUN ( -- )
   T-RESET
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   REFUSAL-CASE
   T-REPORT ;

;package

ARM64-LINK-DATA:RUN

;package
