\ arm64-link-data.f - a stripped program's DATA, written by LINKER:WRITE and
\ run, its kernel the tree's kernel object. One build writes the programs. main
\ prints `12 6 0 11 11 ld-text`: the word an xt cell holds, which reads a
\ variable through a data address; the quotation an xt cell holds, stored as
\ the build ran; a scratch cell the build set to 7; a plain cell; a DATA
\ pointer's target; a string it types after filling 4096 bytes at its `here`,
\ which shows its DP lies past its data, the string's body included. On a
\ macOS host the program is PIE, and these hold only if the start-up slides
\ each code cell once and no DATA cell; a Linux program loads at its link
\ address. over allots until it catches E-DATA-SPACE, prints the bytes
\ it allotted and rethrows uncaught: its DATA less the profiler band, the
\ engine's part below DATA-START and its own data, which is under a step.
\ A second link of main writes the same bytes. On Linux, held sums a table it
\ only reads, prints the sum, then blocks on stdin; while it blocks, its
\ smaps shows the table's pages in DATA as file pages, not Anonymous ones.
\ Each refusal (arm64-link-data-bad.f) dies, so each runs in its own child.
\ The data child also runs on copies of the kernel object, one with a code byte
\ flipped, which fails its checksum, and one with a cell appended and its sum
\ written again, whose layout hb does not read: its read stops the child before
\ it arms.
\ Fixture words are LD- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/core/sha256.f
require src/lib/fd.f
require src/lib/spawn.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/os/linux/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require tools/hb-build-core.f

package ARM64-LINK-DATA

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot

: ROOT$ ( -- str ) ROOT-BUF ROOT-U @ >str ;

: PROG$ ( str -- str )
   {: name:str :}
   ROOT$ name PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u >str ;


$100000 constant OVER-STEP                \ over's first allot step, in bytes

\ over halves its step at each throw, so it allots up to its ceiling.
: OVER-SRC ( -- str )
   SB-RESET
   s" : LD-OVER ( -- ) " SB-APPEND  OVER-STEP FMT:SB-INT
   s"  LD-ST !  begin LD-ST @ 0 <> while [: LD-ST @ allot ;] catch dup 0= if drop LD-AL @ LD-ST @ + LD-AL ! else LD-RC ! LD-ST @ 2 / LD-ST ! then repeat LD-AL @ LD-U  LD-RC @ throw ;" SB-APPEND
   SB$ ;

\ LD-LEAF2 is reached only from the quotation LD-MAKE stores.
: FIXTURE ( -- )
   s" variable LD-V  11 LD-V !" evaluate-closed
   s" : LD-PTR ( -- ) create here ptr-cell-mark 0 , does> ( -- ^^n ) ;" evaluate-closed
   s" : LD-CODE ( -- ) create 0 , does> ( -- ^[ n -- n ] ) ;" evaluate-closed
   s" LD-PTR LD-P  LD-V LD-P !" evaluate-closed
   s" : LD-ADDR ( -- ^n ) LD-V ;" evaluate-closed
   s" : LD-LEAF ( n -- n ) LD-ADDR @ + ;" evaluate-closed
   s" : LD-LEAF2 ( n -- n ) 5 + ;" evaluate-closed
   s" LD-CODE LD-XT  ' LD-LEAF LD-XT xt!" evaluate-closed
   s" variable LD-SCR  7 LD-SCR !  LD-SCR scratch-cell-mark" evaluate-closed
   s" LD-CODE LD-Q" evaluate-closed
   s" : LD-MAKE ( -- ) [: LD-LEAF2 ;] LD-Q xt! ;" evaluate-closed
   s" LD-MAKE" evaluate-closed
   s" : LD-PV ( -- n ) LD-P @ @ ;" evaluate-closed
   s" : LD-FILL ( -- ) here {: p:^ :} 4096 allot 4096 0 ?do [char] x p i + c! loop ;" evaluate-closed
   s" : LD-U ( n -- ) dup 9 > if dup 10 / recurse then 10 mod 48 + emit ;" evaluate-closed
   s" : LD-U_ ( n -- ) LD-U 32 emit ;" evaluate-closed
   s" variable LD-AL  0 LD-AL !  variable LD-ST  variable LD-RC" evaluate-closed
   s\" : LD-MAIN ( -- ) 1 LD-XT @ execute LD-U_  1 LD-Q @ execute LD-U_  LD-SCR @ LD-U_  LD-V @ LD-U_  LD-PV LD-U_  LD-FILL  s\" ld-text\" type cr ;" evaluate-closed
   OVER-SRC evaluate-closed ;


\ The bytes a program can allot from its heap floor to its ceiling.
DATA-SIZE PROF-CNT-BYTES - DATA-START - constant OVER-ROOM


: STORE-ENTRY ( str -- )
   XREF-FIND XREF-START ENTRY-XT
   APP-ENTRY:XT! ;


DYNAMIC-BUFFER FIRST-LINK u8

: FILE-BYTES ( str -- str )
   {: path:str :}
   path FS:FILE-SIZE {: size:n :}
   size FIRST-LINK-RESERVE
   path 0  0 FIRST-LINK size FS:READ-AT
   0 FIRST-LINK size >str ;

DYNAMIC-BUFFER SECOND-LINK u8

\ main linked again from the same build gives the same file.
: SECOND-CASE ( -- )
   s" main" PROG$ FILE-BYTES {: a:str :}
   a STR:LEN SECOND-LINK-RESERVE
   a STR:DATA 0 SECOND-LINK a STR:LEN BYTE-COPY
   s" main" PROG$ LINKER:WRITE
   s" main" PROG$ FILE-BYTES {: b:str :}
   s" a second link writes the same bytes" T-LABEL
   b  0 SECOND-LINK a STR:LEN >str  STR:= TTRUE
   FIRST-LINK-RELEASE SECOND-LINK-RELEASE ;


HB-TARGET-LINUX? [if]

4 PROT-PAGE-MAX * constant HELD-BYTES     \ held's table, each byte 1
\ The kB of the table's pages no other data shares: at least the table less
\ the carried page at each end.
HELD-BYTES 2 PROT-PAGE-MAX * - 1024 / constant HELD-KB
$1000 constant SMAPS-CHUNK
64 constant LINE-CAP
$0A constant LF-BYTE

: HELD-SRC ( -- str )
   SB-RESET  HELD-BYTES FMT:SB-INT
   s"  constant LD-TN  create LD-TAB LD-TN allot" SB-APPEND
   SB$ ;

\ After SECOND-CASE, whose two links of main must see one build.
: HELD-WRITE ( -- )
   HELD-SRC evaluate-closed
   s" : LD-TFILL ( -- ) LD-TN 0 ?do 1 LD-TAB i + c! loop ;  LD-TFILL" evaluate-closed
   s" : LD-HELD ( -- ) 0 LD-TN 0 ?do LD-TAB i + c@ + loop LD-U cr  0 here 1 read drop ;" evaluate-closed
   s" LD-HELD" STORE-ENTRY  s" held" PROG$ LINKER:WRITE ;

[then]
HB-TARGET-LINUX? 0= [if]
: HELD-WRITE ( -- ) ;
[then]


: BODY ( -- )
   FIXTURE
   s" LD-OVER" STORE-ENTRY  s" over" PROG$ LINKER:WRITE
   s" LD-MAIN" STORE-ENTRY  s" main" PROG$ LINKER:WRITE
   SECOND-CASE  HELD-WRITE ;


: RUN-PROG ( CMD:command str -- CMD:command n )
   {: file:str :}
   file PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

: RUN-MAIN ( CMD:command -- CMD:command )
   s" main" RUN-PROG {: rc:n :}
   s" main: xt cell, quotation, scratch, plain, DATA pointer, text past DP" T-LABEL
   CMD:OUT$ s\" 12 6 0 11 11 ld-text\n" T$=
   s" main exits 0" T-LABEL  rc 0 T= ;

: RUN-OVER ( CMD:command -- CMD:command )
   s" over" RUN-PROG {: rc:n :}
   CMD:OUT$ STR:>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH {: got:n :}
   s" over allots no more than the room below its ceiling" T-LABEL
   got OVER-ROOM <= TTRUE
   s" over allots within a step of that room" T-LABEL
   got OVER-ROOM OVER-STEP - > TTRUE
   s" and rethrows the E-DATA-SPACE it caught uncaught" T-LABEL
   CMD:ERR$ s" hb: uncaught throw code -9850" STR:CONTAINS? TTRUE
   s" over exits 85" T-LABEL  rc EXIT-STATUS:UNCAUGHT T= ;


: CHILD-LABEL ( str str -- )
   {: name:str what:str :}
   SB-RESET  s" refusal, " SB-APPEND  name SB-APPEND
   s" : " SB-APPEND  what SB-APPEND  SB$ T-LABEL ;

: CHILD-RC ( CMD:command str str -- CMD:command n )
   {: name:str obj:str :}
   s" --load" CMD:ARG+  s" test/compiler/arm64-link-data-bad.f" CMD:ARG+
   s" --" CMD:ARG+  name CMD:ARG+  obj CMD:ARG+
   s" bad" PROG$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

\ name's child on the object at obj arms, its link is refused for the reason
\ err, and it exits with status want; label names the case.
: BAD-LINK ( CMD:command str str str str n -- CMD:command )
   {: name:str obj:str label:str err:str want:n :}
   name obj CHILD-RC {: rc:n :}
   label s" the child arms before the link" CHILD-LABEL
   CMD:OUT$ s\" ARM64-LINK-DATA-BAD-ARMED\n" T$=
   label s" the link's reason" CHILD-LABEL
   CMD:ERR$ err STR:CONTAINS? TTRUE
   label s" the link's status" CHILD-LABEL  rc want T= ;

: BAD-CHILD ( CMD:command str str n -- CMD:command )
   {: name:str err:str want:n :}
   name HB-BUILD-CORE:KERNEL$ name err want BAD-LINK ;

: NAMES-LABEL ( str -- ) s" the link names what it refuses" CHILD-LABEL ;

: BAD-DATA ( CMD:command -- CMD:command )
   s" data"  s" error[E-LINK-DATA-POINTER]" 70 BAD-CHILD
   s" data" NAMES-LABEL
   CMD:ERR$ s" the data address 1 lies outside the build's space" STR:CONTAINS? TTRUE ;

: BAD-EVAL ( CMD:command -- CMD:command )
   s" eval"  s" error[E-CANNOT-RUN-STRIPPED]: `LD-EV` calls `evaluate-closed`" 70 BAD-CHILD ;


\ The kernel object with a code byte flipped, written to flipped.
: FLIPPED ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES {: f:str :}
   48 FIRST-LINK c@ $ff xor 48 FIRST-LINK c!
   s" flipped" PROG$ f FS:WRITE-ALL  FIRST-LINK-RELEASE ;

create SHA-CTX SHA256-CTX-BYTES allot

\ Writes the object's sum as hb does, so the copy passes its checksum.
: RESEAL ( str -- ) {: s:str :}
   SHA-CTX  s STR:DATA 40 +  s STR:LEN 40 - >str  s STR:DATA 8 +  SHA256-IN ;

\ The kernel object with a zero cell appended and its sum written again, written
\ to grown.
: GROWN ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES STR:LEN {: u:n :}
   u 8 + FIRST-LINK-RESERVE
   0  u FIRST-LINK LE:U64!
   0 FIRST-LINK u 8 + >str {: g:str :}
   g RESEAL
   s" grown" PROG$ g FS:WRITE-ALL  FIRST-LINK-RELEASE ;

\ The data child on the object at name, refused by its read for the reason err
\ before it arms, exits 65.
: BAD-OBJECT ( CMD:command str str -- CMD:command )
   {: name:str err:str :}
   s" data"  name PROG$ CHILD-RC {: rc:n :}
   name s" nothing on stdout" CHILD-LABEL  CMD:OUT$ STR:LEN 0 T=
   name s" the read's reason" CHILD-LABEL
   CMD:ERR$ err STR:CONTAINS? TTRUE
   name s" the read exits 65" CHILD-LABEL  rc 65 T= ;

: BAD-FLIPPED ( CMD:command -- CMD:command )
   s" flipped"  s"  is not the file hb wrote" BAD-OBJECT ;

: BAD-GROWN ( CMD:command -- CMD:command )
   s" grown"  s" does not read the kernel object's layout" BAD-OBJECT ;


HB-TARGET-LINUX? [if]

create LINE LINE-CAP allot
variable LINE-N
DYNAMIC-BUFFER SMAPS u8
variable SMAPS-N
variable AT                          \ the parse's offset in SMAPS
variable START                       \ the start of the mapping being read

: LINE$ ( -- str ) LINE LINE-N @ >str ;

: LINE-DONE? ( -- bool )
   LINE-N @ LINE-CAP =
   LINE-N @ 0 > if LINE LINE-N @ + 1- c@ LF-BYTE = or then ;

\ Reads r until a line or its end.
: READ-LINE ( fd -- )
   {: r:fd :}
   0 LINE-N !
   begin LINE-DONE? 0= while
      r FD>N LINE LINE-N @ + LINE-CAP LINE-N @ - read
      dup 0 <= if drop exit then
      LINE-N +!
   repeat ;

: SMAPS-PATHZ ( pid -- ^u8 )
   SB-RESET  s" /proc/" SB-APPEND  PID>N FMT:SB-INT
   s" /smaps" SB-APPEND  SB$ FS:>PATHZ ;

\ procfs gives no size, so it reads to the end.
: READ-SMAPS ( pid -- )
   SMAPS-PATHZ open-rd {: fd:n :}
   0 SMAPS-N !
   begin
      SMAPS-N @ SMAPS-CHUNK + SMAPS-RESERVE
      fd SMAPS-N @ SMAPS SMAPS-CHUNK read dup 0 > while
      SMAPS-N +!
   repeat drop
   fd 0 >= if fd close then ;

\ The line at AT, AT moved past it.
: NEXT-LINE ( -- str )
   AT @ {: from:n :}
   begin
      AT @ SMAPS-N @ < if AT @ SMAPS c@ LF-BYTE <> else false then
   while 1 AT +! repeat
   from SMAPS  AT @ from - >str  1 AT +! ;

\ A hex digit's value, -1 for another byte.
: HEX-DIGIT ( u8 -- n )
   {: c:u8 :}
   c [char] 0 >= c [char] 9 <= and if c [char] 0 - exit then
   c [char] a >= c [char] f <= and if c [char] a - 10 + exit then
   -1 ;

\ The value of the hex digits the line starts with.
: HEX ( str -- n )
   {: s:str :}
   0 s STR:LEN 0 ?do
      s STR:DATA i + c@ HEX-DIGIT dup 0< if drop unloop exit then
      swap 16 * +
   loop ;

\ The value of the decimal digits in the line, whatever lies between them.
: DEC ( str -- n )
   {: s:str :}
   0 s STR:LEN 0 ?do
      s STR:DATA i + c@ dup [char] 0 >= over [char] 9 <= and
      if [char] 0 - swap 10 * + else drop then
   loop ;

\ A mapping's header starts with its hex start; a field's key, a capital.
: HEADER? ( str -- bool )
   {: s:str :}
   s STR:LEN 0 > if s STR:DATA c@ HEX-DIGIT 0 >= else false then ;

: IN-DATA? ( -- bool )
   START @ LINUX-LAYOUT:DATA-VA >=
   START @ LINUX-LAYOUT:DATA-VA LINUX-LAYOUT:DATA-SIZE + < and ;

\ A field's share of FILE-KB: Rss adds, Anonymous takes away.
: FIELD-KB ( str -- n )
   {: s:str :}
   IN-DATA? 0= if 0 exit then
   s s" Rss:" STR:STARTS-WITH? if s DEC exit then
   s s" Anonymous:" STR:STARTS-WITH? if s DEC negate exit then
   0 ;

: LINE-KB ( str -- n )
   {: s:str :}
   s HEADER? if s HEX START ! 0 exit then
   s FIELD-KB ;

\ The kB of DATA's window resident and not Anonymous, over each mapping that
\ starts in it.
: FILE-KB ( -- n )
   0 AT !  0 START !  0
   begin AT @ SMAPS-N @ < while NEXT-LINE LINE-KB + repeat ;

: HELD-SPAWN ( fd fd -- pid )
   {: in:fd out:fd :}
   s" held" PROG$ PROCESS:NEW
   0 >FD in PROCESS:FD  1 >FD out PROCESS:FD
   PROCESS:SPAWN ;

: HELD-SUM$ ( -- str )
   SB-RESET  HELD-BYTES FMT:SB-INT  LF-BYTE SB-APPEND-C  SB$ ;

\ held's line comes once it has read its table; its smaps is read while it
\ blocks on stdin, which closing in-w ends.
: RUN-HELD ( -- )
   FD:PIPE {: in-r:fd in-w:fd :}
   FD:PIPE {: out-r:fd out-w:fd :}
   in-r out-w HELD-SPAWN {: pid:pid :}
   in-r FD>N close  out-w FD>N close
   out-r READ-LINE  out-r FD>N close
   pid READ-SMAPS  in-w FD>N close
   s" held: the sum of its table" T-LABEL  LINE$ HELD-SUM$ T$=
   s" held: its table's pages in DATA are file pages, not Anonymous" T-LABEL
   FILE-KB HELD-KB min HELD-KB T=
   s" held exits 0" T-LABEL
   pid PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T= ;

[then]
HB-TARGET-LINUX? 0= [if]
: RUN-HELD ( -- ) ;
[then]


public

: RUN ( -- )
   T-RESET
   s" habu-link-data" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA ROOT-BUF d STR:LEN BYTE-COPY  d STR:LEN ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   [: RUN-MAIN ;] CMD:WITH-COMMAND
   [: RUN-OVER ;] CMD:WITH-COMMAND
   [: BAD-DATA ;] CMD:WITH-COMMAND
   FLIPPED
   [: BAD-FLIPPED ;] CMD:WITH-COMMAND
   GROWN
   [: BAD-GROWN ;] CMD:WITH-COMMAND
   [: BAD-EVAL ;] CMD:WITH-COMMAND
   RUN-HELD
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ARM64-LINK-DATA:RUN
