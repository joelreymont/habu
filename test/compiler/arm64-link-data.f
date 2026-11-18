split-str
\ arm64-link-data.f - a stripped program's DATA, written by LINKER:WRITE and
\ run, its kernel the tree's kernel object. One build writes two programs. main
\ prints `12 6 0 11 11 ld-text`: the word an xt cell holds, which reads a
\ variable through a data address; the quotation an xt cell holds, stored as
\ the build ran; a scratch cell the build set to 7; a plain cell; a DATA
\ pointer's target; a string it types after filling 4096 bytes at its `here`,
\ which shows its DP lies past its data, the string's body included. The
\ program is PIE: these hold only if the start-up slides each code cell once
\ and no DATA cell. over allots until it catches E-DATA-SPACE, prints the bytes
\ it allotted and rethrows uncaught: its DATA less the profiler band, the
\ engine's part below DATA-START and its own data, which is under a step.
\ A second link of main writes the same bytes.
\ Each refusal (arm64-link-data-bad.f) dies, so each runs in its own child.
\ The data child also runs on three malformed copies of the kernel object, one
\ failing its checksum, one whose DATA-START is off a cell and one whose Mach-O
\ import names do not end in a NUL, which its read refuses before the child
\ arms; the header child runs on a fourth, whose DP-CELL lies at its
\ DATA-START, which the link refuses at that cell after it arms.
\ Fixture words are LD- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/core/sha256.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require src/os/macos/macho.f
require tools/hb-build-core.f

package ARM64-LINK-DATA

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot

: ROOT$ ( -- ^u8 n ) ROOT-BUF ROOT-U @ ;

: PROG$ ( str -- str )
   {: name:^ name-n:n :}
   ROOT$ name name-n PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;


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
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   APP-ENTRY:XT! ;


DYNAMIC-BUFFER FIRST-LINK u8

: FILE-BYTES ( str -- str )
   {: path:^ path-n:n :}
   path path-n FS:FILE-SIZE {: size:n :}
   size FIRST-LINK-RESERVE
   path path-n 0  0 FIRST-LINK size FS:READ-AT
   0 FIRST-LINK size ;

DYNAMIC-BUFFER SECOND-LINK u8

\ main linked again from the same build gives the same file.
: SECOND-CASE ( -- )
   s" main" PROG$ FILE-BYTES {: a:^ a-n:n :}
   a-n SECOND-LINK-RESERVE
   a 0 SECOND-LINK a-n BYTE-COPY
   s" main" PROG$ LINKER:WRITE
   s" main" PROG$ FILE-BYTES {: b:^ b-n:n :}
   s" a second link writes the same bytes" T-LABEL
   b b-n  0 SECOND-LINK a-n  STR= TTRUE
   FIRST-LINK-RELEASE SECOND-LINK-RELEASE ;


: BODY ( -- )
   FIXTURE
   s" LD-OVER" STORE-ENTRY  s" over" PROG$ LINKER:WRITE
   s" LD-MAIN" STORE-ENTRY  s" main" PROG$ LINKER:WRITE
   SECOND-CASE ;


: RUN-PROG ( CMD:command str -- CMD:command n )
   {: file:^ file-n:n :}
   file file-n PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

: RUN-MAIN ( CMD:command -- CMD:command )
   s" main" RUN-PROG {: rc:n :}
   s" main: xt cell, quotation, scratch, plain, DATA pointer, text past DP" T-LABEL
   CMD:OUT$ s\" 12 6 0 11 11 ld-text\n" T$=
   s" main exits 0" T-LABEL  rc 0 T= ;

: RUN-OVER ( CMD:command -- CMD:command )
   s" over" RUN-PROG {: rc:n :}
   CMD:OUT$ STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH {: got:n :}
   s" over allots no more than the room below its ceiling" T-LABEL
   got OVER-ROOM <= TTRUE
   s" over allots within a step of that room" T-LABEL
   got OVER-ROOM OVER-STEP - > TTRUE
   s" and rethrows the E-DATA-SPACE it caught uncaught" T-LABEL
   CMD:ERR$ s" hb: uncaught throw code -9850" CONTAINS? TTRUE
   s" over exits 67" T-LABEL  rc 67 T= ;


: CHILD-LABEL ( str str -- )
   {: name:^ name-n:n what:^ what-n:n :}
   SB-RESET  s" refusal, " SB-APPEND  name name-n SB-APPEND
   s" : " SB-APPEND  what what-n SB-APPEND  SB$ T-LABEL ;

: CHILD-RC ( CMD:command str str -- CMD:command n )
   {: name:^ name-n:n obj:^ obj-n:n :}
   s" --load" CMD:ARG+  s" test/compiler/arm64-link-data-bad.f" CMD:ARG+
   s" --" CMD:ARG+  name name-n CMD:ARG+  obj obj-n CMD:ARG+
   s" bad" PROG$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N ;

\ name's child on the object at obj arms, its link is refused for the reason
\ err, and it exits with status want; label names the case.
: BAD-LINK ( CMD:command str str str str n -- CMD:command )
   {: name:^ name-n:n obj:^ obj-n:n label:^ label-n:n err:^ err-n:n want:n :}
   name name-n obj obj-n CHILD-RC {: rc:n :}
   label label-n s" the child arms before the link" CHILD-LABEL
   CMD:OUT$ s\" ARM64-LINK-DATA-BAD-ARMED\n" T$=
   label label-n s" the link's reason" CHILD-LABEL
   CMD:ERR$ err err-n CONTAINS? TTRUE
   label label-n s" the link's status" CHILD-LABEL  rc want T= ;

: BAD-CHILD ( CMD:command str str n -- CMD:command )
   {: name:^ name-n:n err:^ err-n:n want:n :}
   name name-n HB-BUILD-CORE:KERNEL$ name name-n err err-n want BAD-LINK ;

: NAMES-LABEL ( str -- ) s" the link names what it refuses" CHILD-LABEL ;

: BAD-DATA ( CMD:command -- CMD:command )
   s" data"  s" error[E-LINK-DATA-POINTER]" 70 BAD-CHILD
   s" data" NAMES-LABEL
   CMD:ERR$ s" the data address 1 lies outside the build's space" CONTAINS? TTRUE ;

: BAD-EVAL ( CMD:command -- CMD:command )
   s" eval"  s" error[E-CANNOT-RUN-STRIPPED]: `LD-EV` calls `evaluate-closed`" 70 BAD-CHILD ;

: BAD-DICT ( CMD:command -- CMD:command )
   s" dict"  s" error[E-CANNOT-RUN-STRIPPED]: `LD-DW` calls `search-wl`" 70 BAD-CHILD ;


SHA256-CTX-BYTES BUFFER: SHA-CTX
24 BUFFER: START-SYM                 \ a symbol's length cell, then its name

\ The offset of the value of the symbol named name, at most 16 bytes, in the
\ object, -1 if none: the name is padded to a cell.
: START-AT ( str str -- n )
   {: a:^ u:n name:^ name-n:n :}
   name-n START-SYM LE:U64!  name START-SYM 8 + name-n BYTE-COPY
   a u START-SYM name-n 8 + FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N 8 +  name-n 7 + -8 and + ENDOF
   ;MATCH ;

\ The checksum, bytes 8-40, as the SHA-256 of the object's bytes 40 on.
: RESUM ( str -- )
   {: a:^ u:n :}
   SHA-CTX  a 40 +  u 40 -  a 8 + SHA256-IN ;

\ The kernel object with a code byte flipped, written to flipped.
: FLIPPED ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES {: a:^ u:n :}
   a 48 + c@ $ff xor a 48 + c!
   s" flipped" PROG$ a u FS:WRITE-ALL  FIRST-LINK-RELEASE ;

\ The kernel object with DATA-START 4 lower and the checksum over it, written
\ to shifted.
: SHIFTED ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES {: a:^ u:n :}
   a u s" DATA-START" START-AT {: at:n :}
   s" the kernel object has a DATA-START symbol" T-LABEL  at 0 >= TTRUE
   at 0< if FIRST-LINK-RELEASE exit then
   a at + LE:U64@ 4 -  a at + LE:U64!
   a u RESUM
   s" shifted" PROG$ a u FS:WRITE-ALL  FIRST-LINK-RELEASE ;

\ The kernel object with the DP-CELL symbol at its DATA-START, a header cell
\ outside the header, and the checksum over it, written to dp-at-start.
: DP-AT-START ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES {: a:^ u:n :}
   a u s" DATA-START" START-AT {: start:n :}
   a u s" DP-CELL" START-AT {: dp:n :}
   s" the kernel object has DATA-START and DP-CELL symbols" T-LABEL
   start 0 >=  dp 0 >=  and TTRUE
   start 0<  dp 0<  or if FIRST-LINK-RELEASE exit then
   a start + LE:U64@  a dp + LE:U64!
   a u RESUM
   s" dp-at-start" PROG$ a u FS:WRITE-ALL  FIRST-LINK-RELEASE ;

\ The kernel object with the NUL ending its Mach-O imports' last name made an
\ x, and the checksum over it, written to unended.
: UNENDED ( -- )
   HB-BUILD-CORE:KERNEL$ FILE-BYTES {: a:^ u:n :}
   SB-RESET  MACHO:IMPORTS 0 ?do i MACHO:IMPORT$ SB-APPEND  0 SB-APPEND-C loop
   a u SB$ FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N SB$ nip + 1 - ENDOF
   ;MATCH {: at:n :}
   s" the kernel object holds the Mach-O import names" T-LABEL  at 0 >= TTRUE
   at 0< if FIRST-LINK-RELEASE exit then
   [char] x a at + c!
   a u RESUM
   s" unended" PROG$ a u FS:WRITE-ALL  FIRST-LINK-RELEASE ;

\ The data child on the object at name, refused by its read for the reason err
\ before it arms, exits 65.
: BAD-OBJECT ( CMD:command str str -- CMD:command )
   {: name:^ name-n:n err:^ err-n:n :}
   s" data"  name name-n PROG$ CHILD-RC {: rc:n :}
   name name-n s" nothing on stdout" CHILD-LABEL  CMD:OUT$ nip 0 T=
   name name-n s" the read's reason" CHILD-LABEL
   CMD:ERR$ err err-n CONTAINS? TTRUE
   name name-n s" the read exits 65" CHILD-LABEL  rc 65 T= ;

: BAD-FLIPPED ( CMD:command -- CMD:command )
   s" flipped"  s" link: the kernel object fails its checksum" BAD-OBJECT ;

: BAD-SHIFTED ( CMD:command -- CMD:command )
   s" shifted"  s" link: the kernel object is cut short or malformed" BAD-OBJECT ;

: BAD-UNENDED ( CMD:command -- CMD:command )
   s" unended"  s" link: the kernel object is cut short or malformed" BAD-OBJECT ;

: BAD-DP-AT-START ( CMD:command -- CMD:command )
   s" header"  s" dp-at-start" PROG$  s" dp-at-start"
   s" link: the kernel object is cut short or malformed" 65 BAD-LINK ;


public

: RUN ( -- )
   T-RESET
   s" habu-link-data" FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   [: RUN-MAIN ;] CMD:WITH-COMMAND
   [: RUN-OVER ;] CMD:WITH-COMMAND
   [: BAD-DATA ;] CMD:WITH-COMMAND
   FLIPPED  SHIFTED  UNENDED  DP-AT-START
   [: BAD-FLIPPED ;] CMD:WITH-COMMAND
   [: BAD-SHIFTED ;] CMD:WITH-COMMAND
   [: BAD-UNENDED ;] CMD:WITH-COMMAND
   [: BAD-DP-AT-START ;] CMD:WITH-COMMAND
   [: BAD-EVAL ;] CMD:WITH-COMMAND
   [: BAD-DICT ;] CMD:WITH-COMMAND
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ARM64-LINK-DATA:RUN
