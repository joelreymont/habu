\ arm64-link-run.f - LINKER:WRITE's stripped programs, run, their kernel the
\ tree's kernel object. One build stores each of four entries and writes a
\ program for it: LR-HI prints `Hi` and exits 0; LR-THROW prints nothing and
\ exits 3, its uncaught throw code; LR-XT stores a quotation with xt! at run
\ time and prints `Hi` only if the product's MARK accepts its address-cell
\ header and index; LR-DEEP overflows the machine stack and exits 102 through
\ the crash path the start-up installs. Each file is under 128 KiB, which the
\ whole kernel alone exceeds: only the kernel code the program reaches is
\ written, and no message it never prints: control never passes from a message's
\ text into what follows it. Each program runs in its own process:
\ the boundary under test is the written file and its start-up.
\ Fixture words are LR- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/process-command.f
require src/core/include.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require tools/hb-build-core.f

package ARM64-LINK-RUN

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

: PROG$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr name-n:n :}
   ROOT$ name name-n PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;


: FIXTURE ( -- )
   s" : LR-HI ( -- ) 72 emit 105 emit cr ;" evaluate-closed
   s" : LR-THROW ( -- ) 3 throw ;" evaluate-closed
   s" variable LR-V" evaluate-closed
   s" : LR-XT ( -- ) [: ;] LR-V xt!  LR-HI ;" evaluate-closed
   s" : LR-REC ( n -- n ) 1 + recurse 1 + ;" evaluate-closed
   s" : LR-DEEP ( -- ) 0 LR-REC drop ;" evaluate-closed ;


: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;


: WRITE-PROG ( ptr u8 n ptr u8 n -- )
   {: word:ptr word-n:n file:ptr file-n:n :}
   word word-n STORE-ENTRY
   file file-n PROG$ LINKER:WRITE ;


: BODY ( -- )
   FIXTURE
   s" LR-HI" s" hi" WRITE-PROG
   s" LR-THROW" s" throw" WRITE-PROG
   s" LR-XT" s" xt" WRITE-PROG
   s" LR-DEEP" s" deep" WRITE-PROG ;


: RUN-PROG ( CMD:command ptr u8 n ptr u8 n n -- CMD:command )
   {: file:ptr file-n:n out:ptr out-n:n want:n :}
   file file-n PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ out out-n STR=  rc want =  and 0= if
      s" arm64-link-run: " type file file-n type s"  exited " type rc .
      s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr
   then
   file file-n T-LABEL  CMD:OUT$ out out-n T$=
   file file-n T-LABEL  rc want T= ;

: RUN-HI ( CMD:command -- CMD:command ) s" hi" s\" Hi\n" 0 RUN-PROG ;
: RUN-THROW ( CMD:command -- CMD:command ) s" throw" s" " 3 RUN-PROG ;
: RUN-XT ( CMD:command -- CMD:command ) s" xt" s\" Hi\n" 0 RUN-PROG ;
: RUN-DEEP ( CMD:command -- CMD:command ) s" deep" s" " 102 RUN-PROG
   s" deep" T-LABEL
   CMD:ERR$ s" E-STACK-BOUNDS habu: stack bounds exceeded (machine) lr " STARTS-WITH? TTRUE ;


\ An ELF file is at least one PROT-PAGE-MAX text page.
: SMALLER ( ptr u8 n -- )
   {: file:ptr file-n:n :}
   file file-n PROG$ FS:FILE-SIZE {: size:n :}
   size $20000 < 0= if
      s" arm64-link-run: " type file file-n type s"  has bytes: " type size .
   then
   s" each file is under 128 KiB: only the kernel code the program reaches is written"
      T-LABEL
   size $20000 < TTRUE ;

$20000 BUFFER: HI-BYTES

\ Texts hi never prints: the profiler's report, and the message laid right after
\ the uncaught-throw message, which every stripped program reaches.
: NO-PROFILER ( -- )
   s" hi" PROG$ {: file:ptr file-n:n :}
   file file-n FS:FILE-SIZE $20000 min {: size:n :}
   file file-n 0 HI-BYTES size FS:READ-AT
   s" hi keeps no message it never prints and no profiler code: control never passes from a message into what follows it"
      T-LABEL
   HI-BYTES size s" profiler not armed" CONTAINS? TFALSE
   HI-BYTES size s" E-UNDERFLOW habu: " CONTAINS? TFALSE ;


\ ---- the kernel build's inputs log, replayed -------------------------------------
\ LINKER:INPUTS-HOLD? over the log the kernel build wrote or adopted, from the root
\ it ran from: stamps are change times, so only a log made on this host holds.
\ The log past the inputs file's head, and a copy with one record edited.
64 constant HEAD-LR
DYNAMIC-BUFFER LOG-LR u8
variable LOG-LR-U
DYNAMIC-BUFFER EDIT-LR u8
variable EDIT-LR-U
PATH-CAP 8 + BUFFER: PAD-LR          \ a padded spelling's length cell and bytes
TYPED-VARIABLE HELD-LR bool
variable RC-LR
variable PADDED-LR                   \ the first part with a top request, -1 for none
variable RESUMED-LR                  \ the first resumed part, -1 for none
variable BEFORE-LR                   \ the part before it, the nested load's last
variable PREV-LR                     \ the scan's last part, -1 for none
variable DEPTH-LR                    \ the scan's open loads

\ The log's number at the offset, and the offset past the length-prefixed field
\ or fields there.
: CELL-LR ( n -- n )
   {: at:n :}
   0 LOG-LR at + LE:U64@ ;
: FIELD-LR ( n -- n ) dup CELL-LR + 8 + ;
: FIELDS-LR ( n n -- n ) 0 ?do FIELD-LR loop ;

\ A part record's top request kind (-1 none, -2 not seen) is past its kind,
\ path, stamp and three INPUT fields; its request kind is past its acts.
: TOP-LR ( n -- n ) 8 + FIELD-LR 24 +  3 FIELDS-LR ;

: ACTS-LR ( n -- n )
   {: at:n :}
   at 8 +  at CELL-LR 0 ?do  8 + 3 FIELDS-LR  loop ;

: REQ-LR ( n -- n ) TOP-LR 8 + 2 FIELDS-LR  ACTS-LR ;

\ A part's kind is 0; an event holds three fields.
: RECORD-LR ( n -- n )
   dup CELL-LR 0<> if 8 + 3 FIELDS-LR exit then
   REQ-LR 8 + 2 FIELDS-LR ;

\ A part resumes when the part before it ended a load (request none), a load
\ is still open and no top request loaded it.
: PART-LR ( n -- )
   {: at:n :}
   at TOP-LR CELL-LR {: top:n :}
   top 0 >=  PADDED-LR @ 0<  and if at PADDED-LR ! then
   PREV-LR @ 0 >= if
      PREV-LR @ REQ-LR CELL-LR -1 =  DEPTH-LR @ 0 >  and  top -1 =  and
      RESUMED-LR @ 0<  and if  at RESUMED-LR !  PREV-LR @ BEFORE-LR !  then
   then
   top -1 <> if 1 DEPTH-LR +! then
   at REQ-LR CELL-LR -1 <> if 1 else -1 then DEPTH-LR +!
   at PREV-LR ! ;

\ The offset the walk ends at.
: SCAN-LR ( -- n )
   -1 PADDED-LR !  -1 RESUMED-LR !  -1 BEFORE-LR !  -1 PREV-LR !  0 DEPTH-LR !
   0 begin dup LOG-LR-U @ < while
      dup CELL-LR 0= if dup PART-LR then
      RECORD-LR
   repeat ;

\ LOG-LR with its bytes from the first offset to the second replaced, in EDIT-LR.
: SPLICE-LR ( n n ptr u8 n -- )
   {: at0:n at1:n new:ptr newu:n :}
   0 LOG-LR {: log:ptr :}
   0 EDIT-LR {: edit:ptr :}
   log edit at0 BYTE-COPY
   new  edit at0 +  newu BYTE-COPY
   log at1 +  edit at0 + newu +  LOG-LR-U @ at1 -  BYTE-COPY
   LOG-LR-U @ at1 - at0 + newu + EDIT-LR-U ! ;

\ The padded part's top spelling as x's that, joined to its root, are one byte
\ past PATH-CAP: each alone PATH-OK? takes, the resolver refuses (E-PATH-RANGE).
: PAD-SPELLING-LR ( -- )
   PADDED-LR @ TOP-LR 8 + {: root:n :}
   root FIELD-LR {: at:n :}
   PATH-CAP root CELL-LR - {: u:n :}
   u PAD-LR LE:U64!
   u 0 ?do  [char] x PAD-LR 8 + i + c!  loop
   at  at FIELD-LR  PAD-LR u 8 +  SPLICE-LR ;

\ The resumed part given the path and stamp of the part before it.
: SWAP-RESUMED-LR ( -- )
   BEFORE-LR @ 8 + {: b:n :}
   RESUMED-LR @ 8 + {: c:n :}
   c  c FIELD-LR 24 +  0 LOG-LR b +  b FIELD-LR 24 + b -  SPLICE-LR ;

: HOLD-LR ( ptr u8 n -- )
   {: a:ptr u:n :}
   false HELD-LR !
   [: a u LINKER:INPUTS-HOLD? HELD-LR ! ;] catch RC-LR ! ;

: EDITED-LR ( -- )
   0 EDIT-LR EDIT-LR-U @ HOLD-LR  RC-LR @ 0 T=  HELD-LR @ TFALSE ;

: READ-LR ( -- )
   HB-BUILD-CORE:KERNEL-INPUTS$ {: p:ptr pu:n :}
   p pu FS:FILE-SIZE HEAD-LR - {: u:n :}
   u LOG-LR-U !
   u LOG-LR-RESERVE  u PATH-CAP + 8 + EDIT-LR-RESERVE
   p pu HEAD-LR  0 LOG-LR u  FS:READ-AT ;

\ The inputs log records the tree's root, so it replays from there.
: CHECK-LR ( -- )
   SOURCE-ROOT:CWD$ [:
      s" the fresh inputs log holds" T-LABEL
      0 LOG-LR LOG-LR-U @ HOLD-LR  RC-LR @ 0 T=  HELD-LR @ TTRUE
      s" a top spelling one byte past PATH-CAP joined to its root does not hold, with no throw" T-LABEL
      PAD-SPELLING-LR EDITED-LR
      s" a resumed part with the path and stamp of the part before it does not hold" T-LABEL
      SWAP-RESUMED-LR EDITED-LR
   ;] SOURCE-ROOT:WITH ;

: REPLAY ( -- )
   READ-LR
   SCAN-LR {: end:n :}
   s" the inputs log walks to its end and has a top request and a resumed part" T-LABEL
   end LOG-LR-U @ T=  PADDED-LR @ 0 >= TTRUE  RESUMED-LR @ 0 >= TTRUE
   end LOG-LR-U @ =  PADDED-LR @ 0 >=  and  RESUMED-LR @ 0 >=  and if CHECK-LR then
   LOG-LR-RELEASE  EDIT-LR-RELEASE ;


public

: RUN ( -- )
   T-RESET
   s" write outside a build is E-COMP-STATE" T-LABEL
   [: s" x" LINKER:WRITE ;] E-COMP-STATE TTHROWSQ
   s" habu-link-run" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   REPLAY
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   [: RUN-HI ;] CMD:WITH-COMMAND
   [: RUN-THROW ;] CMD:WITH-COMMAND
   [: RUN-XT ;] CMD:WITH-COMMAND
   [: RUN-DEEP ;] CMD:WITH-COMMAND
   s" hi" SMALLER  s" throw" SMALLER  s" xt" SMALLER  s" deep" SMALLER
   NO-PROFILER
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ARM64-LINK-RUN:RUN
