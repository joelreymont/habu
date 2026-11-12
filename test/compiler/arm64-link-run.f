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
require src/lib/process-command.f
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
   file file-n PROG$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
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

\ Texts hi never prints: the profiler's report, and a message laid after one
\ every stripped program reaches.
: NO-PROFILER ( -- )
   s" hi" PROG$ {: file:ptr file-n:n :}
   file file-n FS:FILE-SIZE $20000 min {: size:n :}
   file file-n 0 HI-BYTES size FS:READ-AT
   s" hi keeps no message it never prints and no profiler code: control never passes from a message into what follows it"
      T-LABEL
   HI-BYTES size s" profiler not armed" CONTAINS? TFALSE
   HI-BYTES size s" hb: snapshot call map mismatch" CONTAINS? TFALSE ;


public

: RUN ( -- )
   T-RESET
   s" write outside a build is E-COMP-STATE" T-LABEL
   [: s" x" LINKER:WRITE ;] E-COMP-STATE TTHROWSQ
   s" habu-link-run" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
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
