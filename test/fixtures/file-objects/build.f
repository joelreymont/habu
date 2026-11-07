\ build.f - test/file-objects.f's child: a build of count.f, shape.f, main.f,
\ reopen.f and mark.f written as the program `prog` under the out root, with
\ again.f loaded twice in the same build, then host.f loaded after the build
\ closes. After the write the build prints its state and runs three of its
\ words (STATE., RUN-WORDS); the last line is `compiled ` and LINKER:COMPILED.
\ The body lays one cell between shape.f and main.f, which no file's data
\ holds. With the script argument `second` the build loads pad.f first. With
\ `stop`, nine builds each fail, all under the one out root: in
\ STOP-FILE and STOP-BODY every later load and write is LINKER:E-FAILED,
\ STOP-INSIDE's file moves DP back and its own load is LINKER:E-FAILED,
\ STOP-ALIGN's file marks a cell its object cannot place, E-COMP-STATE,
\ STOP-STORE's file marks hb's newest record immediate, LINKER:E-STORE,
\ STOP-GAP's file stores into a cell the body laid, which no file holds,
\ LINKER:E-STORE, STOP-STRADDLE's file stores into a cell straddling the
\ header's entry cell, E-COMP-STATE, STOP-WORDLIST's file makes a wordlist,
\ LINKER:E-WORDLIST, and STOP-STALE's count.f links from STOP-FILE's object, so
\ shape.f, which has no object there, is LINKER:E-STALE and a later write
\ LINKER:E-FAILED. Three builds then load <out>/edit/a.f, edited before the
\ second, and edit-b.f, and `edit: compiled ` prints each one's LINKER:COMPILED.
\ The build's files load from its body: in a build's file `require` is
\ E-UNDEFINED (rc 70), and creating through a `does>` definer ends the process
\ at rec-wide-publish (rc 70).

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/build-out.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/habu/intern.f
require src/compiler/native/shadow.f
require src/habu/linker.f
require src/habu/link-data.f
require src/os/script-argv.f

package FILE-OBJECTS-BUILD

PATH-CAP BUFFER: PROG-BUF

: PROG$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" prog" PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;

70 constant REFUSED   \ the checker's refusal (src/core/check-hook.f CHECK-RC)

: PAD ( -- ) s" test/fixtures/file-objects/pad.f" included ;
: RESIDUE ( -- ) s" test/fixtures/file-objects/residue.f" included ;
: LOAD-AGAIN ( -- ) s" test/fixtures/file-objects/again.f" included ;
: SHAPE ( -- ) s" test/fixtures/file-objects/shape.f" included ;
: MARK ( -- ) s" test/fixtures/file-objects/mark.f" included ;

: ROLLBACK ( -- ) s" test/fixtures/file-objects/rollback.f" included ;
: UNALIGNED ( -- ) s" test/fixtures/file-objects/unaligned.f" included ;
: STRADDLE ( -- ) s" test/fixtures/file-objects/straddle.f" included ;
: FLAG ( -- ) s" test/fixtures/file-objects/flag.f" included ;
: GAP ( -- ) s" test/fixtures/file-objects/gap.f" included ;
: NEW-WORDLIST ( -- ) s" test/fixtures/file-objects/wordlist.f" included ;

\ The one argument, second or stop, or empty; any other ends the build, rc 1.
: MODE$ ( -- ptr u8 n )
   SCRIPT-ARGC 0 = if s" " exit then
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" second" STR= if s" second" exit then
      0 SCRIPT-ARGV$ s" stop" STR= if s" stop" exit then
   then
   s" build: unknown argument" 1 die ;

: SECOND? ( -- bool ) MODE$ s" second" STR= ;
: STOP? ( -- bool ) MODE$ s" stop" STR= ;

\ The case's code is the one wanted, or the build ends rc 1 with the message.
: EXPECT ( n n ptr u8 n -- )
   {: rc:n want:n msg:ptr msgu:n :}
   rc want <> if msg msgu 1 die then ;

: STOP-FILE ( -- )
   s" test/fixtures/file-objects/count.f" included
   ['] RESIDUE catch E-EVAL-RESIDUE s" build: STOP-FILE: residue.f not refused" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-FILE: a load after a refused file ran" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED
   s" build: STOP-FILE: a write after a refused file ran" EXPECT ;

: STOP-BODY ( -- )
   [: s" : FOB-BAD ( -- n ) dup ;" evaluate-closed ;] catch
   REFUSED s" build: STOP-BODY: the definition was not refused" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED s" build: STOP-BODY: a write after a rollback ran" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-BODY: a load after a rollback ran" EXPECT ;

: STOP-INSIDE ( -- )
   ['] ROLLBACK catch LINKER:E-FAILED
   s" build: STOP-INSIDE: a file that moved DP back was not refused" EXPECT ;

: STOP-ALIGN ( -- )
   ['] UNALIGNED catch E-COMP-STATE
   s" build: STOP-ALIGN: an unaligned address cell was not refused" EXPECT ;

\ From STOP-FILE on, count.f links from its object and a later file with no
\ object is LINKER:E-STALE, so these builds' refused file is their first.
: STOP-STORE ( -- )
   ['] FLAG catch LINKER:E-STORE
   s" build: STOP-STORE: a flag set on hb's newest record was not refused" EXPECT ;

: STOP-GAP ( -- )
   8 allot
   ['] GAP catch LINKER:E-STORE
   s" build: STOP-GAP: a store into a cell no file holds was not refused" EXPECT ;

: STOP-STRADDLE ( -- )
   ['] STRADDLE catch E-COMP-STATE
   s" build: STOP-STRADDLE: a cell straddling the entry cell was not refused" EXPECT ;

: STOP-WORDLIST ( -- )
   ['] NEW-WORDLIST catch LINKER:E-WORDLIST
   s" build: STOP-WORDLIST: a file that made a wordlist was not refused" EXPECT ;

: STOP-STALE ( -- )
   s" test/fixtures/file-objects/count.f" included
   ['] SHAPE catch LINKER:E-STALE
   s" build: STOP-STALE: a file with no object after a linked one was not refused" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED
   s" build: STOP-STALE: a write after a stale file ran" EXPECT ;

\ ---- the build's state, printed so a cold and a warm build compare as text ----
variable STRINGS                      \ the bodies INTERN:EACH-STRING visited

CAST: >DOUBLE ( n -- [ n -- n ] )
CAST: >CELL ( n -- [ -- ptr n ] )
CAST: >RUN ( n -- [ -- ] )

\ The host entry of the build's word named by the string.
: ENTRY ( ptr u8 n -- n ) XREF-FIND-INDEX XREF-REC XREF-START ;

: IMAGE-BASE ( -- n ) data-base BOOT:IMAGE-BASE-CELL + @ ;

: RECORD. ( n -- )
   XREF-REC {: rec:ptr :}
   rec XREF-NAME$ type cr
   rec XREF-FLAGS .  rec XREF-WORDLIST .  rec XREF-RAW-LEN . ;

\ Each record from the first, then the counts of emissions and interned
\ strings, then DP's offset in the space.
: STATE. ( n -- )
   {: first:n :}
   ndict@ first ?do i RECORD. loop
   NSHADOW:EMISSIONS .
   0 STRINGS !  [: drop 1 STRINGS +! ;] INTERN:EACH-STRING  STRINGS @ .
   here NULL-PTR - IMAGE-BASE - . ;

\ Three of the build's words, called through their host entries.
: RUN-WORDS ( -- )
   21 s" FILE-OBJECTS-SHAPE:DOUBLE" ENTRY >DOUBLE execute .
   s" FILE-OBJECTS-COUNT:TALLY" ENTRY >CELL execute @ .
   s" FILE-OBJECTS-COUNT:HI" ENTRY >RUN execute ;

: BODY ( -- )
   ndict@ {: first:n :}
   SECOND? if PAD then
   s" test/fixtures/file-objects/count.f" included
   SHAPE
   8 allot
   s" test/fixtures/file-objects/main.f" included
   s" test/fixtures/file-objects/reopen.f" included
   MARK
   LOAD-AGAIN LOAD-AGAIN
   PROG$ LINKER:WRITE
   first STATE.  RUN-WORDS ;

\ ---- three builds of a file edited between the first two, and one that uses it ----
PATH-CAP BUFFER: EDIT-DIR-BUF
PATH-CAP BUFFER: EDIT-A-BUF

: EDIT-DIR$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" edit" EDIT-DIR-BUF FS:JOIN-PATH {: u:n :}
   EDIT-DIR-BUF u ;

: EDIT-A$ ( -- ptr u8 n )
   EDIT-DIR$ s" a.f" EDIT-A-BUF FS:JOIN-PATH {: u:n :}
   EDIT-A-BUF u ;

: EDIT-BUILD ( -- n )
   s" app" [: EDIT-A$ included  s" test/fixtures/file-objects/edit-b.f" included ;]
   LINKER:WITH  LINKER:COMPILED ;

: EDITS ( -- )
   EDIT-DIR$ FS:MAKE-DIR
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable V ;package\n" FS:WRITE-ALL
   EDIT-BUILD {: one:n :}
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable W variable V ;package\n"
   FS:WRITE-ALL
   EDIT-BUILD {: two:n :}
   EDIT-BUILD {: three:n :}
   s" edit: compiled " type  one FMT:.INT space  two FMT:.INT space  three FMT:.INT cr ;

: STOPS ( -- )
   s" app" [: STOP-FILE ;] LINKER:WITH
   s" app" [: STOP-BODY ;] LINKER:WITH
   s" app" [: STOP-INSIDE ;] LINKER:WITH
   s" app" [: STOP-ALIGN ;] LINKER:WITH
   s" app" [: STOP-STORE ;] LINKER:WITH
   s" app" [: STOP-GAP ;] LINKER:WITH
   s" app" [: STOP-STRADDLE ;] LINKER:WITH
   s" app" [: STOP-WORDLIST ;] LINKER:WITH
   s" app" [: STOP-STALE ;] LINKER:WITH
   EDITS
   s" stopped" type cr ;

public

: RUN ( -- )
   STOP? if STOPS exit then
   s" app" [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
