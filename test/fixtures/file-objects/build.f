\ build.f - test/file-objects.f's child: a build of count.f, shape.f, main.f,
\ reopen.f and mark.f written as the program `prog` under the out root, with
\ again.f loaded twice in the same build, then host.f loaded after the build
\ closes. After the write the build prints its state and runs three of its
\ words (STATE., RUN-WORDS); the last line is `compiled ` and LINKER:COMPILED.
\ The body lays one cell between shape.f and main.f, which no file's data
\ holds. With the script argument `second` the build loads pad.f first; with
\ `shifted` it declares a package and lays three cells before count.f. With
\ `stop`, these builds run in the one out root. Ten fail: in
\ STOP-FILE and STOP-BODY every later load and write is LINKER:E-FAILED,
\ STOP-INSIDE's file moves DP back and its own load is LINKER:E-FAILED,
\ STOP-ALIGN's file marks a cell its object cannot place, E-COMP-STATE,
\ STOP-PAST's file marks a cell past its own end, E-COMP-STATE,
\ STOP-STORE's file marks hb's newest record immediate, LINKER:E-STORE,
\ STOP-GAP's file stores into a cell the body laid, which no file holds,
\ LINKER:E-STORE, STOP-STRADDLE's file stores into a cell straddling the
\ header's entry cell, E-COMP-STATE, STOP-WORDLIST's file makes a wordlist,
\ LINKER:E-WORDLIST, and STOP-STALE, after a build of count.f alone, loads
\ count.f and then shape.f, which has no object there: LINKER:E-STALE, and a
\ later write LINKER:E-FAILED. `stale list: ` then prints the size in bytes of the
\ load list STOP-STALE wrote. The next build of the same two files prints
\ `stale: compiled ` and its result. Over a load list whose one row's path holds
\ a 0 byte, the build after it prints `corrupt: compiled ` and its result. Three
\ sequences of three builds then each load two files, one edited after the first
\ build: edit/a.f under the out root, edited, and edit-b.f beside build.f;
\ tail/c.f and tail/d.f under the out root with d.f edited, then a fourth
\ build of d.f alone; share/p.f and share/q.f under the out root with p.f
\ edited to declare q.f's package too. Then twin-x/a.f and twin-y/a.f under the
\ out root, one name in two directories, are each built alone in turn x, y, x,
\ y. `edit: compiled `, `tail: compiled `, `share: compiled ` and
\ `twins: compiled ` print each build's result: its LINKER:COMPILED, or the
\ code it threw.
\ The build's files load from its body: in a build's file `require` is
\ E-UNDEFINED (rc 70), and creating through a `does>` definer ends the process
\ at rec-wide-publish (rc 70).

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/le.f
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
: PAST ( -- ) s" test/fixtures/file-objects/past.f" included ;
: STRADDLE ( -- ) s" test/fixtures/file-objects/straddle.f" included ;
: FLAG ( -- ) s" test/fixtures/file-objects/flag.f" included ;
: GAP ( -- ) s" test/fixtures/file-objects/gap.f" included ;
: NEW-WORDLIST ( -- ) s" test/fixtures/file-objects/wordlist.f" included ;

\ The one argument, second, shifted or stop, or empty; any other ends the build, rc 1.
: MODE$ ( -- ptr u8 n )
   SCRIPT-ARGC 0 = if s" " exit then
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" second" STR= if s" second" exit then
      0 SCRIPT-ARGV$ s" shifted" STR= if s" shifted" exit then
      0 SCRIPT-ARGV$ s" stop" STR= if s" stop" exit then
   then
   s" build: unknown argument" 1 die ;

: SECOND? ( -- bool ) MODE$ s" second" STR= ;
: SHIFTED? ( -- bool ) MODE$ s" shifted" STR= ;
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

: STOP-PAST ( -- )
   ['] PAST catch E-COMP-STATE
   s" build: STOP-PAST: an address cell past its file's end was not refused" EXPECT ;

\ A file with no object after a linked one can be LINKER:E-STALE, so these
\ builds' refused file is their first.
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
   SHIFTED? if                     \ moves later files' records, emissions, wordlist ids and data
      s" package FILE-OBJECTS-SHIFT public : ONE ( -- n ) 1 ; ;package" evaluate-closed
      24 allot
   then
   s" test/fixtures/file-objects/count.f" included
   SHAPE
   8 allot
   s" test/fixtures/file-objects/main.f" included
   s" test/fixtures/file-objects/reopen.f" included
   MARK
   LOAD-AGAIN LOAD-AGAIN
   PROG$ LINKER:WRITE
   first STATE.  RUN-WORDS ;

\ ---- stop mode's later builds: stale and corrupt lists, edit sequences, the twins ----
PATH-CAP BUFFER: DIR-BUF
PATH-CAP BUFFER: FIRST-BUF
PATH-CAP BUFFER: SECOND-BUF

: DIR$ ( ptr u8 n -- ptr u8 n )
   {: d:ptr du:n :}
   BUILD-OUT:BASE$ d du DIR-BUF FS:JOIN-PATH {: u:n :}
   DIR-BUF u ;

\ The named file in the directory under the out root, joined into the buffer.
: FILE-IN ( ptr u8 n ptr u8 n ptr u8 -- ptr u8 n )
   {: d:ptr du:n name:ptr nameu:n dst:ptr :}
   d du DIR$ name nameu dst FS:JOIN-PATH {: u:n :}
   dst u ;

: EDIT-A$ ( -- ptr u8 n ) s" edit" s" a.f" FIRST-BUF FILE-IN ;
: TAIL-C$ ( -- ptr u8 n ) s" tail" s" c.f" FIRST-BUF FILE-IN ;
: TAIL-D$ ( -- ptr u8 n ) s" tail" s" d.f" SECOND-BUF FILE-IN ;
: SHARE-P$ ( -- ptr u8 n ) s" share" s" p.f" FIRST-BUF FILE-IN ;
: SHARE-Q$ ( -- ptr u8 n ) s" share" s" q.f" SECOND-BUF FILE-IN ;
: TWIN-X$ ( -- ptr u8 n ) s" twin-x" s" a.f" FIRST-BUF FILE-IN ;
: TWIN-Y$ ( -- ptr u8 n ) s" twin-y" s" a.f" SECOND-BUF FILE-IN ;

\ Given a build's caught code: that code, or its LINKER:COMPILED when it threw none.
: RESULT ( n -- n ) dup 0= if drop LINKER:COMPILED then ;

\ The text, then three builds' results; the caller ends the line.
: RESULTS. ( ptr u8 n n n n -- )
   {: a:ptr u:n one:n two:n three:n :}
   a u type  one FMT:.INT space  two FMT:.INT space  three FMT:.INT ;

: STALE-BUILD ( -- n )
   [: s" app" [: s" test/fixtures/file-objects/count.f" included  SHAPE ;] LINKER:WITH ;]
   catch RESULT ;

PATH-CAP BUFFER: LOADS-BUF

\ The load list a build writes and the next build's first file reads.
: LOADS$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" loads" LOADS-BUF FS:JOIN-PATH {: u:n :}
   LOADS-BUF u ;

\ count.f alone, then STOP-STALE's count.f and shape.f, then those two again.
: STALES ( -- )
   s" app" [: s" test/fixtures/file-objects/count.f" included ;] LINKER:WITH
   s" app" [: STOP-STALE ;] LINKER:WITH
   s" stale list: " type  LOADS$ FS:FILE-SIZE FMT:.INT cr
   s" stale: compiled " type  STALE-BUILD FMT:.INT cr ;

80 BUFFER: LIST-BUF                   \ CORRUPT's 67 bytes; zeroed, so its key bytes are 0
variable LIST-N

: LIST+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a LIST-BUF LIST-N @ + u BYTE-COPY  u LIST-N +! ;

\ A list field: its length as eight bytes, low byte first, then its bytes.
: LIST-FIELD+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u LIST-BUF LIST-N @ + LE:U64!  8 LIST-N +!  a u LIST+ ;

8 BUFFER: NUL-PATH

\ count, a 0 byte, then .f.
: NUL-PATH$ ( -- ptr u8 n )
   s" count" NUL-PATH swap BYTE-COPY  0 NUL-PATH 5 + c!
   s" .f" NUL-PATH 6 + swap BYTE-COPY
   NUL-PATH 8 ;

\ The magic, then one row: 32 key bytes, the path NUL-PATH$ and the root app.
: CORRUPT ( -- )
   0 LIST-N !
   s" HABULOAD" LIST+  32 LIST-N +!
   NUL-PATH$ LIST-FIELD+  s" app" LIST-FIELD+
   LOADS$ LIST-BUF LIST-N @ FS:WRITE-ALL
   s" corrupt: compiled " type  STALE-BUILD FMT:.INT cr ;

: EDIT-BUILD ( -- n )
   [: s" app" [: EDIT-A$ included  s" test/fixtures/file-objects/edit-b.f" included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ a.f, the earlier file, gains a variable before its first.
: EDITS ( -- )
   s" edit" DIR$ FS:MAKE-DIR
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable V ;package\n" FS:WRITE-ALL
   EDIT-BUILD {: one:n :}
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable W variable V ;package\n"
   FS:WRITE-ALL
   EDIT-BUILD {: two:n :}
   EDIT-BUILD {: three:n :}
   s" edit: compiled " one two three RESULTS. cr ;

: TAIL-BUILD ( -- n )
   [: s" app" [: TAIL-C$ included  TAIL-D$ included ;] LINKER:WITH ;] catch RESULT ;

\ d.f, the later file, gains a variable before its first. The fourth build loads
\ d.f alone: its object's prefix sums c.f, so it compiles.
: TAILS ( -- )
   s" tail" DIR$ FS:MAKE-DIR
   TAIL-C$ s\" package FILE-OBJECTS-TAIL-C public variable V ;package\n" FS:WRITE-ALL
   TAIL-D$ s\" package FILE-OBJECTS-TAIL-D public variable V ;package\n" FS:WRITE-ALL
   TAIL-BUILD {: one:n :}
   TAIL-D$ s\" package FILE-OBJECTS-TAIL-D public variable W variable V ;package\n"
   FS:WRITE-ALL
   TAIL-BUILD {: two:n :}
   TAIL-BUILD {: three:n :}
   [: s" app" [: TAIL-D$ included ;] LINKER:WITH ;] catch RESULT {: four:n :}
   s" tail: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: SHARE-BUILD ( -- n )
   [: s" app" [: SHARE-P$ included  SHARE-Q$ included ;] LINKER:WITH ;] catch RESULT ;

\ p.f, the earlier file, comes to declare q.f's package too, as q.f's object does.
: SHARES ( -- )
   s" share" DIR$ FS:MAKE-DIR
   SHARE-P$ s\" package FILE-OBJECTS-SHARE-P public variable V ;package\n" FS:WRITE-ALL
   SHARE-Q$ s\" package FILE-OBJECTS-SHARE-Q public variable V ;package\n" FS:WRITE-ALL
   SHARE-BUILD {: one:n :}
   SHARE-P$ s\" package FILE-OBJECTS-SHARE-Q public variable W ;package\n" FS:APPEND-FILE
   SHARE-BUILD {: two:n :}
   SHARE-BUILD {: three:n :}
   s" share: compiled " one two three RESULTS. cr ;

: TWIN-X-BUILD ( -- n ) [: s" app" [: TWIN-X$ included ;] LINKER:WITH ;] catch RESULT ;
: TWIN-Y-BUILD ( -- n ) [: s" app" [: TWIN-Y$ included ;] LINKER:WITH ;] catch RESULT ;

\ Two files of one name, each built alone in turn: each keeps an object of its own.
: TWINS ( -- )
   s" twin-x" DIR$ FS:MAKE-DIR  s" twin-y" DIR$ FS:MAKE-DIR
   TWIN-X$ s\" package FILE-OBJECTS-TWIN-X public variable V ;package\n" FS:WRITE-ALL
   TWIN-Y$ s\" package FILE-OBJECTS-TWIN-Y public variable V ;package\n" FS:WRITE-ALL
   TWIN-X-BUILD {: one:n :}
   TWIN-Y-BUILD {: two:n :}
   TWIN-X-BUILD {: three:n :}
   TWIN-Y-BUILD {: four:n :}
   s" twins: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: STOPS ( -- )
   s" app" [: STOP-FILE ;] LINKER:WITH
   s" app" [: STOP-BODY ;] LINKER:WITH
   s" app" [: STOP-INSIDE ;] LINKER:WITH
   s" app" [: STOP-ALIGN ;] LINKER:WITH
   s" app" [: STOP-PAST ;] LINKER:WITH
   s" app" [: STOP-STORE ;] LINKER:WITH
   s" app" [: STOP-GAP ;] LINKER:WITH
   s" app" [: STOP-STRADDLE ;] LINKER:WITH
   s" app" [: STOP-WORDLIST ;] LINKER:WITH
   STALES CORRUPT EDITS TAILS SHARES TWINS
   s" stopped" type cr ;

public

: RUN ( -- )
   STOP? if STOPS exit then
   s" app" [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
