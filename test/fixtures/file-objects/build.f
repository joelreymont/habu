\ build.f - test/file-objects.f's child, which builds the fixtures here in the out root HABU_OUT names.
\ Its arguments: the kernel object every build links, then the mode, if any (MODE$).

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
require src/compiler/target/model.f
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

\ The second argument, second, shifted, stop or facts, or none; any other ends the build, rc 1.
: MODE$ ( -- ptr u8 n )
   SCRIPT-ARGC 1 = if s" " exit then
   SCRIPT-ARGC 2 = if
      1 SCRIPT-ARGV$ s" second" STR= if s" second" exit then
      1 SCRIPT-ARGV$ s" shifted" STR= if s" shifted" exit then
      1 SCRIPT-ARGV$ s" stop" STR= if s" stop" exit then
      1 SCRIPT-ARGV$ s" facts" STR= if s" facts" exit then
   then
   s" build: unknown argument" 1 die ;

: SECOND? ( -- bool ) MODE$ s" second" STR= ;
: SHIFTED? ( -- bool ) MODE$ s" shifted" STR= ;
: STOP? ( -- bool ) MODE$ s" stop" STR= ;
: FACTS? ( -- bool ) MODE$ s" facts" STR= ;

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

\ ---- the build's state, printed so a cold and a warm build compare as text ----
variable STRINGS                      \ the bodies INTERN:EACH-STRING visited

CAST: >DOUBLE ( n -- [ n -- n ] )
CAST: >CELL ( n -- [ -- ptr n ] )
CAST: >RUN ( n -- [ -- ] )
CAST: >LITERAL ( n -- [ -- ptr u8 n ] )
CAST: >TEXT ( n -- ptr u8 )

\ The host entry of the build's word named by the string.
: ENTRY ( ptr u8 n -- n ) XREF-FIND-INDEX XREF-REC XREF-START ;

: IMAGE-BASE ( -- n ) data-base BOOT:IMAGE-BASE-CELL + @ ;

\ The record's facts text's address: bytes 32-39 hold its offset from the image base, 0 for none.
: FACTS-ADDR ( ptr n -- n )
   4 XREF-CELL@ {: off:n :}
   off 0= if 0 exit then
   IMAGE-BASE off + ;

\ An interned text: its bytes, after the cell holding their count.
: TEXT. ( n -- )
   >TEXT {: t:ptr :}
   t  t 8 - LE:U64@  type ;

\ The record's name, then its fields and its facts text, `-` for none.
: RECORD. ( n -- )
   XREF-REC {: rec:ptr :}
   rec XREF-NAME$ type cr
   rec XREF-FLAGS FMT:.INT space  rec XREF-WORDLIST FMT:.INT space  rec XREF-RAW-LEN FMT:.INT space
   rec FACTS-ADDR {: at:n :}
   at 0= if s" -" type else at TEXT. then cr ;

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

\ -1 when main.f's literal, the bytes of count.f's facts text `( -- ) `, lies at HI's text.
: SHARED. ( -- )
   s" FILE-OBJECTS-MAIN:HI-TEXT" ENTRY >LITERAL execute drop NULL-PTR -
   s" FILE-OBJECTS-COUNT:HI" XREF-FIND-INDEX XREF-REC FACTS-ADDR =
   s" shared " type  bool>n FMT:.INT space cr ;

: BODY ( -- )
   ndict@ {: first:n :}
   SECOND? if PAD then
   SHIFTED? if                     \ moves later files' records, emissions, wordlist ids and data
      s" package FILE-OBJECTS-SHIFT public : ONE ( -- n ) 1 ; ;package" evaluate-closed
      24 allot
   then
   s" test/fixtures/file-objects/count.f" included
   SHAPE
   s" : FILE-OBJECTS-BODY-SUM ( n n -- n ) + ;" evaluate-closed
   8 allot
   s" test/fixtures/file-objects/main.f" included
   s" test/fixtures/file-objects/reopen.f" included
   MARK
   LOAD-AGAIN LOAD-AGAIN
   PROG$ LINKER:WRITE
   first STATE.  RUN-WORDS  SHARED. ;

\ ---- stop mode's later builds: a compile after a link, edit sequences, the twins ----
PATH-CAP BUFFER: DIR-BUF
PATH-CAP BUFFER: FIRST-BUF
PATH-CAP BUFFER: SECOND-BUF
PATH-CAP BUFFER: THIRD-BUF
PATH-CAP BUFFER: FOURTH-BUF
PATH-CAP BUFFER: FIFTH-BUF

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
: LAID-R$ ( -- ptr u8 n ) s" laid" s" r.f" FIRST-BUF FILE-IN ;
: LAID-S$ ( -- ptr u8 n ) s" laid" s" s.f" SECOND-BUF FILE-IN ;
: PRIV-P$ ( -- ptr u8 n ) s" private" s" p.f" FIRST-BUF FILE-IN ;
: PRIV-Q$ ( -- ptr u8 n ) s" private" s" q.f" SECOND-BUF FILE-IN ;
: PARTS-A$ ( -- ptr u8 n ) s" parts" s" a.f" FIRST-BUF FILE-IN ;
: PARTS-B$ ( -- ptr u8 n ) s" parts" s" b.f" SECOND-BUF FILE-IN ;
: PARTS-C$ ( -- ptr u8 n ) s" parts" s" c.f" THIRD-BUF FILE-IN ;
: SCOPE-A$ ( -- ptr u8 n ) s" scope" s" a.f" FIRST-BUF FILE-IN ;
: SCOPE-B$ ( -- ptr u8 n ) s" scope" s" b.f" SECOND-BUF FILE-IN ;
: SCOPE-C$ ( -- ptr u8 n ) s" scope" s" c.f" THIRD-BUF FILE-IN ;
: INSIDE-P$ ( -- ptr u8 n ) s" inside" s" p.f" FIRST-BUF FILE-IN ;
: INSIDE-I$ ( -- ptr u8 n ) s" inside" s" i.f" SECOND-BUF FILE-IN ;
: INSIDE-Q$ ( -- ptr u8 n ) s" inside" s" q.f" THIRD-BUF FILE-IN ;
: TWIN-X$ ( -- ptr u8 n ) s" twin-x" s" a.f" FIRST-BUF FILE-IN ;
: TWIN-Y$ ( -- ptr u8 n ) s" twin-y" s" a.f" SECOND-BUF FILE-IN ;
: ORDER-M$ ( -- ptr u8 n ) s" order" s" m.f" FIRST-BUF FILE-IN ;
: ORDER-S$ ( -- ptr u8 n ) s" order" s" s.f" SECOND-BUF FILE-IN ;
: ORDER-T$ ( -- ptr u8 n ) s" order" s" t.f" THIRD-BUF FILE-IN ;
: ORDER-F$ ( -- ptr u8 n ) s" order" s" f.f" FOURTH-BUF FILE-IN ;
: HEAD-W$ ( -- ptr u8 n ) s" header" s" w.f" FIRST-BUF FILE-IN ;
: HEAD-H$ ( -- ptr u8 n ) s" header" s" h.f" SECOND-BUF FILE-IN ;
: HEAD-R$ ( -- ptr u8 n ) s" header" s" r.f" THIRD-BUF FILE-IN ;
: HELD-W$ ( -- ptr u8 n ) s" held" s" w.f" FIRST-BUF FILE-IN ;
: HELD-H$ ( -- ptr u8 n ) s" held" s" h.f" SECOND-BUF FILE-IN ;
: HELD-R$ ( -- ptr u8 n ) s" held" s" r.f" THIRD-BUF FILE-IN ;
: SAME-K$ ( -- ptr u8 n ) s" same" s" k.f" FIRST-BUF FILE-IN ;
: SAME-S1$ ( -- ptr u8 n ) s" same" s" s1.f" SECOND-BUF FILE-IN ;
: SAME-S2$ ( -- ptr u8 n ) s" same" s" s2.f" THIRD-BUF FILE-IN ;

\ Given a build's caught code: that code, or its LINKER:COMPILED when it threw none.
: RESULT ( n -- n ) dup 0= if drop LINKER:COMPILED then ;

\ The text, then three builds' results; the caller ends the line.
: RESULTS. ( ptr u8 n n n n -- )
   {: a:ptr u:n one:n two:n three:n :}
   a u type  one FMT:.INT space  two FMT:.INT space  three FMT:.INT ;

: MIXED-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: s" test/fixtures/file-objects/count.f" included  SHAPE ;] LINKER:WITH ;]
   catch RESULT ;

\ count.f alone, then count.f and shape.f twice: shape.f compiles after count.f links.
: MIXED ( -- )
   [: s" app" RTARGET:HOST-TARGET [: s" test/fixtures/file-objects/count.f" included ;] LINKER:WITH ;]
   catch RESULT {: one:n :}
   MIXED-BUILD {: two:n :}
   MIXED-BUILD {: three:n :}
   s" mixed: compiled " one two three RESULTS. cr ;

: EDIT-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: EDIT-A$ included  s" test/fixtures/file-objects/edit-b.f" included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ a.f, the earlier file, grows its variable.
: EDITS ( -- )
   s" edit" DIR$ FS:MAKE-DIR
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable V ;package\n" FS:WRITE-ALL
   EDIT-BUILD {: one:n :}
   EDIT-A$ s\" package FILE-OBJECTS-EDIT-A public variable V 8 allot ;package\n"
   FS:WRITE-ALL
   EDIT-BUILD {: two:n :}
   EDIT-BUILD {: three:n :}
   s" edit: compiled " one two three RESULTS. cr ;

: TAIL-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: TAIL-C$ included  TAIL-D$ included ;] LINKER:WITH ;] catch RESULT ;

\ d.f, the later file, gains a variable before its first. The fourth build loads
\ d.f alone: its object names c.f, which defines V, a name d.f spells, so it compiles.
: TAILS ( -- )
   s" tail" DIR$ FS:MAKE-DIR
   TAIL-C$ s\" package FILE-OBJECTS-TAIL-C public variable V ;package\n" FS:WRITE-ALL
   TAIL-D$ s\" package FILE-OBJECTS-TAIL-D public variable V ;package\n" FS:WRITE-ALL
   TAIL-BUILD {: one:n :}
   TAIL-D$ s\" package FILE-OBJECTS-TAIL-D public variable W variable V ;package\n"
   FS:WRITE-ALL
   TAIL-BUILD {: two:n :}
   TAIL-BUILD {: three:n :}
   [: s" app" RTARGET:HOST-TARGET [: TAIL-D$ included ;] LINKER:WITH ;] catch RESULT {: four:n :}
   s" tail: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: SHARE-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: SHARE-P$ included  SHARE-Q$ included ;] LINKER:WITH ;] catch RESULT ;

\ p.f, the earlier file, comes to declare q.f's package too, as q.f's object does: q.f
\ spells no other name of p.f's.
: SHARES ( -- )
   s" share" DIR$ FS:MAKE-DIR
   SHARE-P$ s\" package FILE-OBJECTS-SHARE-P public variable V ;package\n" FS:WRITE-ALL
   SHARE-Q$ s\" package FILE-OBJECTS-SHARE-Q public variable U ;package\n" FS:WRITE-ALL
   SHARE-BUILD {: one:n :}
   SHARE-P$ s\" package FILE-OBJECTS-SHARE-Q public variable W ;package\n" FS:APPEND-FILE
   SHARE-BUILD {: two:n :}
   SHARE-BUILD {: three:n :}
   s" share: compiled " one two three RESULTS. cr ;

: LAID-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: LAID-R$ included  LAID-S$ included ;] LINKER:WITH ;] catch RESULT ;

\ r.f, the earlier file, comes to lay the string s.f's object lays, and spells none of s.f's
\ names.
: LAIDS ( -- )
   s" laid" DIR$ FS:MAKE-DIR
   LAID-R$ s\" package FILE-OBJECTS-LAID-R public variable V ;package\n" FS:WRITE-ALL
   LAID-S$ s\" package FILE-OBJECTS-LAID-S public : SAY ( -- ptr u8 n ) s\" late\" ; ;package\n"
   FS:WRITE-ALL
   LAID-BUILD {: one:n :}
   LAID-R$ s\" package FILE-OBJECTS-LAID-R : WORDS ( -- ) s\" late\" type ; ;package\n"
   FS:APPEND-FILE
   LAID-BUILD {: two:n :}
   LAID-BUILD {: three:n :}
   s" laid: compiled " one two three RESULTS. cr ;

: PRIV-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: PRIV-P$ included  PRIV-Q$ included ;] LINKER:WITH ;] catch RESULT ;

\ p.f, the earlier file, changes its private HELPER, a name q.f spells for a word of its own
\ package: q.f opens no package of p.f's, so it links.
: PRIVS ( -- )
   s" private" DIR$ FS:MAKE-DIR
   PRIV-P$ s\" package FILE-OBJECTS-PRIV-P : HELPER ( -- n ) 1 ; public : ONE ( -- n ) HELPER ; ;package\n"
   FS:WRITE-ALL
   PRIV-Q$ s\" package FILE-OBJECTS-PRIV-Q : HELPER ( -- n ) 2 ; public : TWO ( -- n ) HELPER ; ;package\n"
   FS:WRITE-ALL
   PRIV-BUILD {: one:n :}
   PRIV-P$ s\" package FILE-OBJECTS-PRIV-P : HELPER ( -- n ) 3 ; public : ONE ( -- n ) HELPER ; ;package\n"
   FS:WRITE-ALL
   PRIV-BUILD {: two:n :}
   PRIV-BUILD {: three:n :}
   s" private: compiled " one two three RESULTS. cr ;

: PARTS-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: PARTS-A$ included  PARTS-B$ included  PARTS-C$ included ;] LINKER:WITH ;]
   catch RESULT ;

\ b.f, a part of a.f's package, changes its private constant K, which c.f spells before it reopens
\ the package and again to store it into its own V: only the spelling after the open compiles c.f.
: PARTS ( -- )
   s" parts" DIR$ FS:MAKE-DIR
   PARTS-A$ s\" package FILE-OBJECTS-PARTS public : ONE ( -- n ) 1 ; ;package\n" FS:WRITE-ALL
   PARTS-B$ s\" package FILE-OBJECTS-PARTS 5 constant K ;package\n" FS:WRITE-ALL
   PARTS-C$ s\" package FILE-OBJECTS-PARTS-C public variable V ( K ) ;package\n" FS:WRITE-ALL
   PARTS-C$ s\" package FILE-OBJECTS-PARTS K FILE-OBJECTS-PARTS-C:V ! ;package\n" FS:APPEND-FILE
   PARTS-BUILD {: one:n :}
   PARTS-B$ s\" package FILE-OBJECTS-PARTS 6 constant K ;package\n" FS:WRITE-ALL
   PARTS-BUILD {: two:n :}
   PARTS-BUILD {: three:n :}
   s" parts: compiled " one two three RESULTS. cr ;

: SCOPE-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: SCOPE-A$ included  SCOPE-B$ included  SCOPE-C$ included ;] LINKER:WITH ;]
   catch RESULT ;

\ a.f leaves its package open for b.f, which closes it and is edited: neither links (linker.f
\ TOP-SCOPE?), and c.f, which names b.f's SEVEN, links once b.f's piece keeps its time.
: SCOPES ( -- )
   s" scope" DIR$ FS:MAKE-DIR
   SCOPE-A$ s\" package FILE-OBJECTS-SCOPE : HIDDEN ( -- n ) 7 ;\n" FS:WRITE-ALL
   SCOPE-B$ s\" public : SEVEN ( -- n ) HIDDEN ;\n;package\n" FS:WRITE-ALL
   SCOPE-C$ s\" : FILE-OBJECTS-EIGHT ( -- n ) FILE-OBJECTS-SCOPE:SEVEN 1 + ;\n" FS:WRITE-ALL
   SCOPE-BUILD {: one:n :}
   SCOPE-B$ s\" public : SEVEN ( -- n ) HIDDEN 1 + ;\n;package\n" FS:WRITE-ALL
   SCOPE-BUILD {: two:n :}
   SCOPE-BUILD {: three:n :}
   s" scope: compiled " one two three RESULTS. cr ;

: INSIDE-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: INSIDE-P$ included  INSIDE-I$ included  INSIDE-Q$ included ;] LINKER:WITH ;]
   catch RESULT ;

: OUTSIDE-BUILD ( -- n ) [: s" app" RTARGET:HOST-TARGET [: INSIDE-I$ included ;] LINKER:WITH ;] catch RESULT ;

\ i.f inside the package p.f leaves open and q.f closes, alone, inside, alone: a load inside never
\ links and its object replaces the one a load alone wrote, so each build alone compiles i.f.
: INSIDES ( -- )
   s" inside" DIR$ FS:MAKE-DIR
   INSIDE-P$ s\" package FILE-OBJECTS-INSIDE\n" FS:WRITE-ALL
   INSIDE-I$ s\" : FILE-OBJECTS-INNER ( -- n ) 1 ;\n" FS:WRITE-ALL
   INSIDE-Q$ s\" ;package\n" FS:WRITE-ALL
   INSIDE-BUILD {: one:n :}
   OUTSIDE-BUILD {: two:n :}
   INSIDE-BUILD {: three:n :}
   OUTSIDE-BUILD {: four:n :}
   s" inside: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: TWIN-X-BUILD ( -- n ) [: s" app" RTARGET:HOST-TARGET [: TWIN-X$ included ;] LINKER:WITH ;] catch RESULT ;
: TWIN-Y-BUILD ( -- n ) [: s" app" RTARGET:HOST-TARGET [: TWIN-Y$ included ;] LINKER:WITH ;] catch RESULT ;

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

: ORDER-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: ORDER-M$ included  ORDER-S$ included  ORDER-T$ included  ORDER-F$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

: ORDER-SWAPPED ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: ORDER-M$ included  ORDER-T$ included  ORDER-S$ included  ORDER-F$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ s.f and t.f store into one cell and f.f reads it at top level: f.f compiles again when the
\ storers swap.
: ORDERS ( -- )
   s" order" DIR$ FS:MAKE-DIR
   ORDER-M$ s\" package FILE-OBJECTS-ORDER public variable MODE ;package\n" FS:WRITE-ALL
   ORDER-S$ s\" 1 FILE-OBJECTS-ORDER:MODE !\n" FS:WRITE-ALL
   ORDER-T$ s\" 2 FILE-OBJECTS-ORDER:MODE !\n" FS:WRITE-ALL
   ORDER-F$ s\" FILE-OBJECTS-ORDER:MODE @ constant FILE-OBJECTS-ORDER-M\n" FS:WRITE-ALL
   ORDER-BUILD {: one:n :}
   ORDER-SWAPPED {: two:n :}
   ORDER-SWAPPED {: three:n :}
   ORDER-BUILD {: four:n :}
   s" order: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: HEAD-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: HEAD-W$ included  HEAD-R$ included ;] LINKER:WITH ;]
   catch RESULT ;

: HEAD-STORED ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: HEAD-W$ included  HEAD-H$ included  HEAD-R$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ h.f stores into header data, as main.f's last line does, and r.f runs w.f's W: r.f links once
\ h.f joins the build (linker.f "dependencies by run").
: HEADS ( -- )
   s" header" DIR$ FS:MAKE-DIR
   HEAD-W$ s\" package FILE-OBJECTS-HEADER public : W ( -- n ) 3 ; ;package\n" FS:WRITE-ALL
   HEAD-H$ s\" package FILE-OBJECTS-HEADER-H public : H ( -- ) ; ;package\n' FILE-OBJECTS-HEADER-H:H data-base $43A0 + xt!\n"
   FS:WRITE-ALL
   HEAD-R$ s\" FILE-OBJECTS-HEADER:W constant FILE-OBJECTS-HEADER-N\n" FS:WRITE-ALL
   HEAD-BUILD {: one:n :}
   HEAD-STORED {: two:n :}
   HEAD-STORED {: three:n :}
   s" header: compiled " one two three RESULTS. cr ;

: HELD-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: HELD-W$ included  HELD-R$ included ;] LINKER:WITH ;]
   catch RESULT ;

: HELD-STORED ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: HELD-W$ included  HELD-H$ included  HELD-R$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ HEADS with r.f also holding a header-data address: T's literal has the bytes of a facts text
\ the open lays for an owned primitive (linker.f OWNED-RECORDS). r.f compiles when h.f joins.
: HELDS ( -- )
   s" held" DIR$ FS:MAKE-DIR
   HELD-W$ s\" package FILE-OBJECTS-HELD public : W ( -- n ) 3 ; ;package\n" FS:WRITE-ALL
   HELD-H$ s\" package FILE-OBJECTS-HELD-H public : H ( -- ) ; ;package\n' FILE-OBJECTS-HELD-H:H data-base $43A0 + xt!\n"
   FS:WRITE-ALL
   HELD-R$ s\" package FILE-OBJECTS-HELD-R public : T ( -- ptr u8 n ) s\" ( -- ) primitive \" ; ;package\nFILE-OBJECTS-HELD:W constant FILE-OBJECTS-HELD-N\n"
   FS:WRITE-ALL
   HELD-BUILD {: one:n :}
   HELD-STORED {: two:n :}
   HELD-STORED {: three:n :}
   s" held: compiled " one two three RESULTS. cr ;

\ The second build alone writes a program, PROG$ in the out root: its entry is the header store
\ s2.f's object holds, and LINKER:WRITE ends the build rc 74 when the entry cell is empty.
: SAME-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: SAME-K$ included  SAME-S1$ included  SAME-S2$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

: SAME-WRITTEN ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: SAME-K$ included  SAME-S2$ included  PROG$ LINKER:WRITE ;]
      LINKER:WITH ;]
   catch RESULT ;

\ s1.f and s2.f store K's xt into the header cell main.f's last line stores into: s2.f's store
\ leaves the cell as s1.f left it, and its object still holds it once s1.f leaves the build.
: SAMES ( -- )
   s" same" DIR$ FS:MAKE-DIR
   SAME-K$ s\" package FILE-OBJECTS-SAME public : K ( -- ) ; ;package\n" FS:WRITE-ALL
   SAME-S1$ s\" ' FILE-OBJECTS-SAME:K data-base $43A0 + xt!\n" FS:WRITE-ALL
   SAME-S2$ s\" ' FILE-OBJECTS-SAME:K data-base $43A0 + xt!\n" FS:WRITE-ALL
   SAME-BUILD {: one:n :}
   SAME-WRITTEN {: two:n :}
   s" same: compiled " type  one FMT:.INT space  two FMT:.INT cr ;

\ ---- objects the link refuses or links wrong ----
\ Each edit starts from the object e.f's first build wrote (CRAFT-FIRST): a build reads one edit.
8192 constant CRAFT-CAP
192 constant PIECES-FIELD               \ the header's PIECES offset (linker.f WRITE-OBJECT)
PATH-CAP BUFFER: CRAFT-BUF
PATH-CAP BUFFER: CRAFT-PATH   variable CRAFT-PATH-U
PATH-CAP BUFFER: CRAFT-TAIL   variable CRAFT-TAIL-U   \ the end of the path CRAFT-FIND seeks
CRAFT-CAP BUFFER: CRAFT-FIRST   variable CRAFT-U
CRAFT-CAP BUFFER: CRAFT-OBJ
variable AT                              \ the walk's offset in CRAFT-OBJ
variable RECS                            \ the records walked
variable DEFS                            \ the kind 0 entries walked, each its emission's number
variable FLAG-AT                         \ the package entry's own-private flag's offset
variable MID-EM
variable PKG-REC
variable LOW-REC
variable HIGH-REC
variable BYTES-AT                        \ MID's emission's bytes' offset
variable SITE-AT                         \ MID's call row of LOW: its site's offset
variable INDEX-AT                        \ and its reference's sum's offset
variable LATE-FLAG-AT                    \ the late package entry's own-private flag's offset
variable LATE-AT                         \ the late package's KIND-PRIVATE entry's offset
variable SLOT-AT                         \ the first does> slot's P: its offset
variable SLOT-BYTES-AT                   \ its emission's bytes' offset
variable SLOT-S                          \ and their count

: CRAFT-E$ ( -- ptr u8 n ) s" crafted" s" e.f" CRAFT-BUF FILE-IN ;

: CRAFT-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: CRAFT-E$ included ;] LINKER:WITH ;] catch RESULT ;

: OBJ@ ( n -- n ) CRAFT-OBJ swap + LE:U64@ ;
: OBJ! ( n n -- ) CRAFT-OBJ swap + LE:U64! ;
: OBJ-U64 ( -- n ) AT @ OBJ@  8 AT +! ;
: OBJ-SKIP ( n -- ) 8 * AT +! ;

\ The first piece's record n's sum and count, from its TIMES block: the header's pair 16 bytes before
\ PIECES' gives its offset; the block is its word count, then 11 cells a word, sum and count last.
: CRAFT-ID ( n -- n n )
   {: n:n :}
   PIECES-FIELD 16 - OBJ@  8 +  n 88 * + {: w:n :}
   w 72 + OBJ@  w 80 + OBJ@ ;

\ A reference: its tag, then 3, 1, 4, 4, 1, 0, 4 or 2 fields for tags 0-7 (linker.f CODE+).
: REF-SKIP ( -- )
   OBJ-U64 {: tag:n :}
   tag 0 = if 3 OBJ-SKIP exit then
   tag 2 =  tag 3 =  tag 6 =  or or if 4 OBJ-SKIP exit then
   tag 1 =  tag 4 =  or if 1 OBJ-SKIP exit then
   tag 7 = if 2 OBJ-SKIP then ;


\ An entry (linker.f RECORD+, LATE+, SEAL+); kind 4 is a late private wordlist and kind 5 a
\ sealed package, each one reference and no record; kind 6, a type entry, ends at its role.
: ENTRY-WALK ( -- )
   AT @ {: at:n :}
   OBJ-U64 {: kind:n :}
   kind 4 = if at LATE-AT ! then
   kind 4 =  kind 5 =  or if 3 OBJ-SKIP exit then
   OBJ-U64 {: u:n :}
   CRAFT-OBJ AT @ + u {: name:ptr nu:n :}
   u AT +!  1 OBJ-SKIP
   OBJ-U64 {: role:n :}
   role 3 =  role 4 =  or if 3 OBJ-SKIP then        \ a role 3 or 4 names its package
   name nu s" LOW" STR= if RECS @ LOW-REC ! then
   name nu s" HIGH" STR= if RECS @ HIGH-REC ! then
   name nu s" FILE-OBJECTS-CRAFT" STR= {: craft:bool :}
   name nu s" FILE-OBJECTS-CRAFT-LATE" STR= {: late:bool :}
   kind 1 =  craft and if RECS @ PKG-REC ! then
   1 RECS +!
   kind 0 = if
      name nu s" MID" STR= if DEFS @ MID-EM ! then
      1 DEFS +!
      REF-SKIP exit
   then
   kind 1 = if
      craft if AT @ FLAG-AT ! then
      late if AT @ LATE-FLAG-AT ! then
      1 OBJ-SKIP exit
   then
   kind 6 = if exit then
   kind 2 = if 3 else 1 then OBJ-SKIP ;


\ A call row of emission e: site, kind, reference; MID's row naming LOW (tag 0, file 0, LOW's sum
\ and count) is kept.
: CALL-ROW ( n -- )
   {: e:n :}
   AT @ {: site:n :}
   2 OBJ-SKIP
   AT @ {: ref:n :}
   REF-SKIP
   e MID-EM @ <> if exit then
   LOW-REC @ CRAFT-ID {: sum:n count:n :}
   ref OBJ@ 0=  ref 8 + OBJ@ 0=  and  ref 16 + OBJ@ sum =  and  ref 24 + OBJ@ count =  and if
      site SITE-AT !  ref 16 + INDEX-AT !
   then ;


\ Emission e (linker.f CODE+): S, R, the bytes, F offsets, call rows, address rows, P.
: EMISSION-WALK ( n -- )
   {: e:n :}
   OBJ-U64 {: s:n :}
   1 OBJ-SKIP
   AT @ {: bytes:n :}
   e MID-EM @ = if bytes BYTES-AT ! then
   s AT +!
   OBJ-U64 OBJ-SKIP
   OBJ-U64 0 ?do e CALL-ROW loop
   OBJ-U64 0 ?do 2 OBJ-SKIP REF-SKIP loop
   AT @ {: p-at:n :}
   OBJ-U64 0< if exit then
   SLOT-AT @ 0< if p-at SLOT-AT !  bytes SLOT-BYTES-AT !  s SLOT-S ! then
   REF-SKIP ;


\ CRAFT-FIRST into CRAFT-OBJ, then its fields' offsets; the build ends rc 1 when one is missing.
: CRAFT-WALK ( -- )
   CRAFT-FIRST CRAFT-OBJ CRAFT-U @ BYTE-COPY
   PIECES-FIELD OBJ@ AT !
   0 RECS !  0 DEFS !  -1 FLAG-AT !  -1 MID-EM !  -1 PKG-REC !  -1 LOW-REC !  -1 HIGH-REC !
   -1 BYTES-AT !  -1 SITE-AT !  -1 INDEX-AT !
   -1 LATE-FLAG-AT !  -1 LATE-AT !  -1 SLOT-AT !  -1 SLOT-BYTES-AT !  -1 SLOT-S !
   OBJ-U64 0 ?do ENTRY-WALK loop
   OBJ-U64 0 ?do i EMISSION-WALK loop
   FLAG-AT @ 0<  PKG-REC @ 0<  or  HIGH-REC @ 0<  or  SITE-AT @ 0<  or
   LATE-FLAG-AT @ 0<  or  LATE-AT @ 0<  or  SLOT-AT @ 0<  or if
      s" build: CRAFTS: e.f's object lacks a field the edits need" 1 die
   then ;


\ The edited object, u bytes, into the object CRAFT-FIND found.
: CRAFT-WRITE ( n -- )
   {: u:n :}
   CRAFT-PATH CRAFT-PATH-U @ CRAFT-OBJ u FS:WRITE-ALL ;

\ The edited object, u bytes, into e.f's object; then a build.
: CRAFTED-BUILD ( n -- n ) CRAFT-WRITE  CRAFT-BUILD ;


\ A path of the walk that ends with CRAFT-TAIL into CRAFT-PATH.
: CRAFT-SEEN ( ptr u8 n -- )
   {: p:ptr pu:n :}
   p pu CRAFT-TAIL CRAFT-TAIL-U @ ENDS-WITH? 0= if exit then
   p CRAFT-PATH pu BYTE-COPY  pu CRAFT-PATH-U ! ;

\ The object under <out>/obj whose path ends with the tail, into CRAFT-FIRST and CRAFT-PATH.
: CRAFT-FIND ( ptr u8 n -- )
   {: t:ptr tu:n :}
   t CRAFT-TAIL tu BYTE-COPY  tu CRAFT-TAIL-U !
   0 CRAFT-PATH-U !
   s" obj" DIR$ ['] CRAFT-SEEN FS:WALK-FILES
   CRAFT-PATH-U @ 0= if s" build: CRAFT-FIND: no object ends with the tail" 1 die then
   CRAFT-PATH CRAFT-PATH-U @ CRAFT-FIRST CRAFT-CAP FS:READ-ALL CRAFT-U ! ;


\ The package's own-private flag 0 while LOW, a later entry, names its private wordlist.
: NO-PRIVATE ( -- n ) CRAFT-WALK  0 FLAG-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD ;

\ MID's call row's site on the instruction after the call, whose bytes are not 0.
: SITE-MOVED ( -- n )
   CRAFT-WALK
   SITE-AT @ OBJ@ 4 + {: site:n :}
   CRAFT-OBJ BYTES-AT @ + site + LE:U32@ 0= if
      s" build: CRAFTS: the instruction after MID's call is 0" 1 die
   then
   site SITE-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD ;

\ MID's call row naming HIGH, a record made after MID.
: LATER-RECORD ( -- n )
   CRAFT-WALK  HIGH-REC @ CRAFT-ID  INDEX-AT @ 8 + OBJ!  INDEX-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD ;

\ MID's call row naming the package's own record.
: PACKAGE-RECORD ( -- n )
   CRAFT-WALK  PKG-REC @ CRAFT-ID  INDEX-AT @ 8 + OBJ!  INDEX-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD ;

\ V's does> slot, the created word's return a clause patches, moved 4 bytes onto an instruction
\ that is not 0.
: SLOT-SITE ( -- n )
   CRAFT-WALK
   SLOT-AT @ OBJ@ {: p:n :}
   CRAFT-OBJ SLOT-BYTES-AT @ + {: em:ptr :}
   p 4 >= if em p 4 - + LE:U32@ 0<> else false then if
      p 4 - SLOT-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD exit
   then
   p 4 + SLOT-S @ 4 - <= if em p 4 + + LE:U32@ 0<> else false then if
      p 4 + SLOT-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD exit
   then
   s" build: CRAFTS: no instruction beside V's slot" 1 die ;

\ V's slot naming LOW (tag 0, file 0, LOW's sum and count) in place of none: 24 bytes more, in the
\ last section.
: SLOT-TARGET ( -- n )
   CRAFT-WALK
   SLOT-AT @ 8 + {: tag-at:n :}
   tag-at OBJ@ 5 <> if s" build: CRAFTS: V's slot names a target" 1 die then
   CRAFT-U @ 24 + {: u:n :}
   u CRAFT-CAP > if s" build: CRAFTS: the grown object exceeds CRAFT-CAP" 1 die then
   LOW-REC @ CRAFT-ID {: sum:n count:n :}
   CRAFT-FIRST tag-at 8 + +  CRAFT-OBJ tag-at 32 + +  CRAFT-U @ tag-at 8 + -  BYTE-COPY
   0 tag-at OBJ!  0 tag-at 8 + OBJ!  sum tag-at 16 + OBJ!  count tag-at 24 + OBJ!
   PIECES-FIELD 8 + {: len-at:n :}
   len-at OBJ@ 24 + len-at OBJ!
   u CRAFTED-BUILD ;

\ The late package's own-private flag 1: its KIND-PRIVATE entry names a package holding one.
: LATE-OWN ( -- n ) CRAFT-WALK  1 LATE-FLAG-AT @ OBJ!  CRAFT-U @ CRAFTED-BUILD ;

\ The first piece's word count in TIMES, whose pair is the header's 16 bytes before PIECES, one
\ more than its records.
: TIMES-COUNT ( -- n )
   CRAFT-WALK
   PIECES-FIELD 16 - OBJ@ {: count-at:n :}
   count-at OBJ@ 1 +  count-at OBJ!  CRAFT-U @ CRAFTED-BUILD ;


\ e.f's first build, eight builds each over one crafted object, then a plain build. CRAFT's
\ wordlists come between LATE's two, so LATE gains its private one by a KIND-PRIVATE entry.
: CRAFTS ( -- )
   s" crafted" DIR$ FS:MAKE-DIR
   CRAFT-E$ s\" : FILE-OBJECTS-CRAFT-LATE:ONE ( -- n ) 1 ;\n" FS:WRITE-ALL
   CRAFT-E$ s\" package FILE-OBJECTS-CRAFT private : LOW ( -- n ) 1 ;\n" FS:APPEND-FILE
   CRAFT-E$ s\" public : MID ( -- n ) LOW 1 + ; : HIGH ( -- n ) 3 ; ;package\n" FS:APPEND-FILE
   CRAFT-E$ s\" package FILE-OBJECTS-CRAFT-LATE public variable V ;package\n" FS:APPEND-FILE
   CRAFT-BUILD {: one:n :}
   s" /crafted/e.f.hfo" CRAFT-FIND
   NO-PRIVATE {: two:n :}
   SITE-MOVED {: three:n :}
   LATER-RECORD {: four:n :}
   PACKAGE-RECORD {: five:n :}
   SLOT-SITE {: six:n :}
   SLOT-TARGET {: seven:n :}
   LATE-OWN {: eight:n :}
   TIMES-COUNT {: nine:n :}
   CRAFT-BUILD {: ten:n :}
   s" crafted: compiled " one two three RESULTS.
   space four FMT:.INT  space five FMT:.INT  space six FMT:.INT
   space seven FMT:.INT  space eight FMT:.INT  space nine FMT:.INT  space ten FMT:.INT cr ;

\ ---- words a source spells ----
144 constant WORDS-FIELD                \ the header's WORDS offset, then length (linker.f WRITE-OBJECT)
32 constant WORD-ROW-BYTES               \ a WORDS row: FILES row, sum, count, time

: WORDS-A$ ( -- ptr u8 n ) s" words" s" a.f" FIRST-BUF FILE-IN ;
: WORDS-B$ ( -- ptr u8 n ) s" words" s" b.f" SECOND-BUF FILE-IN ;
: WORDS-C$ ( -- ptr u8 n ) s" words" s" c.f" THIRD-BUF FILE-IN ;
: WORDS-D$ ( -- ptr u8 n ) s" words" s" d.f" FOURTH-BUF FILE-IN ;
: WORDS-E$ ( -- ptr u8 n ) s" words" s" e.f" FIFTH-BUF FILE-IN ;

: WORDS-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET
      [: WORDS-A$ included  WORDS-B$ included  WORDS-C$ included  WORDS-D$ included  WORDS-E$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ b.f's object, read afresh, in CRAFT-OBJ, and the offset of its first WORDS row.
: FIRST-WORD-ROW ( -- n )
   s" /words/b.f.hfo" CRAFT-FIND
   CRAFT-FIRST CRAFT-OBJ CRAFT-U @ BYTE-COPY
   WORDS-FIELD 8 + OBJ@ WORD-ROW-BYTES < if s" build: BY-WORD: b.f's object lists no word" 1 die then
   WORDS-FIELD OBJ@ ;

\ b.f's first WORDS row with its time one more; then a build.
: WORD-RETIMED ( -- n )
   FIRST-WORD-ROW 24 + {: at:n :}
   at OBJ@ 1 +  at OBJ!  CRAFT-U @ CRAFT-WRITE  WORDS-BUILD ;

\ b.f's first WORDS row naming sum 1000, which no word of its piece has; then a build, which compiles b.f:
\ the row names no record of its sum and count (WORDS-CK).
: WORD-MISSING ( -- n )
   FIRST-WORD-ROW 8 + {: at:n :}
   1000 at OBJ!  CRAFT-U @ CRAFT-WRITE  WORDS-BUILD ;

\ A file depends on each earlier word its source spells, not on the word's whole piece: A1's edit
\ compiles b.f, d.f and e.f, not c.f, which spells A2; a new C in a.f compiles c.f, which spells C.
: BY-WORD ( -- )
   s" words" DIR$ FS:MAKE-DIR
   WORDS-A$ s\" package FILE-OBJECTS-WORDS public : A1 ( -- n ) 1 ; : A2 ( -- n ) 2 ; ;package\n" FS:WRITE-ALL
   WORDS-B$ s\" package FILE-OBJECTS-WORDS-B public : B ( -- n ) FILE-OBJECTS-WORDS:A1 ; ;package\n" FS:WRITE-ALL
   WORDS-C$ s\" package FILE-OBJECTS-WORDS-C public : C ( -- n ) FILE-OBJECTS-WORDS:A2 ; ;package\n" FS:WRITE-ALL
   WORDS-D$ s\" package FILE-OBJECTS-WORDS-D public EXPORT FILE-OBJECTS-WORDS:A1 ;package\n" FS:WRITE-ALL
   WORDS-E$ s\" package FILE-OBJECTS-WORDS-E public : E ( -- n ) FILE-OBJECTS-WORDS-D:A1 ; ;package\n" FS:WRITE-ALL
   WORDS-BUILD {: one:n :}
   WORDS-A$ s\" package FILE-OBJECTS-WORDS public : A1 ( -- n ) 3 ; : A2 ( -- n ) 2 ; ;package\n" FS:WRITE-ALL
   WORDS-BUILD {: two:n :}
   WORDS-BUILD {: three:n :}
   WORDS-A$ s\" package FILE-OBJECTS-WORDS public : A1 ( -- n ) 3 ; : A2 ( -- n ) 2 ; : C ( -- n ) 9 ; ;package\n"
   FS:WRITE-ALL
   WORDS-BUILD {: four:n :}
   WORD-RETIMED {: five:n :}
   WORD-MISSING {: six:n :}
   WORDS-BUILD {: seven:n :}
   s" words: compiled " one two three RESULTS.
   space four FMT:.INT  space five FMT:.INT  space six FMT:.INT  space seven FMT:.INT cr ;

\ ---- words a run word reaches ----
: RAN-A$ ( -- ptr u8 n ) s" ran" s" a.f" FIRST-BUF FILE-IN ;
: RAN-B$ ( -- ptr u8 n ) s" ran" s" b.f" SECOND-BUF FILE-IN ;
: RAN-C$ ( -- ptr u8 n ) s" ran" s" c.f" THIRD-BUF FILE-IN ;
: RAN-D$ ( -- ptr u8 n ) s" ran" s" d.f" FOURTH-BUF FILE-IN ;

: RAN-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET
      [: RAN-A$ included  RAN-B$ included  RAN-C$ included  RAN-D$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ A file that runs a word depends on each word the run word reaches, not on the word's whole
\ piece: V's edit compiles c.f, which ran W, which calls V, and not d.f, which ran U. Build 2's
\ c.f compiles while a.f and b.f link, build 5's after b.f compiles.
: BY-RUN ( -- )
   s" ran" DIR$ FS:MAKE-DIR
   RAN-A$ s\" package FILE-OBJECTS-RAN public : V ( -- n ) 1 ; : U ( -- n ) 5 ; ;package\n" FS:WRITE-ALL
   RAN-B$ s\" package FILE-OBJECTS-RAN-B public : W ( -- n ) FILE-OBJECTS-RAN:V ; ;package\n" FS:WRITE-ALL
   RAN-C$ s\" package FILE-OBJECTS-RAN-C public FILE-OBJECTS-RAN-B:W constant CW ;package\n" FS:WRITE-ALL
   RAN-D$ s\" package FILE-OBJECTS-RAN-D public FILE-OBJECTS-RAN:U constant CU ;package\n" FS:WRITE-ALL
   RAN-BUILD {: one:n :}
   RAN-C$ s\" package FILE-OBJECTS-RAN-C public FILE-OBJECTS-RAN-B:W constant CW ;package\n\\ CW is W's answer\n"
   FS:WRITE-ALL
   RAN-BUILD {: two:n :}
   RAN-A$ s\" package FILE-OBJECTS-RAN public : V ( -- n ) 3 ; : U ( -- n ) 5 ; ;package\n" FS:WRITE-ALL
   RAN-BUILD {: three:n :}
   RAN-BUILD {: four:n :}
   RAN-A$ s\" package FILE-OBJECTS-RAN public : V ( -- n ) 4 ; : U ( -- n ) 5 ; ;package\n" FS:WRITE-ALL
   RAN-BUILD {: five:n :}
   RAN-BUILD {: six:n :}
   s" ran: compiled " one two three RESULTS.
   space four FMT:.INT  space five FMT:.INT  space six FMT:.INT cr ;

: STOPS ( -- )
   s" app" RTARGET:HOST-TARGET [: STOP-FILE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-BODY ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-INSIDE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-ALIGN ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-PAST ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-STORE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-GAP ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-STRADDLE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-WORDLIST ;] LINKER:WITH
   MIXED EDITS TAILS SHARES LAIDS PRIVS PARTS SCOPES INSIDES TWINS ORDERS HEADS SAMES HELDS CRAFTS BY-WORD BY-RUN
   s" stopped" type cr ;

\ ---- facts mode: a body compiled against the words count.f and facts.f define ----

\ The label, then a case's caught code, on a line of its own.
: CODE. ( n ptr u8 n -- ) {: rc:n a:ptr u:n :}  a u type  rc FMT:.INT cr ;

: FACTS-AFTER ( -- n )
   [: s" : FOB-AFTER ( -- ) FILE-OBJECTS-COUNT:BUMP FILE-OBJECTS-COUNT:HI FILE-OBJECTS-COUNT:TALLY @ drop FILE-OBJECTS-LATE:ONE drop ;  ' FOB-AFTER data-base $43A0 + xt!" evaluate-closed ;]
   catch dup s" after: " CODE. ;

\ Never run: creating through a does> definer in a build ends the process.
: FACTS-WRAP ( -- )
   [: s" : FOB-WRAP ( n -- ) FILE-OBJECTS-FACTS:BOX ;" evaluate-closed ;]
   catch s" wrap: " CODE. ;

: FACTS-MISUSE ( -- )
   [: s\" : FOB-MISUSE ( -- ) s\" x\" FILE-OBJECTS-FACTS:TAKE ;" evaluate-closed ;]
   catch s" misuse: " CODE. ;

: FACTS-DEAD ( -- )
   [: s" : FOB-DEAD ( -- ) FILE-OBJECTS-FACTS:FAIL 2 drop ;" evaluate-closed ;]
   catch s" dead: " CODE. ;

\ FOB-AFTER is the program's entry ($43A0 is APP-ENTRY:XT-CELL, as main.f writes it).
\ The refusals come after the write: a refused definition fails the build (STOP-BODY).
: FACTS-BODY ( -- )
   s" test/fixtures/file-objects/count.f" included
   s" test/fixtures/file-objects/facts.f" included
   FACTS-AFTER {: after:n :}
   FACTS-WRAP
   after 0= if
      s" FOB-AFTER" ENTRY >RUN execute
      s" after ran" type cr
   then
   PROG$ LINKER:WRITE
   FACTS-MISUSE FACTS-DEAD ;

: FACTS ( -- )
   s" app" RTARGET:HOST-TARGET [: FACTS-BODY ;] LINKER:WITH
   s" facts: compiled " type  LINKER:COMPILED FMT:.INT cr ;

public

: RUN ( -- )
   SCRIPT-ARGC 0 = if s" build: the first argument is the kernel object" 1 die then
   0 SCRIPT-ARGV$ LINKER:KERNEL
   STOP? if STOPS exit then
   FACTS? if FACTS exit then
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
