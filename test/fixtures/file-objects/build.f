split-str
\ build.f - test/file-objects.f's child, which builds the fixtures here in the out root HABU_OUT names.
\ Its arguments: the kernel object every build links, then the mode, if any (MODE$).

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/build-out.f
require src/habu/xref.f
require src/habu/intern.f
require src/compiler/native/shadow.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require src/os/script-argv.f
require src/core/rejection.f

package FILE-OBJECTS-BUILD

PATH-CAP BUFFER: PROG-BUF

: PROG$ ( -- str )
   BUILD-OUT:BASE$ s" prog" PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;

: PAD ( -- ) s" test/fixtures/file-objects/pad.f" included ;
: RESIDUE ( -- ) s" test/fixtures/file-objects/residue.f" included ;
: LOAD-AGAIN ( -- ) s" test/fixtures/file-objects/again.f" included ;
: SHAPE ( -- ) s" test/fixtures/file-objects/shape.f" included ;
: MARK ( -- ) s" test/fixtures/file-objects/mark.f" included ;

: ROLLBACK ( -- ) s" test/fixtures/file-objects/rollback.f" included ;
: UNALIGNED ( -- ) s" test/fixtures/file-objects/unaligned.f" included ;
: PAST ( -- ) s" test/fixtures/file-objects/past.f" included ;
: FLAG ( -- ) s" test/fixtures/file-objects/flag.f" included ;
: GAP ( -- ) s" test/fixtures/file-objects/gap.f" included ;
: NEW-WORDLIST ( -- ) s" test/fixtures/file-objects/wordlist.f" included ;

\ The second argument, second, shifted, stop or facts, or none; any other is a usage error.
: MODE$ ( -- str )
   SCRIPT-ARGC 1 = if s" " exit then
   SCRIPT-ARGC 2 = if
      1 SCRIPT-ARGV$ s" second" STR:= if s" second" exit then
      1 SCRIPT-ARGV$ s" shifted" STR:= if s" shifted" exit then
      1 SCRIPT-ARGV$ s" stop" STR:= if s" stop" exit then
      1 SCRIPT-ARGV$ s" facts" STR:= if s" facts" exit then
   then
   s" build: unknown argument" EXIT-STATUS:USAGE die ;

: SECOND? ( -- bool ) MODE$ s" second" STR:= ;
: SHIFTED? ( -- bool ) MODE$ s" shifted" STR:= ;
: STOP? ( -- bool ) MODE$ s" stop" STR:= ;
: FACTS? ( -- bool ) MODE$ s" facts" STR:= ;

\ The case's code is the one wanted, or the build ends rc 1 with the message.
: EXPECT ( n n ^u8 n -- )
   {: rc:n want:n msg:^ msgu:n :}
   rc want <> if msg msgu EXIT-STATUS:FAILURE die then ;

: STOP-FILE ( -- )
   s" test/fixtures/file-objects/count.f" included
   ['] RESIDUE catch 70 s" build: STOP-FILE: residue.f not refused" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-FILE: a load after a refused file ran" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED
   s" build: STOP-FILE: a write after a refused file ran" EXPECT ;

: STOP-BODY ( -- )
   [: s" 16 allot : FOB-BAD ( -- n ) dup ;" evaluate-closed ;] catch
   70 s" build: STOP-BODY: the definition was not refused" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED s" build: STOP-BODY: a write after a caught failed text ran" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-BODY: a load after a caught failed text ran" EXPECT ;

: STOP-INSIDE ( -- )
   ['] ROLLBACK catch LINKER:E-FAILED
   s" build: STOP-INSIDE: a file's caught failed text was not refused" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED s" build: STOP-INSIDE: a write after a file's caught failed text ran" EXPECT ;

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

: STOP-WORDLIST ( -- )
   ['] NEW-WORDLIST catch LINKER:E-WORDLIST
   s" build: STOP-WORDLIST: a file that made a wordlist was not refused" EXPECT ;

\ ---- the build's state, printed so a cold and a warm build compare as text ----
variable STRINGS                      \ the bodies INTERN:EACH-STRING visited

CAST: >DOUBLE ( n -- [ n -- n ] )
CAST: >CELL ( n -- [ -- ^n ] )
CAST: >RUN ( n -- [ -- ] )

\ The host entry of the build's word named by the string.
: ENTRY ( ^u8 n -- n ) XREF-FIND XREF-START ;

\ The build's word named by the string becomes its program's entry.
: ENTRY! ( ^u8 n -- ) ENTRY >RUN APP-ENTRY:XT! ;

\ The record's name, then its fields.
: RECORD. ( n -- )
   XREF-REC {: rec:^ :}
   rec XREF-NAME$ type cr
   rec XREF-FLAGS FMT:.INT space  rec XREF-WORDLIST FMT:.INT space  rec XREF-RAW-LEN FMT:.INT cr ;

\ Each record from the first, then the counts of emissions and interned
\ strings.
: STATE. ( n -- )
   {: first:n :}
   ndict@ first ?do i RECORD. loop
   NSHADOW:EMISSIONS .
   0 STRINGS !  [: drop 1 STRINGS +! ;] INTERN:EACH-STRING  STRINGS @ . ;

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
   s" : FILE-OBJECTS-BODY-SUM ( n n -- n ) + ;" evaluate-closed
   8 allot
   s" test/fixtures/file-objects/main.f" included
   s" FILE-OBJECTS-MAIN:RUN" ENTRY!
   s" test/fixtures/file-objects/reopen.f" included
   MARK
   LOAD-AGAIN LOAD-AGAIN
   PROG$ LINKER:WRITE
   first STATE.  RUN-WORDS ;

\ ---- stop mode's later builds: a compile after a link, edit sequences, the twins ----
PATH-CAP BUFFER: DIR-BUF
PATH-CAP BUFFER: FIRST-BUF
PATH-CAP BUFFER: SECOND-BUF
PATH-CAP BUFFER: THIRD-BUF
PATH-CAP BUFFER: FOURTH-BUF
PATH-CAP BUFFER: FIFTH-BUF

: DIR$ ( str -- str )
   {: d:^ du:n :}
   BUILD-OUT:BASE$ d du DIR-BUF FS:JOIN-PATH {: u:n :}
   DIR-BUF u ;

\ The named file in the directory under the out root, joined into the buffer.
: FILE-IN ( str str ^u8 -- ^u8 n )
   {: d:^ du:n name:^ nameu:n dst:^ :}
   d du DIR$ name nameu dst FS:JOIN-PATH {: u:n :}
   dst u ;

: EDIT-A$ ( -- str ) s" edit" s" a.f" FIRST-BUF FILE-IN ;
: TAIL-C$ ( -- str ) s" tail" s" c.f" FIRST-BUF FILE-IN ;
: TAIL-D$ ( -- str ) s" tail" s" d.f" SECOND-BUF FILE-IN ;
: SHARE-P$ ( -- str ) s" share" s" p.f" FIRST-BUF FILE-IN ;
: SHARE-Q$ ( -- str ) s" share" s" q.f" SECOND-BUF FILE-IN ;
: LAID-R$ ( -- str ) s" laid" s" r.f" FIRST-BUF FILE-IN ;
: LAID-S$ ( -- str ) s" laid" s" s.f" SECOND-BUF FILE-IN ;
: PRIV-P$ ( -- str ) s" private" s" p.f" FIRST-BUF FILE-IN ;
: PRIV-Q$ ( -- str ) s" private" s" q.f" SECOND-BUF FILE-IN ;
: PARTS-A$ ( -- str ) s" parts" s" a.f" FIRST-BUF FILE-IN ;
: PARTS-B$ ( -- str ) s" parts" s" b.f" SECOND-BUF FILE-IN ;
: PARTS-C$ ( -- str ) s" parts" s" c.f" THIRD-BUF FILE-IN ;
: TWIN-X$ ( -- str ) s" twin-x" s" a.f" FIRST-BUF FILE-IN ;
: TWIN-Y$ ( -- str ) s" twin-y" s" a.f" SECOND-BUF FILE-IN ;
: ORDER-M$ ( -- str ) s" order" s" m.f" FIRST-BUF FILE-IN ;
: ORDER-S$ ( -- str ) s" order" s" s.f" SECOND-BUF FILE-IN ;
: ORDER-T$ ( -- str ) s" order" s" t.f" THIRD-BUF FILE-IN ;
: ORDER-F$ ( -- str ) s" order" s" f.f" FOURTH-BUF FILE-IN ;
: NONE-K$ ( -- str ) s" none" s" k.f" FIRST-BUF FILE-IN ;
: NONE-PROG$ ( -- str ) s" none" s" prog" SECOND-BUF FILE-IN ;

\ Given a build's caught code: that code, or its LINKER:COMPILED when it threw none.
: RESULT ( n -- n ) dup 0= if drop LINKER:COMPILED then ;

\ The text, then three builds' results; the caller ends the line.
: RESULTS. ( str n n n -- )
   {: a:^ u:n one:n two:n three:n :}
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
   LAID-S$ s\" package FILE-OBJECTS-LAID-S public : SAY ( -- str ) s\" late\" ; ;package\n"
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

\ The body's caught text leaves a record before any file loads, allotting
\ nothing: k.f's load then answers E-FAILED and nothing is written.
: NONE-WRITTEN ( -- n )
   [: s" app" RTARGET:HOST-TARGET [:
         [: s" : FOB-GONE ( -- ) ; 12345 throw" evaluate-closed ;] catch
         12345 s" build: NONES: the text's throw was not caught" EXPECT
         NONE-K$ included  s" FOB-K" ENTRY!  NONE-PROG$ LINKER:WRITE ;]
      LINKER:WITH ;]
   catch RESULT ;

: NONES ( -- )
   s" none" DIR$ FS:MAKE-DIR
   NONE-K$ s\" : FOB-K ( -- ) ;\n" FS:WRITE-ALL
   s" none: compiled " type  NONE-WRITTEN FMT:.INT cr ;

\ ---- words a source spells ----
: WORDS-A$ ( -- str ) s" words" s" a.f" FIRST-BUF FILE-IN ;
: WORDS-B$ ( -- str ) s" words" s" b.f" SECOND-BUF FILE-IN ;
: WORDS-C$ ( -- str ) s" words" s" c.f" THIRD-BUF FILE-IN ;
: WORDS-D$ ( -- str ) s" words" s" d.f" FOURTH-BUF FILE-IN ;
: WORDS-E$ ( -- str ) s" words" s" e.f" FIFTH-BUF FILE-IN ;

: WORDS-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET
      [: WORDS-A$ included  WORDS-B$ included  WORDS-C$ included  WORDS-D$ included  WORDS-E$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ A file depends on each earlier word its source spells, not on the word's whole piece: A1's edit compiles b.f, d.f
\ and e.f, not c.f, which spells A2; a new C in a.f compiles c.f, and A2's removal compiles it again, rejected for A2.
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
   WORDS-A$ s\" package FILE-OBJECTS-WORDS public : A1 ( -- n ) 3 ; : C ( -- n ) 9 ; ;package\n" FS:WRITE-ALL
   WORDS-BUILD {: five:n :}
   s" words: compiled " one two three RESULTS.
   space four FMT:.INT  space five FMT:.INT cr ;

\ ---- words a run word reaches ----
: RAN-A$ ( -- str ) s" ran" s" a.f" FIRST-BUF FILE-IN ;
: RAN-B$ ( -- str ) s" ran" s" b.f" SECOND-BUF FILE-IN ;
: RAN-C$ ( -- str ) s" ran" s" c.f" THIRD-BUF FILE-IN ;
: RAN-D$ ( -- str ) s" ran" s" d.f" FOURTH-BUF FILE-IN ;

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

\ ---- data a storer stored ----
: STORES-H$ ( -- str ) s" stores" s" h.f" FIRST-BUF FILE-IN ;
: STORES-S$ ( -- str ) s" stores" s" s.f" SECOND-BUF FILE-IN ;
: STORES-R$ ( -- str ) s" stores" s" r.f" THIRD-BUF FILE-IN ;

\ pointer-storage.f loads first for the PERSISTED- definers h.f and s.f use: a file here cannot require them, as the
\ kernel defines no require (h.f's `require` is E-UNDEFINED).
: STORES-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET
      [: s" src/core/pointer-storage.f" included  STORES-H$ included  STORES-S$ included  STORES-R$ included ;]
      LINKER:WITH ;]
   catch RESULT ;

\ s.f with NOTE's and ALPHA's strings, the word whose string table entry 0 points at, the size it stores and the
\ literal the quotation it stores in HOOK answers.
: STORES-S! ( str str str str str -- )
   {: note:^ noteu:n alpha:^ alphau:n entry:^ entryu:n size:^ sizeu:n lit:^ litu:n :}
   STORES-S$ s\" package FILE-OBJECTS-STORES-S public\n: NOTE ( -- str ) s\" " FS:WRITE-ALL
   STORES-S$ note noteu FS:APPEND-FILE
   STORES-S$ s\" \" ;\n: ALPHA ( -- str ) s\" " FS:APPEND-FILE
   STORES-S$ alpha alphau FS:APPEND-FILE
   STORES-S$ s\" \" ;\n: BETA ( -- str ) s\" beta\" ;\n2 PERSISTED-PTR-U8-TABLE T\n;package\nFILE-OBJECTS-STORES-S:"
   FS:APPEND-FILE
   STORES-S$ entry entryu FS:APPEND-FILE
   STORES-S$ s\"  str> drop FILE-OBJECTS-STORES-S:T 0 ptr-field !\nFILE-OBJECTS-STORES-S:BETA str> drop FILE-OBJECTS-STORES-S:T 1 ptr-field !\n"
   FS:APPEND-FILE
   STORES-S$ s" FILE-OBJECTS-STORES-S:T FILE-OBJECTS-STORES:TABLE !  " FS:APPEND-FILE
   STORES-S$ size sizeu FS:APPEND-FILE
   STORES-S$ s\"  FILE-OBJECTS-STORES:SIZE !\n: INSTALL ( -- ) [: " FS:APPEND-FILE
   STORES-S$ lit litu FS:APPEND-FILE
   STORES-S$ s\"  ;] FILE-OBJECTS-STORES:HOOK xt! ;\nINSTALL\n" FS:APPEND-FILE ;

\ r.f reads h.f's TABLE, SIZE and HOOK, which s.f stores: NOTE's edit compiles s.f alone, and a new entry 0, SIZE, entry
\ 0 string's bytes or code in HOOK's quotation each compile r.f too. The cold build compiles pointer-storage.f as well.
: BY-STORES ( -- )
   s" stores" DIR$ FS:MAKE-DIR
   STORES-H$ s\" package FILE-OBJECTS-STORES public PERSISTED-PTR-U8-TABLE-VARIABLE TABLE variable SIZE variable HOOK ;package\n"
   FS:WRITE-ALL
   s" note" s" alpha" s" ALPHA" s" 2" s" 42" STORES-S!
   STORES-R$ s\" package FILE-OBJECTS-STORES-R\nCAST: XT ( n -- [ -- n ] )\npublic\nFILE-OBJECTS-STORES:SIZE @ constant N\nFILE-OBJECTS-STORES:TABLE @ 0 ptr-field @ c@ constant C\nFILE-OBJECTS-STORES:HOOK @ XT execute constant Q\n;package\n"
   FS:WRITE-ALL
   STORES-BUILD {: one:n :}
   s" nota" s" alpha" s" ALPHA" s" 2" s" 42" STORES-S!
   STORES-BUILD {: two:n :}
   s" nota" s" alpha" s" BETA" s" 2" s" 42" STORES-S!
   STORES-BUILD {: three:n :}
   s" nota" s" alpha" s" BETA" s" 3" s" 42" STORES-S!
   STORES-BUILD {: four:n :}
   s" nota" s" olpha" s" ALPHA" s" 3" s" 42" STORES-S!
   STORES-BUILD {: five:n :}
   STORES-BUILD {: six:n :}
   s" nota" s" olpha" s" ALPHA" s" 3" s" 43" STORES-S!
   STORES-BUILD {: seven:n :}
   s" stores: compiled " one two three RESULTS.
   space four FMT:.INT  space five FMT:.INT  space six FMT:.INT  space seven FMT:.INT cr ;

\ ---- a stored quotation whose code is edited ----
variable SEEN-Q   \ the linked Q of the last own or through build, -1 when it threw
PATH-CAP BUFFER: SEQ-DIR  variable SEQ-DIR-U   \ the directory the build loads from: LINKER:WITH keeps its quotation

: SEQ-DIR! ( str -- ) {: d:^ du:n :}  d SEQ-DIR du BYTE-COPY  du SEQ-DIR-U ! ;
: SEQ-DIR$ ( -- str ) SEQ-DIR SEQ-DIR-U @ ;

CAST: >ANSWER ( n -- [ -- n ] )

: Q! ( str -- ) ENTRY >ANSWER execute SEEN-Q ! ;

\ The line's text, then four builds' compiled counts and Qs, then a cold build's
\ of the last sources.
: COLD-WARM. ( str n n n n n n n n n n -- )
   {: a:^ u:n one:n two:n three:n four:n q1:n q2:n q3:n q4:n cold:n qc:n :}
   a u one two three RESULTS.  space four FMT:.INT
   s"  q " type  q1 FMT:.INT space  q2 FMT:.INT space  q3 FMT:.INT space  q4 FMT:.INT
   s"  cold " type  cold FMT:.INT  s"  q " type  qc FMT:.INT cr ;

: OWN-P$ ( str -- str ) s" p.f" FIRST-BUF FILE-IN ;
: OWN-R$ ( str -- str ) s" r.f" SECOND-BUF FILE-IN ;

\ The directory's p.f, whose INSTALL stores in p.f's own HOOK a quotation answering the literal. A rewrite of a file
\ compiles it whatever its bytes, so an edit writes p.f alone.
: OWN-P! ( str str -- )
   {: d:^ du:n lit:^ litu:n :}
   d du OWN-P$ s\" package FILE-OBJECTS-OWN public variable HOOK\n: INSTALL ( -- ) [: " FS:WRITE-ALL
   d du OWN-P$ lit litu FS:APPEND-FILE
   d du OWN-P$ s\"  ;] HOOK xt! ;\nINSTALL\n;package\n" FS:APPEND-FILE ;

\ The directory's r.f, which runs HOOK's quotation.
: OWN-R! ( str -- )
   {: d:^ du:n :}
   d du OWN-R$ s\" package FILE-OBJECTS-OWN-R\nCAST: XT ( n -- [ -- n ] )\npublic\nFILE-OBJECTS-OWN:HOOK @ XT execute constant Q\n;package\n"
   FS:WRITE-ALL ;

: OWN-BUILD ( str -- n )
   SEQ-DIR!  -1 SEEN-Q !
   [: s" app" RTARGET:HOST-TARGET
      [: SEQ-DIR$ OWN-P$ included  SEQ-DIR$ OWN-R$ included  s" FILE-OBJECTS-OWN-R:Q" Q! ;]
      LINKER:WITH ;]
   catch RESULT ;

\ p.f stores into its own HOOK the quotation r.f runs: editing the quotation's literal must compile r.f, whose Q then
\ equals a cold build's. own-cold holds the edited sources, so its build is cold.
: BY-OWN ( -- )
   s" own" DIR$ FS:MAKE-DIR  s" own-cold" DIR$ FS:MAKE-DIR
   s" own" s" 42" OWN-P!  s" own" OWN-R!
   s" own" OWN-BUILD {: one:n :}  SEEN-Q @ {: q1:n :}
   s" own" OWN-BUILD {: two:n :}  SEEN-Q @ {: q2:n :}
   s" own" s" 43" OWN-P!
   s" own" OWN-BUILD {: three:n :}  SEEN-Q @ {: q3:n :}
   s" own" OWN-BUILD {: four:n :}  SEEN-Q @ {: q4:n :}
   s" own-cold" s" 43" OWN-P!  s" own-cold" OWN-R!
   s" own-cold" OWN-BUILD {: cold:n :}  SEEN-Q @ {: qc:n :}
   s" own: compiled " one two three four q1 q2 q3 q4 cold qc COLD-WARM. ;

: THROUGH-H$ ( str -- str ) s" h.f" FIRST-BUF FILE-IN ;
: THROUGH-P$ ( str -- str ) s" p.f" SECOND-BUF FILE-IN ;
: THROUGH-M$ ( str -- str ) s" m.f" THIRD-BUF FILE-IN ;
: THROUGH-R$ ( str -- str ) s" r.f" FOURTH-BUF FILE-IN ;

\ The directory's p.f, whose MAKE answers a quotation answering the literal.
: THROUGH-P! ( str str -- )
   {: d:^ du:n lit:^ litu:n :}
   d du THROUGH-P$ s\" package FILE-OBJECTS-THROUGH-P public\n: MAKE ( -- [ -- n ] ) [: " FS:WRITE-ALL
   d du THROUGH-P$ lit litu FS:APPEND-FILE
   d du THROUGH-P$ s\"  ;] ;\n;package\n" FS:APPEND-FILE ;

\ The directory's h.f holding HOOK, m.f storing MAKE's answer into HOOK, and r.f running HOOK.
: THROUGH-REST! ( str -- )
   {: d:^ du:n :}
   d du THROUGH-H$ s\" package FILE-OBJECTS-THROUGH public variable HOOK ;package\n" FS:WRITE-ALL
   d du THROUGH-M$ s\" package FILE-OBJECTS-THROUGH-M public\n: STORE ( -- ) FILE-OBJECTS-THROUGH-P:MAKE FILE-OBJECTS-THROUGH:HOOK xt! ;\nSTORE\n;package\n"
   FS:WRITE-ALL
   d du THROUGH-R$ s\" package FILE-OBJECTS-THROUGH-R\nCAST: XT ( n -- [ -- n ] )\npublic\nFILE-OBJECTS-THROUGH:HOOK @ XT execute constant Q\n;package\n"
   FS:WRITE-ALL ;

: THROUGH-BUILD ( str -- n )
   SEQ-DIR!  -1 SEEN-Q !
   [: s" app" RTARGET:HOST-TARGET
      [: SEQ-DIR$ THROUGH-H$ included  SEQ-DIR$ THROUGH-P$ included  SEQ-DIR$ THROUGH-M$ included
         SEQ-DIR$ THROUGH-R$ included  s" FILE-OBJECTS-THROUGH-R:Q" Q! ;]
      LINKER:WITH ;]
   catch RESULT ;

\ m.f stores into h.f's HOOK the quotation p.f's MAKE answers, which r.f runs: editing the quotation's literal in p.f
\ must compile r.f, whose Q then equals a cold build's. through-cold holds the edited sources, so its build is cold.
: BY-THROUGH ( -- )
   s" through" DIR$ FS:MAKE-DIR  s" through-cold" DIR$ FS:MAKE-DIR
   s" through" s" 42" THROUGH-P!  s" through" THROUGH-REST!
   s" through" THROUGH-BUILD {: one:n :}  SEEN-Q @ {: q1:n :}
   s" through" THROUGH-BUILD {: two:n :}  SEEN-Q @ {: q2:n :}
   s" through" s" 43" THROUGH-P!
   s" through" THROUGH-BUILD {: three:n :}  SEEN-Q @ {: q3:n :}
   s" through" THROUGH-BUILD {: four:n :}  SEEN-Q @ {: q4:n :}
   s" through-cold" s" 43" THROUGH-P!  s" through-cold" THROUGH-REST!
   s" through-cold" THROUGH-BUILD {: cold:n :}  SEEN-Q @ {: qc:n :}
   s" through: compiled " one two three four q1 q2 q3 q4 cold qc COLD-WARM. ;

\ ---- a warm build at a full dictionary ----
1024 constant RECORD-CAP
RECORD-CAP BUFFER: WARM-RECORD   variable WARM-RECORD-U

: FULL-A$ ( str -- str ) s" a.f" FIRST-BUF FILE-IN ;
: FULL-B$ ( str -- str ) s" b.f" SECOND-BUF FILE-IN ;

\ The directory's a.f, which creates words until the number of records the text spells is free, counted from DICT-CAP,
\ which a.f cannot spell, as a literal.
: FULL-A! ( str str -- )
   {: d:^ du:n free:^ freeu:n :}
   d du FULL-A$ s\" package FILE-OBJECTS-FULL\ncreate NB 15000 allot\n: ENTRY ( n n -- ) {: k:n at:n :}\n" FS:WRITE-ALL
   d du FULL-A$ s\"    15 0 do  s\" create F000000 \" str> drop i + c@  NB at 15 * + i + c!  loop\n" FS:APPEND-FILE
   d du FULL-A$ s\"    k  6 0 do  dup 10 mod 48 +  NB at 15 * 13 + i - + c!  10 /  loop drop ;\n" FS:APPEND-FILE
   d du FULL-A$ s\" : CHUNK ( n n -- ) {: first:n k:n :}\n   k 0 ?do  first i +  i ENTRY  loop\n" FS:APPEND-FILE
   d du FULL-A$ s\"    NB k 15 * >str evaluate-closed ;\n: FILL ( n -- )\n   " FS:APPEND-FILE
   d du FULL-A$ DICT-CAP FMT:INT$ FS:APPEND-FILE
   d du FULL-A$ s\"  ndict@ - swap - {: t:n :}\n   t 0 ?do  i  t i - 1000 min  CHUNK  1000 +loop ;\n" FS:APPEND-FILE
   d du FULL-A$ free freeu FS:APPEND-FILE
   d du FULL-A$ s\"  FILL\n;package\n" FS:APPEND-FILE ;

\ The directory's b.f, three words spelling no name of a.f's.
: FULL-B! ( str -- )
   {: d:^ du:n :}
   d du FULL-B$ s\" : FILE-OBJECTS-FULL-ONE ( -- n ) 1 ;\n: FILE-OBJECTS-FULL-TWO ( -- n ) 2 ;\n" FS:WRITE-ALL
   d du FULL-B$ s\" : FILE-OBJECTS-FULL-THREE ( -- n ) 3 ;\n" FS:APPEND-FILE ;

\ A build of the directory's a.f and b.f, the records it prints captured.
: FULL-BUILD ( str -- n )
   SEQ-DIR!
   REJECTION:QUIET @ {: q:n :}
   0 REJECTION:QUIET !  REJECTION:CAPTURE
   [: s" app" RTARGET:HOST-TARGET [: SEQ-DIR$ FULL-A$ included  SEQ-DIR$ FULL-B$ included ;] LINKER:WITH ;]
   catch RESULT
   REJECTION:BUFFER-OFF  q REJECTION:QUIET ! ;

\ Whether WARM-RECORD is the cold build's record, whose path names full-cold where the warm one's names full.
: SAME-RECORD? ( -- bool )
   REJECTION:BUFFER$ {: c:^ cu:n :}
   WARM-RECORD WARM-RECORD-U @ {: w:^ wu:n :}
   s" full-cold" DIR$ {: d:^ du:n :}
   c cu d du STR:FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N du + 5 - ENDOF
   ;MATCH {: k:n :}
   k 0< if false exit then
   wu cu 5 - <> if false exit then
   w k c k STR:= 0= if false exit then
   w k + wu k - c k + 5 + wu k - STR:= ;

\ a.f leaves eight records free, b.f links, and then two, fewer than b.f's three: b.f's object must not link past the
\ dictionary's end, so b.f compiles and is rejected as a cold build of the same sources is. A fourth build, after
\ that caught rejection, links a.f and is rejected the same way. full-cold holds them.
: BY-FULL ( -- )
   s" full" DIR$ FS:MAKE-DIR  s" full-cold" DIR$ FS:MAKE-DIR
   s" full" s" 8" FULL-A!  s" full" FULL-B!
   s" full" FULL-BUILD {: one:n :}
   s" full" FULL-BUILD {: two:n :}
   s" full" s" 2" FULL-A!
   s" full" FULL-BUILD {: three:n :}
   REJECTION:BUFFER$ RECORD-CAP min {: a:^ u:n :}
   a WARM-RECORD u BYTE-COPY  u WARM-RECORD-U !
   s" full" FULL-BUILD {: four:n :}
   s" full-cold" s" 2" FULL-A!  s" full-cold" FULL-B!
   s" full-cold" FULL-BUILD {: cold:n :}
   s" full: compiled " one two three RESULTS.  space four FMT:.INT  s"  cold " type cold FMT:.INT
   s"  same record " type  SAME-RECORD? bool>n FMT:.INT cr ;

\ ---- a piece that gains a store into reached data ----
: GAIN-H$ ( -- str ) s" gain" s" h.f" FIRST-BUF FILE-IN ;
: GAIN-S$ ( -- str ) s" gain" s" s.f" SECOND-BUF FILE-IN ;
: GAIN-R$ ( -- str ) s" gain" s" r.f" THIRD-BUF FILE-IN ;

: GAIN-BUILD ( -- n )
   [: s" app" RTARGET:HOST-TARGET [: GAIN-H$ included  GAIN-S$ included  GAIN-R$ included ;] LINKER:WITH ;]
   catch RESULT ;

\ A file that reached a piece's data depends on that piece's stores once it stores into data the file reads: r.f
\ reads h.f's SLOT and s.f's SEEN, and adding or dropping s.f's store into SLOT compiles s.f and r.f.
: BY-GAIN ( -- )
   s" gain" DIR$ FS:MAKE-DIR
   GAIN-H$ s\" package FILE-OBJECTS-GAIN public variable SLOT ;package\n" FS:WRITE-ALL
   GAIN-S$ s\" package FILE-OBJECTS-GAIN-S public variable SEEN ;package\n7 FILE-OBJECTS-GAIN-S:SEEN !\n" FS:WRITE-ALL
   GAIN-R$ s\" package FILE-OBJECTS-GAIN-R public FILE-OBJECTS-GAIN:SLOT @ constant A FILE-OBJECTS-GAIN-S:SEEN @ constant B ;package\n"
   FS:WRITE-ALL
   GAIN-BUILD {: one:n :}
   GAIN-S$ s\" package FILE-OBJECTS-GAIN-S public variable SEEN ;package\n7 FILE-OBJECTS-GAIN-S:SEEN !\n9 FILE-OBJECTS-GAIN:SLOT !\n"
   FS:WRITE-ALL
   GAIN-BUILD {: two:n :}
   GAIN-S$ s\" package FILE-OBJECTS-GAIN-S public variable SEEN ;package\n7 FILE-OBJECTS-GAIN-S:SEEN !\n" FS:WRITE-ALL
   GAIN-BUILD {: three:n :}
   GAIN-BUILD {: four:n :}
   s" gain: compiled " one two three RESULTS.  space four FMT:.INT cr ;

: STOPS ( -- )
   s" app" RTARGET:HOST-TARGET [: STOP-FILE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-BODY ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-INSIDE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-ALIGN ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-PAST ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-STORE ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-GAP ;] LINKER:WITH
   s" app" RTARGET:HOST-TARGET [: STOP-WORDLIST ;] LINKER:WITH
   MIXED EDITS TAILS SHARES LAIDS PRIVS PARTS TWINS ORDERS NONES BY-WORD BY-RUN
   BY-STORES BY-GAIN BY-OWN BY-THROUGH BY-FULL
   s" stopped" type cr ;

\ ---- facts mode: a body compiled against the words count.f and facts.f define ----

\ The label, then a case's caught code, on a line of its own.
: CODE. ( n ^u8 n -- ) {: rc:n a:^ u:n :}  a u type  rc FMT:.INT cr ;

: FACTS-AFTER ( -- n )
   [: s" : FOB-AFTER ( -- ) FILE-OBJECTS-COUNT:BUMP FILE-OBJECTS-COUNT:HI FILE-OBJECTS-COUNT:TALLY @ drop FILE-OBJECTS-LATE:ONE drop ;" evaluate-closed ;]
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

\ FOB-AFTER, once it compiles, is the program's entry.
\ The refusals come after the write: a refused definition fails the build (STOP-BODY).
: FACTS-BODY ( -- )
   s" test/fixtures/file-objects/count.f" included
   s" test/fixtures/file-objects/facts.f" included
   FACTS-AFTER {: after:n :}
   FACTS-WRAP
   after 0= if
      s" FOB-AFTER" ENTRY!
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
   SCRIPT-ARGC 0 = if s" build: the first argument is the kernel object" EXIT-STATUS:USAGE die then
   0 SCRIPT-ARGV$ LINKER:KERNEL
   STOP? if STOPS exit then
   FACTS? if FACTS exit then
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
