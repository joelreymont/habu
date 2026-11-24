split-str
\ catch-stale-suite.f - the cells a caught throw leaves stale.
\     bin/hb --load test/catch-stale-suite.f
\

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/adt/option.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/json.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/pcap-result.f

package CS-SHADOW
public
: 0= ( n -- bool ) drop true ;
: 0<> ( n -- bool ) drop false ;
;package

package CATCH-STALE-TEST

7311 constant E-CS-BOOM
public
DEFLINEAR CATCH-STALE-TEST:cslease
private
LINEAR: CS-UNLEASE ( CATCH-STALE-TEST:cslease -- n )
public

variable CS-TICK
: WBOOM ( ^u8 -- ) drop E-CS-BOOM throw ;
: WMAYBE ( ^u8 -- ^u8 ) dup c@ 0= IF E-CS-BOOM throw THEN ;
: WMAYBEN ( n -- n ) dup 0= IF E-CS-BOOM throw THEN ;
: WBOOL ( n -- bool ) dup 0 < IF E-CS-BOOM throw THEN 1 > ;
: WKEEP ( ^u8 -- ^u8 ) dup c@ drop ;
: CLEANMAYBE ( -- ) CS-TICK @ 0= IF E-CS-BOOM throw THEN ;
: FINBODY ( ^u8 -- ^u8 ) dup c@ drop ;
: FINW ( ^u8 -- ^u8 ) [: FINBODY ;] [: CLEANMAYBE ;] finally ;

: WMAYBE2 ( ^u8 -- ^u8 )
   dup c@ 0= IF E-CS-BOOM throw THEN
   dup c@ 65 = IF E-CS-BOOM throw THEN ;
: WPARTLY ( ^u8 -- ^u8 )
   dup c@ 0= IF E-CS-BOOM throw THEN
   dup c@ 65 = IF drop 5 E-CS-BOOM throw THEN ;
: WSWAPT ( n ^u8 -- n ^u8 ) swap E-CS-BOOM throw ;
: WBIND ( ^u8 -- ^u8 ) {: p :} p drop E-CS-BOOM throw ;
: WKEEPTOP ( ^u8 n -- ^u8 n )
   swap drop 5 swap E-CS-BOOM throw ;
: WNUMBER-NEW ( -- n )
   CS-TICK @ 0= if E-CS-BOOM throw then
   31 ;
: WNUMBER ( n -- n )
   drop WNUMBER-NEW ;
: KEEPN ( n -- n ) ;                     \ its output has no proof
: KEEPO ( option<n> -- option<n> ) ;
: SETO ( option<n> -- option<n> )
   drop 4 OPTION:SOME CS-TICK @ 0= IF E-CS-BOOM throw THEN ;
: READO ( option<n> -- n ) MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH ;

: OUTER-ZERO-QUOTE ( n -- n )
   drop
   [: 5 ['] WNUMBER catch dup 0= if drop 1+ else 2drop 0 then ;] execute
   E-CS-BOOM throw ;
: OUTER-PARTIAL-QUOTE ( ^u8 n -- ^u8 n )
   [: swap dup drop swap ['] WNUMBER catch
      dup 0= if drop 1+ else 2drop 0 then ;] catch drop
   E-CS-BOOM throw ;
: INNER-LATER ( n n -- n n )
   [: WNUMBER ;] catch {: code:n :}
   code 0<> if drop code throw then
   swap 91 throw ;
: CODE-REPLACE ( n -- n )
   CS-TICK @ 0= if E-CS-BOOM throw then
   drop 0 ;
: SWAP-CODES ( a a -- a a ) swap ;
: ORDINARY-ZERO ( -- n ) 0 ;
: ANY-ZERO? ( n -- bool ) drop true ;

: CS-CODE< ( str -- )
   REJECTION:CAPTURE  0 0= REJECTION:JSON !
   CHECK-CANDIDATE! 0 T= ;
: CS-CODE? ( str -- )
   REJECTION:BUFFER$ 2swap STR:CONTAINS? TTRUE ;
: CS-CODE-END ( -- ) 0 0= 0= REJECTION:JSON ! REJECTION:BUFFER-OFF ;
\ A body's stale read is the catch's, never the interpret row's unknown cell,
\ so its help keeps the value in a local before the catch.
: CS-STALE? ( -- )
   s\" \"code\":\"E-STALE-READ\"" CS-CODE?
   s\" \"help\":\"bind the value to a local before the `catch`" CS-CODE? ;
: CS-CARRIED? ( -- )
   s\" \"message\":\"the loop body leaves a carried cell different from the one it entered with\"" CS-CODE? ;

\ R1: the throw path swapped the two cells and `catch` put back only the depth.
: CS-SECTION-REPRODUCERS ( -- )
   s" reproducers" T-LABEL
   s" R1 ( n ^u8 -- n ^u8 n ) [: swap -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ `catch` restores the depth of both stacks, never their contents: a window cell
\ keeps its type only when every throw path of the body left it in place. Any
\ other is `stale<t>`: it may be moved, dropped or bound to an untyped local,
\ and a read is E-STALE-READ, named on the token that reads it, not on the
\ binding.
: CS-SECTION-READS ( -- )
   s" a stale cell is rejected where it is read" T-LABEL
   s" R3 ( ^u8 -- n ) [: WBOOM ;] catch {: v code:n :} v c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ arithmetic on the stale cell is named E-STALE-READ on that token, not a ptr
   \ u8 / n mismatch
   s" R4 ( ^u8 -- n ) [: WBOOM ;] catch {: v code:n :} v 1 +" CS-CODE<
   CS-STALE?  s\" \"token\":\"+\"" CS-CODE?  CS-CODE-END
   \ a typed local is itself the read
   s" R5 ( ^u8 -- n ) [: WBOOM ;] catch {: v:n code:n :} code" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ `0=` takes any one cell and still reads it
   s" R6 ( n -- bool ) {: k:n :} [: k WBOOL ;] catch {: rc:n :} 0=" CS-CODE<
   CS-STALE?  s\" \"token\":\"0=\"" CS-CODE?  CS-CODE-END
   s" R7 ( n -- bool ) {: k:n :} [: k WBOOL ;] catch {: rc:n :} rc 0<> if rc throw then 0="
      CHECK-QUIET-CANDIDATE! -1 T= ;

: CS-NP? ( -- )
   s\" \"code\":\"E-NONPARAMETRIC-EFFECT\"" CS-CODE? ;

\ A declared row a catch left stale is read only on the success arm of its code.
: CS-CATCH-FIX? ( -- )
   s" declare row `R` as the row the body leaves and read it only where the code is 0: after `code 0<> if code throw then`"
   CS-CODE? ;

\ A zero branch proves only the matching catch completed normally. Equal
\ numbers, another catch, and a predicate with the same effect cannot stand in
\ for its code. The reader overwrites its input on both real paths.
: CS-SECTION-PROOF ( -- )
   s" a read where the code is proven 0" T-LABEL
   s" P1 ( n n -- n ) [: WNUMBER ;] catch {: a ca:n :} [: WNUMBER ;] catch {: b cb:n :} cb 0= if a 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P2 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} 0 0= if v 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P3 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} ORDINARY-ZERO 0= if v 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P4 ( n -- n ) [: WNUMBER ;] catch {: code:n :} code 0<> if 1+ else drop 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P5 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: code:n :} code 0<> if CS-READ-BUNDLE else drop 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P6 ( n -- n ) [: WNUMBER ;] catch {: old code:n :} [: ;] catch {: later:n :} later 0= if old 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P7 ( n -- n ) [: WNUMBER ;] catch {: code:n :} code 0= if 1+ else then 1+" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P8 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} 1 1 = if code else 0 then 0= if v 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P9 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} code ANY-ZERO? if v 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P10 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} code CS-SHADOW:0= if v 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P11 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} code CS-SHADOW:0<> if 0 else v 1+ then" CS-CODE<
   CS-STALE? CS-CODE-END
   \ Iteration one may skip the read on failure. Iteration two must not treat the
   \ loop-carried ordinary zero as the original status, even after it was
   \ checked on the first pass. The changed back-edge proof is E-LOOP-CARRIED.
   s" P12 ( n -- ) [: WNUMBER ;] catch swap {: v :} 2 0 do dup 0= if v 1+ drop then drop 0 loop drop" CS-CODE<
   s\" \"code\":\"E-LOOP-CARRIED\"" CS-CODE? CS-CODE-END
   \ A loop entered under the status guard may carry the caught cell through a
   \ word that drops its normal proof, read in the body or not.
   s" A4 ( option<n> -- n ) [: SETO ;] catch {: code:n :} code 0= IF 3 BEGIN dup 0 > WHILE 1- swap KEEPO swap REPEAT drop READO ELSE drop 0 THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" N4 ( n -- n ) [: WNUMBER ;] catch {: code:n :} code 0= IF 3 BEGIN dup 0 > WHILE 1- swap KEEPN swap REPEAT drop 1+ ELSE drop 0 THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" N4D ( n -- n ) [: WNUMBER ;] catch {: code:n :} code 0= IF 3 0 DO KEEPN LOOP 1+ ELSE drop 0 THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" U1 ( option<n> -- n ) [: SETO ;] catch {: code:n :} code 0= IF 3 BEGIN dup 0 > WHILE 1- swap {: o :} o READO drop o KEEPO swap REPEAT drop READO ELSE drop 0 THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ A loop entered without the guard may not drop the proof: the join hands the
   \ entry's slot another catch's stale cell, which c1's guard would read as typed.
   s" P22 ( option<n> -- n ) [: SETO ;] catch {: c1:n :} 3 BEGIN dup 0 > WHILE 1- swap OPTION:NONE [: SETO ;] catch drop CS-TICK @ 0= IF swap THEN nip swap REPEAT drop c1 0= IF READO ELSE drop 0 THEN" CS-CODE<
   s\" \"token\":\"REPEAT\"" CS-CODE?  CS-CARRIED?  CS-CODE-END
   \ The same at a LEAVE edge: the swap's join leaves c1's slot c2's stale cell.
   s" P23 ( option<n> option<n> -- n ) swap [: SETO ;] catch {: c1:n :} swap [: SETO ;] catch {: c2:n :} 3 0 DO CS-TICK @ 0<> IF swap THEN LEAVE LOOP drop c1 0= IF READO ELSE drop 0 THEN" CS-CODE<
   s\" \"token\":\"LEAVE\"" CS-CODE?  CS-CARRIED?  CS-CODE-END
   \ Success of the inner catch does not retroactively preserve the original
   \ input on a later throw through the enclosing catch.
   s" P13 ( n n -- n ) [: INNER-LATER ;] catch {: outer:n :} drop 1+" CS-CODE<
   CS-STALE? CS-CODE-END
   \ B's failure can preserve A's code, while B's success replaces it with an
   \ ordinary zero. A zero B code cannot certify the old A result.
   s" P14 ( n -- n ) [: WNUMBER ;] catch swap {: old :} [: CODE-REPLACE ;] catch {: later:n :} later 0= if 0= if old 1+ else 0 then else drop 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   \ A declared polymorphic effect cannot identify which equal-typed status it returned.
   s" P15 ( n n -- n ) [: WNUMBER ;] catch {: a ca:n :} [: WNUMBER ;] catch {: b cb:n :} ca cb SWAP-CODES drop 0= if a 1+ else 0 then" CS-CODE<
   CS-STALE? CS-CODE-END
   \ Facts established in one CASE or MATCH sibling must not authorize another.
   s" P16 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} 0 case 0 of code 0= if v 1+ else 0 then endof 1 of v 1+ endof 0 swap endcase" CS-CODE<
   CS-STALE? CS-CODE-END
   s" P17 ( option<cspt> n -- n ) [: WNUMBER ;] catch {: v code:n :} MATCH option none OF code 0= if v 1+ else 0 then ENDOF some OF drop v 1+ ENDOF ;MATCH" CS-CODE<
   CS-STALE? CS-CODE-END
   \ An outer proof survives a neutral loop; an inner proof is spent per turn.
   s" P18 ( n -- n ) [: WNUMBER ;] catch {: v code:n :} code 0= if 2 0 do v 1+ drop loop v 1+ else 0 then"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" P19 ( n -- n ) 2 0 do 0 [: WNUMBER ;] catch {: v code:n :} code 0= if v 1+ drop then loop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ A zero-input quotation still exports a result derived from an internal
   \ guarded catch. Losing that result's lineage would forge an intact outer n.
   s" P20 ( n -- n ) ['] OUTER-ZERO-QUOTE catch drop 1+" CS-CODE<
   CS-STALE? CS-CODE-END ;

: CS-SECTION-PROOF-PRECISION ( -- )
   \ Two tests of one status mint equivalent facts on sibling paths. Their
   \ join must keep the successful branch's value proof.
   s" two equivalent status tests retain proof" T-LABEL
   s" PP1 ( n bool -- n ) swap [: WNUMBER ;] catch {: v code:n :} if code 0= else code 0= then if v 1+ else 0 then"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ The quotation only replaces its numeric output; the pointer below it
   \ remains intact when the enclosing word throws.
   s" normal output loss stays with its occurrence" T-LABEL
   s" PP2 ( ^u8 n -- n ) ['] OUTER-PARTIAL-QUOTE catch {: code:n :} drop c@"
      CHECK-QUIET-CANDIDATE! -1 T= ;

4096 constant CS-CHILD-CAP
create CS-CHILD-OUT CS-CHILD-CAP allot
create CS-CHILD-ERR CS-CHILD-CAP allot
create CS-PRODUCT-PATH PATH-CAP allot

: CS-HB$ ( -- str ) s" bin/hb" ;

: CS-PRODUCT$ ( -- str )
   SOURCE-ROOT:CWD$ CS-HB$ CS-PRODUCT-PATH FS:JOIN-PATH {: u:n :}
   CS-PRODUCT-PATH u ;

: CS-ENV! ( -- )
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" CS-PRODUCT$ PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ Definition control summaries survive a --repl build. The built hb runs the
\ guarded source as its MAIN, then loads fresh unsafe source, so stale and
\ zero-test semantics must still be enforced.
create CS-IMAGE-ROOT-BUF PATH-CAP allot
create CS-IMAGE-PATH-BUF PATH-CAP allot
variable CS-IMAGE-ROOT-U
variable CS-IMAGE-PATH-U
variable CS-CHILD-OUT-U
variable CS-CHILD-ERR-U

: CS-IMAGE-ROOT$ ( -- str ) CS-IMAGE-ROOT-BUF CS-IMAGE-ROOT-U @ ;
: CS-IMAGE$ ( -- str ) CS-IMAGE-PATH-BUF CS-IMAGE-PATH-U @ ;

: CS-IMAGE-PREPARE ( -- )
   s" habu-catch-success" FS:MKTEMP-DIR {: path:^ size:n :}
   path CS-IMAGE-ROOT-BUF size BYTE-COPY
   size CS-IMAGE-ROOT-U !
   CS-IMAGE-ROOT$ FS:CLEANUP-TREE+
   CS-IMAGE-ROOT$ s" built-hb" CS-IMAGE-PATH-BUF FS:JOIN-PATH
   CS-IMAGE-PATH-U ! ;

: CS-IMAGE-RC ( result<pcap:captured,pcap:failed> -- n )
   PCAP-RESULT:>N >r CS-CHILD-ERR-U ! CS-CHILD-OUT-U ! r> ;

: CS-IMAGE-BUILD ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+  s" tools/hb-build.f" PROC-ARGV+
   s" --" PROC-ARGV+  s" --repl" PROC-ARGV+
   s" test/catch-success-image-subject.f" PROC-ARGV+
   s" -o" PROC-ARGV+  CS-IMAGE$ PROC-ARGV+
   CS-ENV!
   CS-HB$
   s" "
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-CAPTURE CS-IMAGE-RC 0 T=
   CS-IMAGE$ FS:EXECUTABLE? TTRUE ;

: CS-IMAGE-RUN ( -- )
   PROC-ARGV-RESET
   CS-ENV!
   CS-IMAGE$
   S\" s\" test/catch-success-image-consumer.f\" included\n"
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-CAPTURE CS-IMAGE-RC 0 T=
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image: ok" STR:CONTAINS? TTRUE
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image-fresh: ok" STR:CONTAINS? TTRUE ;

: CS-SAVED-REFUSAL ( str str -- )
   {: src:^ size:n want:^ wantu:n :}
   PROC-ARGV-RESET
   CS-ENV!
   CS-IMAGE$ src size
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-CAPTURE CS-IMAGE-RC 70 T=
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image-load: began" STR:CONTAINS? TTRUE
   CS-CHILD-ERR CS-CHILD-ERR-U @ s" error[E-STALE-READ]: stale cell" STR:CONTAINS? TTRUE
   CS-CHILD-ERR CS-CHILD-ERR-U @ want wantu STR:CONTAINS? TTRUE ;

: CS-IMAGE-BODY ( -- )
   CS-IMAGE-BUILD
   CS-IMAGE-RUN
   S\" s\" catch-image-load: began\" type cr\ns\" test/catch-success-wrong-status.f\" included\n"
      s" catch-success-wrong-status.f:11:18" CS-SAVED-REFUSAL
   S\" s\" catch-image-load: began\" type cr\ns\" test/catch-success-override.f\" included\n"
      s" catch-success-override.f:12:24" CS-SAVED-REFUSAL ;

: CS-SECTION-IMAGE ( -- )
   s" a built hb executes a guarded catch and still refuses forged proof" T-LABEL
   FS:CLEANUP-RESET
   CS-IMAGE-PREPARE
   [: CS-IMAGE-BODY ;] catch {: code:n :}
   FS:CLEANUP-RUN
   code 0<> if code throw then ;

\ A callee that throws counts as overwriting every input it declared unless its
\ own body proved otherwise: the checker records, per definition, which declared
\ inputs every throw path left in place, and a call takes the edge with those
\ cells kept. WMAYBE only reads its input, so R6 certifies.
: CS-SECTION-EDGES ( -- )
   s" throw edges" T-LABEL
   s" R6 ( ^u8 -- ^u8 n ) [: WMAYBE ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a `finally` cleanup that throws is a throw edge of the word that runs it
   s" R7 ( ^u8 -- ^u8 n ) [: FINW ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a join: the loop needs the cell's type on the back edge, and WMAYBE's
   \ evidence is what carries it there (with WBOOM in its place this is refused,
   \ and no refinement ever un-stales a cell)
   s" R8 ( ^u8 -- ^u8 ) begin [: WMAYBE ;] catch 0= until" CHECK-QUIET-CANDIDATE! -1 T=
   s" R8B ( ^u8 -- ^u8 ) begin [: WBOOM CS-BUF ;] catch 0= until" CS-CODE<
   CS-CODE-END
   \ A quotation literal infers its window on a fresh row, so at the edge the
   \ window cell is still an unbound variable and a rewrite with a value of the
   \ same type is not evidence that the cell survived: only identity of the term
   \ is, and identity neither binds the quotation (which would make it
   \ monomorphic where it is merely executed) nor keeps a transient row id.
   \ The repair binds the cell to a local before the catch, or pushes the
   \ literal after it.
   s" R9 ( n -- n n ) [: drop 5 -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ The evidence is per declared input, folded with AND over every throw path of
\ the callee's body and measured as a quotation's is: the cell at that position
\ is still the same term at the edge. A cell that went into a locals frame is
\ gone from the row (the frame owns a fresh base), a cell the body swapped or
\ replaced is a different term, and one arm that overwrites clears the bit for
\ every path.
: CS-SECTION-CALLEES ( -- )
   s" callees" T-LABEL
   \ both throw paths leave the input where it was
   s" C1 ( ^u8 -- ^u8 n ) [: WMAYBE2 ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ one of the two overwrote it
   s" C2 ( ^u8 -- ^u8 n ) [: WPARTLY ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ R1's body as a callee: the throw path swapped the two cells
   s" C3 ( n ^u8 -- n ^u8 n ) [: WSWAPT ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ the input was bound to a local: the cells the callee was given are spent
   s" C4 ( ^u8 -- ^u8 n ) [: WBIND ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ per cell, not per word: WKEEPTOP keeps the top of its window and not the
   \ cell below it, so the `n` comes back typed ...
   s" C6 ( ^u8 n -- n ) [: WKEEPTOP ;] catch {: code:n :} nip"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and reading the cell below it is E-STALE-READ on the token that reads
   s" C7 ( ^u8 n -- n ) [: WKEEPTOP ;] catch {: code:n :} drop c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END ;

: CS-SECTION-ADMITS ( -- )
   s" admitted bodies" T-LABEL
   \ window 0: a body that takes nothing has nothing to lose
   s" A1 ( -- n ) [: -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A2 ( -- n ) [: 1 2 3 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A3 ( -- n ) [: 1 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the throw path left the window cell exactly where it was
   s" A4 ( ^u8 -- ^u8 n ) [: dup c@ drop -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a body with no throw edge records nothing and stales nothing
   s" A5 ( ^u8 -- ^u8 n ) [: WKEEP ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a stale cell may be bound to an untyped local and never read
   s" A6 ( ^u8 -- ) [: WBOOM ;] catch {: v code:n :}" CHECK-QUIET-CANDIDATE! -1 T=
   \ and it may be moved and dropped
   s" A7 ( ^u8 ^u8 -- ) [: WBOOM ;] catch drop swap 2drop" CHECK-QUIET-CANDIDATE! -1 T= ;

\ A multicell bundle is one stale value: `catch` wraps each of a layout value's
\ W hidden cells, and every question that finds the group sees through the
\ wrapper, so an `option<cspt>` window comes back as one `stale<option<cspt>>`
\ that one `drop` removes and one `nip` moves; every typed use (word input,
\ typed local, MATCH) is E-STALE-READ.
STRUCTURE cspt
   FIELD x n
   FIELD y n
;STRUCTURE

: CS-PT ( -- cspt ) 17 25 CATCH--STALE--TEST-CSPT:MAKE ;
: CS-SOME ( -- option<cspt> ) CS-PT OPTION:SOME ;
: CS-BUNDLE-THROW ( option<cspt> -- option<cspt> )
   drop CS-SOME E-CS-BOOM throw ;
: CS-BUNDLE-MAYBE ( option<cspt> -- option<cspt> )
   drop CS-SOME CS-TICK @ 0= IF E-CS-BOOM throw THEN ;
: CS-READ-BUNDLE ( option<cspt> -- n )               \ a declared logical input: a read
   MATCH option
      none OF 0 ENDOF
      some OF CATCH--STALE--TEST-CSPT:UNMAKE + ENDOF
   ;MATCH ;
: CS-BUNDLE-RT ( cspt option<cspt> -- n )
   [: CS-BUNDLE-THROW ;] catch {: rc:n :}
   drop drop rc ;
: CS-BUNDLE-GUARD ( option<cspt> -- n )
   [: CS-BUNDLE-MAYBE ;] catch {: o code:n :}
   code 0= IF o CS-READ-BUNDLE ELSE 0 THEN ;
: CS-NUM-THROW ( option<n> -- option<n> )
   drop 4 OPTION:SOME E-CS-BOOM throw ;
\ An option a generic constructor built inline: the call learns the width
\ (`a` := n gives two cells), so the stale group is two cells one drop removes.
: CS-INLINE-RT ( -- n )
   depth {: d:n :}
   5 OPTION:SOME [: CS-NUM-THROW ;] catch drop drop
   depth d - ;

: CS-SECTION-BUNDLES ( -- )
   s" bundles" T-LABEL
   \ the stale bundle is dropped by one drop, the cspt below it by the other
   s" B1 ( cspt option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} drop drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ passing it to a word that declares the logical type reads it, and the
   \ diagnostic names the value, not a hidden field
   s" B2 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} CS-READ-BUNDLE" CS-CODE<
   CS-STALE?  s" stale<option<cspt>>" CS-CODE?  CS-CODE-END
   \ a typed local is the same read
   s" B3 ( option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: o:option<cspt> rc:n :}" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ and so is a MATCH on the bundle
   s" B4 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} MATCH option none OF 0 ENDOF some OF CATCH--STALE--TEST-CSPT:UNMAKE + ENDOF ;MATCH"
      CS-CODE<  CS-STALE?  CS-CODE-END
   \ an untyped local holds the whole bundle and gives it back stale in every
   \ cell: it still drops in one, and it still refuses to be read
   s" B5 ( option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: o rc:n :} o drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B6 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: o rc:n :} o CS-READ-BUNDLE" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ transport moves the group whole: `nip` takes the scalar out from under it
   s" B7 ( n option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} nip drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ The entire captured group gets the normal view under its own status guard.
   s" B8 ( option<cspt> -- n ) [: CS-BUNDLE-MAYBE ;] catch {: o code:n :} code 0= if o CS-READ-BUNDLE else 0 then"
      CHECK-QUIET-CANDIDATE! -1 T=
   1 CS-TICK ! CS-SOME CS-BUNDLE-GUARD 42 T=
   0 CS-TICK ! CS-SOME CS-BUNDLE-GUARD 0 T=
   \ B1's shape, run: the drops agree with the checker on the bundle's width
   CS-PT CS-SOME CS-BUNDLE-RT E-CS-BOOM T=
   \ an option a generic constructor built inline refuses to be read
   s" B10 ( -- n ) 5 OPTION:SOME [: CS-NUM-THROW ;] catch drop MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH"
      CS-CODE<  CS-STALE?  CS-CODE-END
   CS-INLINE-RT 0 T= ;

\ Kept inputs map onto the caller by value: an option is one term at open
\ width, two as option<n>; CS-OPEN-KEEP keeps its deepest.
: CS-OPEN-KEEP ( n n option<b> [ n option<b> -- option<b> ] -- n n option<b> [ n option<b> -- option<b> ] )
   execute E-CS-BOOM throw ;
: CS-WIDE-KEEP ( n n option<n> [ n option<n> -- option<n> ] -- n n option<n> [ n option<n> -- option<n> ] )
   CS-OPEN-KEEP ;
: CS-REPL-OPT ( n option<n> -- option<n> ) drop drop 77 OPTION:SOME ;
: CS-EXP-KEEP ( n bool option<n> -- n bool option<n> ) swap drop 55 swap E-CS-BOOM throw ;
: CS-OPT-ID ( option<a> -- option<a> ) ;     \ leaves the option's width open

: CS-SECTION-WIDTHS ( -- )
   s" widths" T-LABEL
   \ the cell above the deepest was consumed: reading it is refused ...
   s" W1 ( -- n ) 1 2 9 OPTION:SOME ['] CS-REPL-OPT ['] CS-WIDE-KEEP catch drop drop {: o :} nip"
      CS-CODE<  CS-STALE?  CS-CODE-END
   \ ... and the deepest is intact
   s" W2 ( -- n ) 1 2 9 OPTION:SOME ['] CS-REPL-OPT ['] CS-WIDE-KEEP catch drop drop {: o :} drop 1+"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ the window holds the option as one open cell: the bool is stale ...
   s" W3 ( -- n ) 1 false 9 OPTION:SOME [: CS-OPT-ID CS-EXP-KEEP ;] catch drop {: o :} IF 1 ELSE 0 THEN nip"
      CS-CODE<  CS-STALE?  s\" \"token\":\"IF\"" CS-CODE?  CS-CODE-END
   \ ... and the n below it is intact
   s" W4 ( -- n ) 1 false 9 OPTION:SOME [: CS-OPT-ID CS-EXP-KEEP ;] catch drop {: o :} drop 1+"
      CHECK-QUIET-CANDIDATE! -1 T= ;

\ A4's shape, run: the caught body reads the address and throws, and the catch
\ hands back the SAME address with the throw code on top.
16 BUFFER: CS-BUF
: CS-RT ( ^u8 -- str ) [: dup c@ drop E-CS-BOOM throw ;] catch ;

: CS-CRT ( ^u8 -- str ) [: WMAYBE ;] catch ;

\ A body that returns 9 for selector 0 and throws a computed 0 for selector 1:
\ the catch answers E-THROW-ZERO for it, so the success arm reads no cell the
\ throw left.
: CS-ZBODY ( n n -- n n )
   {: p:n c:n :}
   c 1 = if 1 1 - throw then
   9 c ;
: CS-ZERO ( n -- n )
   {: sel:n :}
   5 sel ['] CS-ZBODY catch {: v c code:n :}
   code 0= if v exit then
   code ;

\ A body that changes the stack over the same tail is applied: each cell it
\ leaves reads only where its catch's code is 0, and it may not take a cell
\ that may be linear, which a throw would lose.
: CS-SPEND ( CATCH-STALE-TEST:cslease -- n ) CS-UNLEASE WMAYBEN ;

: CS-SECTION-RESHAPE ( -- )
   s" a catch that changes the stack" T-LABEL
   s" S1 ( str -- n ) [: JSON:PARSE ;] catch nip" CHECK-QUIET-CANDIDATE! -1 T=
   s" S2 ( str -- n ) [: JSON:PARSE ;] catch drop JSON:TEXT$ nip" CS-CODE<
   CS-STALE?  CS-CODE-END
   s" S3 ( str -- n ) [: JSON:PARSE ;] catch {: code:n :} code 0<> if code throw then JSON:TEXT$ nip"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ more, fewer and as many cells as it took, read under the guard
   s" S4 ( n -- n n ) [: WNUMBER dup ;] catch {: code:n :} code 0<> if code throw then"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" S5 ( n n -- n ) [: + WMAYBEN ;] catch {: code:n :} code 0<> if code throw then"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" S6 ( ^u8 -- n ) [: c@ WMAYBEN ;] catch {: code:n :} code 0<> if code throw then"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" S7 ( CATCH-STALE-TEST:cslease -- n n ) [: CS-SPEND ;] catch" CS-CODE<
   s\" \"code\":\"E-LINEAR-DROP\"" CS-CODE?
   s" open the handle inside the caught body" CS-CODE?  CS-CODE-END
   \ at top level the engine's catch gives back the depth the body took
   REJECTION:CAPTURE  0 0= REJECTION:JSON !
   s" ' CATCH-STALE-TEST:WNUMBER-NEW catch 2drop" TEST-EVAL:RC 70 T=
   s\" \"code\":\"E-LAYOUT-VALUE\"" CS-CODE?
   s" move the catch inside a definition" CS-CODE?  CS-CODE-END ;

: CS-SECTION-RUNTIME ( -- )
   s" runtime" T-LABEL
   CS-BUF CS-RT {: p code:n :}
   code E-CS-BOOM T=
   p CS-BUF = TTRUE
   \ WMAYBE reads the zero byte and throws without touching the address: the
   \ catch hands back the address the caller pushed, which is what its evidence
   \ promised the checker
   CS-BUF CS-CRT {: q code2:n :}
   code2 E-CS-BOOM T=
   q CS-BUF = TTRUE
   0 CS-ZERO 9 T=
   1 CS-ZERO E-THROW-ZERO T= ;

\ `['] W` pushes a term of W's declared effect carrying the edge W's own
\ definition proved, as the `[: W ;]` literal's term does. A recorded bit counts
\ from the top of W's declared input row, which is the pushed term's window; T4
\ and T5 are the two halves of that orientation. A word with no throw edge
\ records nothing and its tick carries nothing. A term made from a declared
\ type, and a `defer`'s tick, may throw, so a catch of the typed `xt<effect>`
\ cell (T9) or of the quotation parameter (T10) is rejected as the direct tick
\ of a thrower (T3) is.
\ A tick bound to a local in the same body is the term the tick pushed and
\ keeps its edge (T7, T8).
: WREPL ( ^u8 -- ^u8 ) drop CS-BUF E-CS-BOOM throw ;
TYPED-VARIABLE CS-XT [ ^u8 -- ^u8 ]
: CS-TRT ( ^u8 -- str ) ['] WMAYBE catch ;

: CS-SECTION-TICK ( -- )
   s" ticked callees" T-LABEL
   \ R1's twin with the body as a callee: the tick carries its evidence
   s" T1 ( n ^u8 -- n ^u8 n ) ['] WSWAPT catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ R6's twin: the callee that only reads its input keeps the address typed
   s" T2 ( ^u8 -- ^u8 n ) ['] WMAYBE catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the callee that replaces it does not, and the refusal names the read
   s" T3 ( ^u8 -- n ) ['] WREPL catch {: v code:n :} v c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ per cell, as C6/C7: WKEEPTOP keeps the top of its window ...
   s" T4 ( ^u8 n -- n ) ['] WKEEPTOP catch {: code:n :} nip"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and the cell below it is stale, named on the token that reads it
   s" T5 ( ^u8 n -- n ) ['] WKEEPTOP catch {: code:n :} drop c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ C4's twin: a callee that bound its input to a local proved nothing
   s" T6 ( ^u8 -- ^u8 n ) ['] WBIND catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ the local-bound tick: the same term, so the same edge
   s" T7 ( ^u8 -- ^u8 n ) ['] WREPL {: x :} x catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   s" T8 ( ^u8 -- ^u8 n ) ['] WMAYBE {: x :} x catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the cell read and the parameter are terms of the declared xt type, which
   \ may throw, so the cell each catch keeps is stale
   s" T9 ( ^u8 -- n ) ['] WREPL CS-XT ! CS-XT @ catch {: v code:n :} v c@"
      CS-CODE<  CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   s" T10 ( ^u8 [ ^u8 -- ^u8 ] -- ^u8 n ) catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a callee that never returns is applied by its declared output row: WBOOM
   \ leaves nothing where its window was, so the bind reaches below the inputs;
   \ only a `[: … ;]` literal that never returns keeps its window, never a tick
   s" T11 ( ^u8 -- n ) ['] WBOOM catch {: v code:n :} code" CS-CODE<
   s\" \"code\":\"E-STALE-READ\"" REJECTION:BUFFER$ 2swap STR:CONTAINS? TFALSE
   CS-CODE-END
   \ and the intact cell is honest at run time through the tick as well: WMAYBE
   \ reads the byte and throws, and the catch hands back the address pushed here
   CS-BUF CS-TRT {: r code3:n :}
   code3 E-CS-BOOM T=
   r CS-BUF = TTRUE ;

\ A caught body that reaches the open row below its window leaves it stale<R>,
\ named by the declared row; a helper returns that row only on the success arm.
: CS-EXEC ( R [ R -- R ] -- R [ R -- R ] ) dup >r execute r> ;
: CS-TIE ( R [ R -- R ] -- R [ R -- R ] ) ;
: CS-TAKE ( R [ R -- R ] -- R ) execute ;

: CS-SECTION-OPEN ( -- )
   s" open rows" T-LABEL
   \ a quotation parameter over the row keeps no cell of R intact, so returning
   \ R is a stale read
   s" O1 ( R [ R -- R ] -- R n ) catch" CS-CODE<
   s" stale<R> n" CS-CODE?  s\" \"code\":\"E-STALE-READ\"" CS-CODE?
   s" returns cells `catch` left stale: a throw path of the caught body may have overwritten them" CS-CODE?
   s\" \"help\":\"return them where the code is 0: after `code 0<> if code throw then`" CS-CODE?
   CS-CODE-END
   s" D1 ( R [ R -- R ] -- n ) catch" CS-CODE<
   CS-NP?  s" `R` and the unnamed row" CS-CODE?  CS-CATCH-FIX?  CS-CODE-END
   s" D2 ( R [ R -- R ] -- S n ) catch" CS-CODE<
   CS-NP?  s" `R` and row `S`" CS-CODE?  CS-CATCH-FIX?  CS-CODE-END
   \ a literal that runs the parameter reaches R alike
   s" O2 ( R [ R -- R ] -- R n ) {: q :} [: q execute ;] catch" CS-CODE<
   s" stale<R> n" CS-CODE?  CS-CODE-END
   \ a literal whose window holds a quotation over the caller's row reaches it
   s" O3 ( R [ R -- R ] -- R ) [: CS-EXEC ;] catch {: code:n :} drop" CS-CODE<
   s" stale<R>" CS-CODE?  CS-CODE-END
   \ the expected side then shows its row too, and a stale row is no empty stack
   s" O11 ( R n [ R n -- R n ] -- R n n ) catch" CS-CODE<
   s\" \"expected\":\"R n n\",\"found\":\"stale<R> stale<n> n\"" CS-CODE?  CS-CODE-END
   s" O12 ( R [ R n -- R n ] -- R ) [: 5 swap [: [: CS-EXEC ;] catch drop drop ;] execute ;] execute"
      CS-CODE<
   s\" \"label\":\"the stack holds stale<R> after this\"" CS-CODE?  CS-CODE-END
   \ the inferred effect names its rows by its own rule, never by the pair's
   s" O13 ( R [ R -- R ] -- R ) dup >r catch drop r> execute" CS-CODE<
   s\" \"inferred\":\"R [ R -- R ] -- R\"" CS-CODE?  CS-CODE-END
   \ a quotation on a side of the pair shows the pair's row by its letter too
   s" O14 ( R [ R -- R ] -- R [ R -- R ] ) catch drop" CS-CODE<
   s\" \"expected\":\"R [ R -- R ]\"" CS-CODE?  CS-CODE-END
   \ a stale row may be moved; taking a cell from it is a read
   s" O4 ( R [ R -- R ] -- R ) catch drop drop" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a step that takes the stale row whole reads the row
   s" O15 ( R [ R -- R ] -- R ) dup >r catch drop r> CS-TAKE" CS-CODE<
   s\" \"code\":\"E-STALE-READ\"" CS-CODE?
   s" stale row: `CS-TAKE` takes cells `catch` left stale: a throw path of the caught body may have overwritten them" CS-CODE?
   s\" \"help\":\"pass them on where the code is 0: after `code 0<> if code throw then`" CS-CODE?
   CS-CODE-END
   \ a quotation on a side of the pair shows a row of its own by its cells alone
   s" O16 ( R [ R -- R ] -- R [ n -- n ] ) catch drop [: 1+ ;]" CS-CODE<
   s\" \"expected\":\"R [ n -- n ]\",\"found\":\"stale<R> [ n -- n ]\"" CS-CODE?
   CS-CODE-END
   \ a row cycle's notes name a stale pair's row once
   s" O17 ( R [ R -- R ] -- R ) dup >r catch drop r> dup execute" CS-CODE<
   s\" \"expected\":\"R\",\"found\":\"stale<R> [ R -- R ]\"" CS-CODE?
   CS-CODE-END
   \ a stale tail in a quotation on a side of the pair is named once, by stale<R>
   s" O18 ( R [ R -- R ] [ S -- S ] -- R [ S -- S ] ) {: g :} catch drop g catch drop g" CS-CODE<
   s\" \"expected\":\"R [ stale<R> -- stale<R> ]\",\"found\":\"stale<R> [ stale<R> -- stale<R> ]\"" CS-CODE?
   CS-CODE-END
   \ the success arm of the exact code gives the row back, past a join
   s" O5 ( R [ R -- R ] -- R ) catch {: code:n :} code 0<> if code throw then"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" O6 ( R [ R -- R ] -- R ) [: CS-EXEC ;] catch {: code:n :} drop code 0= if exit then code throw"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" O7 ( R [ R -- R ] -- R ) catch {: code:n :} 7 KEEPN code 0= if 1+ else 2 + then drop code 0<> if code throw then"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ each run binds a quotation's row to a row ending in the last catch's
   \ stale row: every stale row is named by the row under all of them
   s" O9 ( R [ R -- R ] [ S -- S ] [ T -- T ] -- R ) {: g h :} catch drop 5 [: g catch ;] execute 6 [: h catch ;] execute"
      CS-CODE<
   s" [ stale<R> n n -- stale<R> n n ] -- stale<R> n" CS-CODE?  CS-CODE-END
   \ a body over a row of its own cannot reach the caller's
   s" O8 ( R ^u8 -- R ^u8 n ) [: WMAYBE ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ nor through a quotation over the caller's row below its window
   s" BW1 ( R [ R -- R ] n -- R [ R -- R ] n n ) [: WMAYBEN ;] catch"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ a nested literal whose window holds a quotation over the caller's row
   s" O9 ( R [ R -- R ] -- R ) [: [: CS-EXEC ;] catch drop drop ;] execute" CS-CODE<
   s" stale<R>" CS-CODE?  CS-CODE-END
   \ a stale row held only in a cell's type stays one across a literal's run
   s" O10 ( R [ R -- R ] -- R [ R -- R ] ) [: [: CS-EXEC ;] catch >r [: ;] CS-TIE r> dup 0<> if throw then drop ;] execute"
      CS-CODE<
   s\" \"code\":\"E-MISMATCH\"" CS-CODE?  s" [ stale<R> stale<[ -- ]> --" CS-CODE?  CS-CODE-END ;

: RUN ( -- )
   T-RESET
   CS-SECTION-REPRODUCERS
   CS-SECTION-READS
   CS-SECTION-PROOF
   CS-SECTION-PROOF-PRECISION
   CS-SECTION-EDGES
   CS-SECTION-CALLEES
   CS-SECTION-ADMITS
   CS-SECTION-BUNDLES
   CS-SECTION-WIDTHS
   CS-SECTION-RESHAPE
   CS-SECTION-RUNTIME
   CS-SECTION-TICK
   CS-SECTION-OPEN
   CS-SECTION-IMAGE
   T-REPORT ;

RUN

;package
