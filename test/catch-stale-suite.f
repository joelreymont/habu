\ catch-stale-suite.f - the cells a caught throw leaves stale
\ (dot habu-preserve-exceptional-stack-89902bde).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/checker-assert.f
\   test/catch-stale-suite.f
\
\ `catch` restores the DEPTH of both stacks and never their CONTENTS, so a cell
\ in the caught quotation's window - its declared fixed input prefix - is only
\ still worth its input type when EVERY throw path of that body provably left it
\ where it was. Every other window cell is `stale<t>` afterwards: it may be
\ moved, dropped or bound to an untyped local, and any READ of it is
\ E-STALE-READ, named on the token that reads it.
\
\ Every refused row below was ACCEPTED by the engine before this rule
\ (3da80b23) except R4 and R5, which it refused as an ordinary type mismatch
\ without naming the reason; each row says which. That engine accepted the
\ CERTIFIED rows too - R6, R8, C1, C6 and the runtime ones - for want of any
\ stale rule to apply, so only the refusal rows failed on it. The file passes on
\ the engine this rule and the callee evidence below are built into.
\
\ The exceptional edge is not part of a quotation's TYPE, so it travels on a
\ TERM: a quotation literal's, and the one `['] W` pushes, which takes W's own
\ edge with W's own evidence ("the tick route" below, where R1's twin
\ `['] WSWAPT catch` is refused for R1's reason). The routes this rule still
\ does NOT cover are the ones whose term is a fresh instance of a DECLARED
\ type - `catch` of a quotation parameter, of a typed `xt<effect>` cell and of
\ a `defer` - and they keep the window typed even when the target throws.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/checker-assert.f

package CS-SHADOW
public
: 0= ( n -- bool ) drop true ;
: 0<> ( n -- bool ) drop false ;
;package

package CATCH-STALE-TEST

7311 constant E-CS-BOOM
public

\ ---- fixtures the candidate sources below call ------------------------------
variable CS-TICK
: WBOOM ( ptr u8 -- ) drop E-CS-BOOM throw ;                     \ overwrites its input, then throws
: WMAYBE ( ptr u8 -- ptr u8 ) dup c@ 0= IF E-CS-BOOM throw THEN ; \ reads the cell, never writes it
: WKEEP ( ptr u8 -- ptr u8 ) dup c@ drop ;                       \ no throw edge at all
: CLEANMAYBE ( -- ) CS-TICK @ 0= IF E-CS-BOOM throw THEN ;       \ a cleanup that may throw
: FINBODY ( ptr u8 -- ptr u8 ) dup c@ drop ;
: FINW ( ptr u8 -- ptr u8 ) [: FINBODY ;] [: CLEANMAYBE ;] finally ;

\ ---- callees whose own throw paths say what they did with their inputs ------
: WMAYBE2 ( ptr u8 -- ptr u8 )            \ two throw paths, both leave the cell
   dup c@ 0= IF E-CS-BOOM throw THEN
   dup c@ 65 = IF E-CS-BOOM throw THEN ;
: WPARTLY ( ptr u8 -- ptr u8 )            \ two throw paths, one of them overwrote it first
   dup c@ 0= IF E-CS-BOOM throw THEN
   dup c@ 65 = IF drop 5 E-CS-BOOM throw THEN ;
: WSWAPT ( n ptr u8 -- n ptr u8 ) swap E-CS-BOOM throw ;   \ R1's body as a callee
: WBIND ( ptr u8 -- ptr u8 ) {: p :} p drop E-CS-BOOM throw ; \ the input went into the frame
: WKEEPTOP ( ptr u8 n -- ptr u8 n )       \ keeps the top cell, replaces the one below it
   swap drop 5 swap E-CS-BOOM throw ;
: WRSWAP ( n | ptr u8 -- n | ptr u8 ) r> swap >r E-CS-BOOM throw ;  \ R2's body as a callee
: WNUMBER-NEW ( -- n )
   CS-TICK @ 0= if E-CS-BOOM throw then
   31 ;
: WNUMBER ( n -- n )
   drop WNUMBER-NEW ;

\ A caught callee can replace a return-window value before either outcome.
\ Its successful replacement is read only after its own status is proved.
83 constant E-CS-RETURN
variable CS-RETURN-FAIL
create CS-RETURN-OLD 1 allot
create CS-RETURN-NEW 1 allot
: CS-MAKE-RETURN ( -- n )
   CS-RETURN-FAIL @ 0<> if E-CS-RETURN throw then
   66 ;
: CS-RETURN-READER ( n | ptr u8 -- n | ptr u8 )
   r> drop CS-RETURN-NEW >r
   drop CS-MAKE-RETURN ;
: CS-RETURN-GUARD ( n -- ptr u8 )
   dup 0< if 1 else 0 then CS-RETURN-FAIL !
   CS-RETURN-OLD >r [: CS-RETURN-READER ;] catch {: code:n :}
   code 0<> if drop r> drop code throw then
   drop r> ;
: CS-RETURN-NEW? ( ptr u8 -- bool ) CS-RETURN-NEW = ;
: OUTER-ZERO-QUOTE ( n -- n )
   drop
   [: 5 ['] WNUMBER catch dup 0= if drop 1+ else 2drop 0 then ;] execute
   E-CS-BOOM throw ;
: OUTER-PARTIAL-QUOTE ( ptr u8 n -- ptr u8 n )
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

\ Checker-internal readers probed at top level go through named trusted shims
\ (the type-export-suite boundary): CTL-MASKS is the store's own answer for a
\ word, XFER-PACK the one cell those facts travel in.
TRUSTED: CS-CTL-MASKS ( ptr u8 n -- n n ) CTL-MASKS ;
TRUSTED: CS-XPACK ( n n n -- n ) XFER-PACK ;
TRUSTED: CS-XFLAGS ( n -- n ) XFER-FLAGS ;
TRUSTED: CS-XDMASK ( n -- n ) XFER-DMASK ;
TRUSTED: CS-XRMASK ( n -- n ) XFER-RMASK ;

\ ---- the refusal shape: a candidate with JSON diagnostics captured ----------
create CS-DBUF 8192 allot
: CS-CODE< ( ptr u8 n -- )   \ check a candidate with JSON diags captured; assert reject
   CS-DBUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: CS-CODE? ( ptr u8 n -- )   \ assert the captured diagnostic names this text
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;
: CS-CODE-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;
: CS-STALE? ( -- ) s\" \"code\":\"E-STALE-READ\"" CS-CODE? ;

\ ---- the two reproducers from the dot ---------------------------------------
\ SWAP-THROW returned (address, 17, code) where its signature said (n, ptr u8,
\ n): the throw path had swapped the two cells and `catch` put back only the
\ depth. RETURN-THROW is the same defect on the return stack.
: CS-SECTION-REPRODUCERS ( -- )
   s" R1 ( n ptr u8 -- n ptr u8 n ) [: swap -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   s" R2 ( n | ptr u8 -- n n | ptr u8 ) [: r> swap >r -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ ---- reading a stale cell ----------------------------------------------------
\ The refusal is named on the token that reads the cell, not on the binding: an
\ untyped local may hold a stale cell.
: CS-SECTION-READS ( -- )
   s" R3 ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ arithmetic on the same cell was already refused as a ptr u8 / n mismatch;
   \ now it is named for the reason it is one, on the same token
   s" R4 ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v 1 +" CS-CODE<
   CS-STALE?  s\" \"token\":\"+\"" CS-CODE?  CS-CODE-END
   \ a TYPED local is itself the read (also refused before this rule, as the
   \ same ordinary mismatch)
   s" R5 ( ptr u8 -- n ) [: WBOOM ;] catch {: v:n code:n :} code" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ A zero branch proves only the matching catch completed normally. Equal
\ numbers, another catch, and a predicate with the same effect cannot stand in
\ for its code. The reader overwrites its input on both real paths.
: CS-SECTION-PROOF ( -- )
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
   \ checked on the first pass. The changed back-edge proof is E-REJECTED.
   s" P12 ( n -- ) [: WNUMBER ;] catch swap {: v :} 2 0 do dup 0= if v 1+ drop then drop 0 loop drop" CS-CODE<
   s\" \"code\":\"E-REJECTED\"" CS-CODE? CS-CODE-END
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
   s" PP2 ( ptr u8 n -- n ) ['] OUTER-PARTIAL-QUOTE catch {: code:n :} drop c@"
      CHECK-QUIET-CANDIDATE! -1 T= ;

\ Candidate checks isolate the diagnostic. These two children enter the
\ enforcing check-tool load path, which reports the named code as JSON.
4096 constant CS-CHILD-CAP
create CS-CHILD-OUT CS-CHILD-CAP allot
create CS-CHILD-ERR CS-CHILD-CAP allot
create CS-CHILD-EMPTY 1 allot

: CS-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: CS-LOAD-WITH ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n hb:ptr hbu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --json-errors" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   hb hbu >LEN CS-CHILD-EMPTY 0 >LEN
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   10000 >MS RUN-ARGV-STDIN-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF
      err OF PCAP-FAILED:UNMAKE {: out:len err:len rc:rc :}
         rc RC>N 70 T=
         CS-CHILD-ERR err LEN>N s\" \"code\":\"E-STALE-READ\"" CONTAINS? TTRUE
      ENDOF
   ;MATCH ;

: CS-LOAD-REFUSAL ( ptr u8 n -- ) CS-HB$ CS-LOAD-WITH ;

: CS-SECTION-LOAD ( -- )
   s" check-tool load refuses a plain-zero status" T-LABEL
   s" test/catch-success-wrong-status.f" CS-LOAD-REFUSAL
   s" check-tool load refuses a live failure join" T-LABEL
   s" test/catch-success-unsafe-join.f" CS-LOAD-REFUSAL ;

\ Definition control summaries survive APP-IMAGE:SAVE. Execute the guarded
\ source from the restored image, then load fresh unsafe source against that
\ same image so stale and zero-test semantics must still be enforced.
create CS-IMAGE-ROOT-BUF FS-PATH-CAP allot
create CS-IMAGE-PATH-BUF FS-PATH-CAP allot
variable CS-IMAGE-ROOT-U
variable CS-IMAGE-PATH-U
variable CS-CHILD-OUT-U
variable CS-CHILD-ERR-U

: CS-IMAGE-ROOT$ ( -- ptr u8 n ) CS-IMAGE-ROOT-BUF CS-IMAGE-ROOT-U @ ;
: CS-IMAGE$ ( -- ptr u8 n ) CS-IMAGE-PATH-BUF CS-IMAGE-PATH-U @ ;

: CS-IMAGE-PREPARE ( -- )
   s" habu-catch-success" HB-TMP-MKDIR {: path:ptr size:n :}
   path CS-IMAGE-ROOT-BUF size BYTE-COPY
   size CS-IMAGE-ROOT-U !
   CS-IMAGE-ROOT$ CLEANUP-TREE+
   CS-IMAGE-ROOT$ s" saved-hb" CS-IMAGE-PATH-BUF JOIN-PATH
   CS-IMAGE-PATH-U ! ;

: CS-IMAGE-RC ( result<pcap:captured,pcap:failed> -- n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: out:len err:len :}
         out LEN>N CS-CHILD-OUT-U !
         err LEN>N CS-CHILD-ERR-U !
         0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: out:len err:len rc:rc :}
         out LEN>N CS-CHILD-OUT-U !
         err LEN>N CS-CHILD-ERR-U !
         rc RC>N ENDOF
   ;MATCH ;

: CS-IMAGE-BUILD ( -- )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   CS-IMAGE$ >LEN PROC-ARGV+
   CS-HB$ >LEN
   S\" require src/habu/app-image.f\nrequire test/catch-success-image-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   180000 >MS RUN-ARGV-STDIN-CAPTURE CS-IMAGE-RC 0 T=
   CS-IMAGE$ EXECUTABLE? TTRUE ;

: CS-IMAGE-RUN ( -- )
   PROC-ARGV-RESET
   CS-IMAGE$ >LEN
   S\" CATCH-IMAGE:RUN\ns\" test/catch-success-image-consumer.f\" included\n" >LEN
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   10000 >MS RUN-ARGV-STDIN-CAPTURE CS-IMAGE-RC 0 T=
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image: ok" CONTAINS? TTRUE
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image-fresh: ok" CONTAINS? TTRUE ;

: CS-SAVED-REFUSAL ( ptr u8 n ptr u8 n -- )
   {: src:ptr size:n want:ptr wantu:n :}
   PROC-ARGV-RESET
   CS-IMAGE$ >LEN src size >LEN
   CS-CHILD-OUT CS-CHILD-CAP >LEN CS-CHILD-ERR CS-CHILD-CAP >LEN
   10000 >MS RUN-ARGV-STDIN-CAPTURE CS-IMAGE-RC 70 T=
   CS-CHILD-OUT CS-CHILD-OUT-U @ s" catch-image-load: began" CONTAINS? TTRUE
   CS-CHILD-ERR CS-CHILD-ERR-U @ s" stale cell" CONTAINS? TTRUE
   CS-CHILD-ERR CS-CHILD-ERR-U @ want wantu CONTAINS? TTRUE ;

: CS-IMAGE-BODY ( -- )
   CS-IMAGE-BUILD
   CS-IMAGE-RUN
   S\" s\" catch-image-load: began\" type cr\ns\" test/catch-success-wrong-status.f\" included\n"
      s" wrong-status" CS-SAVED-REFUSAL
   S\" s\" catch-image-load: began\" type cr\ns\" test/catch-success-override.f\" included\n"
      s" unsafe" CS-SAVED-REFUSAL ;

: CS-SECTION-IMAGE ( -- )
   s" a saved image executes a guarded catch and still refuses forged proof" T-LABEL
   CLEANUP-RESET
   CS-IMAGE-PREPARE
   [: CS-IMAGE-BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0<> if code throw then ;

\ ---- where a throw edge comes from ------------------------------------------
\ A callee that throws counts as overwriting every input it declared UNLESS its
\ own body proved otherwise: the checker records, per definition, which declared
\ inputs every throw path of that body left where they were, and a call takes
\ the edge with those cells kept. WMAYBE only READS its input, so R6 certifies;
\ the callee rows below are where that evidence is measured.
: CS-SECTION-EDGES ( -- )
   s" R6 ( ptr u8 -- ptr u8 n ) [: WMAYBE ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a `finally` cleanup that throws is a throw edge of the word that runs it
   s" R7 ( ptr u8 -- ptr u8 n ) [: FINW ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a join: the loop needs the cell's type on the back edge, and WMAYBE's
   \ evidence is what carries it there (with WBOOM in its place this is refused,
   \ and no refinement ever un-stales a cell)
   s" R8 ( ptr u8 -- ptr u8 ) begin [: WMAYBE ;] catch 0= until" CHECK-QUIET-CANDIDATE! -1 T=
   s" R8B ( ptr u8 -- ptr u8 ) begin [: WBOOM CS-BUF ;] catch 0= until" CS-CODE<
   CS-CODE-END
   \ A quotation literal infers its window on a fresh row, so at the edge the
   \ window cell is still an unbound variable and a rewrite with a value of the
   \ same type is not evidence that the cell survived: only identity of the term
   \ is, and identity neither binds the quotation (which would make it
   \ monomorphic where it is merely executed) nor keeps a transient row id.
   \ Migration: bind the cell to a local before the catch, or push the literal
   \ after it.
   s" R9 ( n -- n n ) [: drop 5 -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ ---- what a callee's own body proves about the inputs it was given ----------
\ The evidence is per DECLARED input, folded with AND over every throw path of
\ the callee's body and measured the same way a quotation's is: the cell at that
\ position is still THE SAME TERM at the edge. A cell that went into a locals
\ frame is gone from the row (the frame owns a fresh base), a cell the body
\ swapped or replaced is a different term, and one arm that overwrites is enough
\ to clear the bit for every path.
: CS-SECTION-CALLEES ( -- )
   \ both throw paths leave the input where it was
   s" C1 ( ptr u8 -- ptr u8 n ) [: WMAYBE2 ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ one of the two overwrote it
   s" C2 ( ptr u8 -- ptr u8 n ) [: WPARTLY ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ R1's body as a callee: the throw path swapped the two cells
   s" C3 ( n ptr u8 -- n ptr u8 n ) [: WSWAPT ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ the input was bound to a local: the cells the callee was given are spent
   s" C4 ( ptr u8 -- ptr u8 n ) [: WBIND ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ R2's body as a callee: the return-stack twin
   s" C5 ( n | ptr u8 -- n n | ptr u8 ) [: WRSWAP ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ per cell, not per word: WKEEPTOP keeps the top of its window and not the
   \ cell below it, so the `n` comes back typed ...
   s" C6 ( ptr u8 n -- n ) [: WKEEPTOP ;] catch {: code:n :} nip"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and reading the cell below it is E-STALE-READ on the token that reads
   s" C7 ( ptr u8 n -- n ) [: WKEEPTOP ;] catch {: code:n :} drop c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END ;

\ The masks the checker recorded for those same fixtures, read out of its store.
\ Bit 0 is the TOP declared input and the mask is cut to the width the signature
\ declared - a bit above it would speak for a cell that does not exist - so each
\ row names the whole recorded word.
: CS-SECTION-RECORDED ( -- )
   s" WMAYBE" CS-CTL-MASKS drop 1 T=              \ one declared input, left alone
   s" WMAYBE2" CS-CTL-MASKS drop 1 T=
   s" WBOOM" CS-CTL-MASKS drop 0 T=
   s" WPARTLY" CS-CTL-MASKS drop 0 T=
   s" WBIND" CS-CTL-MASKS drop 0 T=
   s" WSWAPT" CS-CTL-MASKS drop 0 T=              \ two inputs, the throw path swapped them
   s" WKEEPTOP" CS-CTL-MASKS drop 1 T=            \ top kept, the cell below it not
   s" WRSWAP" CS-CTL-MASKS 0 T= 0 T=              \ the return cell was moved, and so was the data one
   \ a word with no throw edge at all records nothing: its call takes no edge
   s" WKEEP" CS-CTL-MASKS 0 T= 0 T= ;

\ The one cell a symbol's control facts travel in (the owner-ABI handover and
\ the captured signature graph): the flags keep the low bits and the masks ride
\ above them, 20 bits each, so a mask wider than that arrives truncated - which
\ is the conservative direction.
: CS-SECTION-XFER ( -- )
   $1000A $ABCDE $12345 CS-XPACK {: packed:n :}
   packed CS-XFLAGS $1000A T=
   packed CS-XDMASK $ABCDE T=
   packed CS-XRMASK $12345 T=
   $2 $FFFFFFFFFFFF $FFFFFFFFFFFF CS-XPACK {: wide:n :}
   wide CS-XFLAGS $2 T=
   wide CS-XDMASK $FFFFF T=
   wide CS-XRMASK $FFFFF T= ;

\ ---- what the rule still admits ---------------------------------------------
: CS-SECTION-ADMITS ( -- )
   \ window 0: a body that takes nothing has nothing to lose
   s" A1 ( -- n ) [: -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A2 ( -- n ) [: 1 2 3 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A3 ( -- n ) [: 1 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the throw path left the window cell exactly where it was
   s" A4 ( ptr u8 -- ptr u8 n ) [: dup c@ drop -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a body with no throw edge records nothing and stales nothing
   s" A5 ( ptr u8 -- ptr u8 n ) [: WKEEP ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a stale cell may be bound to an untyped local and never read
   s" A6 ( ptr u8 -- ) [: WBOOM ;] catch {: v code:n :}" CHECK-QUIET-CANDIDATE! -1 T=
   \ and it may be moved and dropped
   s" A7 ( ptr u8 ptr u8 -- ) [: WBOOM ;] catch drop swap 2drop" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- a multicell bundle is ONE stale value ----------------------------------
\ A layout value occupies W hidden physical cells on the row, and `catch` wraps
\ each of them one at a time. Staleness is a property of the VALUE: every
\ question that finds the group sees through the wrapper, so an `option<cspt>`
\ window comes back as one `stale<option<cspt<>>>` that one `drop` removes and
\ one `nip` moves, and every typed use of it - a word input, a typed local, a
\ `MATCH` - is E-STALE-READ. Read as three lone cells instead, two `drop`s left
\ one behind and the third was E-NELAB-UNDER in the native elaborator, which
\ holds the bundle as one value.
PRODUCT cspt 0
   FIELD x n
   FIELD y n
;PRODUCT

: CS-PT ( -- cspt ) 17 25 CATCH--STALE--TEST-CSPT:MAKE ;
: CS-SOME ( -- option<cspt> ) CS-PT OPTION:SOME ;
: CS-BUNDLE-THROW ( option<cspt> -- option<cspt> )   \ overwrites the bundle, then throws
   drop CS-SOME E-CS-BOOM throw ;
: CS-BUNDLE-MAYBE ( option<cspt> -- option<cspt> )
   drop CS-SOME CS-TICK @ 0= IF E-CS-BOOM throw THEN ;
: CS-READ-BUNDLE ( option<cspt> -- n )               \ a declared logical input: a read
   MATCH option
      none OF 0 ENDOF
      some OF CATCH--STALE--TEST-CSPT:UNMAKE + ENDOF
   ;MATCH ;
: CS-BUNDLE-RT ( cspt option<cspt> -- n )            \ one drop for the stale bundle, one for the cspt
   [: CS-BUNDLE-THROW ;] catch {: rc:n :}
   drop drop rc ;
: CS-BUNDLE-GUARD ( option<cspt> -- n )
   [: CS-BUNDLE-MAYBE ;] catch {: o code:n :}
   code 0= IF o CS-READ-BUNDLE ELSE 0 THEN ;

: CS-SECTION-BUNDLES ( -- )
   \ the stale bundle is dropped by ONE drop, the cspt below it by the other
   s" B1 ( cspt option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} drop drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ passing it to a word that declares the logical type reads it, and the
   \ diagnostic names the value, not a hidden field
   s" B2 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} CS-READ-BUNDLE" CS-CODE<
   CS-STALE?  s" stale<option<cspt<>>>" CS-CODE?  CS-CODE-END
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
   CS-PT CS-SOME CS-BUNDLE-RT E-CS-BOOM T= ;

\ ---- the intact cell is honest at run time ----------------------------------
\ A4's shape, run: the caught body reads the address and throws, and the catch
\ hands back the SAME address with the throw code on top.
16 BUFFER: CS-BUF
: CS-RT ( ptr u8 -- ptr u8 n ) [: dup c@ drop E-CS-BOOM throw ;] catch ;

: CS-CRT ( ptr u8 -- ptr u8 n ) [: WMAYBE ;] catch ;   \ the same, through a CALLEE

: CS-SECTION-RUNTIME ( -- )
   CS-BUF CS-RT {: p code:n :}
   code E-CS-BOOM T=
   p CS-BUF = TTRUE
   \ WMAYBE reads the zero byte and throws without touching the address: the
   \ catch hands back the address the caller pushed, which is what its evidence
   \ promised the checker
   CS-BUF CS-CRT {: q code2:n :}
   code2 E-CS-BOOM T=
   q CS-BUF = TTRUE ;

\ ---- the tick route ----------------------------------------------------------
\ `['] W` pushes a term of W's declared effect, and W IS the body a catch of
\ that term runs, so the term carries the edge W's own definition proved -
\ exactly what the `[: W ;]` literal's term carries. Nothing is recomposed: a
\ recorded bit counts from the TOP of W's declared input row, and that row IS
\ the pushed term's window, which is what the catch measures the mask against.
\ T4 and T5 are the two halves of that orientation, per cell and in W's order.
\ A word with no throw edge records nothing and its tick carries nothing.
\
\ The edge still is not part of the quotation TYPE, so the two routes whose
\ term is a fresh instance of a DECLARED type keep the window typed even when
\ the target throws: the typed `xt<effect>` cell (T9) and the quotation
\ parameter (T10), with the direct tick of that same callee (T3) refused beside
\ them. `['] DEF catch` for a `defer` is the same boundary - the defer's symbol
\ records no body's evidence (test/xt-cell-test.f GC4). A tick bound to a LOCAL
\ in the same body is the very term the tick pushed, and keeps its edge (T7,T8).
: WREPL ( ptr u8 -- ptr u8 ) drop CS-BUF E-CS-BOOM throw ;  \ same type, another cell
TYPED-VARIABLE CS-XT [ ptr u8 -- ptr u8 ]
: CS-CATCHP ( ptr u8 [ ptr u8 -- ptr u8 ] -- ptr u8 n ) catch ;
: CS-TRT ( ptr u8 -- ptr u8 n ) ['] WMAYBE catch ;

: CS-SECTION-TICK ( -- )
   \ R1's twin: the body is a callee now, and the tick carries its evidence
   s" T1 ( n ptr u8 -- n ptr u8 n ) ['] WSWAPT catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ R6's twin: the callee that only READS its input keeps the address typed
   s" T2 ( ptr u8 -- ptr u8 n ) ['] WMAYBE catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the callee that replaces it does not, and the refusal names the read
   s" T3 ( ptr u8 -- n ) ['] WREPL catch {: v code:n :} v c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ per cell, as C6/C7: WKEEPTOP keeps the top of its window ...
   s" T4 ( ptr u8 n -- n ) ['] WKEEPTOP catch {: code:n :} nip"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and the cell below it is stale, named on the token that reads it
   s" T5 ( ptr u8 n -- n ) ['] WKEEPTOP catch {: code:n :} drop c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ C4's twin: a callee that bound its input to a local proved nothing
   s" T6 ( ptr u8 -- ptr u8 n ) ['] WBIND catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ the local-bound tick: the same term, so the same edge
   s" T7 ( ptr u8 -- ptr u8 n ) ['] WREPL {: x :} x catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   s" T8 ( ptr u8 -- ptr u8 n ) ['] WMAYBE {: x :} x catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the boundary: the cell read and the parameter are fresh instances of the
   \ declared xt type, which carries no edge, so both still certify
   s" T9 ( ptr u8 -- n ) ['] WREPL CS-XT ! CS-XT @ catch {: v code:n :} v c@"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" T10 ( ptr u8 -- n ) ['] WREPL CS-CATCHP {: v code:n :} v c@"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ a callee that never returns is fit-checked against its declared output row
   \ as it always was: WBOOM leaves nothing where its window was, and THAT is
   \ the refusal - not the stale rule, which would need the dead flag the tick
   \ deliberately does not take (src/core/checker.f BTICK-EDGE)
   s" T11 ( ptr u8 -- n ) ['] WBOOM catch {: v code:n :} code" CS-CODE<
   s\" \"code\":\"E-STALE-READ\"" DIAG-BUFFER$ 2swap CONTAINS? TFALSE
   CS-CODE-END
   \ and the intact cell is honest at run time through the tick as well: WMAYBE
   \ reads the byte and throws, and the catch hands back the address pushed here
   CS-BUF CS-TRT {: r code3:n :}
   code3 E-CS-BOOM T=
   r CS-BUF = TTRUE ;

: RUN ( -- )
   T-RESET
   s" caught return replacement needs its matching success status" T-LABEL
   1 CS-RETURN-GUARD CS-RETURN-NEW? TTRUE
   [: -1 CS-RETURN-GUARD drop ;] E-CS-RETURN TTHROWSQ
   CS-SECTION-REPRODUCERS
   CS-SECTION-READS
   CS-SECTION-PROOF
   CS-SECTION-PROOF-PRECISION
   CS-SECTION-LOAD
   CS-SECTION-EDGES
   CS-SECTION-CALLEES
   CS-SECTION-RECORDED
   CS-SECTION-XFER
   CS-SECTION-ADMITS
   CS-SECTION-BUNDLES
   CS-SECTION-RUNTIME
   CS-SECTION-TICK
   CS-SECTION-IMAGE
   T-REPORT ;

RUN

;package
