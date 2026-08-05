\ native-immediate.f - checked immediate-word contract table tests.
\
\ Proves the section 7.1 contract of src/compiler/native/immediate.f: an
\ immediate word is classified as a front-end intrinsic, as compile-time
\ computation, or as a named unmodeled boundary; the gate admits the first two
\ and refuses the third by name, and refuses an undeclared word the same way;
\ an unmodeled boundary must name the capability it is waiting for and a
\ modeled word must not; declarations are unique, bounded, owned by one
\ module, and walkable in order for an inventory; a corrupted stored class or
\ reason cell is refused; and the gate applied to a sealed source tape reads
\ the spelling of a name token and refuses to classify a literal.

require lib/test.f
require test/checker-assert.f
require src/compiler/native/immediate.f

package NIMM-TEST
private

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A module's symbol store and contract table.
: TAB-NEW ( IR-CTX:ctx n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx cap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key
   c key 8 96 IR-SYM:NEW
   c key cap NIMM:NEW ;

\ The three words every fixture classifies, plus the capability name an
\ unmodeled boundary is waiting for.
: FILL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id IR-ID:ir-symbol-id IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sy:IR-ARENA:arena
      tb:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c sp sy key s" [FOLD]" IR-SYM:INTERN {: w1:IR-ID:ir-symbol-id :}
   c sp sy key s" DOES>" IR-SYM:INTERN {: w2:IR-ID:ir-symbol-id :}
   c sp sy key s" habu-does-lowering" IR-SYM:INTERN {: why:IR-ID:ir-symbol-id :}
   c tb sy w0 NIMM-CLASS:INTRINSIC NIMM:DECLARE
   c tb sy w1 NIMM-CLASS:COMPILE-TIME NIMM:DECLARE
   c tb sy w2 why NIMM:DECLARE-UNMODELED
   w0 w1 w2 ;

\ ---- the three classes -------------------------------------------------------
: CL-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL {: w0:IR-ID:ir-symbol-id w1:IR-ID:ir-symbol-id w2:IR-ID:ir-symbol-id :}
   tb NIMM:DECLARED
   tb w0 NIMM:CLASS@ NIMM-CLASS:INTRINSIC NIMM-CLASS:EQ
   tb w1 NIMM:CLASS@ NIMM-CLASS:COMPILE-TIME NIMM-CLASS:EQ
   tb w2 NIMM:CLASS@ NIMM-CLASS:UNMODELED NIMM-CLASS:EQ
   tb w0 NIMM:ADMIT NIMM-CLASS:INTRINSIC NIMM-CLASS:EQ
   tb w1 NIMM:ADMIT NIMM-CLASS:COMPILE-TIME NIMM-CLASS:EQ ;

: CL-CASE ( -- )
   s" the three classes are declared and read back" T-LABEL
   BND [: CL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 3 T= ;

\ Declaration order is observable, which is what an inventory of the remaining
\ boundaries walks.
: AT-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL drop drop drop
   tb key 0 NIMM:AT IR-ID:SYMBOL-LOCAL
   tb key 1 NIMM:AT IR-ID:SYMBOL-LOCAL
   tb key 2 NIMM:AT IR-ID:SYMBOL-LOCAL ;

: AT-CASE ( -- )
   s" declarations walk in declaration order" T-LABEL
   BND [: AT-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 1 T= 0 T= ;

\ An unmodeled boundary names the capability it is waiting for.
: RS-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL {: w0:IR-ID:ir-symbol-id w1:IR-ID:ir-symbol-id w2:IR-ID:ir-symbol-id :}
   tb key w2 NIMM:REASON@ IR-ID:SYMBOL-LOCAL ;

: RS-CASE ( -- )
   s" an unmodeled boundary names the capability it waits for" T-LABEL
   BND [: RS-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= ;

\ ---- refusals ----------------------------------------------------------------
: UNMOD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL {: w0:IR-ID:ir-symbol-id w1:IR-ID:ir-symbol-id w2:IR-ID:ir-symbol-id :}
   tb w2 NIMM:ADMIT drop ;

: UNMOD ( -- )
   BND [: UNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

: UNDEC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL drop drop drop
   c sp sy key s" POSTPONE" IR-SYM:INTERN {: w3:IR-ID:ir-symbol-id :}
   tb w3 NIMM:ADMIT drop ;

: UNDEC ( -- )
   BND [: UNDEC-BODY ;] IR-CTX:WITH-CONTEXT ;

: RSCLASS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL {: w0:IR-ID:ir-symbol-id w1:IR-ID:ir-symbol-id w2:IR-ID:ir-symbol-id :}
   tb key w0 NIMM:REASON@ drop ;

: RSCLASS ( -- )
   BND [: RSCLASS-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The general declarer must not be a back door into the unmodeled class: an
\ unmodeled boundary has to name what it waits for, so it has its own word.
: DECU-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c tb sy w0 NIMM-CLASS:UNMODELED NIMM:DECLARE ;

: DECU ( -- )
   BND [: DECU-BODY ;] IR-CTX:WITH-CONTEXT ;

: DUPD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL {: w0:IR-ID:ir-symbol-id w1:IR-ID:ir-symbol-id w2:IR-ID:ir-symbol-id :}
   c tb sy w0 NIMM-CLASS:COMPILE-TIME NIMM:DECLARE ;

: DUPD ( -- )
   BND [: DUPD-BODY ;] IR-CTX:WITH-CONTEXT ;

: REFUSE-CASES ( -- )
   s" a declared unmodeled boundary is refused by the gate" T-LABEL
   [: UNMOD ;] E-NIMM-UNMODELED TTHROWSQ
   s" a word with no contract at all is refused the same way" T-LABEL
   [: UNDEC ;] E-NIMM-UNMODELED TTHROWSQ
   s" asking a modeled word what it waits for is refused" T-LABEL
   [: RSCLASS ;] E-NIMM-CLASS TTHROWSQ
   s" the general declarer refuses the unmodeled class" T-LABEL
   [: DECU ;] E-NIMM-CLASS TTHROWSQ
   s" a second declaration of one word is refused" T-LABEL
   [: DUPD ;] E-NIMM-DUP TTHROWSQ ;

\ ---- ownership ---------------------------------------------------------------
: XSYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c 8 TAB-NEW
   {: k2:IR-ID:ir-module-key sp2:IR-ARENA:arena
      sy2:IR-ARENA:arena tb2:IR-ARENA:arena :}
   c sp2 sy2 k2 s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c tb sy2 w0 NIMM-CLASS:INTRINSIC NIMM:DECLARE ;

: XSYM ( -- )
   BND [: XSYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: GHOST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c tb sy key 4 IR-ID:PACK-SYMBOL NIMM-CLASS:INTRINSIC NIMM:DECLARE ;

: GHOST ( -- )
   BND [: GHOST-BODY ;] IR-CTX:WITH-CONTEXT ;

: XKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c 8 TAB-NEW
   {: k2:IR-ID:ir-module-key sp2:IR-ARENA:arena
      sy2:IR-ARENA:arena tb2:IR-ARENA:arena :}
   c sp sy tb key FILL drop drop drop
   tb k2 0 NIMM:AT drop ;

: XKEY ( -- )
   BND [: XKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The contract table and the symbol rows are the same checker type, so a swap
\ at the call site has to die on a header tag.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c sy tb w0 NIMM-CLASS:INTRINSIC NIMM:DECLARE ;

: SWAPPED ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW NIMM:DECLARED drop ;

: RAW ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES ( -- )
   s" another module's word cannot be classified here" T-LABEL
   [: XSYM ;] E-NIMM-OWNER TTHROWSQ
   s" a word the interner never minted cannot be classified" T-LABEL
   [: GHOST ;] E-IR-SYM-BOUND TTHROWSQ
   s" a foreign module key is refused by the inventory walk" T-LABEL
   [: XKEY ;] E-NIMM-OWNER TTHROWSQ
   s" a table and a symbol store swapped at the call site die on the tag" T-LABEL
   [: SWAPPED ;] E-NIMM-STATE TTHROWSQ
   s" a bare arena is not a contract table" T-LABEL
   [: RAW ;] E-NIMM-STATE TTHROWSQ ;

\ ---- bounds and capacity -----------------------------------------------------
: BD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL drop drop drop
   tb key 3 NIMM:AT drop ;

: BD ( -- )
   BND [: BD-BODY ;] IR-CTX:WITH-CONTEXT ;

: BDNEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy tb key FILL drop drop drop
   tb key -1 NIMM:AT drop ;

: BDNEG ( -- )
   BND [: BDNEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 0 NIMM:NEW IR-ARENA:ABORT ;

: CAPZERO ( -- )
   BND [: CAPZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPHUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key $60000000 NIMM:NEW IR-ARENA:ABORT ;

: CAPHUGE ( -- )
   BND [: CAPHUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPFULL-THIRD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx tb:IR-ARENA:arena sy:IR-ARENA:arena w:IR-ID:ir-symbol-id :}
   c tb sy w
   c tb sy w NIMM-CLASS:INTRINSIC NIMM:DECLARE ;

: CAPFULL-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 2 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c sp sy key s" THEN" IR-SYM:INTERN {: w1:IR-ID:ir-symbol-id :}
   c sp sy key s" ELSE" IR-SYM:INTERN {: w2:IR-ID:ir-symbol-id :}
   c tb sy w0 NIMM-CLASS:INTRINSIC NIMM:DECLARE
   c tb sy w1 NIMM-CLASS:INTRINSIC NIMM:DECLARE
   c tb sy w2 [: CAPFULL-THIRD ;] catch
   {: c2:IR-CTX:ctx tb2:IR-ARENA:arena sy2:IR-ARENA:arena
      w3:IR-ID:ir-symbol-id rc:n :}
   rc
   tb2 NIMM:DECLARED ;

: BOUND-CASES ( -- )
   s" an inventory index past the count is refused" T-LABEL
   [: BD ;] E-NIMM-BOUND TTHROWSQ
   s" a negative inventory index is refused" T-LABEL
   [: BDNEG ;] E-NIMM-BOUND TTHROWSQ
   s" a zero table capacity is refused at creation" T-LABEL
   [: CAPZERO ;] E-NIMM-CAP TTHROWSQ
   s" a capacity past the arena ordinal range is refused at creation" T-LABEL
   [: CAPHUGE ;] E-NIMM-CAP TTHROWSQ
   s" a full table refuses named and stays whole" T-LABEL
   BND [: CAPFULL-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= E-NIMM-CAP T= ;

\ ---- corrupted rows ----------------------------------------------------------
\ A holder who bypasses the package and appends raw cells writes a row whose
\ shape is right and whose content is not.
: RAW-ROW ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx tb:IR-ARENA:arena so:n cls:n rsn:n :}
   c tb so IR-ARENA:PUSH drop
   c tb cls IR-ARENA:PUSH drop
   c tb rsn IR-ARENA:PUSH drop ;

: CORRUPT ( IR-CTX:ctx n n -- IR-ID:ir-symbol-id IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx cls:n rsn:n :}
   c 4 TAB-NEW
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena
      sy:IR-ARENA:arena tb:IR-ARENA:arena :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c tb 0 cls rsn RAW-ROW
   w0 tb key ;

: BADCLASS-BODY ( IR-CTX:ctx -- )
   9 0 CORRUPT drop swap NIMM:CLASS@ drop ;

: BADCLASS ( -- )
   BND [: BADCLASS-BODY ;] IR-CTX:WITH-CONTEXT ;

: BADREASON-BODY ( IR-CTX:ctx -- )
   2 0 CORRUPT {: w0:IR-ID:ir-symbol-id tb:IR-ARENA:arena key:IR-ID:ir-module-key :}
   tb key w0 NIMM:REASON@ drop ;

: BADREASON ( -- )
   BND [: BADREASON-BODY ;] IR-CTX:WITH-CONTEXT ;

: OKROW-BODY ( IR-CTX:ctx -- bool )
   0 0 CORRUPT drop swap NIMM:CLASS@ NIMM-CLASS:INTRINSIC NIMM-CLASS:EQ ;

: CORRUPT-CASES ( -- )
   \ positive control: the same hand-written row with a legal class reads back,
   \ so the two rejections below fail for their stated reason
   s" a hand-written row with a legal class reads back" T-LABEL
   BND [: OKROW-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE
   s" a stored class outside the vocabulary is refused" T-LABEL
   [: BADCLASS ;] E-NIMM-CLASS TTHROWSQ
   s" an unmodeled row that names nothing is refused" T-LABEL
   [: BADREASON ;] E-NIMM-STATE TTHROWSQ ;

\ ---- the tape join -----------------------------------------------------------
\ The gate applied to a sealed tape: a name token is classified by its
\ spelling, and a literal is not a call, so it cannot be classified at all.
: TAPE-BUILD ( IR-CTX:ctx -- IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 4 IR-SOURCE:NEW {: sr:IR-ARENA:arena :}
   c key 8 96 IR-SYM:NEW {: sp:IR-ARENA:arena sy:IR-ARENA:arena :}
   c key 4 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c key 4 NIMM:NEW {: tb:IR-ARENA:arena :}
   c sr key s" IF 1 DOES>" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c sp sy key s" IF" IR-SYM:INTERN {: w0:IR-ID:ir-symbol-id :}
   c sp sy key s" 1" IR-SYM:INTERN {: w1:IR-ID:ir-symbol-id :}
   c sp sy key s" DOES>" IR-SYM:INTERN {: w2:IR-ID:ir-symbol-id :}
   c sp sy key s" habu-does-lowering" IR-SYM:INTERN {: why:IR-ID:ir-symbol-id :}
   c tb sy w0 NIMM-CLASS:INTRINSIC NIMM:DECLARE
   c tb sy w2 why NIMM:DECLARE-UNMODELED
   c tp sr sy
      sr s0 0 2 IR-SOURCE:SPAN w0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 3 1 IR-SOURCE:SPAN w1 NTAPE-MODE:COMPILING 1 NTAPE:INT-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 5 5 IR-SOURCE:SPAN w2 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL key tb ;

: TJ-BODY ( IR-CTX:ctx -- bool )
   TAPE-BUILD 0 NIMM:ADMIT-TOKEN NIMM-CLASS:INTRINSIC NIMM-CLASS:EQ ;

: TJ-CASE ( -- )
   s" a name token is classified by its spelling" T-LABEL
   BND [: TJ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: TJLIT-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD 1 NIMM:ADMIT-TOKEN drop ;

: TJLIT ( -- )
   BND [: TJLIT-BODY ;] IR-CTX:WITH-CONTEXT ;

: TJUNMOD-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD 2 NIMM:ADMIT-TOKEN drop ;

: TJUNMOD ( -- )
   BND [: TJUNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

: TJ-REJECT-CASES ( -- )
   s" a literal token cannot be classified as a word" T-LABEL
   [: TJLIT ;] E-NTAPE-KIND TTHROWSQ
   s" a name token naming an unmodeled boundary is refused" T-LABEL
   [: TJUNMOD ;] E-NIMM-UNMODELED TTHROWSQ ;

\ ---- the checker keeps the identities and the API sealed ---------------------
: CHECKER-CASES ( -- )
   s" NIPOS ( IR-ARENA:arena -- n ) NIMM:DECLARED"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" NICLASS-FORGE ( n -- NIMM:class )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NIDECL-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id NIMM:class -- ) NIMM:DECLARE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NIADMIT-RAW ( IR-ARENA:arena n -- NIMM:class ) NIMM:ADMIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NIREASON-KEYLESS ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id ) NIMM:REASON@"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Grouped harness contexts for the same reason as the tape suite: a fixture
\ that throws leaves its context to be reclaimed at the harness exit, and a
\ module here owns three or more arenas against a sixty-four slot registry.
: GROUP-CLASSES ( IR-CTX:ctx -- )
   drop
   CL-CASE
   AT-CASE
   RS-CASE
   REFUSE-CASES ;

: GROUP-OWNER ( IR-CTX:ctx -- )
   drop
   OWNER-CASES
   BOUND-CASES ;

: GROUP-TAPE ( IR-CTX:ctx -- )
   drop
   CORRUPT-CASES
   TJ-CASE
   TJ-REJECT-CASES ;

public

: RUN ( -- )
   T-RESET
   BND [: GROUP-CLASSES ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TAPE ;] IR-CTX:WITH-CONTEXT
   CHECKER-CASES
   T-REPORT ;

;package

NIMM-TEST:RUN
