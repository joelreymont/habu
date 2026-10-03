\ render.fs — render the checker's inferred residual stack (DCUR) back to readable
\ type names. Type variables get canonical letters a,b,c… (assigned bottom-to-top),
\ generic int=n, old flag=f, float=r; concrete types render by name. The "render"
\ half of the native sigparse/checker. Needs
\ checker.fs. Standalone has no emit/+!; chars go through a 1-byte buffer + type.
\ Also the checker's sig RECORDER: certified words render "in -- out" to a buffer
\ and append it to USIGS (installed via RECXT), so callers of certified words
\ certify too.
using TFAM

create ECH 1 allot
variable RDST   0 RDST !                 \ 0 = stdout, 1 = RSBUF (sig recording)
16384 constant RSBUF-CAP
create RSBUF RSBUF-CAP allot   variable RSN
variable RQM                             \ a '?' rendered = unknown tag, don't record
variable RJSON   0 RJSON !               \ inside a JSON string: EMIT1 escapes
variable RDIAG-ON
variable RDIAG-FD   2 RDIAG-FD !
PTR-VARIABLE RDIAG-A
variable RDIAG-CAP
variable RDIAG-U
variable RDIAG-I

\ A record that does not fit its buffer, the renderer's own or the caller's, is
\ refused by this code. lib/errors.f owns it; this file compiles before any lib/
\ file exists, so the same (code, name) pair is re-registered here -- the one
\ form tools/error-code-lint.f admits -- and test/diag-buffer-capacity.f keeps
\ the two spellings equal.
package RDIAG
public
-2901 constant E-DIAG-CAPACITY
;package

\ RSBUF holds one record whole, a diagnostic or a recorded effect. A record
\ past it is refused by a throw, with rendering back on stdout and outside any
\ JSON string, so a refused record leaves the renderer as a delivered one does.
: EMIT-RAW {: c :}
   c 63 = IF 1 RQM ! THEN
   RDST @ IF
     RSN @ RSBUF-CAP 2 - > IF
        0 RDST !  0 RSN !  0 RJSON !
        RDIAG:E-DIAG-CAPACITY throw
     THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;

: JHEX ( n -- ) {: d:n :}
   d 10 < IF d 48 + ELSE d 55 + THEN EMIT-RAW ;

\ JCHAR writes one byte as the inside of a JSON string: `"`, `\` and every byte
\ below 32 escaped, as RFC 8259 section 7 requires. LF, CR and TAB take their
\ short escapes and every other one \u00XX in upper-case hex, the form
\ tools/lint/json-writer.f writes.
: JCHAR {: c :}
   c case
      10 of 92 EMIT-RAW 110 EMIT-RAW endof
      13 of 92 EMIT-RAW 114 EMIT-RAW endof
      9 of 92 EMIT-RAW 116 EMIT-RAW endof
      34 of 92 EMIT-RAW c EMIT-RAW endof
      92 of 92 EMIT-RAW c EMIT-RAW endof
      c 32 < IF
         92 EMIT-RAW 117 EMIT-RAW 48 EMIT-RAW 48 EMIT-RAW
         c 4 rshift JHEX  c 15 and JHEX
      ELSE c EMIT-RAW THEN
   endcase ;

\ Between JOPEN and JCLOSE every byte EMIT1 writes is escaped, so a rendered
\ type, row or family name is a well-formed JSON string whatever its names
\ spell: a package may be named `\`, and its families render as `\:tail`.
: EMIT1 {: c :}
   RJSON @ IF c JCHAR ELSE c EMIT-RAW THEN ;
: JOPEN ( -- )   34 EMIT-RAW  -1 RJSON ! ;
: JCLOSE ( -- )  0 RJSON !  34 EMIT-RAW ;

: DIAG-BUFFER! ( ptr u8 n -- )
   {: a:ptr cap:n :}
   a RDIAG-A !
   cap RDIAG-CAP !
   0 RDIAG-U !
   -1 RDIAG-ON ! ;

: DIAG-BUFFER-OFF ( -- )
   0 RDIAG-ON !
   0 RDIAG-U ! ;

\ Write each unbuffered diagnostic to FD as it is rendered, 2 until set.
: DIAG-FD! ( n -- )
   RDIAG-FD ! ;

\ The diagnostic buffer pointer lives in a declared pointer cell (dot
\ habu-refuse-a-ptr-5ad2734e), so a plain fetch keeps the checked ptr u8 view
\ the byte store below needs.
: DIAG-BUFFER$ ( -- ptr u8 n )
   RDIAG-A @ RDIAG-U @ ;

: RDIAG-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 RDIAG-I !
   BEGIN RDIAG-I @ u < WHILE
      a RDIAG-I @ + c@
      RDIAG-A @ RDIAG-U @ + RDIAG-I @ + c!
      RDIAG-I @ 1 + RDIAG-I !
   REPEAT ;

\ The buffer is the caller's and cannot grow, so a record that does not fit is
\ refused whole by a throw the caller's catch recovers from: the buffer keeps
\ every record before it.
: RDIAG-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   RDIAG-ON @ 0= IF RDIAG-FD @ a u write drop EXIT THEN
   RDIAG-U @ u + RDIAG-CAP @ > IF RDIAG:E-DIAG-CAPACITY throw THEN
   a u RDIAG-COPY
   RDIAG-U @ u + RDIAG-U ! ;

\ Hand the record rendered in RSBUF on, with rendering back on stdout first,
\ so a refused record leaves the renderer as a delivered one does.
: RSBUF-FLUSH ( -- )
   RSBUF RSN @
   0 RDST !  0 RSN !
   RDIAG-APPEND ;
PERSISTED-PTR-VARIABLE SEEN-P   NULL-PTR SEEN-P !
variable SEEN-CAP   0 SEEN-CAP !
\ Only the assigned prefix needs clearing; allocation initializes every cell
\ above SEEN-HW to UNBOUND, including a grown tail.
variable SEEN-HW   0 SEEN-HW !
variable NLET                                      \ SEEN is indexed by typevar (PAY)
variable RBIND-N                                   \ lexical names while rendering forall
64 constant RATOM-CAP
create RATOM-KEY RATOM-CAP cells allot
variable RATOM-N
variable RATOM-I
$7FFFFFFFFFFFFFFF CELL / constant SEEN-MAX-CAP

: SEEN-UNMAP-RC ( ptr a n -- n ) {: base:ptr cap:n :}
   cap 0= IF 0 EXIT THEN
   base cap cells munmap ;

: SEEN-RELEASE ( -- )
   SEEN-P @ SEEN-CAP @ SEEN-UNMAP-RC 0 <> IF
      s" render: seen munmap failed" 76 die
   THEN
   NULL-PTR SEEN-P !   0 SEEN-CAP !   0 SEEN-HW ! ;

: SEEN-GROW-CAP ( n -- n ) {: need:n :}
   need MAXTV-INIT max
   SEEN-CAP @ SEEN-MAX-CAP 2 / <= IF SEEN-CAP @ 2 * max THEN ;

: SEEN-GROW ( n -- ) {: need:n :}
   need SEEN-GROW-CAP {: nc:n :}
   SEEN-CAP @ {: oc:n :}
   nc cells ARENA-ALLOC {: next:ptr :}
   oc 0 > IF SEEN-P @ BYTE-VIEW next BYTE-VIEW oc cells ARENA-COPY THEN
   next oc nc ARENA-CELLS-UNBOUND
   SEEN-P @ oc SEEN-UNMAP-RC 0 <> IF
      next nc SEEN-UNMAP-RC 0 <> IF s" render: seen cleanup munmap failed" 76 die THEN
      s" render: seen munmap failed" 76 die
   THEN
   next SEEN-P !   nc SEEN-CAP ! ;

: SEEN-ENSURE ( -- )
   MAXTV MAXTV-INIT max {: need:n :}
   need 0 < need SEEN-MAX-CAP > or IF s" render: seen capacity overflow" 76 die THEN
   need SEEN-CAP @ <= IF EXIT THEN
   need SEEN-GROW ;

: SEEN ( -- ptr n ) SEEN-ENSURE SEEN-P @ ;

\ Every entry into the renderer clears only the span written by LET-OF.
: SEEN-RESET ( -- )
   SEEN-ENSURE
   0 BEGIN dup SEEN-HW @ < WHILE
      UNBOUND over cells SEEN + !
      1 +
   REPEAT drop
   0 SEEN-HW ! 0 RBIND-N ! ;

: SEEN-SNAPSHOT-RESET ( -- )
   SEEN-RELEASE ;

: REG-SCRATCH-SNAP-INSTALL ( -- ) [: SEEN-SNAPSHOT-RESET ;] is REG-SCRATCH-SNAP-XT ;
REG-SCRATCH-SNAP-INSTALL
: RATOM-RESET ( -- )
   0 RATOM-N ! ;

\ Diagnostic alpha-renaming has its own a..z namespace.  It renders generic
\ checker variables and is deliberately independent of declaration positions.
\ Signature variables and binders share one printable alphabet. The parser
\ reserves f, n and r as built-in types, so no inferred name may use them.
: RLETTER ( n -- n ) {: idx:n :}
   idx 0 < idx 22 > or IF 63 EXIT THEN
   idx 97 +
   dup 102 >= IF 1+ THEN
   dup 110 >= IF 1+ THEN
   dup 114 >= IF 1+ THEN ;
: LET-OF {: vp :}
   SEEN-ENSURE
   vp 1 + SEEN-HW @ < 0= IF vp 1 + SEEN-HW ! THEN
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ RLETTER ;
: RATOM-CHAR ( n -- n ) {: idx:n :}
   idx 26 < IF idx 97 + ELSE 1 RQM ! 63 THEN ;
: RATOM-FIND ( n -- n bool ) {: key:n :}
   0 RATOM-I !
   BEGIN RATOM-I @ RATOM-N @ < WHILE
      RATOM-I @ cells RATOM-KEY + @ key = IF RATOM-I @ 0 0= EXIT THEN
      RATOM-I @ 1 + RATOM-I !
   REPEAT
   0 1 0= ;
: RATOM-ADD ( n -- n ) {: key:n :}
   RATOM-N @ RATOM-CAP >= IF 1 RQM ! 0 EXIT THEN
   key RATOM-N @ cells RATOM-KEY + !
   RATOM-N @
   RATOM-N @ 1 + RATOM-N ! ;
: RATOM-ORD ( n -- n ) {: key:n :}
   key RATOM-FIND IF EXIT THEN drop
   key RATOM-ADD ;

: RSTR ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

\ RFOLD is RSTR for a name whose stored capitalisation is incidental: it emits
\ the canonical case-folded spelling. Diagnostics name identifiers folded — the
\ rejected word renders as its folded symbol tail, and the used-package
\ collision row prints a package the checker already folded on the way in
\ (checker.f CHECKER-USING). Only a declaring-package name reaches the renderer
\ unfolded, because TFAM-DECL interns whatever the source `package` line typed.
: RFOLD ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ CORE-FOLD-C EMIT1 1 + REPEAT drop ;

: CON-OUT ( n -- ) {: p:n :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CTN @ < and IF                 \ any registered type (built-in OR user-declared CT type)
      p CT-NAME$ dup 0 <> IF RSTR ELSE 2drop 63 EMIT1 THEN
   ELSE 63 EMIT1 THEN THEN ;

: ATOM-REND {: t :}
   t ATOM>K 0 = IF t ATOM>A t ATOM>U RSTR EXIT THEN
   s" fresh-" RSTR
   t ATOM>A t ATOM>U RSTR
   45 EMIT1
   t ATOM>K RATOM-ORD RATOM-CHAR EMIT1 ;

: RNUM ( n -- )                 \ small non-negative number (hidden slot index)
   dup 10 >= IF dup 10 / RECURSE THEN
   10 mod 48 + EMIT1 ;

\ a T-PARAM's stored name span may point into a transient scan buffer (the TKF
\ token-fold buffer for a {: :} annotation, a callee-sig scratch for an
\ on-stack term) that later tokens overwrite before DIAG-PRINT/REC-SIG render.
\ The family id is the term's identity, so the renderer reads the interned
\ registry name instead — qualified `pkg:tail` for a foreign named package;
\ the global "" package and the reserved internal "@" package render the bare
\ tail. An out-of-range id (negative, or >= TFAM-N: a term minted before the
\ registry loads) takes the stored-span fallback; a partial rollback that
\ repurposes a still-in-range slot renders the new occupant's name — no crash,
\ wrong spelling, and unreachable on the command path because a mismatch
\ diagnostic renders inline, before any rollback.
\
\ The package half of that qualified name renders case-folded (RFOLD). The tail
\ half is already canonical lowercase because TFAM-DECL runs TF-REQUIRE-CANON,
\ but the declaring package name is interned exactly as the source `package`
\ line typed it, and that capitalisation carries no meaning: a package is
\ identified case-insensitively everywhere it is compared (TFAM-PKG-MATCH?,
\ TFQ-FOLD-COPY on a signature's `PKG:tail` qualifier, TFAM-QUAL-RESOLVE,
\ SYM-STR=CI, the checker's CAST-OWNER?). Folding on output is what makes one
\ family render one way no matter which file's `package` line the reader is
\ looking at, and it matches how the renderer already spells every other
\ identifier: the rejected word prints as its folded tail, and the used-package
\ collision row prints an already-folded package. The printed `pkg:tail` still
\ names the family back exactly, since the qualifier folds on the way in. Raw
\ source echoes are a different thing and stay verbatim: `token`,
\ `definition_source` and `declared_effect_source` quote what the author wrote.
\ FAM-FOREIGN? below still compares exactly, and that is not an inconsistency:
\ the engine canonicalises a package to its first-registered spelling, so
\ reopening `package MEM` as `package mem` reports `MEM` either way and both
\ sides of this compare read the one interned string.
: FAM-INTERNED? ( n -- f ) {: fam:n :}
   fam 0 >=  fam TFAM-N@ <  and ;
: FAM-FOREIGN? ( n -- f ) {: fam:n :}
   fam TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0 = IF RES-FALSE EXIT THEN
   pa pu s" @" CORE-STR= IF RES-FALSE EXIT THEN
   pa pu TFAM-ACTIVE-PKG$ CORE-STR= 0= ;
: FAM-QNAME-REND ( n -- ) {: fam:n :}    \ interned qualified name: folded pkg:tail if foreign, else bare tail
   fam FAM-FOREIGN? IF fam TFAM-PKG$ RFOLD 58 EMIT1 THEN
   fam TFAM-NAME$ RSTR ;
: FAM-NAME-REND ( n -- ) {: t:n :}
   t PARAM>FAM {: fam:n :}
   fam FAM-INTERNED? 0= IF t PARAM>NAME-A t PARAM>NAME-U RSTR EXIT THEN
   fam FAM-QNAME-REND ;

\ PARAM-HEAD renders a family application's name; QREND adds the argument list.
\ A hidden physical field renders as the diagnostic-only '@family.slotN<args>' /
\ '@family.tag<args>' form (docs §20) and sets RQM so REC-SIG never records a
\ sig containing a lone hidden cell. Full runs never reach here: row rendering
\ (REND-COLLECT / QREND's row mode) compacts them to the logical family type.
: PARAM-HEAD {: t:n :}
   t HIDDEN-PARAM? IF
      1 RQM !
      64 EMIT1
      t FAM-NAME-REND
      46 EMIT1
      t HIDDEN-SLOT@  t PARAM>FAM TFAM-WIDTH@* 1 -  = IF
         s" tag" RSTR
      ELSE
         s" slot" RSTR  t HIDDEN-SLOT@ RNUM
      THEN
   ELSE
      t FAM-NAME-REND
   THEN ;

\ HID-RUN-REST ( n -- n bool ) : from a resolved S-PUSH node whose type is a
\ hidden field, walk the whole run (tag W-1 on top down to slot0, one family).
\ true: row below the full W-cell run (compact to the logical type). false:
\ lone/malformed run — row below the single cell (render the '@' form).
\ HRS: does the run just walked hold a cell `catch` left stale? A stale group is
\ ONE stale logical value — a throw path that clobbered any one of its payload
\ cells left the whole value unusable — so the compaction below wraps the logical
\ type once and the row prints `stale<option<pt>>`, never three cells.
variable HRC  variable HRI  variable HRF  variable HRS
: HID-RUN-CELL? ( n n n -- bool ) {: node:n fam:n slot:n :}
   node TAG S-PUSH <> IF RES-FALSE EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   t CELL>FAM fam <> IF RES-FALSE EXIT THEN
   t HIDDEN-SLOT@ slot <> IF RES-FALSE EXIT THEN
   t TAG T-STALE = IF -1 HRS ! THEN
   RES-TRUE ;
: HID-RUN-REST ( n -- n bool ) {: node:n :}
   node P>TYPE T-RES {: t:n :}
   t CELL>FAM {: fam:n :}
   fam TFAM-WIDTH@* {: w:n :}
   t HIDDEN-SLOT@ w 1 - <> IF node P>REST RES-FALSE EXIT THEN
   node HRC !  -1 HRF !
   t TAG T-STALE = IF -1 ELSE 0 THEN HRS !
   w 1 - HRI !
   BEGIN HRI @ 0 >  HRF @ 0 <>  and WHILE
      HRC @ P>REST R-RES  fam  HRI @ 1 -  HID-RUN-CELL? IF
         HRC @ P>REST R-RES HRC !
      ELSE
         0 HRF !
      THEN
      HRI @ 1 - HRI !
   REPEAT
   HRF @ 0 <> IF HRC @ P>REST RES-TRUE ELSE node P>REST RES-FALSE THEN ;

\ a quot type renders [ in -- out ] or [ in -- out | rin -- rout ] when the
\ quotation has a non-neutral return-stack effect. Rendering is fully recursive
\ to a bounded nesting depth (QDEPTH-MAX levels) with a cycle guard, so a deeply
\ nested quot (combinator over combinator, typed loop/tile combinators) renders
\ in full instead of capping the 3rd level at '?'.
\ Gap2/3: quot-bearing sigs now RECORD as scheme-strings and round-trip, so
\ combinator call sites (dip, keep) are checked against them. Only a genuine '?'
\ (an unmodeled tag, via RQM) still blocks recording — see REC-SIG below.
6 constant QDEPTH-MAX                        \ quotation nesting render budget
create QPATH QDEPTH-MAX 1 + cells allot      \ quot node on the current render path, by depth
create RBIND QDEPTH-MAX cells allot

: QRET? ( n -- bool ) {: q:n :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

\ is quot node r already being rendered above depth d (a type-graph cycle)?
: QANCESTOR? {: r:n d:n :}
   0 BEGIN dup d < WHILE
      dup cells QPATH + @ r = IF drop -1 EXIT THEN
      1 +
   REPEAT drop 0 ;

\ QREND ( x d mode -- ) : one recursive renderer. mode>0 renders a row
\ bottom-to-top (space-separated); mode=0 renders a type. RECURSE re-enters with
\ the mode flag, so nested quots reuse it at depth d+1 up to QDEPTH-MAX.
: QREND ( n n n -- ) {: x:n d:n mode:n :}
   mode 0 > IF
      x R-RES dup TAG S-PUSH = IF                 \ ( node )
         dup P>TYPE T-RES HIDDEN-PARAM? IF        \ hidden run: compact or '@' form (docs §20)
            dup HID-RUN-REST IF                   \ ( node rest ) full run -> logical type
               swap P>TYPE T-RES MK-LOGICAL        \ read HRS before a nested run resets it
               HRS @ IF MK-STALE THEN              \ ( rest logical )
               swap dup R-RES TAG S-PUSH = IF d 1 RECURSE 32 EMIT1 ELSE drop THEN
               d 0 RECURSE
            ELSE                                  \ ( node rest ) lone/malformed -> '@' cell
               drop
               dup P>REST dup R-RES TAG S-PUSH = IF d 1 RECURSE 32 EMIT1 ELSE drop THEN
               P>TYPE d 0 RECURSE
            THEN
         ELSE
            dup P>REST dup R-RES TAG S-PUSH = IF d 1 RECURSE 32 EMIT1 ELSE drop THEN
            P>TYPE d 0 RECURSE
         THEN
      ELSE drop THEN
      EXIT
   THEN
   x T-RES {: r:n :}
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER d 0 RECURSE endof
      \ A cell a caught throw may have overwritten. It names the type it lost so
      \ the diagnostic can say which one, and it sets RQM so REC-SIG never
      \ records a row carrying one: `stale<...>` is not a declarable type.
      T-STALE of
        1 RQM !
        s" stale<" RSTR  r STALE>INNER d 0 RECURSE  62 EMIT1
      endof
      T-QUOT of
        d QDEPTH-MAX <  r d QANCESTOR? 0=  and IF
           r d cells QPATH + !
           91 EMIT1 32 EMIT1  r Q>DIN d 1+ 1 RECURSE
           45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT d 1+ 1 RECURSE
           r QRET? IF
              32 EMIT1 124 EMIT1 32 EMIT1
              r Q>RIN d 1+ 1 RECURSE 45 EMIT1 45 EMIT1 32 EMIT1
              r Q>ROUT d 1+ 1 RECURSE
           THEN
           93 EMIT1
        ELSE 63 EMIT1 THEN
      endof
      T-FORALL of
        NLET @ 23 >= IF
           NLET @ 1 + NLET ! 63 EMIT1
        ELSE RBIND-N @ QDEPTH-MAX < IF
           NLET @ RLETTER {: letter:n :}
           letter RBIND-N @ cells RBIND + !
           NLET @ 1 + NLET !
           r F>DOMAIN BIND-REGION = IF s" forall-region<" ELSE s" forall<" THEN RSTR letter EMIT1
           r F>PARENT dup 0= IF drop ELSE
              s"  inside " RSTR d 1+ 0 RECURSE THEN
           RBIND-N @ 1 + RBIND-N !
           44 EMIT1
           r F>BODY d 1+ 0 RECURSE 62 EMIT1
           RBIND-N @ 1 - RBIND-N !
        ELSE 63 EMIT1 THEN THEN
      endof
      T-BVAR of
        r B>DEPTH 0 >= r B>DEPTH RBIND-N @ < and IF
           RBIND-N @ r B>DEPTH - 1 - cells RBIND + @ EMIT1
        ELSE 63 EMIT1 THEN
      endof
      T-SCOPE of
        1 RQM !
        s" <live-scope-" RSTR r PAY RNUM 62 EMIT1
      endof
      T-ATOM of r ATOM-REND endof
      \ Brackets only around arguments: a family applied to none renders as its
      \ bare name, the spelling a signature uses and SIG-TYPE reads back (a bare
      \ family token builds the same zero-argument application as `name<>`).
      T-PARAM of
        d QDEPTH-MAX <  r d QANCESTOR? 0=  and IF
           r d cells QPATH + !
           r PARAM-HEAD
           r PARAM>ARGC 0 > IF
              60 EMIT1
              0 BEGIN dup r PARAM>ARGC < WHILE
                dup 0 > IF 44 EMIT1 THEN
                r over PARAM>ARG d 1+ 0 RECURSE
                1 +
              REPEAT drop 62 EMIT1
           THEN
        ELSE 63 EMIT1 THEN
      endof
      63 EMIT1
   endcase ;

: REND-TYPE {: t:n :}  t 0 0 QREND ;
64 constant RBUF-CAP                             \ diagnostic-row render budget (values)
create RBUF RBUF-CAP cells allot   variable RBN
variable RSHOW-DST

\ RBUF+ appends one collected row value; a reject diagnostic must fail closed, so
\ a pathological row (>RBUF-CAP values) is truncated here rather than overflowing
\ RBUF into adjacent DATA (which produced garbage type pointers -> SIGSEGV in the
\ later REND-TYPE walk). The checker still rejects with its normal diagnostic + rc.
: RBUF+ ( n -- )
   RBN @ RBUF-CAP >= IF drop EXIT THEN
   RBN @ cells RBUF + !  RBN @ 1 + RBN ! ;

\ REND-COLLECT compacts each full hidden-field run to ONE logical family term
\ (docs §20); a lone/malformed hidden cell stays and renders as its '@' form.
: REND-COLLECT {: s:n :}  0 RBN !  s
   BEGIN R-RES dup TAG S-PUSH = WHILE          \ no locals inside the loop
     dup P>TYPE T-RES HIDDEN-PARAM? IF
        dup HID-RUN-REST IF
           swap P>TYPE T-RES MK-LOGICAL  HRS @ IF MK-STALE THEN  RBUF+
        ELSE
           swap P>TYPE RBUF+
        THEN
     ELSE
        dup P>TYPE RBUF+  P>REST
     THEN
   REPEAT drop ;

\ RENDER ( -- ) : print DCUR's residual stack bottom-to-top, space-separated.
: RENDER  SEEN-RESET RATOM-RESET 0 NLET !  DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

: SHOW-LOCAL-TYPE ( ptr u8 n n -- ) {: name:ptr nameu:n t:n :}
   RDST @ RSHOW-DST !
   0 RDST !
   s" inferred " RSTR
   name nameu RSTR
   s" : " RSTR
   SEEN-RESET RATOM-RESET 0 NLET !
   t REND-TYPE
   10 EMIT1
   RSHOW-DST @ RDST ! ;
: LOCSHOW-INSTALL ( -- ) [: SHOW-LOCAL-TYPE ;] is LOCSHOWXT ;
LOCSHOW-INSTALL

\ REND-SIG ( -- a u ) : render the just-checked word's effect "in -- out" —
\ inputs from the base row's instantiation (BROW), outputs from DCUR.
: REND-SIG
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET RATOM-RESET 0 NLET !
   BROW @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop
   45 EMIT1  45 EMIT1
   DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - 32 EMIT1 dup cells RBUF + @ REND-TYPE REPEAT drop
   0 RDST !  RSBUF RSN @ ;

\ DIAG-PRINT ( -- ) : reject diagnostic, one line to stderr —
\   habu: in NAME: at 'TOK' expected: <row> actual: <row>
\ Rows render bottom-to-top with the shared var-letter naming; expected/actual
\ only appear when the failing unify was captured (STEP/SUNI).
: DTXT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: DROW {: s :}  s REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

\ structured diagnostics: `JSON-DIAGS ON` emits one JSON object per reject or
\ uncheckable verdict for LLM repair.
create JNBUF 20 allot  variable JNV  variable JNN
variable DSUGE  variable DSUGA
: JNUM
   JNV !  0 JNN !
   JNV @ 0= IF 48 EMIT1 EXIT THEN
   BEGIN JNV @ 0 > WHILE
      JNV @ 10 mod 48 +  JNBUF JNN @ + c!
      JNN @ 1 + JNN !
      JNV @ 10 / JNV !
   REPEAT
   JNN @ BEGIN dup 0 > WHILE
      1 - dup JNBUF + c@ EMIT1
   REPEAT drop ;
: JSTR ( ptr u8 n -- )
   JOPEN DTXT JCLOSE ;
: JKEY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u JSTR  58 EMIT1 ;
: JROW {: s :}  JOPEN  s DROW  JCLOSE ;
: SIG-WS? {: c :}  c 32 =  c 9 = or  c 10 = or  c 13 = or ;
: SIG-LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ SIG-WS? 0= IF dup a + u rot - EXIT THEN
      1 +
   REPEAT drop a 0 ;
: SIG-RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u BEGIN dup 0 > WHILE
      a over 1 - + c@ SIG-WS? IF 1 - ELSE a swap EXIT THEN
   REPEAT drop a 0 ;
: SIG-TRIM ( ptr u8 n -- ptr u8 n )  SIG-LTRIM SIG-RTRIM ;
: JEFFECT {: din dout rin rout hasr :}
   JOPEN
   din DROW  s" -- " DTXT  dout DROW
   hasr IF s" | " DTXT  rin DROW  s" -- " DTXT  rout DROW THEN
   JCLOSE ;
\ --- item 9 slice 4: match/construct reason surface (docs §24). The checker
\ latches an MDIAG reason code with the token pin; these words map it to a
\ stable JSON code, repair class, suggestion, and §24 prose. MD-NONEXH also
\ walks the latched (family, seen-bitset, count) to the missing variant NAMES
\ (declaration-order tags index the family's contiguous SUMV rows).
variable MDV-I   variable MDV-F

: MDIAG-CODE$ ( -- ptr u8 n )
   MDIAG @ case
      MD-FAM-UNKNOWN  of s" E-MATCH-UNKNOWN-FAMILY" endof
      MD-FAM-KIND     of s" E-MATCH-FAMILY-KIND" endof
      MD-SCRUT        of s" E-MATCH-SCRUTINEE" endof
      MD-FAM-MISMATCH of s" E-MATCH-FAMILY-MISMATCH" endof
      MD-VAR-UNKNOWN  of s" E-MATCH-UNKNOWN-VARIANT" endof
      MD-VAR-DUP      of s" E-MATCH-DUPLICATE-VARIANT" endof
      MD-MISSING-OF   of s" E-MATCH-MISSING-OF" endof
      MD-NONEXH       of s" E-MATCH-NONEXHAUSTIVE" endof
      MD-STRAY        of s" E-MATCH-STRAY" endof
      MD-TRUNC        of s" E-MATCH-UNTERMINATED" endof
      MD-DEPTH        of s" E-MATCH-DEPTH" endof
      MD-QUOT         of s" E-MATCH-QUOTATION" endof
      MD-OPEN-ARGS    of s" E-MATCH-OPEN-ARGS" endof
      MD-JOIN         of s" E-MATCH-BRANCH-JOIN" endof
      MD-CON-FAM      of s" E-CONSTRUCT-UNKNOWN-FAMILY" endof
      MD-CON-KIND     of s" E-CONSTRUCT-FAMILY-KIND" endof
      MD-CON-VAR      of s" E-CONSTRUCT-UNKNOWN-VARIANT" endof
      MD-CON-TRUNC    of s" E-CONSTRUCT-UNTERMINATED" endof
      MD-DIVBAR       of s" E-DIVERGENT-BARRIER" endof
      MD-EXEC-OPAQUE  of s" E-EXEC-OPAQUE-XT" endof
      MD-CATCH-OPAQUE of s" E-EXEC-OPAQUE-XT" endof
      MD-RAW-PTR      of s" E-RAW-CELL-PTR" endof
      MD-NULL-PTR     of s" E-RAW-CELL-PTR" endof
      MD-DBASE-PTR    of s" E-RAW-CELL-PTR" endof
      MD-RAW-FIELD    of s" E-RAW-CELL-PTR" endof
      MD-RAW-EXEC     of s" E-RAW-CELL-PTR" endof
      MD-STALE-READ   of s" E-STALE-READ" endof
      MD-SCOPE-STORE  of s" E-SCOPED-STORAGE" endof
      MD-SCOPE-KIND   of s" E-SCOPE-TYPE" endof
      MD-C2-COPY      of s" E-C2-COPY" endof
      MD-C2-DROP      of s" E-C2-DROP" endof
      MD-C2-ESCAPE    of s" E-C2-SCOPE-ESCAPE" endof
      MD-UNDERFLOW    of s" E-INPUT-UNDERFLOW" endof
      MD-RIGID-REGION of s" E-RIGID-REGION-MISMATCH" endof
      MD-RIGID-EXTENT of s" E-RIGID-EXTENT-MISMATCH" endof
      MD-RIGID-GEN    of s" E-RIGID-STALE-GENERATION" endof
      MD-RIGID-XDOM   of s" E-RIGID-DOMAIN-CONFUSION" endof
      s" E-REJECTED" rot
   endcase ;

: MDIAG-CLASS$ ( -- ptr u8 n )
   MDIAG @ case
      MD-NONEXH       of s" add_missing_branches" endof
      MD-VAR-DUP      of s" remove_duplicate_branch" endof
      MD-VAR-UNKNOWN  of s" fix_variant_reference" endof
      MD-CON-VAR      of s" fix_variant_reference" endof
      MD-FAM-UNKNOWN  of s" fix_family_reference" endof
      MD-FAM-KIND     of s" fix_family_reference" endof
      MD-CON-FAM      of s" fix_family_reference" endof
      MD-CON-KIND     of s" fix_family_reference" endof
      MD-SCRUT        of s" fix_match_scrutinee" endof
      MD-FAM-MISMATCH of s" fix_match_scrutinee" endof
      MD-QUOT         of s" fix_match_scrutinee" endof
      MD-OPEN-ARGS    of s" fix_match_scrutinee" endof
      MD-JOIN         of s" fix_branch_outputs" endof
      MD-DEPTH        of s" factor_match_nesting" endof
      MD-DIVBAR       of s" move_collective_to_block_uniform_control" endof
      MD-EXEC-OPAQUE  of s" fix_opaque_execute" endof
      MD-CATCH-OPAQUE of s" fix_opaque_execute" endof
      MD-RAW-PTR      of s" declare_pointer_cell" endof
      MD-NULL-PTR     of s" declare_pointer_cell" endof
      MD-DBASE-PTR    of s" declare_pointer_cell" endof
      MD-RAW-FIELD    of s" declare_pointer_cell" endof
      MD-RAW-EXEC     of s" declare_xt_cell" endof
      MD-STALE-READ   of s" keep_value_before_catch" endof
      MD-SCOPE-STORE  of s" keep_scoped_value_on_stack" endof
      MD-SCOPE-KIND   of s" fix_scope_parameter" endof
      MD-C2-COPY      of s" move_exclusive_value" endof
      MD-C2-DROP      of s" consume_exclusive_value" endof
      MD-C2-ESCAPE    of s" keep_borrow_within_scope" endof
      MD-UNDERFLOW    of s" supply_missing_input" endof
      MD-RIGID-REGION of s" fix_host_region" endof
      MD-RIGID-EXTENT of s" fix_host_extent" endof
      MD-RIGID-GEN    of s" fix_stale_generation" endof
      MD-RIGID-XDOM   of s" fix_rigid_domain" endof
      s" fix_match_syntax" rot
   endcase ;

: MDIAG-SUGGEST$ ( -- ptr u8 n )
   MDIAG @ case
      MD-NONEXH       of s" Add an OF branch for every listed variant; v1 has no default branch." endof
      MD-VAR-DUP      of s" Remove the repeated variant branch; each variant may appear once." endof
      MD-VAR-UNKNOWN  of s" Use a variant declared by this family, lowercase as declared." endof
      MD-CON-VAR      of s" Use a variant declared by this family, lowercase as declared." endof
      MD-FAM-UNKNOWN  of s" Name a visible sum family: own package first, else a unique public family." endof
      MD-CON-FAM      of s" construct resolves only families declared in the active package." endof
      MD-FAM-KIND     of s" MATCH eliminates sum or enum families only." endof
      MD-CON-KIND     of s" construct builds sum or enum families only." endof
      MD-SCRUT        of s" Put the family value on top of the stack before MATCH." endof
      MD-FAM-MISMATCH of s" The value on top belongs to a different family; match that family." endof
      MD-QUOT         of s" Move the match out of the quotation; quotation rows cannot carry a scrutinee." endof
      MD-OPEN-ARGS    of s" Instantiate the family arguments to concrete types before matching." endof
      MD-JOIN         of s" Make every branch leave the same stack shape; ;MATCH has one continuation." endof
      MD-DEPTH        of s" Factor the inner match into a named word; control frames are capped." endof
      MD-MISSING-OF   of s" Write `variant OF ... ENDOF` for each branch." endof
      MD-DIVBAR       of s" Call the block collective on the straight-line (block-uniform) path; do not place it inside if/loop/case or a quotation." endof
      MD-EXEC-OPAQUE  of s" Execute an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not execute an xt fetched from untyped memory." endof
      MD-CATCH-OPAQUE of s" Catch an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not catch an xt fetched from untyped memory." endof
      MD-RAW-PTR      of s" Declare the cell that holds an address: PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, or TYPED-BUFFER. A plain variable, create or constant cell holds scalars, roles and atoms only." endof
      MD-NULL-PTR     of s" NULL-PTR is the address of nothing: it compares, subtracts, tests and stores like any pointer, but no value is ever read through it. Take the value from the cell that really holds it." endof
      MD-DBASE-PTR    of s" Reach the cell through a declared accessor instead: give the word that adds the offset a concrete pointee (ptr n, ptr u8), or take the field of a declared pointer cell with ptr-field. data-base addresses no declared element, so a cell of the DATA region holds a plain value at every depth -- it is not the address of an address either." endof
      MD-RAW-EXEC     of s" Declare the cell that holds the execution token: TYPED-VARIABLE NAME [ in -- out ], a TYPED-BUFFER or DYNAMIC-BUFFER of [ in -- out ], or bind a defer with is. An undeclared variable, create, constant or data-base cell holds scalars, roles and atoms only, so what comes back from it is an integer, not code." endof
      MD-RAW-FIELD    of s" Take the pointer field of a declared cell: PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, or TYPED-BUFFER. 0 ptr-field on a plain variable or create cell laundered a raw cell into a typed pointer." endof
      MD-RIGID-REGION of s" These are different host allocations; a value carrying one region's identity cannot stand in for another. Thread the same allocation through, or re-borrow from the target." endof
      MD-RIGID-EXTENT of s" These host allocations have different extents; a bound proved for one does not carry to another. Use the value whose extent identity the position requires." endof
      MD-RIGID-GEN    of s" This index or borrow is from an earlier mutation generation; the container was mutated since. Re-derive the index after the mutation." endof
      MD-RIGID-XDOM   of s" A host region, an extent, and a mutation generation are distinct identities that never interchange. Supply the identity the position requires." endof
      MD-UNDERFLOW    of s" Push the missing inputs before the call, or declare them in the signature; a definition may not consume below its declared inputs." endof
      MD-STALE-READ   of s" `catch` puts the stacks back to the DEPTH it was entered at and never to their contents, so a cell the caught body may have written on a throw path holds an unknown machine word afterwards. Bind the value to a local BEFORE the catch and use the local, or drop the cell. A stale cell may still be moved, dropped, or bound to an untyped local." endof
      MD-SCOPE-STORE  of s" Keep the scoped value on the stack or in a local; ordinary memory has no scope dependency declaration." endof
      MD-SCOPE-KIND   of s" Use a scope parameter only in a declared scope slot; it is neither a value type nor an allocation identity." endof
      MD-C2-COPY      of s" Move the exclusive value once instead of copying it." endof
      MD-C2-DROP      of s" Return or consume the exclusive value instead of discarding it." endof
      MD-C2-ESCAPE    of s" Return only values independent of the owner or loan opened by this operation." endof
      s" Complete the form: MATCH family, variant OF ... ENDOF per variant, ;MATCH." rot
   endcase ;

: MDIAG-REASON$ ( -- ptr u8 n )
   MDIAG @ case
      MD-FAM-UNKNOWN  of s" bad match: unknown type family" endof
      MD-FAM-KIND     of s" bad match: family is not a sum or enum" endof
      MD-SCRUT        of s" bad match: expected sum or enum value on stack" endof
      MD-FAM-MISMATCH of s" bad match: family mismatch" endof
      MD-VAR-UNKNOWN  of s" bad match: unknown variant" endof
      MD-VAR-DUP      of s" bad match: duplicate variant" endof
      MD-MISSING-OF   of s" bad match: variant token must be followed by OF" endof
      MD-NONEXH       of s" bad match: missing variants:" endof
      MD-STRAY        of s" bad match: misplaced match token" endof
      MD-TRUNC        of s" bad match: unterminated match" endof
      MD-DEPTH        of s" bad match: nesting exceeds control-frame capacity" endof
      MD-QUOT         of s" bad match: quotation rows cannot carry a scrutinee" endof
      MD-OPEN-ARGS    of s" bad match: scrutinee type arguments are unresolved" endof
      MD-JOIN         of s" bad match: branch output mismatch" endof
      MD-CON-FAM      of s" bad construct: family not declared in the active package" endof
      MD-CON-KIND     of s" bad construct: family is not a sum or enum" endof
      MD-CON-VAR      of s" bad construct: unknown variant" endof
      MD-CON-TRUNC    of s" bad construct: missing family or variant token" endof
      MD-DIVBAR       of s" divergent barrier: block collective requires block-uniform control" endof
      MD-EXEC-OPAQUE  of s" execute: opaque xt of unknown provenance (fetched from untyped memory)" endof
      MD-CATCH-OPAQUE of s" catch: opaque xt of unknown provenance (fetched from untyped memory)" endof
      MD-RAW-PTR      of s" raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell" endof
      MD-NULL-PTR     of s" null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one" endof
      MD-DBASE-PTR    of s" base address: a cell reached from data-base, or from a pointer computed off NULL-PTR, holds a plain value at every pointee depth, never a nominal type or a pointer" endof
      MD-RAW-FIELD    of s" ptr-field: base is an undeclared raw storage cell, not a declared pointer cell" endof
      MD-RAW-EXEC     of s" raw storage cell: an undeclared cell cannot hold an execution token / a quotation" endof
      MD-STALE-READ   of s" stale cell: this reads a cell `catch` left stale (a throw path of the caught body may have overwritten it)" endof
      MD-SCOPE-STORE  of s" scoped storage: ordinary memory cannot retain a scope dependency" endof
      MD-SCOPE-KIND   of s" scope parameter: a scope identity cannot stand in for a value type" endof
      MD-C2-COPY      of s" exclusive value copied" endof
      MD-C2-DROP      of s" exclusive value discarded" endof
      MD-C2-ESCAPE    of s" scoped value escapes its owner or loan" endof
      MD-RIGID-REGION of s" rigid host: region mismatch (different allocation)" endof
      MD-RIGID-EXTENT of s" rigid host: extent mismatch (different bounds identity)" endof
      MD-RIGID-GEN    of s" rigid host: stale mutation generation" endof
      MD-RIGID-XDOM   of s" rigid host: identity domain confusion" endof
      MD-UNDERFLOW    of s" input underflow: the call takes more cells than the definition's declared inputs leave" endof
      s" bad match: rejected" rot
   endcase ;

: MDIAG-MISSING-WALK ( bool -- ) {: json:bool :}   \ unseen variant tails, space-led
   MDIAG-FAM @ TFAM-VAR-START@ {: vstart:n :}
   0 MDV-I !  0 MDV-F !
   BEGIN MDV-I @ MDIAG-VCNT @ < WHILE
      MDIAG-SEEN @ MDV-I @ MSEEN-GET 0= IF
         json MDV-F @ 0= and 0= IF s"  " DTXT THEN
         vstart MDV-I @ + SUMV-NAME$ DTXT
         -1 MDV-F !
      THEN
      MDV-I @ 1 + MDV-I !
   REPEAT ;

: MDIAG-MISSING-JSTR ( -- )
   JOPEN  0 0= MDIAG-MISSING-WALK  JCLOSE ;

: MDIAG-MISSING-PROSE ( -- )
   0 0= 0= MDIAG-MISSING-WALK ;

\ The underflow shortfall, latched with the reason. Digits and spaces only, so
\ the one word serves the prose line and the inside of the JSON reason string.
: MDIAG-UF-COUNTS ( -- )
   s"  (needs " DTXT  MDIAG-NEED @ JNUM
   s" , has " DTXT  MDIAG-HAVE @ JNUM  s" )" DTXT ;

: IMM-CODE$ ( -- ptr u8 n )
   s" E-UNMODELED-IMMEDIATE" ;

: IMM-CLASS$ ( -- ptr u8 n )
   s" model_compile_immediate" ;

: IMM-SUGGEST$ ( -- ptr u8 n )
   s" Declare a stack-neutral parsing immediate with parse-imm, or remove it from the compiled body." ;

\ The three LOCALBAD kinds (checker.f LOC-REJECT) carry their own code, class
\ and text: an over-wide name and an over-count group name the exceeded limit,
\ the shape reject keeps its placement text.
: LOCALBAD-CODE$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" E-LOCAL-NAME-TOO-LONG" EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" E-TOO-MANY-LOCALS" EXIT THEN
   s" E-BAD-LOCAL-SHAPE" ;

: LOCALBAD-CLASS$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" shorten_local_name" EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" reduce_local_count" EXIT THEN
   s" factor_local_shape" ;

: LOCALBAD-SUGGEST$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" Shorten the local name to at most 16 bytes." EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" Bind at most 64 locals in one definition, or factor a helper." EXIT THEN
   s" Move locals to a live top-level path or factor a helper." ;

\ One diagnostic line for a LOCALBAD reject; FAILTK is the declaration token
\ pinned by the first failure, so a later body reference to the rejected name
\ cannot relabel the reject as an undefined word.
: LOCALBAD-PROSE ( -- )
   LOCALBAD-CODE$ DTXT  s"  habu: in " DTXT  NMA @ NMU @ DTXT
   LOCALBAD-KIND @ 2 = IF
     s" : local '" DTXT  FAILTK FAILTU @ DTXT  s" ' has a " DTXT  LOCALBAD-LEN @ JNUM
     s" -byte name; the limit is " DTXT  LOC-NAME-W JNUM  s"  bytes" DTXT EXIT
   THEN
   LOCALBAD-KIND @ 1 = IF
     s" : local '" DTXT  FAILTK FAILTU @ DTXT  s" ' is one over the " DTXT  LOC-CAP JNUM
     s"  locals a definition may bind" DTXT EXIT
   THEN
   s" : at '" DTXT  FAILTK FAILTU @ DTXT
   s" ': a local cannot be bound or referenced inside a quotation, or bound on a dead path" DTXT ;

: DCODE
   IMMERR @ if IMM-CODE$ exit then
   NPBAD @ IF s" E-NONPARAMETRIC-EFFECT" ELSE
   CAPREQ @ IF s" E-CAP-TRUSTED" ELSE
   UNSAFE @ IF s" E-UNSAFE" ELSE
   LOCALBAD @ IF LOCALBAD-CODE$ ELSE
   LINLOCBAD @ IF s" E-LINEAR-LOCAL" ELSE
   MDIAG @ 0 <> IF MDIAG-CODE$ ELSE
   DEADERR @ IF s" E-DEAD-CODE" ELSE
   QUALBAD @ IF s" E-BAD-QUALIFIED" ELSE
   UNDEFERR @ IF s" E-UNDEFINED" ELSE
   DVERD @ 1 = IF s" E-UNCHECKABLE" ELSE
   SGBAD @ IF SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" ELSE SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" ELSE SGBAD-ARITY? IF s" E-WRONG-ARITY" ELSE s" E-BAD-SIGNATURE" THEN THEN THEN ELSE
   DEXP @ 0 <> IF s" E-MISMATCH" ELSE s" E-REJECTED" THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN ;
: DVERDICT ( -- ptr u8 n )
   UNDEFERR @ IF
      s" rejected"
   ELSE
      DVERD @ 1 = IF s" uncheckable" ELSE s" rejected" THEN
   THEN ;
: RETURN-BORROWED? ( -- f )   \ the body bound the return tail below its declared frame
   SGRBASE @ dup 0= IF drop RES-FALSE EXIT THEN ROW-OPEN? 0= ;
: RETURN-MISMATCH? ( -- f )
   SGHASR @ IF
      RCUR @ R-RES  SGROUT @ R-RES  <>
   ELSE
      RCUR @ R-RES  RBROW @ R-RES  <>
   THEN ;
: REPAIR-CLASS ( -- ptr u8 n )
   IMMERR @ if IMM-CLASS$ exit then
   NPBAD @ IF s" fix_parametric_effect" EXIT THEN
   CAPREQ @ IF s" trusted_boundary_required" EXIT THEN
   UNSAFE @ IF s" trusted_boundary_required" EXIT THEN
   LOCALBAD @ IF LOCALBAD-CLASS$ EXIT THEN
   LINLOCBAD @ IF s" factor_linear_local" EXIT THEN
   MDIAG @ 0 <> IF MDIAG-CLASS$ EXIT THEN
   DEADERR @ IF s" remove_dead_code" EXIT THEN
   QUALBAD @ IF s" fix_qualified_name" EXIT THEN
   UNDEFERR @ IF s" unknown_rejection" EXIT THEN
   DVERD @ 1 = IF s" rewrite_uncheckable" EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF s" fix_signature_type" ELSE SGBAD-BAREPTR? IF s" fix_bare_ptr_element" ELSE SGBAD-ARITY? IF s" fix_signature_arity" ELSE s" fix_signature_syntax" THEN THEN THEN
      EXIT
   THEN
   RETURN-BORROWED? IF s" fix_return_stack" EXIT THEN
   RETURN-MISMATCH? IF s" fix_return_stack" EXIT THEN
   DEXP @ 0= IF
      s" unknown_rejection" EXIT
   THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF s" remove_producer" ELSE
   DSUGA @ DSUGE @ < IF s" add_producer" ELSE
   s" fix_type" THEN THEN ;
\ Short repair hint derived from the stable class. Raw stack rows stay in their
\ own JSON fields; this text is only for LLM action selection.
: SUGGEST-TEXT ( -- ptr u8 n )
   IMMERR @ if IMM-SUGGEST$ exit then
   NPBAD @ IF
      NPBAD-KIND @ 4 = IF
         s" Declare the pointee this base address really reaches (ptr n, ptr u8, ...); a pointer derived from data-base cannot be published under a type variable, nor handed to one a caller instantiates." EXIT
      THEN
      NPBAD-KIND @ 5 = IF
         s" Declare the pointee this null stands in for (ptr n, ptr u8, ...); NULL-PTR may be stored through a declared parameter, but not published under a type variable." EXIT
      THEN
      NPBAD-KIND @ 3 = IF
         s" Declare the concrete storage type, or keep the body polymorphic over the type variable." EXIT
      THEN
      NPBAD-KIND @ 0= IF
         s" Declare the concrete family in the signature, or keep the body polymorphic over the type variable."
      ELSE NPBAD-KIND @ 1 = IF
         s" Keep each declared type variable distinct; do not unify two quantifiers in the body."
      ELSE
         s" Bind the minted phantom's type arguments to the inputs, or mint it behind an audited TRUSTED: boundary."
      THEN THEN EXIT
   THEN
   CAPREQ @ IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   UNSAFE @ IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   LOCALBAD @ IF LOCALBAD-SUGGEST$ EXIT THEN
   LINLOCBAD @ IF s" Keep the linear value on the stack; do not bind it to a local." EXIT THEN
   MDIAG @ 0 <> IF MDIAG-SUGGEST$ EXIT THEN
   DEADERR @ IF s" Remove tokens after the terminating control word, or move the work before it." EXIT THEN
   QUALBAD @ IF s" Use one ':' qualifier, e.g. PKG:WORD." EXIT THEN
   UNDEFERR @ IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   DVERD @ 1 = IF s" Rewrite with modeled words or isolate an audited primitive." EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF
         s" Use a known stack-signature type or a single-letter type variable."
      ELSE SGBAD-BAREPTR? IF
         s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'."
      ELSE SGBAD-ARITY? IF
         s" Give the type family its exact declared number of arguments."
      ELSE
         s" Repair the stack-effect comment syntax, including --."
      THEN THEN THEN
      EXIT
   THEN
   RETURN-BORROWED? IF s" Read or pop only return-row cells this definition pushed with >r or declared after |; below them lies the caller's frame." EXIT THEN
   RETURN-MISMATCH? IF s" Balance return-stack transfers before the definition exits." EXIT THEN
   DEXP @ 0= IF
      s" Inspect the token, signature, and raw stack evidence." EXIT
   THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF  s" Remove an extra producer or drop the surplus value."
   ELSE DSUGA @ DSUGE @ < IF  s" Add the missing producer or stop consuming a required value."
   ELSE  s" Change the body so produced types match the signature."
   THEN THEN ;
\ A packet's position: the file line, column and byte of its token's first byte,
\ and the byte after its last.
variable JLOC-L  variable JLOC-C  variable JLOC-B  variable JLOC-E
\ Locate the token [a, e) in the file (src/core/checker.f DIAG-LOCATE); false
\ when its text has no map. The end needs only its byte: the file byte of the
\ source's first byte plus the end's offset in the source.
: JLOCATE ( ptr u8 ptr u8 -- bool )
   {: a:ptr e:ptr :}
   e DIAG>SRC 0= IF drop 0 0= 0= EXIT THEN
   DSRC-B @ + JLOC-E !
   a DIAG-LOCATE 0= IF drop drop drop 0 0= 0= EXIT THEN
   JLOC-B !  JLOC-C !  JLOC-L !
   0 0= ;
\ The checked token with no map: the definition's origin plus the token's
\ offset in the checked text.
: JLOC-ORIGIN ( -- )
   TBASE@  TBASE@ FAILB @ +  DIAGL0 @ DIAGC0 @ DIAGB0 @  DIAG-POS
   JLOC-B !  JLOC-C !  JLOC-L !
   DIAGB0 @ FAILE @ + JLOC-E ! ;
: JLOC-FIELDS ( -- )
   s" line" JKEY  JLOC-L @ JNUM  44 EMIT1
   s" column" JKEY  JLOC-C @ JNUM  44 EMIT1
   s" byte_start" JKEY  JLOC-B @ JNUM  44 EMIT1
   s" byte_end" JKEY  JLOC-E @ JNUM  44 EMIT1 ;
\ The position fields of a packet that names its token by pointer, present only
\ when the token locates in the file.
: JTOKEN-FIELDS ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 > IF
      a  a u +  JLOCATE IF JLOC-FIELDS THEN
   THEN ;
\ The position fields of the checked definition's name token, where the
\ scanner read it.
: JNAME-FIELDS ( -- )
   NMOFF @ TADDR NMU @ JTOKEN-FIELDS ;
: NP-FAM-REND ( -- )   \ append the specialized family's qualified name to the diagnostic
   NPBAD-TERM @ NP-FAM {: fam:n :}
   fam 0 >= IF s" family '" DTXT  fam FAM-QNAME-REND  s" '" DTXT
   ELSE s" a concrete type" DTXT THEN ;
: DIAG-PROSE
   IMMERR @ if
     IMM-CODE$ DTXT  s"  habu: in " DTXT  NMA @ NMU @ DTXT
     s" : compile-time immediate '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is not a stack-neutral parsing immediate; declare it with parse-imm or remove it from the compiled body" DTXT exit
   then
   NPBAD @ IF
     s" E-NONPARAMETRIC-EFFECT habu: in " DTXT  NMA @ NMU @ DTXT
     NPBAD-KIND @ 4 = IF
       s" : declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' is restricted to a base address -- the DATA region, or a pointer computed off NULL-PTR; its declared kind must stay unchanged" DTXT EXIT
     THEN
     NPBAD-KIND @ 5 = IF
       s" : declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' is restricted to the null address; its declared kind must stay unchanged" DTXT EXIT
     THEN
     NPBAD-KIND @ 3 = IF
       s" : declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' is restricted by raw storage; its declared kind must stay unchanged" DTXT EXIT
     THEN
     NPBAD-KIND @ 0= IF
       s" : declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' is specialized to " DTXT  NP-FAM-REND
       s" ; a declared effect must stay parametric over its quantifier" DTXT
     ELSE NPBAD-KIND @ 1 = IF
       s" : declared type variables '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' and '" DTXT  NPBAD-Q2 @ EMIT1
       s" ' are unified; each declared quantifier must stay a distinct variable" DTXT
     ELSE
       s" : declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
       s" ' is minted into " DTXT  NP-FAM-REND
       s"  but is unbound in the inputs; a checked definition cannot mint a phantom of input-unrelated type — use an audited TRUSTED: boundary" DTXT
     THEN THEN EXIT
   THEN
   CAPREQ @ IF
     s" E-CAP-TRUSTED habu: in " DTXT  NMA @ NMU @ DTXT
     s" : '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is a trust-boundary primitive; call it only from a TRUSTED: definition" DTXT EXIT
   THEN
   SGBAD-UNKNOWN? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : unknown type '" DTXT
     FAILTK FAILTU @ DTXT  s" ' in signature" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT
     s" : 'ptr' needs an element type, e.g. 'ptr u8' or 'ptr a'" DTXT EXIT
   THEN
   SGBAD-ARITY? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : wrong arity for type family '" DTXT
     FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   LOCALBAD @ IF LOCALBAD-PROSE EXIT THEN
   QUALBAD @ IF
     s" E-BAD-QUALIFIED habu: in " DTXT  NMA @ NMU @ DTXT
     s" : malformed qualified name '" DTXT  FAILTK FAILTU @ DTXT
     s" ' (more than one ':')" DTXT EXIT
   THEN
   UNDEFERR @ IF
     s" E-UNDEFINED habu: in " DTXT  NMA @ NMU @ DTXT
     s" : undefined word '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   LINLOCBAD @ IF
     s" E-LINEAR-LOCAL habu: in " DTXT  NMA @ NMU @ DTXT
     s" : linear value cannot be bound to a local; keep it on the stack" DTXT EXIT
   THEN
   s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
   MDIAG @ 0 <> IF
     s"  " DTXT  MDIAG-REASON$ DTXT
     MDIAG @ MD-NONEXH = IF MDIAG-MISSING-PROSE THEN
     MDIAG @ MD-UNDERFLOW = IF MDIAG-UF-COUNTS THEN
   THEN
   DEADERR @ IF s"  after '" DTXT DEADTA @ DEADTU @ DTXT s" '" DTXT THEN
   DEXP @ 0 <> IF
     s"  expected: " DTXT  DEXP @ DROW
     s" actual: " DTXT  DACT @ DROW THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
     s"  actual: " DTXT  DF-ACT @ REND-TYPE THEN ;

\ ADT family field (item 13): the exact failed type pair captured by U-FAIL.
\ Expected takes precedence over actual; unrelated matched row cells cannot leak
\ into the diagnostic, and a scalar pair emits no family. The hint carries the
\ interned qualified spelling (FAM-QNAME-REND: pkg:tail for a foreign package,
\ bare tail for the global/internal package) so it matches the expected/actual
\ rows and resolves from the failing definition's scope. Family and package
\ names are registry-validated lowercase identifiers, so the quoted raw emit
\ needs no JSON escaping (the JROW pattern).
: TERM-FAM ( n -- n )                    \ layout-family id for one type term, else -1
   T-RES dup LAYOUT-PARAM? IF PARAM>FAM EXIT THEN
   drop -1 ;
\ ADT variant field (item 13): the sum-variant/tag captured by the checker at the
\ mismatch point (DVAR = SUMV id, -1 = none). A `construct family variant` payload
\ mismatch latches the variant being built; a pure-scalar or non-construct
\ mismatch leaves DVAR -1 and emits no variant/tag.
: DIAG-FAM-ID ( n n -- n )               \ term family: expected precedence, then actual, else the captured variant's family
   {: efam:n afam:n :}
   efam 0 >= IF efam EXIT THEN
   afam 0 >= IF afam EXIT THEN
   DVAR @ dup 0 >= IF SUMV-FAM@ THEN ;
\ Payload slot (item 13): the checker's DPOS is the slot-from-top of the failed
\ expected-row element; the packet reports the declaration-order payload index
\ (0-based, first declared payload = 0). Absent when the position is unknown or
\ the failure was not inside the variant's payload cells.
: DIAG-PAYLOAD-POS ( -- )                \ "payload_pos":<decl-order slot> when captured
   DPOS @ 0 < IF EXIT THEN
   DVAR @ SUMV-PAY-N {: cnt:n :}
   DPOS @ cnt < 0= IF EXIT THEN
   cnt 1 - DPOS @ - {: pos:n :}
   44 EMIT1 s" payload_pos" JKEY  pos JNUM
   DVAR @ pos SUMV-PAY-FIELD IF
      44 EMIT1 s" field" JKEY  PF-NAME$ JSTR
   ELSE drop THEN ;
: DIAG-VARIANT ( -- )                    \ "variant":"<name>","tag":<n> for the captured arm
   DVAR @ 0 < IF EXIT THEN
   44 EMIT1 s" variant" JKEY  DVAR @ SUMV-NAME$ JSTR
   44 EMIT1 s" tag" JKEY  DVAR @ SUMV-TAG@ JNUM
   DIAG-PAYLOAD-POS ;
: DIAG-FAMILY ( -- )
   DF-EXP @ TERM-FAM  DF-ACT @ TERM-FAM  DIAG-FAM-ID {: fam:n :}
   fam 0 >= IF
      44 EMIT1 s" family" JKEY  JOPEN fam FAM-QNAME-REND JCLOSE
   THEN
   DIAG-VARIANT ;
: DIAG-JSON
   TBASE@ FAILB @ +  TBASE@ FAILE @ +  JLOCATE 0= IF JLOC-ORIGIN THEN
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY   DCODE JSTR  44 EMIT1
   s" repair_class" JKEY REPAIR-CLASS JSTR  44 EMIT1
   s" verdict" JKEY DVERDICT JSTR  44 EMIT1
   s" word" JKEY   NMA @ NMU @ JSTR   44 EMIT1
   s" token" JKEY  FAILTK FAILTU @ JSTR  44 EMIT1
   DEADERR @ IF s" dead_owner" JKEY DEADTA @ DEADTU @ JSTR 44 EMIT1 THEN
   MDIAG @ 0 <> IF
     s" reason" JKEY
     \ The shortfall belongs IN the reason, so the underflow builds its own
     \ string from the reason and its counts.
     MDIAG @ MD-UNDERFLOW = IF
       JOPEN  MDIAG-REASON$ DTXT  MDIAG-UF-COUNTS  JCLOSE
     ELSE MDIAG-REASON$ JSTR THEN
     44 EMIT1
     MDIAG @ MD-NONEXH = IF s" missing_variants" JKEY MDIAG-MISSING-JSTR 44 EMIT1 THEN
   THEN
   s" token_index" JKEY  FAILIX @ JNUM  44 EMIT1
   s" file" JKEY  DIAGFB DIAGFU @ JSTR  44 EMIT1
   JLOC-FIELDS
   s" definition_source" JKEY  TBASE @ TBLEN @ JSTR  44 EMIT1
   SGSEEN @ IF
     s" declared_effect" JKEY
     SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ JEFFECT  44 EMIT1
     s" declared_effect_source" JKEY
     SGA @ SGU @ SIG-TRIM JSTR  44 EMIT1
   THEN
   s" inferred_effect" JKEY
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT  44 EMIT1
   s" return_stack" JKEY
   123 EMIT1
   s" expected" JKEY  SGHASR @ IF SGROUT @ ELSE RBROW @ THEN JROW  44 EMIT1
   s" actual" JKEY    RCUR @ JROW
   125 EMIT1
   DEXP @ 0 <> IF
     44 EMIT1 s" expected" JKEY DEXP @ JROW
     44 EMIT1 s" actual"   JKEY DACT @ JROW
     DIAG-FAMILY THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
      44 EMIT1 s" actual_type" JKEY 34 EMIT1 DF-ACT @ REND-TYPE 34 EMIT1 THEN
   NPBAD @ IF                                             \ non-parametric declared effect
     44 EMIT1 s" quantifier" JKEY JOPEN NPBAD-Q1 @ EMIT1 JCLOSE
     NPBAD-KIND @ 1 = IF
       44 EMIT1 s" quantifier2" JKEY JOPEN NPBAD-Q2 @ EMIT1 JCLOSE
     ELSE
       NPBAD-TERM @ NP-FAM dup 0 >= IF
         44 EMIT1 s" family" JKEY JOPEN FAM-QNAME-REND JCLOSE
       ELSE drop THEN
     THEN
   THEN
   SGBAD-ARITY? IF                                        \ item 13: E-WRONG-ARITY counts
     44 EMIT1 s" arity_expected" JKEY SGBAD-AR-DECL @ JNUM
     44 EMIT1 s" arity_actual" JKEY SGBAD-AR-GOT @ JNUM THEN
   44 EMIT1 s" suggestion" JKEY SUGGEST-TEXT JSTR
   125 EMIT1 ;                                            \ }
: DIAG-PRINT
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !
   JSON-DIAGS @ IF DIAG-JSON ELSE DIAG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: DIAG-PRINT-INSTALL ( -- ) [: DIAG-PRINT ;] is DIAGXT ;
DIAG-PRINT-INSTALL

\ --- bad stored-signature diagnostics (multi-error TRUST rows; USIG-ADD-BAD).
\ SGBAD state from the failed parse is still live, so class + suggestion mirror
\ REPAIR-CLASS's signature arm (same stable strings). The JSON is a refused
\ record (docs/repair-diagnostics.md): the row's name is its token, and the
\ signature as written the field its code adds.
: BADSIG-CLASS ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" fix_signature_type" EXIT THEN
   SGBAD-BAREPTR? IF s" fix_bare_ptr_element" EXIT THEN
   SGBAD-ARITY? IF s" fix_signature_arity" EXIT THEN
   s" fix_signature_syntax" ;
: BADSIG-SUGGEST ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" Use a known stack-signature type or a single-letter type variable." EXIT THEN
   SGBAD-BAREPTR? IF s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." EXIT THEN
   SGBAD-ARITY? IF s" Give the type family its exact declared number of arguments." EXIT THEN
   s" Repair the stack-effect comment syntax, including --." ;
: BADSIG-JSON ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-STORED-SIGNATURE" JSTR 44 EMIT1
   s" repair_class" JKEY BADSIG-CLASS JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY na nu JSTR 44 EMIT1
   s" signature" JKEY sa su SIG-TRIM JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY BADSIG-SUGGEST JSTR
   125 EMIT1 ;                                            \ }
: BADSIG-PROSE ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   s" habu: in " DTXT  na nu DTXT  s" : bad stored signature '" DTXT
   sa su SIG-TRIM DTXT  s" '" DTXT ;
: BADSIG-DIAG ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   1 RDST !  0 RSN !
   sa su na nu JSON-DIAGS @ IF BADSIG-JSON ELSE BADSIG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: BADSIG-DIAG-INSTALL ( -- ) [: BADSIG-DIAG ;] is BADSIG-XT ;
BADSIG-DIAG-INSTALL

\ --- top-level type-family declaration diagnostics (PLAN item 6). A bad
\ NEWTYPE/SUMTYPE reports a declaration-shaped packet: decl kind, family,
\ offending token, and reason — with NO invented definition fields (no
\ declared_effect, definition_source, or return_stack; docs/type-families.md
\ §24). The token's position follows `file` when it locates in the file.
: TDECL-SUGGEST$ ( -- ptr u8 n )
   s" Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks." ;
: TDECL-DIAG-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-DECLARATION" JSTR 44 EMIT1
   s" repair_class" JKEY s" fix_family_declaration" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" decl" JKEY ka ku JSTR 44 EMIT1
   s" family" JKEY fa fu JSTR 44 EMIT1
   s" token" JKEY ta tu JSTR 44 EMIT1
   s" reason" JKEY wa wu JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   ta tu JTOKEN-FIELDS
   s" suggestion" JKEY TDECL-SUGGEST$ JSTR
   125 EMIT1 ;                                            \ }
: TDECL-DIAG-PROSE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   s" habu: bad " DTXT  ka ku DTXT  s"  declaration '" DTXT  fa fu DTXT
   s" ': " DTXT  wa wu DTXT
   tu 0 > IF s"  at '" DTXT  ta tu DTXT  s" '" DTXT THEN ;
: TDECL-DIAG ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   ka ku fa fu ta tu wa wu
   JSON-DIAGS @ IF TDECL-DIAG-JSON ELSE TDECL-DIAG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ REC-SIG ( ptr u8 n -- ) : record a certified sig-less word. Refuses
\ (conservatively, the word stays unrecorded) on unknown tags or absurd var
\ counts — and reports which word and why, since callers otherwise fail later
\ as undefined with no hint that the producer was the problem.
: REC-REFUSE-WHY ( -- ptr u8 n )
   \ RLETTER emits '?' when names run out; the count distinguishes that
   \ placeholder from an unmodeled type tag.
   NLET @ 24 >= IF
      s" more than 23 type variables or binders in inferred effect" EXIT
   THEN
   s" unmodeled type tag in inferred effect" ;

: REC-REFUSE-PROSE ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   s" habu: in " DTXT  na nu DTXT
   s" : effect not recorded: " DTXT  wa wu DTXT ;

\ A warning, outside the repair contract (docs/repair-diagnostics.md): the word
\ loaded, so the JSON carries no verdict or repair class.
: REC-REFUSE-JSON ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" W-EFFECT-NOT-RECORDED" JSTR 44 EMIT1
   s" word" JKEY na nu JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   JNAME-FIELDS
   s" reason" JKEY wa wu JSTR
   125 EMIT1 ;

: REC-REFUSE-EMIT ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   na nu wa wu JSON-DIAGS @ IF REC-REFUSE-JSON ELSE REC-REFUSE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ The checker writes a warning, a packet with no verdict, while WARN-DIAGS is
\ on. tools/check.f's run turns it off: the run loads what the check before it
\ checked, and that check wrote the warnings with their files and positions.
variable WARN-DIAGS   -1 WARN-DIAGS !

: REC-REFUSE-DIAG ( ptr u8 n -- )
   WARN-DIAGS @ 0= IF 2drop EXIT THEN
   REC-REFUSE-WHY REC-REFUSE-EMIT ;

\ The render is no longer thrown away. It was here only to detect unmodeled tags
\ and an absurd var count, and it is ALSO the text an AOT capture has to carry
\ for a word whose effect was inferred rather than declared - the other arm of
\ CHECK's fork hands the declared text straight to CHECKER-USIG-CERT-ADD. The
\ capture runs after the record exists, because it reads the symbol that record
\ was written under; when nothing is armed it is a single flag test.
: REC-SIG ( ptr u8 n -- ) {: na:ptr nu:n :}
   REND-SIG {: sa:ptr su:n :}
   RQM @ 0 =  NLET @ 24 <  and IF
      na nu CHECKER-USIG-CERT-CURRENT
      sa su CHECKER-ASIG-CAPTURE
      EXIT
   THEN
   na nu REC-REFUSE-DIAG ;
: REC-SIG-INSTALL ( -- ) [: REC-SIG ;] is RECXT ;
REC-SIG-INSTALL

\ --- global-vs-used-public shadow diagnostic (dot habu-err-on-global-e62f806c).
\ CHECKER-USED-SHADOW captured the ambiguous bare token and the two colliding
\ candidates (global and used public); this renders the reference-site reject
\ naming both with their effect arity so the author knows to qualify (PKG:WORD)
\ or rename the collision. The sym effects read through the read-only USIGS
\ accessors (CHECKER-FIND-USIG-SYM / E-DIN@ / E-DOUT@ / EFF-ROW-N), never through
\ the bare-name resolver, so rendering cannot re-trigger the shadow throw.
: USH-EFF ( n -- )                        \ append " (Din -- Dout)" for a sym's effect, " (?)" if none
   {: sym:n :}
   sym CHECKER-FIND-USIG-SYM 0= IF s"  (?)" DTXT EXIT THEN
   s"  (" DTXT
   FEP @ E-DIN@ EFF-ROW-N RNUM
   s"  -- " DTXT
   FEP @ E-DOUT@ EFF-ROW-N RNUM
   41 EMIT1 ;
: USHADOW-PROSE ( -- )
   s" E-USING-SHADOW-GLOBAL habu: bare '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" ' is ambiguous under using: the global '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1
   USH-GSYM @ USH-EFF
   s"  and used public '" DTXT  USH-PKG-A @ USH-PKG-U @ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1
   USH-USYM @ USH-EFF
   s"  export the same name; qualify " DTXT  USH-PKG-A @ USH-PKG-U @ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT
   s"  for the package word, or rename the collision to reach the global" DTXT ;
\ The used packages a bare token resolves in: the used-scan slots
\ CHECKER-USED-SYM marked in CK-USED-MASK, in the order the using scan reads
\ them, each package once however often it is used.
: UPKG-MATCHED? ( n -- bool )             \ used-scan slot n exports the token
   1 swap lshift CK-USED-MASK @ and 0 <> ;
: UPKG-NAME$ ( n -- ptr u8 n )            \ slot n's folded package name
   dup CK-USE-SLOT swap CK-USE-LEN@ ;
: UPKG-FIRST? ( n -- bool )               \ slot n matched and no earlier slot names its package
   {: slot:n :}
   slot UPKG-MATCHED?
   slot 0 ?DO
      i UPKG-MATCHED? IF  i UPKG-NAME$ slot UPKG-NAME$ CORE-STR= IF drop RES-FALSE THEN  THEN
   LOOP ;
: UPKG-EACH ( [ n bool -- ] -- )          \ XT on each package's slot, true the first time
   {: xt :}
   RES-TRUE
   CK-USE-MAX 0 ?DO
      i UPKG-FIRST? IF  i over xt execute  drop RES-FALSE  THEN
   LOOP drop ;
\ The packet of a bare token refused at its reference under `using`, this
\ refusal's and the ambiguity's below: the token where the file holds it and,
\ as `used_packages`, each used package it resolves in. For the shadow that is
\ one package: every slot that matched holds its one used public.
: USING-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY code codeu JSTR 44 EMIT1
   s" repair_class" JKEY class classu JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY USH-TOK-A @ USH-TOK-U @ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   USH-TOK-A @ USH-TOK-U @ JTOKEN-FIELDS
   s" used_packages" JKEY 91 EMIT1
   [: 0= IF 44 EMIT1 THEN  UPKG-NAME$ JSTR ;] UPKG-EACH
   93 EMIT1 44 EMIT1
   s" suggestion" JKEY sug sugu JSTR
   125 EMIT1 ;
: USHADOW-JSON ( -- )
   s" E-USING-SHADOW-GLOBAL" s" disambiguate_using_shadow"
   s" A global word and a used package public share this name. Qualify the package word as PKG:WORD, or rename the collision; the global has no bare qualifier."
   USING-JSON ;
: USHADOW-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF USHADOW-JSON ELSE USHADOW-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ --- a package public its own private tail shadows (checker.f SHADOW-ARITY-CK).
\ The definition-site twin of the diagnostic above: there two scopes claimed one
\ bare token, here one package does, and the reject again names both candidates
\ with their arity so the author can see which of the two to change. The widths
\ come from the check itself rather than from the store, because the refused
\ record is never written: the rule runs before the append, so the public word has
\ no row here to read.
\
\ CELLS on both sides, because cells are what the compiler reads a definition's
\ contract in and what this rule compares. The package and the tail are read
\ from the private twin's symbol, which carries the refused public's own: the
\ same package and the same tail, in the checker's folded spellings, as the
\ shadow diagnostic above renders a package. Every name reaches the record
\ intake already folded, so no raw token survives this far, and a word is found
\ by a case-insensitive search anyway.
: SBA-CELLS-TXT ( n n -- )                \ append " (in -- out)"
   {: in:n out:n :}
   s"  (" DTXT  in RNUM  s"  -- " DTXT  out RNUM  41 EMIT1 ;
: SBARITY-PROSE ( -- )
   s" E-SHADOWED-ARITY habu: public '" DTXT
   SBA-TWIN @ SYM-PKG$ DTXT  58 EMIT1  SBA-TWIN @ SYM-NAME$ DTXT  39 EMIT1
   SBA-NIN @ SBA-NOUT @ SBA-CELLS-TXT
   s"  does not bind its own name: the same package's private '" DTXT
   SBA-TWIN @ SYM-NAME$ DTXT  39 EMIT1
   SBA-PIN @ SBA-POUT @ SBA-CELLS-TXT
   s"  owns that bare tail, a definition's contract is read from the binding its" DTXT
   s"  own name has, and the two do not move the same cells - so the public word" DTXT
   s"  would be compiled against the private word's arity. Declare the same" DTXT
   s"  effect on both (a public forwarder repeats its private word's signature)" DTXT
   s"  or rename one of the two" DTXT ;
: SBARITY-JSON ( -- )
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-SHADOWED-ARITY" JSTR 44 EMIT1
   s" repair_class" JKEY s" match_shadowed_private_effect" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY SBA-TWIN @ SYM-NAME$ JSTR 44 EMIT1
   s" package" JKEY SBA-TWIN @ SYM-PKG$ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   JNAME-FIELDS
   s" suggestion" JKEY s" A private word of this package owns the same tail, and a bare tail binds the private word first, so the native compiler reads this definition's arity from it. Give the public definition the private word's effect, or rename one of the two." JSTR
   125 EMIT1 ;
: SBARITY-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF SBARITY-JSON ELSE SBARITY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ --- a bare token two used publics export (checker.f CHECKER-RESOLVE:RAISE).
\ The twin of the using-shadow reference site above: there a global and one
\ used public claim the token, here used publics of two packages do. The reject
\ names each package the token resolves in (UPKG-EACH above), so the author
\ can qualify the word meant.
: UAMB-PROSE ( -- )
   s" E-USING-AMBIGUOUS habu: bare '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" ' is ambiguous under using: used publics " DTXT
   [: 0= IF s" , " DTXT THEN
      39 EMIT1  UPKG-NAME$ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1 ;] UPKG-EACH
   s"  export the same name; qualify the one meant as PKG:" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" , or rename the collision" DTXT ;
: UAMB-JSON ( -- )
   s" E-USING-AMBIGUOUS" s" disambiguate_using_ambiguous"
   s" Used publics of more than one package share this name. Qualify the one meant as PKG:WORD, or rename the collision."
   USING-JSON ;
: UAMB-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF UAMB-JSON ELSE UAMB-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ The shadow and ambiguity diagnostics ride ONE checker hook, selected by its
\ argument (checker.f SHADOW-DIAG-XT: 0 = the using-shadow reference site,
\ 1 = the arity-shadow definition site, 2 = a using ambiguity), because every
\ defer written before `: TRUST` takes a slot of the engine's pre-trust pending
\ table (src/habu/layout.f PD-CAP).
: SHADOW-DIAG ( n -- )
   {: sel:n :}
   sel 1 = IF SBARITY-DIAG EXIT THEN
   sel 2 = IF UAMB-DIAG EXIT THEN
   USHADOW-DIAG ;
: SHADOW-DIAG-INSTALL ( -- ) [: SHADOW-DIAG ;] is SHADOW-DIAG-XT ;
SHADOW-DIAG-INSTALL

\ --- a `trust` row naming a word the engine resolves to nothing. Rendered on
\ the same template as the shadow diagnostic above, and beside it on purpose:
\ this is the refusal that stops a stale row from becoming that one.
: TSTALE-PROSE ( -- )
   s" E-TRUST-UNRESOLVED habu: trust row for '" DTXT  TSR-TOK-A @ TSR-TOK-U @ DTXT
   s" ' names no word where its record lands: nothing in the open" DTXT
   s"  section's wordlist, or the global wordlist outside a package, is" DTXT
   s"  spelled that way, so the effect would be recorded against a symbol" DTXT
   s"  the engine never defined. Delete the row, correct the name to the" DTXT
   s"  word it was meant to describe, or write it in the section that" DTXT
   s"  defines that word" DTXT ;
: TSTALE-JSON ( -- )
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-TRUST-UNRESOLVED" JSTR 44 EMIT1
   s" repair_class" JKEY s" fix_stale_trust_row" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY TSR-TOK-A @ TSR-TOK-U @ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY s" This trust row names no word in the wordlist its record lands in: the open section's, or the global wordlist outside a package. Delete the row if the word is gone, correct the spelling, or write the row in the section that defines the word; a qualified PKG:TAIL name is not checked yet." JSTR
   125 EMIT1 ;
: TSTALE-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF TSTALE-JSON ELSE TSTALE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ --- a checker storage registrar called outside the verifier window (checker.f
\ CHECKER-REPLAY-NAME-OK?), on the same template: it names the word the
\ registrar would have recorded and the definer that makes that word soundly.
: REPLAY-ONLY-PROSE ( -- )
   s" E-PKG-CONTEXT habu: storage record for '" DTXT  RPL-TOK-A @ RPL-TOK-U @ DTXT
   s" ' refused outside the engine's verifier window: a checker storage" DTXT
   s"  registrar records a definer's accessor only while the source pre-pass" DTXT
   s"  replays the definer that defines the word, so from source the record" DTXT
   s"  would certify callers against an effect the engine never binds to that" DTXT
   s"  name. Define the storage with its definer: TYPED-VARIABLE, TYPED-BUFFER," DTXT
   s"  LAYOUT-BUFFER or DYNAMIC-BUFFER" DTXT ;
: REPLAY-ONLY-JSON ( -- )
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-PKG-CONTEXT" JSTR 44 EMIT1
   s" repair_class" JKEY s" use_storage_definer" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY RPL-TOK-A @ RPL-TOK-U @ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY s" A checker storage registrar records a definer's accessor only inside the engine's verifier window. Define the storage with its definer (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) instead of calling the registrar." JSTR
   125 EMIT1 ;
: REPLAY-ONLY-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF REPLAY-ONLY-JSON ELSE REPLAY-ONLY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ --- a record for a malformed qualified name (checker.f CHECKER-RECORD-NAME), on
\ the same template, with the repair class and suggestion a call to such a name
\ gets (E-BAD-QUALIFIED above). The record has a code of its own, in a refused
\ record's shape, where the call's is a definition's (tools/diag-code.f); its
\ refusal still throws E-BAD-QUALIFIED.
: BADQUAL-PROSE ( -- )
   s" E-BAD-QUALIFIED-RECORD habu: record for '" DTXT  TSR-TOK-A @ TSR-TOK-U @ DTXT
   s" ' refused: malformed qualified name, where one non-edge ':' selects a" DTXT
   s"  package and a second ':' names no word. Use one ':' qualifier, e.g." DTXT
   s"  PKG:WORD" DTXT ;
: BADQUAL-JSON ( -- )
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-QUALIFIED-RECORD" JSTR 44 EMIT1
   s" repair_class" JKEY s" fix_qualified_name" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" token" JKEY TSR-TOK-A @ TSR-TOK-U @ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY s" Use one ':' qualifier, e.g. PKG:WORD." JSTR
   125 EMIT1 ;
: BADQUAL-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF BADQUAL-JSON ELSE BADQUAL-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ The three refused-record diagnostics ride ONE checker hook (checker.f
\ RECORD-DIAG-XT: 0 = the stale trust row, 1 = the storage record, 2 = the
\ malformed name).
: RECORD-DIAG ( n -- ) {: which:n :}
   which 1 = IF REPLAY-ONLY-DIAG EXIT THEN
   which 2 = IF BADQUAL-DIAG EXIT THEN
   TSTALE-DIAG ;
: RECORD-DIAG-INSTALL ( -- ) [: RECORD-DIAG ;] is RECORD-DIAG-XT ;
RECORD-DIAG-INSTALL

\ --- storage declaration refusals (checker.f CHECKER-STORAGE-REFUSE). A refused
\ LAYOUT-BUFFER, DEFER-LAYOUT-BUFFER, TYPED-BUFFER, TYPED-VARIABLE or
\ DYNAMIC-BUFFER line names the declared word, the refused token and the reason;
\ with no name on its line the definer stands in for the word and the token.
\ It is not a definition, so it carries no definition fields. A refusal the
\ verifier located carries the token's place in its file; a run-time refusal,
\ whose place nothing recorded, carries none.
: STGR-NAME$ ( -- ptr u8 n )  STGR-NAME-A @ STGR-NAME-U @ ;
: STGR-TOK$ ( -- ptr u8 n )  STGR-TOK-A @ STGR-TOK-U @ ;
: STGR-NAME-WHY? ( -- f )
   STGR-WHY @ STG-MALFORMED-NAME =  STGR-WHY @ STG-SEALED-NAME = or
   STGR-WHY @ STG-NO-NAME = or ;
: STGR-COUNT-WHY? ( -- f )
   STGR-WHY @ STG-BAD-COUNT =  STGR-WHY @ STG-NO-COUNT = or
   STGR-WHY @ STG-COUNT-WORD = or ;
: STGR-REASON$ ( -- ptr u8 n )
   STGR-WHY @ STG-UNKNOWN-TYPE = IF s" unknown type" EXIT THEN
   STGR-WHY @ STG-MALFORMED-TYPE = IF s" malformed type" EXIT THEN
   STGR-WHY @ STG-UNSTORABLE-TYPE = IF s" type this definer cannot store" EXIT THEN
   STGR-WHY @ STG-SCHEME-TYPE = IF s" scheme in a stored type" EXIT THEN
   STGR-WHY @ STG-MALFORMED-NAME = IF s" more than one ':' in name" EXIT THEN
   STGR-WHY @ STG-SEALED-NAME = IF s" name in a sealed package" EXIT THEN
   STGR-WHY @ STG-BAD-COUNT = IF s" count outside the buffer's extent" EXIT THEN
   STGR-WHY @ STG-COUNT-WORD = IF s" count resolves to no ( -- n ) word" EXIT THEN
   STGR-WHY @ STG-NO-TYPE = IF s" no type for" EXIT THEN
   STGR-WHY @ STG-NO-NAME = IF s" no name for" EXIT THEN
   s" no count for" ;
: STGR-CLASS$ ( -- ptr u8 n )
   STGR-NAME-WHY? IF s" fix_storage_name" EXIT THEN
   STGR-COUNT-WHY? IF s" fix_storage_count" EXIT THEN
   s" fix_storage_type" ;
: STGR-SUGGEST$ ( -- ptr u8 n )
   STGR-NAME-WHY? IF s" Name the storage with at most one inner ':', outside a sealed system package." EXIT THEN
   STGR-COUNT-WHY? IF s" Put a positive count before the definer whose cells fit in memory: a literal, a constant or an expression." EXIT THEN
   s" Declare the type before the storage, or store a closed, copyable type this definer admits." ;
: STGR-LOCATE ( -- )   \ the token's place, counted from the start of the verifier's buffer
   STGR-SRC-A @ STGR-TOK-A @ STGR-SRC-LINE @ STGR-SRC-COL @ STGR-SRC-BYTE @
   DIAG-ORIGIN-SPAN! ;
: STGR-JSON ( -- )
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-STORAGE" JSTR 44 EMIT1
   s" repair_class" JKEY STGR-CLASS$ JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" word" JKEY STGR-NAME$ JSTR 44 EMIT1
   s" token" JKEY STGR-TOK$ JSTR 44 EMIT1
   s" reason" JKEY STGR-REASON$ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   STGR-AT @ IF
      s" line" JKEY DIAGL0 @ JNUM 44 EMIT1
      s" column" JKEY DIAGC0 @ JNUM 44 EMIT1
      s" byte_start" JKEY DIAGB0 @ JNUM 44 EMIT1
      s" byte_end" JKEY DIAGB0 @ STGR-TOK-U @ + JNUM 44 EMIT1
   THEN
   s" suggestion" JKEY STGR-SUGGEST$ JSTR
   125 EMIT1 ;
: STGR-PROSE ( -- )
   STGR-AT @ IF
      DIAGFB DIAGFU @ DTXT  58 EMIT1  DIAGL0 @ JNUM  58 EMIT1  DIAGC0 @ JNUM
      s" : " DTXT
   THEN
   s" habu: in " DTXT  STGR-NAME$ DTXT  s" : " DTXT  STGR-REASON$ DTXT
   s"  '" DTXT  STGR-TOK$ DTXT  s" '" DTXT ;
: STGR-DIAG ( -- )
   STGR-AT @ IF STGR-LOCATE THEN
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF STGR-JSON ELSE STGR-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: STGR-DIAG-INSTALL ( -- ) [: STGR-DIAG ;] is STORAGE-DIAG-XT ;
STGR-DIAG-INSTALL

;using
