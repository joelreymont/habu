\ render.f: renders checker types and rows as text: a residual stack (DCUR), a
\ refusal's diagnostic line and JSON packet. REC-SIG (installed via RECXT)
\ records a certified sig-less word's inferred effect under its name, so its
\ callers certify; rendering only decides whether that effect is declarable
\ (RQM, NLET). Compiles after checker.f and before any src/lib/ file: no emit or
\ +!, so bytes go out through the one-byte ECH buffer.
using TFAM

create ECH 1 allot
variable RDST   0 RDST !                 \ 0 = stdout, 1 = RSBUF (sig recording)
16384 constant RSBUF-CAP
\ RSBUF and RSN outlive no record; snap-lib.f SND-ZERO-LIVE zeroes them in an image.
create RSBUF RSBUF-CAP allot   variable RSN
variable RQM                             \ a '?' rendered = unknown tag, don't record
variable RJSON   0 RJSON !               \ inside a JSON string: EMIT1 escapes
variable RDIAG-ON
variable RDIAG-FD   2 RDIAG-FD !
PTR-VARIABLE RDIAG-A
variable RDIAG-CAP
variable RDIAG-U
variable RDIAG-I

\ E-DIAG-CAPACITY refuses a record that does not fit RSBUF or the caller's
\ buffer. src/lib/errors.f owns it but compiles after this file, so the same
\ (code, name) pair is re-registered here, the one form tools/error-code-lint.f
\ admits.
package RDIAG
public
-2901 constant E-DIAG-CAPACITY
;package

\ RTAILS is set while TAIL-CAPTURE renders: every quotation then shows its row
\ tails.
variable RTAILS   0 RTAILS !


\ A record past RSBUF is refused whole by a throw; the reset before it leaves the
\ renderer on stdout, outside any JSON string and without row tails, as a
\ delivered record does.
: EMIT-RAW ( n -- )
   {: c:n :}
   c 63 = IF 1 RQM ! THEN
   RDST @ IF
     RSN @ RSBUF-CAP 2 - > IF
        0 RDST !  0 RSN !  0 RJSON !  0 RTAILS !
        RDIAG:E-DIAG-CAPACITY throw
     THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;


: JHEX ( n -- ) {: d:n :}
   d 10 < IF d 48 + ELSE d 55 + THEN EMIT-RAW ;

\ Escapes per RFC 8259 section 7, in the form src/lib/json-write.f writes.
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

\ Between JOPEN and JCLOSE every byte is escaped: a package may be named `\`.
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

: DIAG-FD! ( n -- )
   RDIAG-FD ! ;

\ RDIAG-A is a pointer cell, so a plain fetch keeps the ptr u8 view the byte
\ store in RDIAG-COPY needs.
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

\ The caller's buffer cannot grow: a record that does not fit is refused whole by
\ a throw, and the buffer keeps every record before it.
: RDIAG-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   RDIAG-ON @ 0= IF RDIAG-FD @ a u write drop EXIT THEN
   RDIAG-U @ u + RDIAG-CAP @ > IF RDIAG:E-DIAG-CAPACITY throw THEN
   a u RDIAG-COPY
   RDIAG-U @ u + RDIAG-U ! ;

\ Rendering returns to stdout before the append, so a refused record leaves the
\ renderer as a delivered one does.
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
   oc 0 > IF SEEN-P @ next oc ARENA-CELLS-COPY THEN
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

\ Diagnostic letters are their own a..z namespace, independent of declaration
\ positions. The parser reserves f, n and r as built-in types, so no inferred
\ name may use them.
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

\ RFOLD emits the case-folded spelling. Diagnostics name identifiers folded; only
\ a declaring-package name reaches the renderer unfolded, because TFAM-DECL
\ interns the spelling the source `package` line typed.
: RFOLD ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ CORE-FOLD-C EMIT1 1 + REPEAT drop ;

: CON-OUT ( n -- ) {: p:n :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CTN @ < and IF
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

\ A T-PARAM's stored name span may point into a transient scan buffer (TKF, a
\ callee-sig scratch) that later tokens overwrite before DIAG-PRINT or REC-SIG
\ renders, so the name comes from the family registry: `pkg:tail` for a foreign
\ package, the bare tail for "" and "@". An id outside the registry (a term
\ minted before it loads) uses the stored span. A partial rollback that reuses an
\ in-range slot renders the new occupant's name; the command path never sees it,
\ because a mismatch diagnostic renders before any rollback.
\ The package half folds because a package compares case-insensitively
\ everywhere (TFAM-PKG-MATCH?, TFAM-QUAL-RESOLVE, CAST-OWNER?); raw source echoes
\ (`token`, `definition_source`, `declared_effect_source`) stay verbatim.
\ FAM-FOREIGN? compares exactly: the engine keeps a package's first-registered
\ spelling, and both sides read the one interned string.
: FAM-INTERNED? ( n -- f ) {: fam:n :}
   fam 0 >=  fam TFAM-N@ <  and ;
: FAM-FOREIGN? ( n -- f ) {: fam:n :}
   fam TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0 = IF RES-FALSE EXIT THEN
   pa pu s" @" CORE-STR= IF RES-FALSE EXIT THEN
   pa pu TFAM-ACTIVE-PKG$ CORE-STR= 0= ;
: FAM-QNAME-REND ( n -- ) {: fam:n :}
   fam FAM-FOREIGN? IF fam TFAM-PKG$ RFOLD 58 EMIT1 THEN
   fam TFAM-NAME$ RSTR ;
: FAM-NAME-REND ( n -- ) {: t:n :}
   t PARAM>FAM {: fam:n :}
   fam FAM-INTERNED? 0= IF t PARAM>NAME-A t PARAM>NAME-U RSTR EXIT THEN
   fam FAM-QNAME-REND ;

\ A hidden physical field renders as the diagnostic-only '@family.slotN<args>'
\ or '@family.tag<args>' form and sets RQM, so REC-SIG never records a lone
\ hidden cell. A full run never reaches here: REND-COLLECT compacts it to the
\ logical family type.
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

\ HID-RUN-REST ( node -- rest f ): f is true for a full W-cell run of one family
\ (tag W-1 on top down to slot0), rest the row below it; else rest is the row
\ below the one cell, which renders in its '@' form.
\ HRS: the run holds a cell `catch` left stale. The whole logical value is
\ unusable, so the row prints `stale<option<pt>>` once, never three cells.
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

\ A quotation's row renders its top RBUF-CAP values, as a top-level row does: the
\ renderer recurses once per value, so a quotation leaving 10000 cells faulted
\ the data stack (exit 102) and one leaving 8000 filled RSBUF.
6 constant QDEPTH-MAX
64 constant RBUF-CAP                         \ row render budget (values), top-level or nested
create QPATH QDEPTH-MAX 1 + cells allot      \ quot node on the current render path, by depth
create RBIND QDEPTH-MAX cells allot

: QRET? ( n -- bool ) {: q:n :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

\ A refusal for an escaped callable tail (MD-TAIL-ESCAPE, TAIL-CAPTURE) names
\ every quotation's row tails, so the callable that keeps its input stack shows
\ it: `[ C -- C n ]` against `[ A -- B ]`. Rows letter in their own namespace, A
\ to Z, then '?'. Every other rendering shows cells only.
26 constant RROW-CAP
create RROW-KEY RROW-CAP cells allot   variable RROW-N   0 RROW-N !


: RROW-LETTER ( n -- n )
   {: t:n :}
   t PAY {: id:n :}
   0 BEGIN dup RROW-N @ < WHILE
      dup cells RROW-KEY + @ id = IF 65 + EXIT THEN
      1 +
   REPEAT drop
   RROW-N @ RROW-CAP >= IF 63 EXIT THEN
   id RROW-N @ cells RROW-KEY + !
   RROW-N @ 65 +  RROW-N @ 1 + RROW-N ! ;


: QTAIL ( n -- )
   {: row:n :}
   row ROW-TAIL RROW-LETTER EMIT1  32 EMIT1 ;


\ A quot node already on QPATH below depth d is a type-graph cycle.
: QANCESTOR? {: r:n d:n :}
   0 BEGIN dup d < WHILE
      dup cells QPATH + @ r = IF drop -1 EXIT THEN
      1 +
   REPEAT drop 0 ;


\ QREND ( x d mode -- ): mode>0 renders a row bottom-to-top, each value followed
\ by a space, from its mode-th value down to its RBUF-CAP-th; mode=0 renders a
\ type.
: QREND ( n n n -- )
   {: x:n d:n mode:n :}
   mode 0 > IF
      x R-RES dup TAG S-PUSH = IF                 \ ( node )
         dup P>TYPE T-RES HIDDEN-PARAM? IF        \ hidden run: compact or '@' form (type-family.f TFAM-WIDTH@)
            dup HID-RUN-REST IF                   \ ( node rest ) full run -> logical type
               swap P>TYPE T-RES MK-LOGICAL        \ read HRS before a nested run resets it
               HRS @ IF MK-STALE THEN              \ ( rest logical )
               swap dup R-RES TAG S-PUSH =  mode RBUF-CAP <  and IF
                  d mode 1 + RECURSE
               ELSE drop THEN
               d 0 RECURSE
            ELSE                                  \ ( node rest ) lone/malformed -> '@' cell
               drop
               dup P>REST dup R-RES TAG S-PUSH =  mode RBUF-CAP <  and IF
                  d mode 1 + RECURSE
               ELSE drop THEN
               P>TYPE d 0 RECURSE
            THEN
         ELSE
            dup P>REST dup R-RES TAG S-PUSH =  mode RBUF-CAP <  and IF
               d mode 1 + RECURSE
            ELSE drop THEN
            P>TYPE d 0 RECURSE
         THEN
         32 EMIT1
      ELSE drop THEN
      EXIT
   THEN
   x T-RES {: r:n :}
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER d 0 RECURSE endof
      \ A cell a caught throw may have overwritten names the type it lost.
      \ `stale<...>` is not a declarable type, so it sets RQM: REC-SIG never
      \ records a row carrying one.
      T-STALE of
        1 RQM !
        s" stale<" RSTR  r STALE>INNER d 0 RECURSE  62 EMIT1
      endof
      T-QUOT of
        d QDEPTH-MAX <  r d QANCESTOR? 0=  and IF
           r d cells QPATH + !
           RTAILS @ 0 <> {: named:bool :}
           91 EMIT1 32 EMIT1  named IF r Q>DIN QTAIL THEN  r Q>DIN d 1+ 1 RECURSE
           45 EMIT1 45 EMIT1 32 EMIT1  named IF r Q>DOUT QTAIL THEN  r Q>DOUT d 1+ 1 RECURSE
           r QRET? IF
              124 EMIT1 32 EMIT1
              r Q>RIN d 1+ 1 RECURSE  45 EMIT1 45 EMIT1 32 EMIT1
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
      \ A family applied to no arguments renders as its bare name, the spelling a
      \ signature uses: a bare family token reads back as `name<>`.
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
create RBUF RBUF-CAP cells allot   variable RBN
variable RSHOW-DST

\ A row past RBUF-CAP values is truncated: a reject diagnostic must fail closed,
\ and overflowing RBUF into adjacent DATA gave garbage type pointers and a SIGSEGV
\ in REND-TYPE. The checker still rejects with its normal diagnostic and rc.
: RBUF+ ( n -- )
   RBN @ RBUF-CAP >= IF drop EXIT THEN
   RBN @ cells RBUF + !  RBN @ 1 + RBN ! ;

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

\ REND-SIG ( -- a u ): inputs from the base row's instantiation (BROW), outputs
\ from DCUR.
: REND-SIG
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET RATOM-RESET 0 NLET !
   BROW @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop
   45 EMIT1  45 EMIT1
   DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - 32 EMIT1 dup cells RBUF + @ REND-TYPE REPEAT drop
   0 RDST !  RSBUF RSN @ ;

\ DIAG-PRINT writes one reject line to stderr:
\   habu: in NAME: at 'TOK' expected: <row> actual: <row>
\ A top-level step's line names its place instead (DSITE): habu: PATH:LINE: ...
\ expected and actual appear only when the failing unify was captured.
: DTXT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: DROW {: s :}  s REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

\ `JSON-DIAGS ON` emits one JSON object per reject or uncheckable verdict.
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
\ A tool's label (checker.f DIAG-FILE!) names a top step's file in place of its
\ path while the load reads that file or none.
: TOP-FILE$ ( -- ptr u8 n )
   LOWER-CERT-HOOK:TOP-SITE-PATH$ {: pa:ptr pu:n :}
   pa pu DIAG-LABELS? IF DIAGFB DIAGFU @ EXIT THEN
   pa pu ;
\ A token outside the file's bytes, read from a string `evaluate` runs, names the
\ file alone; a step with no file and no label names no place.
: DSITE ( -- )
   TOP-STEP @ 0= IF s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : " DTXT EXIT THEN
   s" habu: " DTXT
   TOP-FILE$ {: fa:ptr fu:n :}
   fu 0= IF EXIT THEN
   fa fu DTXT
   LOWER-CERT-HOOK:TOP-SITE IF 58 EMIT1 JNUM ELSE drop THEN
   s" : " DTXT ;
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


: JEFFECT-TEXT ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   din DROW  s" -- " DTXT  dout DROW
   hasr IF s" | " DTXT  rin DROW  s" -- " DTXT  rout DROW THEN ;


: JEFFECT ( n n n n bool -- ) JOPEN JEFFECT-TEXT JCLOSE ;


: INFERRED-EFFECT ( -- )   \ the body's effect so far: the declared inputs to the current rows
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT-TEXT ;


\ The escape refusal (MD-TAIL-ESCAPE) as checker.f CELL-DIAG! named it: the
\ inferred effect, then the declared callable and the argument compared with it,
\ rendered in DIAG-PRINT's letter order into TCAP before later tokens bind their
\ rows. Naming the callable pair, not the stacks around it, it reads the same
\ whatever the definition holds beneath. The held row names the tail that
\ escaped (checker.f TAIL-SKOLEM) and the row that would have come to equal or
\ hold it (TAIL-ROW), so a join whose two callables read alike still says which
\ side escaped. MDIAG gates every read, so a capture an abandoned trial made is
\ never rendered.
create TCAP RSBUF-CAP allot
variable TCAP-INF   variable TCAP-EXP   variable TCAP-ACT   variable TCAP-HELD
: TCAP-INF$ ( -- ptr u8 n ) TCAP TCAP-INF @ ;
: TCAP-EXP$ ( -- ptr u8 n ) TCAP TCAP-INF @ +  TCAP-EXP @ TCAP-INF @ - ;
: TCAP-ACT$ ( -- ptr u8 n ) TCAP TCAP-EXP @ +  TCAP-ACT @ TCAP-EXP @ - ;
: TCAP-HELD$ ( -- ptr u8 n ) TCAP TCAP-ACT @ +  TCAP-HELD @ TCAP-ACT @ -  SIG-RTRIM ;


: TCAP-ROW ( n -- )   \ the escaped-into row: its tail's letter and cells
   {: x:n :}
   x ISVAR IF x REND-TYPE EXIT THEN
   x QTAIL  x DROW ;


: TAIL-CAPTURE ( -- )
   RDST @ {: dst:n :}  RSN @ {: base:n :}  RQM @ {: qm:n :}
   1 RDST !  SEEN-RESET 0 NLET !  0 RROW-N !
   INFERRED-EFFECT  RSN @ base - TCAP-INF !
   -1 RTAILS !
   DF-EXP @ REND-TYPE 32 EMIT1  RSN @ base - TCAP-EXP !
   DF-ACT @ REND-TYPE 32 EMIT1  RSN @ base - TCAP-ACT !
   TAIL-SKOLEM @ ROW-TAIL RROW-LETTER EMIT1  s"  would escape into " DTXT
   TAIL-ROW @ TCAP-ROW  RSN @ base - TCAP-HELD !
   0 RTAILS !
   0 BEGIN dup TCAP-HELD @ < WHILE
      dup base + RSBUF + c@  over TCAP + c!  1 +
   REPEAT drop
   base RSN !  qm RQM !  dst RDST ! ;


: TAIL-CAPTURE-INSTALL ( -- ) [: TAIL-CAPTURE ;] is TAIL-CAPTURE-XT ;
TAIL-CAPTURE-INSTALL
\ Match/construct reasons (docs/forth.md "Structures And Enums"): MDIAG's latched
\ code maps to a JSON code, repair class, suggestion and prose. MD-NONEXH names
\ the missing variants from the latched (family, seen-bitset, count):
\ declaration-order tags index the family's contiguous SUMV rows.
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
      MD-TOP-UNKNOWN  of s" E-STALE-READ" endof
      MD-TAIL-ESCAPE  of s" E-MISMATCH" endof
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
      MD-EFFECT-DEPTH of s" E-UNCHECKABLE" endof
      MD-INPUT-WIDTH  of s" E-UNCHECKABLE" endof
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
      MD-TOP-UNKNOWN  of s" drop_unknown_cell" endof
      MD-TAIL-ESCAPE  of s" fix_type" endof
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
      MD-EFFECT-DEPTH of s" rewrite_uncheckable" endof
      MD-INPUT-WIDTH  of s" rewrite_uncheckable" endof
      s" fix_match_syntax" rot
   endcase ;


\ Shared by a definition's refusal and a stored signature's (BADSIG-REASON).
: DEPTH-REASON$ ( -- ptr u8 n )
   s" effect too deep to record" ;

: WIDTH-REASON$ ( -- ptr u8 n )
   s" input row too wide to record" ;


: TAIL-SUGGEST$ ( -- ptr u8 n )
   s" The callee declares this callable with an implicit input row, [ -- S ], so it may run the callable on any stack and expects S back; a callable that keeps or reads the stack it runs on cannot promise that. Declare the callable [ R -- S ] from the word's input row, as in ( R [ R -- S ] -- S ), or pass a callable whose output does not depend on the stack beneath it." ;


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
      MD-EXEC-OPAQUE  of s" Execute an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not execute an xt fetched from untyped memory or ticked from a primitive or a word with no stack effect." endof
      MD-CATCH-OPAQUE of s" Catch an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not catch an xt fetched from untyped memory or ticked from a primitive or a word with no stack effect." endof
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
      MD-TOP-UNKNOWN  of s" The interpret row knows only the cells pushed while it checks; a cell beneath them was left before checking began or by a `0 set-check` window, so its type is unknown. Push the value with checking on, or drop the cell. An unknown cell may still be moved, dropped, or bound to an untyped local." endof
      MD-TAIL-ESCAPE  of TAIL-SUGGEST$ endof
      MD-SCOPE-STORE  of s" Keep the scoped value on the stack or in a local; ordinary memory has no scope dependency declaration." endof
      MD-SCOPE-KIND   of s" Use a scope parameter only in a declared scope slot; it is neither a value type nor an allocation identity." endof
      MD-C2-COPY      of s" Move the exclusive value once instead of copying it." endof
      MD-C2-DROP      of s" Return or consume the exclusive value instead of discarding it." endof
      MD-C2-ESCAPE    of s" Return only values independent of the owner or loan opened by this operation." endof
      MD-EFFECT-DEPTH of s" Leave fewer cells: keep bulk values in a buffer, not on the stack." endof
      MD-INPUT-WIDTH  of s" Take fewer cells: pass bulk values in a buffer, not on the stack." endof
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
      MD-EXEC-OPAQUE  of s" execute: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive or a word with no stack effect)" endof
      MD-CATCH-OPAQUE of s" catch: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive or a word with no stack effect)" endof
      MD-RAW-PTR      of s" raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell" endof
      MD-NULL-PTR     of s" null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one" endof
      MD-DBASE-PTR    of s" base address: a cell reached from data-base, or from a pointer computed off NULL-PTR, holds a plain value at every pointee depth, never a nominal type or a pointer" endof
      MD-RAW-FIELD    of s" ptr-field: base is an undeclared raw storage cell, not a declared pointer cell" endof
      MD-RAW-EXEC     of s" raw storage cell: an undeclared cell cannot hold an execution token / a quotation" endof
      MD-STALE-READ   of s" stale cell: this reads a cell `catch` left stale (a throw path of the caught body may have overwritten it)" endof
      MD-TOP-UNKNOWN  of s" unknown cell: this reads a cell beneath the cells the interpret row knows (left before checking began or by a `0 set-check` window)" endof
      MD-TAIL-ESCAPE  of s" callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on" endof
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
      MD-EFFECT-DEPTH of DEPTH-REASON$ endof
      MD-INPUT-WIDTH  of WIDTH-REASON$ endof
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

\ The counts a reason latches: the underflow's shortfall, or a too-deep effect's
\ depth or a too-wide input row's width against its ceiling, a definition's
\ (MDIAG-DEPTH, MDIAG-WIDTH) or a stored signature's (SGBAD-SIZE). Digits,
\ letters and spaces only, so one word serves the prose line and the JSON string.
: MDIAG-UF-COUNTS ( -- )
   s"  (needs " DTXT  MDIAG-NEED @ JNUM
   s" , has " DTXT  MDIAG-HAVE @ JNUM  s" )" DTXT ;

: DEPTH-COUNTS ( n -- )
   {: depth:n :}
   s"  (depth " DTXT  depth JNUM
   s" , at most " DTXT  EFFECT-DEPTH-MAX JNUM  s" )" DTXT ;

: WIDTH-COUNTS ( n -- )
   {: width:n :}
   s"  (" DTXT  width JNUM
   s"  cells, at most " DTXT  EFFECT-MIN-IN-MAX JNUM  s" )" DTXT ;


: MDIAG-COUNTS ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF s" ; held row " DTXT  TCAP-HELD$ DTXT EXIT THEN
   MDIAG @ MD-UNDERFLOW = IF MDIAG-UF-COUNTS EXIT THEN
   MDIAG @ MD-EFFECT-DEPTH = IF MDIAG-DEPTH @ DEPTH-COUNTS EXIT THEN
   MDIAG @ MD-INPUT-WIDTH = IF MDIAG-WIDTH @ WIDTH-COUNTS THEN ;


: IMM-CODE$ ( -- ptr u8 n )
   s" E-UNMODELED-IMMEDIATE" ;

: IMM-CLASS$ ( -- ptr u8 n )
   s" model_compile_immediate" ;

: IMM-SUGGEST$ ( -- ptr u8 n )
   s" Declare a stack-neutral parsing immediate with parse-imm, or remove it from the compiled body." ;

\ The LOCALBAD kinds come from checker.f LOC-REJECT.
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

\ FAILTK is the declaration token pinned by the first failure, so a later body
\ reference to the rejected name cannot relabel the reject as an undefined word.
: LOCALBAD-PROSE ( -- )
   LOCALBAD-CODE$ DTXT  s"  " DTXT  DSITE
   LOCALBAD-KIND @ 2 = IF
     s" local '" DTXT  FAILTK FAILTU @ DTXT  s" ' has a " DTXT  LOCALBAD-LEN @ JNUM
     s" -byte name; the limit is " DTXT  LOC-NAME-W JNUM  s"  bytes" DTXT EXIT
   THEN
   LOCALBAD-KIND @ 1 = IF
     s" local '" DTXT  FAILTK FAILTU @ DTXT  s" ' is one over the " DTXT  LOC-CAP JNUM
     s"  locals a definition may bind" DTXT EXIT
   THEN
   s" at '" DTXT  FAILTK FAILTU @ DTXT
   s" ': a local cannot be bound or referenced inside a quotation, or bound on a dead path" DTXT ;

\ A definition's signature, a local's type or a stored signature that does not
\ parse; SGBAD's state from the failed parse names the kind.
: SGBAD-CODE$ ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" EXIT THEN
   SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" EXIT THEN
   SGBAD-ARITY? IF s" E-WRONG-ARITY" EXIT THEN
   s" E-BAD-SIGNATURE" ;
: SGBAD-CLASS$ ( -- ptr u8 n )
   SGBAD-RETURN? IF s" fix_return_stack" EXIT THEN
   SGBAD-UNKNOWN? IF s" fix_signature_type" EXIT THEN
   SGBAD-BAREPTR? IF s" fix_bare_ptr_element" EXIT THEN
   SGBAD-ARITY? IF s" fix_signature_arity" EXIT THEN
   SGBAD-SIZE? IF s" fix_signature_size" EXIT THEN
   s" fix_signature_syntax" ;
: RCLAUSE-FIX$ ( -- ptr u8 n )
   s" after '|' write one row variable, the same on both sides, or no '|'" ;
\ A syntax fault has no line of its own: it reads as a refusal at its token.
: SGBAD-PROSE ( -- )
   DSITE
   SGBAD-UNKNOWN? IF
     s" unknown type '" DTXT  FAILTK FAILTU @ DTXT  s" ' in signature" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF
     s" 'ptr' needs an element type, e.g. 'ptr u8' or 'ptr a'" DTXT EXIT
   THEN
   SGBAD-ARITY? IF
     s" wrong arity for type family '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   SGBAD-RETURN? IF
     s" return-stack effect in signature '" DTXT  FAILTK FAILTU @ DTXT
     s" '; " DTXT  RCLAUSE-FIX$ DTXT EXIT
   THEN
   s" at '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT ;

\ The code, repair class, suggestion, prose, verdict and per-code JSON fields all
\ name the one refusal checker.f REFUSAL@ chose.
: DCODE ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-CODE$ EXIT THEN
   k RF-UNDEF = IF s" E-UNDEFINED" EXIT THEN
   k RF-QUAL = IF s" E-BAD-QUALIFIED" EXIT THEN
   k RF-LOCAL = IF LOCALBAD-CODE$ EXIT THEN
   k RF-SIG = IF SGBAD-CODE$ EXIT THEN
   k RF-DEAD = IF s" E-DEAD-CODE" EXIT THEN
   k RF-NP = IF s" E-NONPARAMETRIC-EFFECT" EXIT THEN
   k RF-CAP = IF s" E-CAP-TRUSTED" EXIT THEN
   k RF-UNSAFE = IF s" E-UNSAFE" EXIT THEN
   k RF-LINLOCAL = IF s" E-LINEAR-LOCAL" EXIT THEN
   k RF-REASON = IF MDIAG-CODE$ EXIT THEN
   k RF-UNCHECKABLE = IF s" E-UNCHECKABLE" EXIT THEN
   k RF-MISMATCH = IF s" E-MISMATCH" EXIT THEN
   s" E-REJECTED" ;
: DVERDICT ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-UNCHECKABLE = IF s" uncheckable" EXIT THEN
   k RF-REASON =  MDIAG @ UNFIT-REASON?  and IF s" uncheckable" EXIT THEN
   s" rejected" ;
\ repair_class is the stable machine name a consumer keys its repair on. This
\ word picks it for checker refusals; tools/check-core.f and tools/diag-code.f
\ pick it for the refusals the check tool finds itself. A class means what its
\ suggestion says (SUGGEST-TEXT, the *-SUGGEST$ words): short prose for choosing
\ the action; the raw evidence stays in the expected, actual and return_stack
\ fields.
: REPAIR-CLASS ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-CLASS$ EXIT THEN
   k RF-UNDEF = IF s" unknown_rejection" EXIT THEN
   k RF-QUAL = IF s" fix_qualified_name" EXIT THEN
   k RF-LOCAL = IF LOCALBAD-CLASS$ EXIT THEN
   k RF-SIG = IF SGBAD-CLASS$ EXIT THEN
   k RF-DEAD = IF s" remove_dead_code" EXIT THEN
   k RF-NP = IF s" fix_parametric_effect" EXIT THEN
   k RF-CAP = IF s" trusted_boundary_required" EXIT THEN
   k RF-UNSAFE = IF s" trusted_boundary_required" EXIT THEN
   k RF-LINLOCAL = IF s" factor_linear_local" EXIT THEN
   k RF-REASON = IF MDIAG-CLASS$ EXIT THEN
   k RF-UNCHECKABLE = IF s" rewrite_uncheckable" EXIT THEN
   k RF-RETURN = IF s" fix_return_stack" EXIT THEN
   k RF-MISMATCH <> IF s" unknown_rejection" EXIT THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF s" remove_producer" ELSE
   DSUGA @ DSUGE @ < IF s" add_producer" ELSE
   s" fix_type" THEN THEN ;
\ Shared by SUGGEST-TEXT, a bad stored signature's diagnostic and a refused
\ `generates:` row's.
: SGBAD-SUGGEST$ ( -- ptr u8 n )
   SGBAD-RETURN? IF s" Declare no return-stack effect: after '|' write one row variable, the same on both sides, or no '|'; park a value with >r and take it back with r> in one definition." EXIT THEN
   SGBAD-UNKNOWN? IF s" Use a known stack-signature type or a single-letter type variable." EXIT THEN
   SGBAD-BAREPTR? IF s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." EXIT THEN
   SGBAD-ARITY? IF s" Give the type family its exact declared number of arguments." EXIT THEN
   SGBAD-SIZE? IF s" Declare fewer cells: keep bulk values in a buffer, not on the stack." EXIT THEN
   s" Repair the stack-effect comment syntax, including --." ;
: NP-SUGGEST$ ( -- ptr u8 n )
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
      s" Declare the concrete family in the signature, or keep the body polymorphic over the type variable." EXIT
   THEN
   NPBAD-KIND @ 1 = IF
      s" Keep each declared type variable distinct; do not unify two quantifiers in the body." EXIT
   THEN
   NPBAD-KIND @ 6 = IF
      s" Keep each declared row distinct: declare the row the body really leaves, or reach the declared output row on every path, as by running the quotation that yields it." EXIT
   THEN
   NPBAD-KIND @ 7 = IF
      s" Declare the cells the body leaves on the row, or keep the body polymorphic over the row variable." EXIT
   THEN
   s" Bind the minted phantom's type arguments to the inputs, or mint it behind an audited TRUSTED: boundary." ;
: SUGGEST-TEXT ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-SUGGEST$ EXIT THEN
   k RF-UNDEF = IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   k RF-QUAL = IF s" Use one ':' qualifier, e.g. PKG:WORD." EXIT THEN
   k RF-LOCAL = IF LOCALBAD-SUGGEST$ EXIT THEN
   k RF-SIG = IF SGBAD-SUGGEST$ EXIT THEN
   k RF-DEAD = IF s" Remove tokens after the terminating control word, or move the work before it." EXIT THEN
   k RF-NP = IF NP-SUGGEST$ EXIT THEN
   k RF-CAP = IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   k RF-UNSAFE = IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   k RF-LINLOCAL = IF s" Keep the linear value on the stack; do not bind it to a local." EXIT THEN
   k RF-REASON = IF MDIAG-SUGGEST$ EXIT THEN
   k RF-UNCHECKABLE = IF s" Rewrite with modeled words or isolate an audited primitive." EXIT THEN
   k RF-RETURN = IF
      RSBAD @ 2 = IF s" Read or pop only return-row cells this definition pushed with >r; below them lies the caller's frame." EXIT THEN
      RSBAD @ 3 = IF s" Balance return-stack transfers before the quotation returns." EXIT THEN
      RSBAD @ 4 = IF s" Read or pop only return-row cells this quotation pushed with >r; below them lies the caller's frame." EXIT THEN
      s" Balance return-stack transfers before the definition exits." EXIT
   THEN
   k RF-MISMATCH <> IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF  s" Remove an extra producer or drop the surplus value."
   ELSE DSUGA @ DSUGE @ < IF  s" Add the missing producer or stop consuming a required value."
   ELSE  s" Change the body so produced types match the signature."
   THEN THEN ;
\ A packet's position: the file line, column and byte of its token's first byte,
\ and the byte after its last.
variable JLOC-L  variable JLOC-C  variable JLOC-B  variable JLOC-E
\ False when the token's text has no map (src/core/checker.f DIAG-LOCATE). The
\ end is the source's first file byte plus the end's offset in the source.
: JLOCATE ( ptr u8 ptr u8 -- bool )
   {: a:ptr e:ptr :}
   e DIAG>SRC 0= IF drop 0 0= 0= EXIT THEN
   DSRC-B @ + JLOC-E !
   a DIAG-LOCATE 0= IF drop drop drop 0 0= 0= EXIT THEN
   JLOC-B !  JLOC-C !  JLOC-L !
   0 0= ;
\ With no map: the definition's origin plus the token's offset in the checked text.
: JLOC-ORIGIN ( -- )
   TBASE@  TBASE@ FAILB @ +  DIAGL0 @ DIAGC0 @ DIAGB0 @  DIAG-POS
   JLOC-B !  JLOC-C !  JLOC-L !
   DIAGB0 @ FAILE @ + JLOC-E ! ;
: JLOC-FIELDS ( -- )
   s" line" JKEY  JLOC-L @ JNUM  44 EMIT1
   s" column" JKEY  JLOC-C @ JNUM  44 EMIT1
   s" byte_start" JKEY  JLOC-B @ JNUM  44 EMIT1
   s" byte_end" JKEY  JLOC-E @ JNUM  44 EMIT1 ;
: JTOKEN-FIELDS ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 > IF
      a  a u +  JLOCATE IF JLOC-FIELDS THEN
   THEN ;
: NM-SPAN ( -- ptr u8 n )
   NMOFF @ TADDR NMU @ ;
\ A packet opens with JSTART (schema version and code), or JHEAD for a refusal
\ (adds repair class and verdict); JTAIL closes it with the file, the span's
\ place when it locates, and the suggestion. A code's own fields go between.
\ DIAG-JSON, USING-JSON and STGR-JSON close themselves: fields or a differently
\ sourced place follow the file.
: JSTART ( ptr u8 n -- )
   {: ca:ptr cu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY ca cu JSTR 44 EMIT1 ;
: JHEAD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: ca:ptr cu:n ka:ptr ku:n va:ptr vu:n :}
   ca cu JSTART
   s" repair_class" JKEY ka ku JSTR 44 EMIT1
   s" verdict" JKEY va vu JSTR 44 EMIT1 ;
: JTAIL ( ptr u8 n ptr u8 n -- )
   {: aa:ptr au:n sa:ptr su:n :}
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   aa au JTOKEN-FIELDS
   s" suggestion" JKEY sa su JSTR
   125 EMIT1 ;                                            \ }
: NP-FAM-REND ( -- )
   NPBAD-TERM @ NP-FAM {: fam:n :}
   fam 0 >= IF s" family '" DTXT  fam FAM-QNAME-REND  s" '" DTXT
   ELSE s" a concrete type" DTXT THEN ;
: NP-PROSE ( -- )
   s" E-NONPARAMETRIC-EFFECT " DTXT  DSITE
   NPBAD-KIND @ 4 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted to a base address -- the DATA region, or a pointer computed off NULL-PTR; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 5 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted to the null address; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 3 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted by raw storage; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 0= IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is specialized to " DTXT  NP-FAM-REND
     s" ; a declared effect must stay parametric over its quantifier" DTXT EXIT
   THEN
   NPBAD-KIND @ 1 = IF
     s" declared type variables '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' and '" DTXT  NPBAD-Q2 @ EMIT1
     s" ' are unified; each declared quantifier must stay a distinct variable" DTXT EXIT
   THEN
   NPBAD-KIND @ 6 = IF
     s" declared row variable '" DTXT  NPBAD-Q1 @ EMIT1
     NPBAD-Q2 @ IF s" ' and row '" DTXT  NPBAD-Q2 @ EMIT1  s" '" DTXT
     ELSE s" ' and the unnamed row" DTXT THEN
     s"  are unified; each declared row must stay a distinct row variable" DTXT EXIT
   THEN
   NPBAD-KIND @ 7 = IF
     NPBAD-Q1 @ IF s" declared row variable '" DTXT  NPBAD-Q1 @ EMIT1  s" '" DTXT
     ELSE s" the unnamed row" DTXT THEN
     s"  is bound to fixed cells; a declared effect must stay polymorphic over its row" DTXT EXIT
   THEN
   s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
   s" ' is minted into " DTXT  NP-FAM-REND
   s"  but is unbound in the inputs; a checked definition cannot mint a phantom of input-unrelated type — use an audited TRUSTED: boundary" DTXT ;
\ The return rows `return_stack` names, expected then actual: a quotation
\ literal's own when its seal refused it (RSBAD 3, 4), else the definition's.
: RS-ROWS ( -- n n )
   RSBAD @ 3 >= IF QRS-EXP @ QRS-ACT @ EXIT THEN
   SGHASR @ IF SGROUT @ ELSE RBROW @ THEN  RCUR @ ;

: RS-REASON$ ( -- ptr u8 n )
   RSBAD @ 2 = IF s" return stack: this reads below the return-stack cells pushed here" EXIT THEN
   RSBAD @ 3 = IF s" return stack: the quotation leaves its return row unbalanced" EXIT THEN
   RSBAD @ 4 = IF s" return stack: the quotation reads below the return-stack cells it pushed" EXIT THEN
   s" return stack: the return row does not balance at exit" ;

: ROWS-REFUSAL? ( n -- bool )
   {: k:n :}
   k RF-MISMATCH =  k RF-REASON = or ;


: DIAG-PROSE
   REFUSAL@ {: k:n :}
   k RF-IMM = IF
     IMM-CODE$ DTXT  s"  " DTXT  DSITE
     s" compile-time immediate '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is not a stack-neutral parsing immediate; declare it with parse-imm or remove it from the compiled body" DTXT EXIT
   THEN
   k RF-SIG = IF SGBAD-PROSE EXIT THEN
   k RF-NP = IF NP-PROSE EXIT THEN
   k RF-CAP = IF
     s" E-CAP-TRUSTED " DTXT  DSITE
     s" '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is a trust-boundary primitive; call it only from a TRUSTED: definition" DTXT EXIT
   THEN
   k RF-LOCAL = IF LOCALBAD-PROSE EXIT THEN
   k RF-QUAL = IF
     s" E-BAD-QUALIFIED " DTXT  DSITE
     s" malformed qualified name '" DTXT  FAILTK FAILTU @ DTXT
     s" ' (more than one ':')" DTXT EXIT
   THEN
   k RF-UNDEF = IF
     s" E-UNDEFINED " DTXT  DSITE
     s" undefined word '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   k RF-LINLOCAL = IF
     s" E-LINEAR-LOCAL " DTXT  DSITE
     s" linear value cannot be bound to a local; keep it on the stack" DTXT EXIT
   THEN
   DSITE  s" at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
   k RF-REASON = IF
     s"  " DTXT  MDIAG-REASON$ DTXT
     MDIAG @ MD-NONEXH = IF MDIAG-MISSING-PROSE THEN
     MDIAG-COUNTS
   THEN
   k RF-DEAD = IF s"  after '" DTXT DEADTA @ DEADTU @ DTXT s" '" DTXT THEN
   k RF-RETURN = IF
     s"  " DTXT  RS-REASON$ DTXT
     RS-ROWS {: exp:n act:n :}
     s"  expected: " DTXT  exp DROW
     s" actual: " DTXT  act DROW EXIT
   THEN
   k ROWS-REFUSAL? 0= IF EXIT THEN
   MDIAG @ MD-TAIL-ESCAPE = IF
     s"  expected: " DTXT  TCAP-EXP$ DTXT
     s" actual: " DTXT  TCAP-ACT$ DTXT EXIT THEN
   DEXP @ 0 <> IF
     s"  expected: " DTXT  DEXP @ DROW
     s" actual: " DTXT  DACT @ DROW THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
     s"  actual: " DTXT  DF-ACT @ REND-TYPE THEN ;


\ Expected takes precedence over actual, so unrelated matched row cells cannot
\ leak into the diagnostic; a scalar pair emits no family. The hint spells the
\ family as FAM-QNAME-REND does, matching the rows and resolving from the
\ failing definition's scope.
: TERM-FAM ( n -- n )                    \ layout-family id for one type term, else -1
   T-RES dup LAYOUT-PARAM? IF PARAM>FAM EXIT THEN
   drop -1 ;
\ DVAR is the SUMV id a `construct family variant` payload mismatch latched, -1
\ for none: a scalar or non-construct mismatch emits no variant or tag.
: DIAG-FAM-ID ( n n -- n )
   {: efam:n afam:n :}
   efam 0 >= IF efam EXIT THEN
   afam 0 >= IF afam EXIT THEN
   DVAR @ dup 0 >= IF SUMV-FAM@ THEN ;
\ DPOS is the slot-from-top of the failed expected-row element; the packet
\ reports the 0-based declaration-order payload index, absent when the position
\ is unknown or outside the variant's payload cells.
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
\ A top step's place as its prose names it (DSITE): its line alone, no line for a
\ token outside the file's bytes; else the checked token in its mapped or origin
\ source.
: DFILE$ ( -- ptr u8 n )
   TOP-STEP @ 0= IF DIAGFB DIAGFU @ EXIT THEN
   TOP-FILE$ dup 0 > IF EXIT THEN drop drop
   DIAG-NO-FILE$ ;
: DLOC-FIELDS ( -- )
   TOP-STEP @ IF
      LOWER-CERT-HOOK:TOP-SITE IF s" line" JKEY  JNUM  44 EMIT1 ELSE drop THEN
      EXIT
   THEN
   TBASE@ FAILB @ +  TBASE@ FAILE @ +  JLOCATE 0= IF JLOC-ORIGIN THEN
   JLOC-FIELDS ;


\ A field that belongs to one refusal goes only on that refusal's packet: the
\ flags of a failure the refusal outranks leave their fields behind.
: DIAG-JSON
   REFUSAL@ {: k:n :}
   DCODE REPAIR-CLASS DVERDICT JHEAD
   s" word" JKEY   NMA @ NMU @ JSTR   44 EMIT1
   s" token" JKEY  FAILTK FAILTU @ JSTR  44 EMIT1
   k RF-DEAD = IF s" dead_owner" JKEY DEADTA @ DEADTU @ JSTR 44 EMIT1 THEN
   k RF-REASON = IF
     s" reason" JKEY
     \ A reason's counts go inside its string, after its text.
     JOPEN  MDIAG-REASON$ DTXT  MDIAG-COUNTS  JCLOSE
     44 EMIT1
     MDIAG @ MD-NONEXH = IF s" missing_variants" JKEY MDIAG-MISSING-JSTR 44 EMIT1 THEN
   THEN
   s" token_index" JKEY  FAILIX @ JNUM  44 EMIT1
   s" file" JKEY  DFILE$ JSTR  44 EMIT1
   DLOC-FIELDS
   s" definition_source" JKEY  TBASE @ TBLEN @ JSTR  44 EMIT1
   SGSEEN @ IF
     s" declared_effect" JKEY
     SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ JEFFECT  44 EMIT1
     s" declared_effect_source" JKEY
     SGA @ SGU @ SIG-TRIM JSTR  44 EMIT1
   THEN
   s" inferred_effect" JKEY
   MDIAG @ MD-TAIL-ESCAPE = IF TCAP-INF$ JSTR ELSE JOPEN INFERRED-EFFECT JCLOSE THEN
   44 EMIT1
   s" return_stack" JKEY
   123 EMIT1
   RS-ROWS swap
   s" expected" JKEY  JROW  44 EMIT1
   s" actual" JKEY    JROW
   125 EMIT1
   k ROWS-REFUSAL? IF
     MDIAG @ MD-TAIL-ESCAPE = IF
       44 EMIT1 s" expected" JKEY TCAP-EXP$ JSTR
       44 EMIT1 s" actual"   JKEY TCAP-ACT$ JSTR
     ELSE
       DEXP @ 0 <> IF
         44 EMIT1 s" expected" JKEY DEXP @ JROW
         44 EMIT1 s" actual"   JKEY DACT @ JROW
         DIAG-FAMILY THEN
       DF-ACT @ 0 <>  DEXP @ 0= and IF
          44 EMIT1 s" actual_type" JKEY 34 EMIT1 DF-ACT @ REND-TYPE 34 EMIT1 THEN
     THEN
   THEN
   k RF-NP = IF                                           \ non-parametric declared effect
     NPBAD-Q1 @ IF 44 EMIT1 s" quantifier" JKEY JOPEN NPBAD-Q1 @ EMIT1 JCLOSE THEN
     NPBAD-Q2 @ IF
       44 EMIT1 s" quantifier2" JKEY JOPEN NPBAD-Q2 @ EMIT1 JCLOSE
     ELSE
       NPBAD-TERM @ NP-FAM dup 0 >= IF
         44 EMIT1 s" family" JKEY JOPEN FAM-QNAME-REND JCLOSE
       ELSE drop THEN
     THEN
   THEN
   k RF-SIG =  SGBAD-ARITY? and IF                        \ E-WRONG-ARITY counts
     44 EMIT1 s" arity_expected" JKEY SGBAD-AR-DECL @ JNUM
     44 EMIT1 s" arity_actual" JKEY SGBAD-AR-GOT @ JNUM THEN
   44 EMIT1 s" suggestion" JKEY SUGGEST-TEXT JSTR
   125 EMIT1 ;                                            \ }


: DIAG-PRINT
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !  0 RROW-N !
   JSON-DIAGS @ IF DIAG-JSON ELSE DIAG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH
   JSON-DIAGS @ IF CHECKER-REPORT-MARK THEN ;


: DIAG-PRINT-INSTALL ( -- ) [: DIAG-PRINT ;] is DIAGXT ;
DIAG-PRINT-INSTALL

\ Bad stored-signature diagnostics (checker.f USIG-ADD-BAD, every mode). SGBAD's
\ state from the failed parse is still live, so class and suggestion are a
\ signature's; a row that parsed but is too deep or too wide to record has its
\ own class and a reason naming the bound, count and limit. The JSON is a refused
\ record (its tools/diag-code.f row), placed at the declaring name AT in the
\ checked text.
: BADSIG-REASON ( -- )
   SGBAD-DEPTH? IF DEPTH-REASON$ DTXT  SGBAD-SIZE @ DEPTH-COUNTS EXIT THEN
   WIDTH-REASON$ DTXT  SGBAD-SIZE @ WIDTH-COUNTS ;
: BADSIG-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n at:ptr atu:n :}
   s" E-BAD-STORED-SIGNATURE" SGBAD-CLASS$ s" rejected" JHEAD
   s" token" JKEY na nu JSTR 44 EMIT1
   s" signature" JKEY sa su SIG-TRIM JSTR 44 EMIT1
   SGBAD-SIZE? IF s" reason" JKEY JOPEN BADSIG-REASON JCLOSE 44 EMIT1 THEN
   at atu SGBAD-SUGGEST$ JTAIL ;
: BADSIG-PROSE ( ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n :}
   s" habu: in " DTXT  na nu DTXT  s" : bad stored signature '" DTXT
   sa su SIG-TRIM DTXT  s" '" DTXT
   SGBAD-SIZE? IF s"  " DTXT BADSIG-REASON THEN
   SGBAD-RETURN? IF s" ; return-stack effect: " DTXT RCLAUSE-FIX$ DTXT THEN ;
: BADSIG-DIAG ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n at:ptr atu:n :}
   1 RDST !  0 RSN !
   JSON-DIAGS @ IF sa su na nu at atu BADSIG-JSON ELSE sa su na nu BADSIG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: BADSIG-DIAG-INSTALL ( -- ) [: BADSIG-DIAG ;] is BADSIG-XT ;
BADSIG-DIAG-INSTALL

\ A bad NEWTYPE or SUMTYPE declaration reports a declaration-shaped packet (kind,
\ family, token, reason) with no definition fields.
: TDECL-SUGGEST$ ( -- ptr u8 n )
   s" Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks." ;
: TDECL-DIAG-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   s" E-BAD-DECLARATION" s" fix_family_declaration" s" rejected" JHEAD
   s" decl" JKEY ka ku JSTR 44 EMIT1
   s" family" JKEY fa fu JSTR 44 EMIT1
   s" token" JKEY ta tu JSTR 44 EMIT1
   s" reason" JKEY wa wu JSTR 44 EMIT1
   ta tu TDECL-SUGGEST$ JTAIL ;
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

\ A certified sig-less word with an unknown tag or an absurd var count stays
\ unrecorded, and the refusal names the word and why: its callers otherwise fail
\ later as undefined with no hint that the producer was the problem.
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

\ W-EFFECT-NOT-RECORDED is a warning outside the repair contract: the word
\ certified and loaded, but a later call refuses it as undefined. The JSON
\ carries no verdict, repair class or suggestion, and the run's status stands.
: REC-REFUSE-JSON ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   s" W-EFFECT-NOT-RECORDED" JSTART
   s" word" JKEY na nu JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   NM-SPAN JTOKEN-FIELDS
   s" reason" JKEY wa wu JSTR
   125 EMIT1 ;

: REC-REFUSE-EMIT ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   na nu wa wu JSON-DIAGS @ IF REC-REFUSE-JSON ELSE REC-REFUSE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ tools/check.f's run turns WARN-DIAGS off: the run loads what the check before
\ it checked, and that check wrote the warnings with their positions.
variable WARN-DIAGS   -1 WARN-DIAGS !

: REC-REFUSE-DIAG ( ptr u8 n -- )
   WARN-DIAGS @ 0= IF 2drop EXIT THEN
   REC-REFUSE-WHY REC-REFUSE-EMIT ;

\ The render is also the text an AOT capture carries for a word whose effect was
\ inferred; CHECK's declared arm hands its text to CHECKER-USIG-CERT-ADD. The
\ capture runs after the record exists, because it reads the record's symbol.
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

\ Global-vs-used-public shadow (CK-SHADOW-CAPTURE): the reject names both
\ candidates with their arity. Effects read through the read-only USIGS accessors
\ (CHECKER-FIND-USIG-SYM, E-DIN@, E-DOUT@, EFF-ROW-N), never the bare-name
\ resolver, so rendering cannot re-trigger the shadow throw.
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
\ The used packages a bare token resolves in: the slots checker.f CK-USED-MARK
\ set in CK-USED-MASK, in using-scan order, each package once.
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
\ `used_packages` lists each used package the token resolves in, between the
\ token's place and the suggestion, so this packet closes itself. For the shadow
\ that is one package: every matched slot holds its one used public.
: USING-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   code codeu class classu s" rejected" JHEAD
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

\ A package public its own private tail shadows (checker.f SHADOW-ARITY-CK). The
\ widths come from the check, not the store: the rule runs before the append, so
\ the public has no row to read. Both sides are in cells, what the compiler reads
\ a contract in. Package and tail come from the private twin's symbol, already
\ folded.
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
   s" E-SHADOWED-ARITY" s" match_shadowed_private_effect" s" rejected" JHEAD
   s" token" JKEY SBA-TWIN @ SYM-NAME$ JSTR 44 EMIT1
   s" package" JKEY SBA-TWIN @ SYM-PKG$ JSTR 44 EMIT1
   NM-SPAN s" A private word of this package owns the same tail, and a bare tail binds the private word first, so the native compiler reads this definition's arity from it. Give the public definition the private word's effect, or rename one of the two." JTAIL ;
: SBARITY-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF SBARITY-JSON ELSE SBARITY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ A bare token two used publics export (checker.f CHECKER-RESOLVE:RAISE): the
\ reject names each package it resolves in.
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
\ A top-level token the load refuses, a stretch the pre-pass deferred, or a
\ deferred definition (checker.f CHECKER-VERIFY-TOP, CHECKER-VERIFY-DEFERRED,
\ CHECKER-VERIFY-DEFERRED-BODY): records at the token, no definition fields. A
\ refusal has its own code (tools/diag-code.f) with the class and suggestion a
\ body's reference gets; a deferral is no refusal, under W-CHECK-DEFERRED.
: TOP-SPAN-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n k:ptr ku:n v:ptr vu:n g:ptr gu:n :}
   c cu k ku v vu JHEAD
   s" token" JKEY USH-TOK-A @ USH-TOK-U @ JSTR 44 EMIT1
   USH-TOK-A @ USH-TOK-U @ g gu JTAIL ;
\ `CODE habu: FILE:LINE:COLUMN: TEXT 'TOKEN'`, the place when the token locates.
: TOP-SPAN-PROSE ( ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n t:ptr tu:n :}
   c cu DTXT  s"  habu: " DTXT
   USH-TOK-A @  USH-TOK-A @ USH-TOK-U @ +  JLOCATE IF
      DIAGFB DIAGFU @ DTXT  58 EMIT1  JLOC-L @ JNUM  58 EMIT1  JLOC-C @ JNUM  s" : " DTXT
   THEN
   t tu DTXT  s"  '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1 ;
: TOP-UNDEF-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-UNDEFINED-TOP-LEVEL" s" unknown_rejection" s" rejected"
      s" Inspect the token, signature, and raw stack evidence." TOP-SPAN-JSON
   ELSE s" E-UNDEFINED-TOP-LEVEL" s" undefined word" TOP-SPAN-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: TOP-BADQUAL-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-BAD-QUALIFIED-TOP-LEVEL" s" fix_qualified_name" s" rejected"
      s" Use one ':' qualifier, e.g. PKG:WORD." TOP-SPAN-JSON
   ELSE s" E-BAD-QUALIFIED-TOP-LEVEL" s" malformed qualified name" TOP-SPAN-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ Only the verifier's child reports a deferral, and it writes packets.
: TOP-DEFERRED-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   s" W-CHECK-DEFERRED" s" rewrite_uncheckable" s" deferred"
   s" Rewrite with modeled words or isolate an audited primitive." TOP-SPAN-JSON
   10 EMIT1
   RSBUF-FLUSH ;
\ checker.f SHADOW-DIAG-XT selects: 0 the using-shadow reference site, 1 the
\ arity-shadow definition site, 2 a using ambiguity, 3 and 4 a top-level token
\ undefined or malformed, 5 a deferred top-level stretch or definition. One hook
\ because every defer before `: TRUST` takes a slot of the engine's pre-trust
\ pending table (src/habu/layout.f PD-CAP).
: SHADOW-DIAG ( n -- )
   {: sel:n :}
   sel 1 = IF SBARITY-DIAG EXIT THEN
   sel 2 = IF UAMB-DIAG EXIT THEN
   sel 3 = IF TOP-UNDEF-DIAG EXIT THEN
   sel 4 = IF TOP-BADQUAL-DIAG EXIT THEN
   sel 5 = IF TOP-DEFERRED-DIAG EXIT THEN
   USHADOW-DIAG ;
: SHADOW-DIAG-INSTALL ( -- ) [: SHADOW-DIAG ;] is SHADOW-DIAG-XT ;
SHADOW-DIAG-INSTALL

\ A `trust` row naming a word the engine resolves to nothing. It sits beside the
\ shadow diagnostic because it stops a stale row from becoming one. Placed at the
\ row's name (checker.f TRUST) when that lies in checked text.
: TSTALE-PROSE ( -- )
   s" E-TRUST-UNRESOLVED habu: trust row for '" DTXT  TSR-TOK-A @ TSR-TOK-U @ DTXT
   s" ' names no word where its record lands: nothing in the open" DTXT
   s"  section's wordlist, the global wordlist outside a package, or PKG's" DTXT
   s"  public wordlist for PKG:TAIL is" DTXT
   s"  spelled that way, so the effect would be recorded against a symbol" DTXT
   s"  the engine never defined. Delete the row, correct the name to the" DTXT
   s"  word it was meant to describe, or write it in the section that" DTXT
   s"  defines that word" DTXT ;
: TSTALE-JSON ( -- )
   s" E-TRUST-UNRESOLVED" s" fix_stale_trust_row" s" rejected" JHEAD
   s" token" JKEY TSR-TOK-A @ TSR-TOK-U @ JSTR 44 EMIT1
   TSR-TOK-A @ TSR-TOK-U @ s" This trust row names no word in the wordlist its record lands in: the open section's, the global wordlist outside a package, or PKG's public wordlist for PKG:TAIL. Delete the row if the word is gone, correct the spelling, or write the row in the section that defines the word." JTAIL ;
: TSTALE-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF TSTALE-JSON ELSE TSTALE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A storage registrar called outside the verifier window (checker.f
\ CHECKER-REPLAY-NAME-OK?): names the word and the definer that makes it soundly.
\ The name is the run's, so the packet carries no place.
: REPLAY-ONLY-PROSE ( -- )
   s" E-PKG-CONTEXT habu: storage record for '" DTXT  RPL-TOK-A @ RPL-TOK-U @ DTXT
   s" ' refused outside the engine's verifier window: a checker storage" DTXT
   s"  registrar records a definer's accessor only while the source pre-pass" DTXT
   s"  replays the definer that defines the word, so from source the record" DTXT
   s"  would certify callers against an effect the engine never binds to that" DTXT
   s"  name. Define the storage with its definer: TYPED-VARIABLE, TYPED-BUFFER," DTXT
   s"  LAYOUT-BUFFER or DYNAMIC-BUFFER" DTXT ;
: REPLAY-ONLY-JSON ( -- )
   s" E-PKG-CONTEXT" s" use_storage_definer" s" rejected" JHEAD
   s" token" JKEY RPL-TOK-A @ RPL-TOK-U @ JSTR 44 EMIT1
   NULL-PTR 0 s" A checker storage registrar records a definer's accessor only inside the engine's verifier window. Define the storage with its definer (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) instead of calling the registrar." JTAIL ;
: REPLAY-ONLY-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF REPLAY-ONLY-JSON ELSE REPLAY-ONLY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A malformed qualified name's record (checker.f CHECKER-RECORD-NAME) has the
\ class and suggestion of E-BAD-QUALIFIED, and a code of its own in a refused
\ record's shape (tools/diag-code.f); the refusal still throws E-BAD-QUALIFIED.
\ Placed at TSR-AT-A (checker.f CHECKER-RECORD-NAME-AT) when in checked text.
: BADQUAL-PROSE ( -- )
   s" E-BAD-QUALIFIED-RECORD habu: record for '" DTXT  TSR-TOK-A @ TSR-TOK-U @ DTXT
   s" ' refused: malformed qualified name, where one non-edge ':' selects a" DTXT
   s"  package and a second ':' names no word. Use one ':' qualifier, e.g." DTXT
   s"  PKG:WORD" DTXT ;
: BADQUAL-JSON ( -- )
   s" E-BAD-QUALIFIED-RECORD" s" fix_qualified_name" s" rejected" JHEAD
   s" token" JKEY TSR-TOK-A @ TSR-TOK-U @ JSTR 44 EMIT1
   TSR-AT-A @ TSR-AT-U @ s" Use one ':' qualifier, e.g. PKG:WORD." JTAIL ;
: BADQUAL-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF BADQUAL-JSON ELSE BADQUAL-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ checker.f RECORD-DIAG-XT selects: 0 the stale trust row, 1 the storage
\ record, 2 the malformed name.
: RECORD-DIAG ( n -- ) {: which:n :}
   which 1 = IF REPLAY-ONLY-DIAG EXIT THEN
   which 2 = IF BADQUAL-DIAG EXIT THEN
   TSTALE-DIAG ;
: RECORD-DIAG-INSTALL ( -- ) [: RECORD-DIAG ;] is RECORD-DIAG-XT ;
RECORD-DIAG-INSTALL

\ Storage declaration refusals (checker.f CHECKER-STORAGE-REFUSE): the declared
\ word, refused token and reason, the definer standing in for both when the line
\ has no name; no definition fields. A run-time refusal carries no place.
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
   s" E-BAD-STORAGE" STGR-CLASS$ s" rejected" JHEAD
   s" word" JKEY STGR-NAME$ JSTR 44 EMIT1
   s" token" JKEY STGR-TOK$ JSTR 44 EMIT1
   s" reason" JKEY STGR-REASON$ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   STGR-AT @ IF
      DIAGL0 @ JLOC-L !  DIAGC0 @ JLOC-C !  DIAGB0 @ JLOC-B !
      DIAGB0 @ STGR-TOK-U @ + JLOC-E !
      JLOC-FIELDS
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

\ A `generates:` row the checker cannot keep (checker.f CHECKER-GENERATES), on
\ TSTALE-JSON's template: the kind says which of three claims failed, each with
\ its own repair (correct the name, delete the row, or the signature refusal's
\ class and text). Placed at the name where the pre-pass scanner read it.
: GENR-BADSIG-PROSE ( -- )
   SGBAD-UNKNOWN? IF
      s" unknown type '" DTXT  SGBAD-A @ SGBAD-U @ DTXT  s" ' in its effect" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF s" 'ptr' needs an element type, e.g. 'ptr u8' or 'ptr a'" DTXT EXIT THEN
   SGBAD-ARITY? IF
      s" wrong arity for type family '" DTXT  SGBAD-A @ SGBAD-U @ DTXT  s" '" DTXT EXIT
   THEN
   SGBAD-RETURN? IF s" return-stack effect: " DTXT  RCLAUSE-FIX$ DTXT EXIT THEN
   s" its effect is not a stack-effect comment" DTXT ;
: GENR-PROSE ( n -- )
   {: kind:n :}
   s" E-GENERATES-ROW habu: generates: row for '" DTXT  GENR-TOK-A @ GENR-TOK-U @ DTXT
   kind GENR-UNRESOLVED = IF
      s" ' names no word here: the row follows its definer's definition and" DTXT
      s"  names it as the code there spells it. Define the definer first or" DTXT
      s"  correct the name" DTXT EXIT
   THEN
   kind GENR-CREATES = IF
      s" ' already creates: its does> clause, an earlier generates: row or the" DTXT
      s"  definer it wraps already states what it makes. Delete the row" DTXT EXIT
   THEN
   s" ': " DTXT  GENR-BADSIG-PROSE ;
: GENR-SUGGEST$ ( n -- ptr u8 n )
   {: kind:n :}
   kind GENR-UNRESOLVED = IF
      s" This generates: row names no word here. Write it after the definer's definition, spelled as the definition spells it." EXIT
   THEN
   kind GENR-CREATES = IF
      s" This definer already states what it makes: its does> clause, an earlier generates: row or the definer it wraps. Delete the row." EXIT
   THEN
   SGBAD-SUGGEST$ ;
: GENR-CLASS$ ( n -- ptr u8 n )
   {: kind:n :}
   kind GENR-UNRESOLVED = IF s" fix_generates_row" EXIT THEN
   kind GENR-CREATES = IF s" delete_generates_row" EXIT THEN
   SGBAD-CLASS$ ;
: GENR-JSON ( n -- )
   {: kind:n :}
   s" E-GENERATES-ROW" kind GENR-CLASS$ s" rejected" JHEAD
   s" token" JKEY GENR-TOK-A @ GENR-TOK-U @ JSTR 44 EMIT1
   GENR-TOK-A @ GENR-TOK-U @ kind GENR-SUGGEST$ JTAIL ;
: GENR-DIAG ( n -- )
   {: kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF kind GENR-JSON ELSE kind GENR-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: GENERATES-DIAG-INSTALL ( -- ) [: GENR-DIAG ;] is GENERATES-DIAG-XT ;
GENERATES-DIAG-INSTALL

\ A `parses:` or `parses-through:` row the checker refused (checker.f
\ CHECKER-PARSES-ROW), named at its target or else its keyword: a target fault
\ takes fix_parses_row, a syntax fault fix_parses_syntax.
: PRS-SYNTAX? ( n -- bool )
   PARSES-NO-TARGET >= ;
: PRS-REASON ( n -- )
   {: kind:n :}
   kind PARSES-UNDEFINED = IF
      s" ' names no word here: the row follows the word's definition and names" DTXT
      s"  it as the code there spells it" DTXT EXIT
   THEN
   kind PARSES-QUALIFIED = IF
      s" ' is a malformed qualified name: one colon between a package and a" DTXT
      s"  public tail" DTXT EXIT
   THEN
   kind PARSES-SHADOW = IF
      s" ' is refused here: a used package's public shadows the global word of" DTXT
      s"  that name" DTXT EXIT
   THEN
   kind PARSES-AMBIGUOUS = IF
      s" ' is refused here: two used packages export that name" DTXT EXIT
   THEN
   kind PARSES-READS-NONE = IF
      s" ' names a word that reads no source after it: only a word that parses" DTXT
      s"  takes a row" DTXT EXIT
   THEN
   kind PARSES-NO-TARGET = IF s" ' has no target after it" DTXT EXIT THEN
   kind PARSES-COUNT = IF
      s" ': its count is not a number of tokens, 0 or more" DTXT EXIT
   THEN
   s" ': its terminators are not a standalone ( , at least one terminator and a" DTXT
   s"  standalone )" DTXT ;
: PRS-PROSE ( n -- )
   {: kind:n :}
   s" E-PARSES-ROW habu: parses: row '" DTXT  PRS-TOK-A @ PRS-TOK-U @ DTXT
   kind PRS-REASON ;
: PRS-SUGGEST$ ( n -- ptr u8 n )
   PRS-SYNTAX? IF
      s" Write the row as parses: W n or parses-through: W n ( E1 E2 ): a count of 0 or more, then at least one terminator between a standalone ( and )." EXIT
   THEN
   s" This row's target is no word here that reads the source after it. Write the row after the definition of a word that parses, spelled as the definition spells it." ;
: PRS-CLASS$ ( n -- ptr u8 n )
   PRS-SYNTAX? IF s" fix_parses_syntax" EXIT THEN
   s" fix_parses_row" ;
: ROW-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: ca:ptr cu:n la:ptr lu:n sa:ptr su:n :}
   ca cu la lu s" rejected" JHEAD
   s" token" JKEY PRS-TOK-A @ PRS-TOK-U @ JSTR 44 EMIT1
   PRS-TOK-A @ PRS-TOK-U @ sa su JTAIL ;
: PRS-DIAG ( n -- )
   {: kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-PARSES-ROW" kind PRS-CLASS$ kind PRS-SUGGEST$ ROW-JSON
   ELSE
      kind PRS-PROSE
   THEN
   10 EMIT1
   RSBUF-FLUSH ;
: PARSES-DIAG-INSTALL ( -- ) [: PRS-DIAG ;] is PARSES-DIAG-XT ;
PARSES-DIAG-INSTALL

\ A `names:` row the checker refused (checker.f CHECKER-NAMES-ROW): with no
\ target fix_names_syntax, else fix_names_row.
: NMS-SYNTAX? ( n -- bool )
   PARSES-NO-TARGET = ;
: NMS-PROSE ( n -- )
   {: kind:n :}
   s" E-NAMES-ROW habu: names: row '" DTXT  PRS-TOK-A @ PRS-TOK-U @ DTXT
   kind NAMES-NO-STRING = IF
      s" ' names a word that takes no string: only a word whose input row ends in" DTXT
      s"  ptr u8 n takes a row" DTXT EXIT
   THEN
   kind PRS-REASON ;
: NMS-SUGGEST$ ( n -- ptr u8 n )
   NMS-SYNTAX? IF
      s" Write the row as names: W, after the definition of a word W that looks its string operand up as a name." EXIT
   THEN
   s" This row's target is no word here that takes a string ( ptr u8 n ). Write the row after the definition of a word that looks its string operand up as a name, spelled as the definition spells it." ;
: NMS-CLASS$ ( n -- ptr u8 n )
   NMS-SYNTAX? IF s" fix_names_syntax" EXIT THEN
   s" fix_names_row" ;
: NMS-DIAG ( n -- )
   {: kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-NAMES-ROW" kind NMS-CLASS$ kind NMS-SUGGEST$ ROW-JSON
   ELSE
      kind NMS-PROSE
   THEN
   10 EMIT1
   RSBUF-FLUSH ;
: NAMES-DIAG-INSTALL ( -- ) [: NMS-DIAG ;] is NAMES-DIAG-XT ;
NAMES-DIAG-INSTALL

;using
