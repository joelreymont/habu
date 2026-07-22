\ primitive-effect-inventory.f - PEINV ratchet: independent identity inventory of
\ the live primitive-effect axiom rows (PRIM:/PPRIM:), separate from the TRUSTED
\ site classes in tools/trusted-inventory.f.
\
\ Background. The checker's typing trust root is the primitive effect table (PES)
\ in src/core/checker.f: every checked primitive's stack effect is one axiom row
\ declared by `PRIM: name eff... PRIM;` or `PPRIM: pkg name eff... PPRIM;`. The
\ prop census (test/prop-test-core.f AX-CENSUS) difftests each live row's arity,
\ and the trusted-inventory `prim-axiom` class counts the checker's axiom-model
\ TRUST sites (nominal casts, the census readers) -- but neither ratchets the
\ authoritative PRIM:/PPRIM: rows against a committed inventory. A row could be
\ added, deleted, duplicated, or reordered without an audited migration.
\
\ This tool closes that gap. It streams the three boot-prefix sources that host
\ the PES axioms (checker.f, sumtype.f, layout-buffer.f -- the PRIM:/PPRIM:-bearing
\ files, in tools/boot-pin.f BP-EACH load order, which is the live table order),
\ parses each row, and assigns it a stable identity: the canonical tuple of
\   <kind> <defining-package> <word-spelling> <flags> <normalized-effect-tokens>
\ where kind is prim|pprim, package is `-` for a bare PRIM: (global) or the folded
\ PPRIM: package, spelling and effect tokens are folded lowercase (Forth is
\ case-insensitive), and flags is one token naming the row's audited properties:
\ `trusted-only` when a following PRIM-TRUSTED-ONLY! marks the row, `private`
\ when a PPRIM: row is closed with CLOSE-PRIVATE instead of PPRIM; (the axiom is
\ interned into the package PRIVATE wordlist, not its public one),
\ `private-trusted-only` for both, else `-`. Recording the visibility means a
\ closer swap that would publish a private axiom changes the identity and fails
\ the ratchet. The identity is NEVER a path, line, ordinal, or PES
\ address, so case/whitespace/comment-only source edits preserve it.
\
\ The committed manifest (a separately-committed ORDERED list -- deliberately not
\ sorted, so a pure reorder is detectable and the exact row can be named) lives in
\ TRUSTED.md behind the `primitive-effect-inventory-manifest` marker. The ratchet
\ fails closed when the live source rows differ from the manifest by an added,
\ deleted, duplicated, or reordered identity, naming the exact row. Strict mode
\ additionally cross-checks the parsed rows against the live PES registry
\ (#PE, package/name, arity, trusted-only flag) row-for-row, so the source parse
\ is proven faithful to the authoritative in-image table.
\
\ Run: bin/hb --load tools/primitive-effect-inventory.f                    (report)
\ Or:  bin/hb --load tools/primitive-effect-inventory.f -- strict          (report + live cross-check)
\ Or:  bin/hb --load tools/primitive-effect-inventory.f -- baseline PATH   (ratchet vs committed manifest)
\ Or:  bin/hb --load tools/primitive-effect-inventory.f -- manifest        (emit the canonical manifest block)

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/adt/option.f
require tools/lint/text.f
require lib/argv.f

package PEINV

public

\ ---- live PES registry leaves (trusted: the axiom table is outside the checker
\ model; these read it exactly as the prop census does, wrapped so the internal
\ engine words stay referenceable only from a TRUSTED: boundary) ---------------
TRUSTED: LIVE-COUNT ( -- n )
   #PE @ ;
TRUSTED: LIVE-PKG$ ( n -- ptr u8 n )
   PE-SYM@ SYM-PKG$ ;
TRUSTED: LIVE-NAME$ ( n -- ptr u8 n )
   PE-SYM@ SYM-NAME$ ;
TRUSTED: LIVE-STK ( n -- n )   \ effect-node offset -> count of EN-PUSH nodes to the base row
   0 swap begin dup E-PTR EN.TAG @ EN-PUSH = while swap 1+ swap E-PTR EN.B @ repeat drop ;
TRUSTED: LIVE-ARITY ( n -- n n )   \ pe-idx -> declared ( din dout )
   PE-EFF@ E-PTR dup ER.DIN @ LIVE-STK swap ER.DOUT @ LIVE-STK ;
TRUSTED: LIVE-TRUSTED-ONLY? ( n -- bool )
   PE-TRUSTED-ONLY? ;

private

\ ---- exit codes (positive, sysexits-style; error-code-lint scans negatives) ---
90 constant E-PEINV-RATCHET
91 constant E-PEINV-BASELINE
92 constant E-PEINV-CROSS

\ ---- capacities --------------------------------------------------------------
384 constant ROW-MAX          \ 298 live rows + headroom
15 constant COLS
384 constant BASE-MAX
2 constant BCOLS
$80000 constant FILE-CAP      \ >= largest scanned source (checker.f ~ 0x53000)
$80000 constant MAN-CAP       \ >= TRUSTED.md
$80000 constant ARENA-CAP     \ pooled strings: paths, pkgs, names, effs, ids
512 constant EFF-CAP          \ one row's normalized effect token span
256 constant ID-CAP           \ one canonical identity line

\ ---- char constants ----------------------------------------------------------
9 constant CH-TAB
10 constant CH-LF
32 constant CH-SPACE
40 constant CH-LPAREN
41 constant CH-RPAREN
92 constant CH-BSLASH

\ ---- row flag bits -----------------------------------------------------------
\ The flag column is one token in the canonical identity, so the two bits share a
\ single spelling: `-`, `trusted-only`, `private`, `private-trusted-only`.
1 constant FLAG-TRUSTED-ONLY
2 constant FLAG-PRIVATE

\ ---- column indices ----------------------------------------------------------
0 constant C-KIND
1 constant C-LINE
2 constant C-FLAGS
3 constant C-DIN
4 constant C-DOUT
5 constant C-PATH-O
6 constant C-PATH-U
7 constant C-PKG-O
8 constant C-PKG-U
9 constant C-NAME-O
10 constant C-NAME-U
11 constant C-EFF-O
12 constant C-EFF-U
13 constant C-ID-O
14 constant C-ID-U

\ ---- allocated storage (pointers held in cells, typed through ptr-field) -----
variable ROW-A
variable BASE-A
variable SRC-A
variable MAN-A
variable ARENA-A

variable ROW#            \ scanned rows
variable BASE#           \ committed manifest rows
variable ARENA-END

\ ---- scanner state -----------------------------------------------------------
variable SRC-LEN
variable SPOS
variable SLINE
variable WT-S             \ token start scratch
variable CT-O  variable CT-U  variable CT-LINE     \ current token
variable PT-O  variable PT-U                        \ previous token (definer lookback)
variable RS               \ row state: 0 idle, 1 expect-pkg, 2 expect-name, 3 collect-eff
variable CUR-KIND
variable CUR-LINE
variable CUR-DIN  variable CUR-DOUT
variable PEND-PKG-O  variable PEND-PKG-U
variable PEND-NAME-O  variable PEND-NAME-U
variable CUR-PATH-O  variable CUR-PATH-U

\ ---- loop / scratch indices --------------------------------------------------
variable SF-I
variable BI-I
variable RP-I
variable CX-I
variable ADD-I
variable REM-I
variable ORD-I
variable MC-I   variable MC-ACC
variable OC-I   variable OC-ACC
variable ML-S  variable ML-X
variable MK-M  variable MK-E  variable MK-L
variable RD-BAD
variable MAN-OK
variable CNT-ACC
variable EFF-LEN
variable IDB-LEN

create EFFBUF EFF-CAP allot
create IDBUF ID-CAP allot
create NUM-BUF 32 allot
variable NUM-L
create OUT-CH 1 allot

\ ---- typed pointer views over the allocated blocks ---------------------------
: ROW-BASE ( -- ptr a ) ROW-A 0 ptr-field @ ;
: BASE-BASE ( -- ptr a ) BASE-A 0 ptr-field @ ;
: SRC@ ( -- ptr u8 ) SRC-A 0 ptr-field @ ;
: SRC! ( ptr u8 -- ) SRC-A 0 ptr-field ! ;
: MAN-BUF ( -- ptr u8 ) MAN-A 0 ptr-field @ ;
: ARENA@ ( -- ptr u8 ) ARENA-A 0 ptr-field @ ;

: INIT ( -- )
   ROW-A @ 0= if ROW-MAX COLS * >COUNT MEM-ALLOC-CELLS ROW-A 0 ptr-field ! then
   BASE-A @ 0= if BASE-MAX BCOLS * >COUNT MEM-ALLOC-CELLS BASE-A 0 ptr-field ! then
   SRC-A @ 0= if FILE-CAP MEM-ALLOC-PTR SRC-A 0 ptr-field ! then
   MAN-A @ 0= if MAN-CAP MEM-ALLOC-PTR MAN-A 0 ptr-field ! then
   ARENA-A @ 0= if ARENA-CAP MEM-ALLOC-PTR ARENA-A 0 ptr-field ! then
   0 ROW# !  0 BASE# !  0 ARENA-END ! ;

: RESET ( -- )
   0 ROW# !  0 BASE# !  0 ARENA-END ! ;

\ ---- output through the shared lint sink (testable) --------------------------
: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;
: OUT-C ( n -- )
   OUT-CH c!  OUT-CH 1 OUT ;
: OUT-NL ( -- ) CH-LF OUT-C ;
: OUT-TAB ( -- ) CH-TAB OUT-C ;
: OUT-U ( n -- )
   0 NUM-L !
   dup 0= if drop 48 OUT-C exit then
   begin dup 0 > while
      dup 10 mod 48 + NUM-BUF NUM-L @ + c!
      10 /  NUM-L @ 1+ NUM-L !
   repeat drop
   begin NUM-L @ 0 > while
      NUM-L @ 1- NUM-L !
      NUM-BUF NUM-L @ + c@ OUT-C
   repeat ;

\ ---- row table ---------------------------------------------------------------
: RCELL ( n n -- ptr a )
   ROW-MAX * + cells ROW-BASE + ;
: RC@ ( n n -- n ) RCELL @ ;
: RC! ( n n n -- ) RCELL ! ;

: ROW-KIND ( n -- n )   C-KIND RC@ ;
: ROW-LINE ( n -- n )   C-LINE RC@ ;
: ROW-FLAGS ( n -- n )  C-FLAGS RC@ ;
: ROW-DIN ( n -- n )    C-DIN RC@ ;
: ROW-DOUT ( n -- n )   C-DOUT RC@ ;
: O$ ( n n -- ptr u8 n )  swap ARENA@ + swap ;
: ROW-PATH$ ( n -- ptr u8 n ) {: i:n :} i C-PATH-O RC@ i C-PATH-U RC@ O$ ;
: ROW-PKG$ ( n -- ptr u8 n )  {: i:n :} i C-PKG-O RC@ i C-PKG-U RC@ O$ ;
: ROW-NAME$ ( n -- ptr u8 n ) {: i:n :} i C-NAME-O RC@ i C-NAME-U RC@ O$ ;
: ROW-EFF$ ( n -- ptr u8 n )  {: i:n :} i C-EFF-O RC@ i C-EFF-U RC@ O$ ;
: ROW-ID$ ( n -- ptr u8 n )   {: i:n :} i C-ID-O RC@ i C-ID-U RC@ O$ ;

: ROWS ( -- n ) ROW# @ ;
: TRUSTED-ONLY? ( n -- bool ) ROW-FLAGS FLAG-TRUSTED-ONLY and 0 <> ;
: PRIVATE-ROW? ( n -- bool ) ROW-FLAGS FLAG-PRIVATE and 0 <> ;

\ ---- string arena ------------------------------------------------------------
: STORE-RAW ( ptr u8 n -- n n ) {: a:ptr u:n :}
   ARENA-END @ u + ARENA-CAP > if s" primitive-effect-inventory: string arena overflow" E-PEINV-RATCHET die then
   a ARENA@ ARENA-END @ + u LINT-BMOVE
   ARENA-END @ u
   ARENA-END @ u + ARENA-END ! ;

: STORE-FOLDED ( ptr u8 n -- n n ) {: a:ptr u:n :}
   ARENA-END @ u + ARENA-CAP > if s" primitive-effect-inventory: string arena overflow" E-PEINV-RATCHET die then
   ARENA-END @
   0 SF-I !
   begin SF-I @ u < while
      a SF-I @ + c@ LINT-FOLD  ARENA@ ARENA-END @ + SF-I @ + c!
      SF-I @ 1+ SF-I !
   repeat
   u
   over u + ARENA-END ! ;

\ ---- effect + identity byte builders -----------------------------------------
: EFF-RESET ( -- ) 0 EFF-LEN ! ;
: EFF-C+ ( n -- )
   EFF-LEN @ EFF-CAP >= if s" primitive-effect-inventory: effect overflow" E-PEINV-RATCHET die then
   EFFBUF EFF-LEN @ + c!  1 EFF-LEN +! ;
: EFF-FOLD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SF-I !
   begin SF-I @ u < while a SF-I @ + c@ LINT-FOLD EFF-C+ SF-I @ 1+ SF-I ! repeat ;
: EFF$ ( -- ptr u8 n ) EFFBUF EFF-LEN @ ;

: IDB-RESET ( -- ) 0 IDB-LEN ! ;
: IDB-C+ ( n -- )
   IDB-LEN @ ID-CAP >= if s" primitive-effect-inventory: identity overflow" E-PEINV-RATCHET die then
   IDBUF IDB-LEN @ + c!  1 IDB-LEN +! ;
: IDB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SF-I !
   begin SF-I @ u < while a SF-I @ + c@ IDB-C+ SF-I @ 1+ SF-I ! repeat ;
: IDB-SP ( -- ) CH-SPACE IDB-C+ ;
: IDB$ ( -- ptr u8 n ) IDBUF IDB-LEN @ ;

: DASH-IF-EMPTY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if s" -" exit then a u ;
: KIND-STR ( n -- ptr u8 n ) 0 = if s" prim" exit then s" pprim" ;
: FLAG-STR ( n -- ptr u8 n ) {: f:n :}
   f FLAG-PRIVATE and 0 <> if
      f FLAG-TRUSTED-ONLY and 0 <> if s" private-trusted-only" exit then
      s" private" exit
   then
   f FLAG-TRUSTED-ONLY and 0 <> if s" trusted-only" exit then
   s" -" ;

\ ---- streaming scanner (shared lexeme rules; line + paren comments and string
\ bodies are skipped so a comment/string mention never produces a row) ---------
: SRC$ ( n n -- ptr u8 n ) swap SRC@ + swap ;
: SEND? ( -- bool ) SPOS @ SRC-LEN @ >= ;
: SCUR ( -- n ) SRC@ SPOS @ + c@ ;
: SADV ( -- )
   SCUR CH-LF = if SLINE @ 1+ SLINE ! then
   SPOS @ 1+ SPOS ! ;
: SKIP-LINE-COMMENT ( -- )
   begin SEND? 0= while SCUR CH-LF = if exit then SADV repeat ;
: PAREN-STANDALONE? ( -- bool )
   SPOS @ 1+ SRC-LEN @ >= if LINT-TRUE exit then
   SRC@ SPOS @ 1+ + c@ LINT-WS? ;
: SKIP-PAREN ( -- )
   SADV
   begin SEND? 0= while
      SCUR CH-RPAREN = if SADV exit then
      SADV
   repeat ;
: CAPTURE-QUOTE ( -- )   \ after a plain string opener token: skip to closing quote
   SEND? if exit then
   SADV
   begin SEND? 0= while SCUR DQUOTE = if SADV exit then SADV repeat ;
: SKIP-ESC-QUOTE ( -- )  \ after an escaped string opener token: skip escape-aware
   SEND? if exit then
   SADV
   begin SEND? 0= while
      SCUR DQUOTE = if SADV exit then
      SCUR CH-BSLASH = if SADV SEND? if exit then then
      SADV
   repeat ;
: WORD-END? ( -- bool ) SEND? if LINT-TRUE exit then SCUR LINT-WS? ;
: CT$ ( -- ptr u8 n ) CT-O @ CT-U @ SRC$ ;
: READ-WORD ( -- )   \ read the raw current token into CT-* (no string-body skip)
   SPOS @ WT-S !
   SLINE @ CT-LINE !
   begin WORD-END? 0= while SADV repeat
   WT-S @ CT-O !
   SPOS @ WT-S @ - CT-U ! ;
\ Skip a string body ONLY when idle: a real `s" ... "` in the source must not
\ produce a false row opener. Inside a row the name/effect tokens are read raw,
\ because a primitive can be NAMED `s"`, `c"`, `."`, `s\"`, `c\"`, or `.\"`.
: MAYBE-SKIP-STRING ( -- )
   CT$ LINT-NORMAL-STRING-OPENER? if CAPTURE-QUOTE exit then
   CT$ LINT-ESC-STRING-OPENER? if SKIP-ESC-QUOTE then ;

: PT$ ( -- ptr u8 n ) PT-O @ PT-U @ SRC$ ;
: DEFINER-REF? ( -- bool )   \ current token is in name position (`: PRIM:`, `' set-check`, ...)
   PT-U @ 0= if LINT-FALSE exit then
   PT$ s" :" LINT-STR= if LINT-TRUE exit then
   PT$ s" '" LINT-STR= if LINT-TRUE exit then
   PT$ s" [']" LINT-STR= if LINT-TRUE exit then
   PT$ s" postpone" LINT-STR=CI if LINT-TRUE exit then
   PT$ s" undefine" LINT-STR=CI ;

\ ---- row state machine -------------------------------------------------------
\ Two closers exist for a package row. `PPRIM;` interns the axiom into the
\ package PUBLIC wordlist; `CLOSE-PRIVATE` interns it into the package PRIVATE
\ wordlist. That visibility is part of the row identity, so swapping one closer
\ for the other changes the committed identity and trips the ratchet instead of
\ silently publishing a private axiom. A bare `PRIM:` row has no package, so
\ `CLOSE-PRIVATE` is not a closer there and stays an ordinary effect token.
: CLOSER$ ( -- ptr u8 n ) CUR-KIND @ 0 = if s" prim;" exit then s" pprim;" ;
: PRIVATE-CLOSER$ ( -- ptr u8 n ) s" close-private" ;

: FINALIZE-ROW ( n -- ) {: flags:n :}
   ROW# @ ROW-MAX >= if s" primitive-effect-inventory: row overflow" E-PEINV-RATCHET die then
   CUR-KIND @ ROW# @ C-KIND RC!
   CUR-LINE @ ROW# @ C-LINE RC!
   flags ROW# @ C-FLAGS RC!
   CUR-DIN @ ROW# @ C-DIN RC!
   CUR-DOUT @ ROW# @ C-DOUT RC!
   CUR-PATH-O @ ROW# @ C-PATH-O RC!
   CUR-PATH-U @ ROW# @ C-PATH-U RC!
   PEND-PKG-O @ ROW# @ C-PKG-O RC!
   PEND-PKG-U @ ROW# @ C-PKG-U RC!
   PEND-NAME-O @ ROW# @ C-NAME-O RC!
   PEND-NAME-U @ ROW# @ C-NAME-U RC!
   EFF$ STORE-RAW ROW# @ C-EFF-U RC! ROW# @ C-EFF-O RC!
   ROW# @ 1+ ROW# !
   0 RS ! ;

: START-PRIM ( -- )
   0 CUR-KIND !  CT-LINE @ CUR-LINE !
   0 PEND-PKG-O !  0 PEND-PKG-U !
   EFF-RESET  0 CUR-DIN !  0 CUR-DOUT !
   2 RS ! ;
: START-PPRIM ( -- )
   1 CUR-KIND !  CT-LINE @ CUR-LINE !
   EFF-RESET  0 CUR-DIN !  0 CUR-DOUT !
   1 RS ! ;
: MARK-TRUSTED-ONLY ( -- )   \ add the flag to the most recently finalized row
   ROW# @ 0 > if
      ROW# @ 1- C-FLAGS RC@ FLAG-TRUSTED-ONLY or ROW# @ 1- C-FLAGS RC!
   then ;

: FEED-IDLE ( -- )
   DEFINER-REF? if exit then
   CT$ s" prim:" LINT-STR=CI if START-PRIM exit then
   CT$ s" pprim:" LINT-STR=CI if START-PPRIM exit then
   CT$ s" prim-trusted-only!" LINT-STR=CI if MARK-TRUSTED-ONLY then ;
: FEED-PKG ( -- )
   CT$ STORE-FOLDED PEND-PKG-U ! PEND-PKG-O !
   2 RS ! ;
: FEED-NAME ( -- )
   CT$ STORE-FOLDED PEND-NAME-U ! PEND-NAME-O !
   3 RS ! ;
: APPEND-EFF-TOKEN ( -- )
   EFF-LEN @ 0 > if CH-SPACE EFF-C+ then
   CT$ EFF-FOLD+
   CT$ s" pe-in" LINT-STR=CI if 1 CUR-DIN +! then
   CT$ s" pe-out" LINT-STR=CI if 1 CUR-DOUT +! then ;
: PRIVATE-CLOSE? ( -- bool )   \ only a package row can close into a private wordlist
   CUR-KIND @ 0 = if LINT-FALSE exit then
   CT$ PRIVATE-CLOSER$ LINT-STR=CI ;

: FEED-EFF ( -- )
   CT$ CLOSER$ LINT-STR=CI if 0 FINALIZE-ROW exit then
   PRIVATE-CLOSE? if FLAG-PRIVATE FINALIZE-ROW exit then
   APPEND-EFF-TOKEN ;

: FEED ( -- )
   RS @ 0 = if FEED-IDLE exit then
   RS @ 1 = if FEED-PKG exit then
   RS @ 2 = if FEED-NAME exit then
   FEED-EFF ;

: RING-ROTATE ( -- )
   CT-O @ PT-O !  CT-U @ PT-U ! ;
: SCAN-TOKEN ( -- )
   READ-WORD
   RS @ 0 = if MAYBE-SKIP-STRING then
   FEED  RING-ROTATE ;
: SCAN-RESET ( -- )
   0 SPOS !  1 SLINE !  0 PT-U !  0 RS ! ;

public
: SCAN-SOURCE ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n sa:ptr su:n :}
   pa pu STORE-RAW CUR-PATH-U ! CUR-PATH-O !
   sa SRC!  su SRC-LEN !
   SCAN-RESET
   begin SEND? 0= while
      SCUR LINT-WS? if SADV
      else SCUR CH-BSLASH = if SKIP-LINE-COMMENT
      else SCUR CH-LPAREN = PAREN-STANDALONE? and if SKIP-PAREN
      else SCAN-TOKEN then then then
   repeat ;
private

\ ---- canonical identities ----------------------------------------------------
: ROW-CANON$ ( n -- ptr u8 n ) {: i:n :}
   IDB-RESET
   i ROW-KIND KIND-STR IDB+ IDB-SP
   i ROW-PKG$ DASH-IF-EMPTY IDB+ IDB-SP
   i ROW-NAME$ IDB+ IDB-SP
   i ROW-FLAGS FLAG-STR IDB+ IDB-SP
   i ROW-EFF$ DASH-IF-EMPTY IDB+
   IDB$ ;

public
: BUILD-IDS ( -- )   \ fill each row's canonical identity column (after flags settle)
   0 BI-I !
   begin BI-I @ ROW# @ < while
      BI-I @ ROW-CANON$ STORE-RAW BI-I @ C-ID-U RC! BI-I @ C-ID-O RC!
      BI-I @ 1+ BI-I !
   repeat ;
private

\ ---- committed manifest (ordered identity list behind the marker) ------------
: MARKER$ ( -- ptr u8 n ) s" primitive-effect-inventory-manifest" ;

: BASE-ID$ ( n -- ptr u8 n ) {: j:n :}
   j BCOLS * cells BASE-BASE + @
   j BCOLS * 1+ cells BASE-BASE + @  O$ ;
: BASE+ ( n n -- ) {: off:n len:n :}
   BASE# @ BASE-MAX >= if s" primitive-effect-inventory: manifest overflow" E-PEINV-BASELINE die then
   off BASE# @ BCOLS * cells BASE-BASE + !
   len BASE# @ BCOLS * 1+ cells BASE-BASE + !
   BASE# @ 1+ BASE# ! ;

: KIND-TOK-OK? ( ptr u8 n -- bool )
   2dup s" prim" LINT-STR= if 2drop LINT-TRUE exit then s" pprim" LINT-STR= ;
: FLAG-TOK-OK? ( ptr u8 n -- bool )
   2dup s" trusted-only" LINT-STR= if 2drop LINT-TRUE exit then
   2dup s" private" LINT-STR= if 2drop LINT-TRUE exit then
   2dup s" private-trusted-only" LINT-STR= if 2drop LINT-TRUE exit then
   s" -" LINT-STR= ;

\ Re-canonicalise one committed line: split, validate the fixed fields, then join
\ every token with a single space so it matches ROW-CANON$ regardless of the
\ committed line's incidental whitespace. Blank lines are skipped; a malformed
\ line rejects the whole manifest fail-closed.
: BASE-LINE+ ( ptr u8 n -- bool )
   LINT-TRIM {: a:ptr u:n :}
   u 0= if LINT-TRUE exit then
   a u SPLIT-WHITESPACE
   SN# @ 4 < if LINT-FALSE exit then
   0 S@ KIND-TOK-OK? 0= if LINT-FALSE exit then
   3 S@ FLAG-TOK-OK? 0= if LINT-FALSE exit then
   1 S@ nip 0= if LINT-FALSE exit then
   2 S@ nip 0= if LINT-FALSE exit then
   IDB-RESET
   0 begin dup SN# @ < while
      dup 0 > if IDB-SP then
      dup S@ IDB+
      1+
   repeat drop
   IDB$ STORE-RAW BASE+
   LINT-TRUE ;

: MAN-LINES ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LINT-TRUE MAN-OK !
   0 ML-S !  0 ML-X !
   begin ML-X @ u < while
      a ML-X @ + c@ CH-LF = if
         a ML-S @ +  ML-X @ ML-S @ -  BASE-LINE+ 0= if LINT-FALSE MAN-OK ! then
         ML-X @ 1+ ML-S !
      then
      ML-X @ 1+ ML-X !
   repeat
   ML-S @ u < if a ML-S @ +  u ML-S @ -  BASE-LINE+ 0= if LINT-FALSE MAN-OK ! then then
   MAN-OK @ ;

: MAN-DUP? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a second marker after MK-M shadows the block
   a MK-M @ 1+ +  u MK-M @ 1+ -  MARKER$ LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF drop LINT-TRUE ENDOF
   ;MATCH ;
: MAN-ROWS ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ rows between the first LF after the marker and the --> end
   a MK-M @ +  MK-E @  CH-LF LINT-INDEX-OF MATCH option
     none OF LINT-TRUE ENDOF
     some OF MK-L !
        a MK-M @ + MK-L @ 1+ +  MK-E @ MK-L @ 1+ -  MAN-LINES ENDOF
   ;MATCH ;
: MAN-END ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ locate the --> terminator, then parse rows
   a MK-M @ +  u MK-M @ -  s" -->" LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF MK-E !  a u MAN-ROWS ENDOF
   ;MATCH ;

public
: PARSE-MANIFEST$ ( ptr u8 n -- bool )
   0 BASE# !
   {: a:ptr u:n :}
   a u MARKER$ LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF MK-M !
        a u MAN-DUP? if LINT-FALSE else a u MAN-END then ENDOF
   ;MATCH ;
: BASELINE# ( -- n ) BASE# @ ;
private

\ ---- ratchet: source rows vs committed manifest -----------------------------
\ Occurrence-aware (multiset) comparison: identical axioms may legitimately be
\ declared more than once (e.g. `path0` appears twice with the same effect), so a
\ pure set model would false-flag the committed reality. Instead the manifest
\ records EVERY row in order (repeats included); an occurrence beyond the
\ committed multiplicity is an added/duplicated row, a shortfall is a removed row.
: SRC-ID-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 MC-ACC !  0 MC-I !
   begin MC-I @ ROW# @ < while
      MC-I @ ROW-ID$ a u LINT-STR= if 1 MC-ACC +! then
      MC-I @ 1+ MC-I !
   repeat MC-ACC @ ;
: BASE-ID-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 MC-ACC !  0 MC-I !
   begin MC-I @ BASE# @ < while
      MC-I @ BASE-ID$ a u LINT-STR= if 1 MC-ACC +! then
      MC-I @ 1+ MC-I !
   repeat MC-ACC @ ;
: SRC-OCC-UPTO ( n -- n ) {: i:n :}   \ 1-based occurrence index of ROW-ID$[i] among rows 0..i
   0 OC-ACC !  0 OC-I !
   begin OC-I @ i <= while
      OC-I @ ROW-ID$ i ROW-ID$ LINT-STR= if 1 OC-ACC +! then
      OC-I @ 1+ OC-I !
   repeat OC-ACC @ ;
: BASE-OCC-UPTO ( n -- n ) {: j:n :}
   0 OC-ACC !  0 OC-I !
   begin OC-I @ j <= while
      OC-I @ BASE-ID$ j BASE-ID$ LINT-STR= if 1 OC-ACC +! then
      OC-I @ 1+ OC-I !
   repeat OC-ACC @ ;

: ADD-MSG ( n -- )   \ base multiplicity of ROW-ID$[i] -> classify: new vs excess copy
   0 = if s" primitive-effect-inventory: added row (no committed identity) " OUT exit then
   s" primitive-effect-inventory: duplicated identity (excess copy) " OUT ;
: ADDED# ( -- n )
   0 RD-BAD !  0 ADD-I !
   begin ADD-I @ ROW# @ < while
      ADD-I @ SRC-OCC-UPTO  ADD-I @ ROW-ID$ BASE-ID-COUNT  > if
         ADD-I @ ROW-ID$ BASE-ID-COUNT ADD-MSG
         ADD-I @ ROW-PATH$ OUT s" :" OUT ADD-I @ ROW-LINE OUT-U
         s"  " OUT ADD-I @ ROW-ID$ OUT OUT-NL
         RD-BAD @ 1+ RD-BAD !
      then
      ADD-I @ 1+ ADD-I !
   repeat RD-BAD @ ;
: REMOVED# ( -- n )
   0 RD-BAD !  0 REM-I !
   begin REM-I @ BASE# @ < while
      REM-I @ BASE-OCC-UPTO  REM-I @ BASE-ID$ SRC-ID-COUNT  > if
         s" primitive-effect-inventory: removed row (committed identity absent from source) " OUT
         REM-I @ BASE-ID$ OUT OUT-NL
         RD-BAD @ 1+ RD-BAD !
      then
      REM-I @ 1+ REM-I !
   repeat RD-BAD @ ;
\ Reorder is meaningful only once the multisets agree (no add/delete): identical
\ identities in a different position. Report the first divergent position.
: ORDER-BAD# ( -- n )
   ROW# @ BASE# @ <> if
      s" primitive-effect-inventory: manifest row count " OUT BASE# @ OUT-U
      s"  differs from source " OUT ROW# @ OUT-U OUT-NL
      1 exit
   then
   0 ORD-I !
   begin ORD-I @ ROW# @ < while
      ORD-I @ ROW-ID$ ORD-I @ BASE-ID$ LINT-STR= 0= if
         s" primitive-effect-inventory: reordered row at position " OUT ORD-I @ OUT-U OUT-NL
         s"   expected " OUT ORD-I @ BASE-ID$ OUT OUT-NL
         s"   got      " OUT ORD-I @ ROW-ID$ OUT OUT-NL
         1 exit
      then
      ORD-I @ 1+ ORD-I !
   repeat 0 ;

public
: RATCHET-BAD# ( -- n )
   ADDED# {: a:n :}
   REMOVED# {: r:n :}
   a r + {: sub:n :}
   sub 0 > if a r + exit then           \ multiset differs: order comparison would be noise
   ORDER-BAD# ;

: RATCHET-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if
      s" primitive-effect-inventory: manifest file not found: " OUT a u OUT OUT-NL
      E-PEINV-BASELINE throw
   then
   a u MAN-BUF MAN-CAP READ-FILE PARSE-MANIFEST$ 0= if
      s" primitive-effect-inventory: no/invalid manifest block in " OUT a u OUT OUT-NL
      E-PEINV-BASELINE throw
   then
   BASE# @ 0= if
      s" primitive-effect-inventory: empty manifest block in " OUT a u OUT OUT-NL
      E-PEINV-BASELINE throw
   then ;

: RATCHET-REPORT ( -- )
   RATCHET-BAD# 0 > if
      s" primitive-effect-inventory: ratchet FAILED - primitive identity added, deleted, duplicated, or reordered without a manifest migration" OUT OUT-NL
      E-PEINV-RATCHET throw
   then
   s" primitive-effect-inventory: ratchet ok (" OUT ROW# @ OUT-U
   s"  primitive rows match the committed manifest)" OUT OUT-NL ;
private

\ ---- live-registry cross-check ----------------------------------------------
: CX-ROW-BAD? ( n -- bool ) {: i:n :}
   i ROW-PKG$ i LIVE-PKG$ LINT-STR= 0= if LINT-TRUE exit then
   i ROW-NAME$ i LIVE-NAME$ LINT-STR= 0= if LINT-TRUE exit then
   i LIVE-ARITY {: din:n dout:n :}
   i ROW-DIN din <> if LINT-TRUE exit then
   i ROW-DOUT dout <> if LINT-TRUE exit then
   i TRUSTED-ONLY? i LIVE-TRUSTED-ONLY? xor ;
: CX-ROW. ( n -- ) {: i:n :}
   i LIVE-ARITY {: ld:n lo:n :}
   s" primitive-effect-inventory: live mismatch at " OUT
   i ROW-PATH$ OUT s" :" OUT i ROW-LINE OUT-U
   s"  source " OUT i ROW-PKG$ DASH-IF-EMPTY OUT s" /" OUT i ROW-NAME$ OUT
   s"  (" OUT i ROW-DIN OUT-U s" -" OUT i ROW-DOUT OUT-U s" )" OUT
   s"  vs live " OUT i LIVE-PKG$ DASH-IF-EMPTY OUT s" /" OUT i LIVE-NAME$ OUT
   s"  (" OUT ld OUT-U s" -" OUT lo OUT-U s" )" OUT OUT-NL ;

public
: CROSS-CHECK-BAD# ( -- n )
   ROW# @ LIVE-COUNT <> if
      s" primitive-effect-inventory: parsed row count " OUT ROW# @ OUT-U
      s"  differs from live #PE " OUT LIVE-COUNT OUT-U
      s"  (add the new PRIM:/PPRIM: source file to the scan list)" OUT OUT-NL
      1 exit
   then
   0 RD-BAD !  0 CX-I !
   begin CX-I @ ROW# @ < while
      CX-I @ CX-ROW-BAD? if CX-I @ CX-ROW. RD-BAD @ 1+ RD-BAD ! then
      CX-I @ 1+ CX-I !
   repeat RD-BAD @ ;

: CROSS-CHECK-REPORT ( -- )
   CROSS-CHECK-BAD# 0 > if
      s" primitive-effect-inventory: cross-check FAILED - parsed rows diverge from the live PES registry" OUT OUT-NL
      E-PEINV-CROSS throw
   then
   s" primitive-effect-inventory: cross-check ok (" OUT ROW# @ OUT-U
   s"  axioms match the live registry)" OUT OUT-NL ;
private

\ ---- scan the authoritative PRIM:/PPRIM: sources -----------------------------
\ In tools/boot-pin.f BP-EACH load order, which is the live PES table order.
\ A new prim-bearing boot-prefix file must be added here (the cross-check count
\ mismatch fails closed until it is).
: SCAN-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu FILE? 0= if
      s" primitive-effect-inventory: source not found: " OUT pa pu OUT OUT-NL
      E-PEINV-BASELINE throw
   then
   pa pu SRC@ FILE-CAP READ-FILE {: sa:ptr su:n :}
   pa pu sa su SCAN-SOURCE ;

public
: SCAN-REPO ( -- )
   RESET
   s" src/core/checker.f" SCAN-FILE
   s" src/core/sumtype.f" SCAN-FILE
   s" src/core/layout-buffer.f" SCAN-FILE
   BUILD-IDS ;
private

\ ---- report / manifest emit --------------------------------------------------
: ROW. ( n -- ) {: i:n :}
   i ROW-KIND KIND-STR OUT OUT-TAB
   i ROW-PATH$ OUT OUT-TAB
   i ROW-LINE OUT-U OUT-TAB
   i ROW-PKG$ DASH-IF-EMPTY OUT OUT-TAB
   i ROW-NAME$ OUT OUT-TAB
   i ROW-DIN OUT-U OUT-TAB
   i ROW-DOUT OUT-U OUT-TAB
   i ROW-FLAGS FLAG-STR OUT OUT-TAB
   i ROW-EFF$ DASH-IF-EMPTY OUT OUT-NL ;

: PRIM# ( -- n )
   0 CNT-ACC !  0 RP-I !
   begin RP-I @ ROW# @ < while
      RP-I @ ROW-KIND 0 = if 1 CNT-ACC +! then
      RP-I @ 1+ RP-I !
   repeat CNT-ACC @ ;
: TRUSTED-ONLY# ( -- n )
   0 CNT-ACC !  0 RP-I !
   begin RP-I @ ROW# @ < while
      RP-I @ TRUSTED-ONLY? if 1 CNT-ACC +! then
      RP-I @ 1+ RP-I !
   repeat CNT-ACC @ ;

: FOOTER ( -- )
   s" primitive-effect-inventory: PRIM " OUT PRIM# OUT-U
   s"  PPRIM " OUT ROW# @ PRIM# - OUT-U
   s"  trusted-only " OUT TRUSTED-ONLY# OUT-U
   s"  total " OUT ROW# @ OUT-U OUT-NL ;

public
: REPORT ( -- )
   0 RP-I !
   begin RP-I @ ROW# @ < while RP-I @ ROW. RP-I @ 1+ RP-I ! repeat
   FOOTER ;

: EMIT-MANIFEST ( -- )   \ the canonical block a developer pastes into TRUSTED.md
   s" <!-- " OUT MARKER$ OUT OUT-NL
   0 RP-I !
   begin RP-I @ ROW# @ < while RP-I @ ROW-ID$ OUT OUT-NL RP-I @ 1+ RP-I ! repeat
   s" -->" OUT OUT-NL ;
private

\ ---- CLI ---------------------------------------------------------------------
: STRICT-ARGS? ( -- bool )
   ARGV:POS# 1 = 0= if LINT-FALSE exit then
   0 ARGV:POS$ s" strict" STR= ;
: MANIFEST-ARGS? ( -- bool )
   ARGV:POS# 1 = 0= if LINT-FALSE exit then
   0 ARGV:POS$ s" manifest" STR= ;
: BASELINE-ARGS? ( -- bool )
   ARGV:POS# 2 = 0= if LINT-FALSE exit then
   0 ARGV:POS$ s" baseline" STR= ;

: MAIN ( -- )
   s" tools/primitive-effect-inventory.f [-- strict | baseline PATH | manifest]" ARGV:USAGE!
   ARGV:PARSE
   INIT
   ARGV:POS# 0= if SCAN-REPO REPORT exit then
   MANIFEST-ARGS? if SCAN-REPO EMIT-MANIFEST exit then
   STRICT-ARGS? if SCAN-REPO REPORT CROSS-CHECK-REPORT exit then
   BASELINE-ARGS? 0= if s" expected no arguments, strict, manifest, or: baseline PATH" ARGV:FAIL then
   SCAN-REPO
   1 ARGV:POS$ RATCHET-LOAD
   RATCHET-REPORT ;

MAIN

\ Promote the fixture-facing row-query and reset words to the package boundary so
\ tools/primitive-effect-inventory-test.f can drive scan/parse/ratchet directly.
public
EXPORT RESET      EXPORT ROWS         EXPORT ROW-KIND    EXPORT ROW-LINE
EXPORT ROW-FLAGS  EXPORT ROW-DIN      EXPORT ROW-DOUT    EXPORT ROW-PATH$
EXPORT ROW-PKG$   EXPORT ROW-NAME$    EXPORT ROW-EFF$    EXPORT ROW-ID$
EXPORT TRUSTED-ONLY?                  EXPORT PRIVATE-ROW?

;package
