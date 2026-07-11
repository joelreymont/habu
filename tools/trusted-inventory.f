\ trusted-inventory.f - TRUSTED ratchet: inventory trust sites, enforce baseline.
\ Streams over repo .f/.fs sources with the shared lexeme rules (line and paren
\ comments, string bodies after openers are skipped; repo-scale lints stream,
\ not vectorize) collecting TRUSTED: definitions, s" name" s" effect" TRUST
\ rows, 0 set-check boundaries, ' NAME set-check / ['] NAME set-check hook
\ installs (HOOK-INSTALL, named by the installed hook), and the fail-closed
\ TRUST-BARE catch-all, then
\ prints a deterministic TSV report (kind TAB file TAB line TAB name TAB effect
\ TAB class TAB dot, sorted by file then line) plus a summary footer. Class, owning
\ dot, and coverage count come from the committed classification block in
\ TRUSTED.md; strict mode fails on unclassified sites, on owning dots that do not
\ exist in .dots/, and on dead (unmatched) or duplicate mapping rows.
\ Ratchet mode derives the ceiling from the classification block itself, so there
\ is no separate hand-edited count that conflicts on every parallel merge. Each
\ `file:name` row covers exactly one site; each `file class dot N` file-level row
\ carries its own site count N. The ratchet fails if any site is uncovered (a new
\ trust site added without an audited row), if a file-level row's committed count
\ grew or shrank against the live tree, if a file-level row is missing its count,
\ or if a mapping key is duplicated. Adding an audited site is a new named row (a
\ distinct, mergeable line) with no shared count to bump.
\ Run: bin/hb --load tools/trusted-inventory.f                        (report)
\ Or:  bin/hb --load tools/trusted-inventory.f -- strict              (report, fail on unclassified)
\ Or:  bin/hb --load tools/trusted-inventory.f -- baseline TRUSTED.md (ratchet)

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/sort.f
require lib/adt/option.f
require tools/lint/text.f
require tools/argv.f

package TINV

public

1 constant CL-TRUSTED
2 constant CL-TRUST
3 constant CL-SETCHECK
4 constant CL-BARE
5 constant CL-HOOK

private

78 constant E-TINV-RATCHET
79 constant E-TINV-BASELINE
80 constant E-TINV-CLASSES
81 constant E-TINV-STRICT

8192 constant ROW-MAX
9 constant TAB#
variable FB-COUNT   \ committed `fold-baseline N` (separable file-level rows allowed)
1024 constant CMAX   \ row-granularity classification block (fold split) exceeds 512
11 constant CTAB#
-1 constant COUNT-UNSET
$80000 constant FILE-CAP   \ >= largest scanned source (checker.f grew past $40000)
$40000 constant STR-CAP
$10000 constant CSTR-CAP
32 constant NUM-CAP
9 constant CH-TAB
10 constant CH-LF
40 constant CH-LPAREN
41 constant CH-RPAREN
48 constant CH-ZERO
58 constant CH-COLON
92 constant CH-BSLASH
32 constant CH-BLANK
115 constant CH-S

variable TAB-A
variable ARENA-A
variable FILE-A
variable ROW#
variable ARENA-END
variable CUR-PATH-O
variable CUR-PATH-U
variable NUM-L
variable SI
variable CC-I
variable CC-N
variable SR-I
variable RI
variable RT-K
variable RT-R
variable RD-I
variable RD-BAD
variable RD-STALE
variable CTAB-A
variable CARENA-A
variable CM#
variable C-END
variable RN-O
variable RN-U
variable RE-O
variable RE-U
variable KS-I
variable CF-I
variable CP-X
variable CP-S
variable CP-OK
variable PC-M
variable PC-E
variable PC-L
variable UC-I
variable UC-N
variable DOTP-U
variable DB-I
variable DV-I
variable DV-N
variable DV-MISS   variable DV-SEP
variable MR-I
variable MR-J
variable MR-N
variable MD-I
variable MD-J
variable MD-N
variable DV-DEAD
variable DV-DUP

\ streaming scanner state
variable SRC-PA
variable SRC-LEN
variable SPOS
variable SLINE
variable WT-S
variable QB-S
variable PR-S
variable W1-O
variable W1-U
variable W1-BO
variable W1-BU
variable W1-SQ
variable W2-O
variable W2-U
variable W2-BO
variable W2-BU
variable W2-SQ
variable WC-O
variable WC-U
variable WC-BO
variable WC-BU
variable WC-SQ
variable WC-LINE
variable PEND-NAME
variable PEND-EFF
variable PEND-LINE
variable PEND-NO
variable PEND-NU

create OUT-CH 1 allot
create NUM-BUF NUM-CAP allot

1024 constant DOTP-CAP
create DOTP-BUF DOTP-CAP allot

\ Injectable dots root (default s" .dots/"). Production leaves the default so
\ output/behaviour is unchanged; tests point it at a scratch fixture tree so the
\ suite never depends on mutable live .dots/ task state.
512 constant DOTS-ROOT-CAP
create DOTS-ROOT-BUF DOTS-ROOT-CAP allot
variable DOTS-ROOT-U

: TAB-A-FIELD ( -- ptr ptr a )
   TAB-A 0 ptr-field ;

: TAB-BASE ( -- ptr a )
   TAB-A-FIELD @ ;

: ARENA-A-FIELD ( -- ptr ptr u8 )
   ARENA-A 0 ptr-field ;

: ARENA@ ( -- ptr u8 )
   ARENA-A-FIELD @ ;

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: FILE-BUF ( -- ptr u8 )
   FILE-A-FIELD @ ;

: CTAB-A-FIELD ( -- ptr ptr a )
   CTAB-A 0 ptr-field ;

: CTAB-BASE ( -- ptr a )
   CTAB-A-FIELD @ ;

: CARENA-A-FIELD ( -- ptr ptr u8 )
   CARENA-A 0 ptr-field ;

: CARENA@ ( -- ptr u8 )
   CARENA-A-FIELD @ ;

: SRC-PA-FIELD ( -- ptr ptr u8 )
   SRC-PA 0 ptr-field ;

: SRC@ ( -- ptr u8 )
   SRC-PA-FIELD @ ;

: SRC! ( ptr u8 -- )
   SRC-PA-FIELD ! ;

\ ---- output through the shared lint sink so tests can capture it ----------
: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: OUT-C ( n -- )
   OUT-CH c!
   OUT-CH 1 OUT ;

: OUT-NL ( -- )
   CH-LF OUT-C ;

: OUT-TAB ( -- )
   CH-TAB OUT-C ;

: OUT-U ( n -- )
   0 NUM-L !
   dup 0= if drop CH-ZERO OUT-C exit then
   begin dup 0 > while
      dup 10 mod CH-ZERO + NUM-BUF NUM-L @ + c!
      10 /
      NUM-L @ 1+ NUM-L !
   repeat drop
   begin NUM-L @ 0 > while
      NUM-L @ 1- NUM-L !
      NUM-BUF NUM-L @ + c@ OUT-C
   repeat ;

\ ---- row tables + string arena ---------------------------------------------
: TAB@ ( ptr a n -- n )
   cells + @ ;

: TAB! ( n ptr a n -- )
   cells + ! ;

: TABLE ( n -- ptr a )
   ROW-MAX * cells TAB-BASE swap + ;

: R-CLASS ( -- ptr a ) 0 TABLE ;
: R-PO ( -- ptr a ) 1 TABLE ;
: R-PU ( -- ptr a ) 2 TABLE ;
: R-LINE ( -- ptr a ) 3 TABLE ;
: R-NO ( -- ptr a ) 4 TABLE ;
: R-NU ( -- ptr a ) 5 TABLE ;
: R-IX ( -- ptr a ) 6 TABLE ;
: R-EO ( -- ptr a ) 7 TABLE ;
: R-EU ( -- ptr a ) 8 TABLE ;

: CTAB ( n -- ptr a )
   CMAX * cells CTAB-BASE swap + ;

: K-FO ( -- ptr a ) 0 CTAB ;
: K-FU ( -- ptr a ) 1 CTAB ;
: K-NO ( -- ptr a ) 2 CTAB ;
: K-NU ( -- ptr a ) 3 CTAB ;
: K-CO ( -- ptr a ) 4 CTAB ;
: K-CU ( -- ptr a ) 5 CTAB ;
: K-DO ( -- ptr a ) 6 CTAB ;
: K-DU ( -- ptr a ) 7 CTAB ;
: K-CNT ( -- ptr a ) 8 CTAB ;
: K-MATCHED ( -- ptr a ) 9 CTAB ;
: K-UNNAME ( -- ptr a ) 10 CTAB ;   \ file-level row matched by an unnameable site

: STORE$ ( ptr u8 n -- n n ) {: a:ptr u:n :}
   ARENA-A @ 0= if s" trusted-inventory: not initialised" 1 die then
   ARENA-END @ u + STR-CAP > if s" trusted-inventory: string arena overflow" 1 die then
   a ARENA@ ARENA-END @ + u LINT-BMOVE
   ARENA-END @ u
   ARENA-END @ u + ARENA-END ! ;

: O$ ( n n -- ptr u8 n )
   swap ARENA@ + swap ;

: CSTORE$ ( ptr u8 n -- n n ) {: a:ptr u:n :}
   CARENA-A @ 0= if s" trusted-inventory: not initialised" 1 die then
   C-END @ u + CSTR-CAP > if s" trusted-inventory: class arena overflow" 1 die then
   a CARENA@ C-END @ + u LINT-BMOVE
   C-END @ u
   C-END @ u + C-END ! ;

: CO$ ( n n -- ptr u8 n )
   swap CARENA@ + swap ;

: ROW-NAME! ( ptr u8 n -- )
   STORE$ RN-U ! RN-O ! ;

: ROW-EFFECT! ( ptr u8 n -- )
   STORE$ RE-U ! RE-O ! ;

: ROW+ ( n n -- ) {: class:n line:n :}
   ROW# @ ROW-MAX >= if s" trusted-inventory: row overflow" 1 die then
   class R-CLASS ROW# @ TAB!
   CUR-PATH-O @ R-PO ROW# @ TAB!
   CUR-PATH-U @ R-PU ROW# @ TAB!
   line R-LINE ROW# @ TAB!
   RN-O @ R-NO ROW# @ TAB!
   RN-U @ R-NU ROW# @ TAB!
   RE-O @ R-EO ROW# @ TAB!
   RE-U @ R-EU ROW# @ TAB!
   ROW# @ 1+ ROW# ! ;

: DASH-IF-EMPTY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if s" -" exit then
   a u ;

\ ---- streaming token classification -----------------------------------------
\ One pass over the source bytes with the shared lexeme rules: `\` line
\ comments produce nothing, `(`-initial tokens are paren comments, and string
\ bodies after `s"`/`c"`/`."`-class openers are skipped (escape-aware for the
\ `s\"` forms), so comment and string mentions can never produce trust rows.
\ A two-token ring provides the literal-shape lookback and a small pending
\ state captures a TRUSTED: definition's name and effect comment.
: SRC$ ( n n -- ptr u8 n )
   swap SRC@ + swap ;

: SEND? ( -- bool )
   SPOS @ SRC-LEN @ >= ;

: SCUR ( -- n )
   SRC@ SPOS @ + c@ ;

: SADV ( -- )
   SCUR CH-LF = if SLINE @ 1+ SLINE ! then
   SPOS @ 1+ SPOS ! ;

: SKIP-LINE-COMMENT ( -- )
   begin SEND? 0= while
      SCUR CH-LF = if exit then
      SADV
   repeat ;

\ Engine parity: `(` opens a comment only as a standalone token (followed by
\ whitespace or EOF). A `(`-initial token like `(CMP)` is a word name, so it
\ must reach the token ring instead of being swallowed to the next `)`.
: PAREN-STANDALONE? ( -- bool )
   SPOS @ 1+ SRC-LEN @ >= if LINT-TRUE exit then
   SRC@ SPOS @ 1+ + c@ LINT-WS? ;

\ Consume a paren comment and return its content span.
: PAREN-BODY$ ( -- ptr u8 n )
   SADV
   SPOS @ PR-S !
   begin SEND? 0= while
      SCUR CH-RPAREN = if
         PR-S @ SPOS @ PR-S @ - SRC$
         SADV exit
      then
      SADV
   repeat
   PR-S @ SPOS @ PR-S @ - SRC$ ;

: SQ-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a c@ LINT-FOLD CH-S <> if LINT-FALSE exit then
   a 1+ c@ DQUOTE = ;

: WC$ ( -- ptr u8 n )
   WC-O @ WC-U @ SRC$ ;

\ After a plain string opener token: skip one delimiter, capture to the quote.
: CAPTURE-QUOTE ( -- )
   0 WC-BO !
   0 WC-BU !
   SEND? if exit then
   SADV
   SPOS @ QB-S !
   begin SEND? 0= while
      SCUR DQUOTE = if
         QB-S @ WC-BO !
         SPOS @ QB-S @ - WC-BU !
         SADV exit
      then
      SADV
   repeat
   QB-S @ WC-BO !
   SPOS @ QB-S @ - WC-BU ! ;

: SKIP-ESC-QUOTE ( -- )
   SEND? if exit then
   SADV
   begin SEND? 0= while
      SCUR DQUOTE = if SADV exit then
      SCUR CH-BSLASH = if
         SADV
         SEND? if exit then
      then
      SADV
   repeat ;

: WORD-END? ( -- bool )
   SEND? if LINT-TRUE exit then
   SCUR LINT-WS? ;

: READ-WORD ( -- )
   SPOS @ WT-S !
   SLINE @ WC-LINE !
   begin WORD-END? 0= while SADV repeat
   WT-S @ WC-O !
   SPOS @ WT-S @ - WC-U !
   0 WC-BO !
   0 WC-BU !
   LINT-FALSE WC-SQ !
   WC$ LINT-NORMAL-STRING-OPENER? if
      WC$ SQ-TOK? WC-SQ !
      CAPTURE-QUOTE exit
   then
   WC$ LINT-ESC-STRING-OPENER? if SKIP-ESC-QUOTE then ;

: P1$ ( -- ptr u8 n )
   W1-O @ W1-U @ SRC$ ;

: P2$ ( -- ptr u8 n )
   W2-O @ W2-U @ SRC$ ;

: P1-BODY$ ( -- ptr u8 n )
   W1-BO @ W1-BU @ SRC$ ;

: P2-BODY$ ( -- ptr u8 n )
   W2-BO @ W2-BU @ SRC$ ;

: RING-ROTATE ( -- )
   W1-O @ W2-O !
   W1-U @ W2-U !
   W1-BO @ W2-BO !
   W1-BU @ W2-BU !
   W1-SQ @ W2-SQ !
   WC-O @ W1-O !
   WC-U @ W1-U !
   WC-BO @ W1-BO !
   WC-BU @ W1-BU !
   WC-SQ @ W1-SQ ! ;

\ The current token is in name position (": TRUSTED:", "' set-check"), not a use.
: DEFINER-REF? ( -- bool )
   W1-U @ 0= if LINT-FALSE exit then
   P1$ s" :" LINT-STR= if LINT-TRUE exit then
   P1$ s" postpone" LINT-STR=CI if LINT-TRUE exit then
   P1$ s" '" LINT-STR= if LINT-TRUE exit then
   P1$ s" [']" LINT-STR= if LINT-TRUE exit then
   P1$ s" undefine" LINT-STR=CI ;

: FINISH-TRUSTED ( ptr u8 n ptr u8 n -- )
   ROW-EFFECT!
   ROW-NAME!
   LINT-FALSE PEND-NAME !
   LINT-FALSE PEND-EFF !
   CL-TRUSTED PEND-LINE @ ROW+ ;

: PEND-NAME$ ( -- ptr u8 n )
   PEND-NO @ PEND-NU @ SRC$ ;

\ Pending TRUSTED: interactions for the current word token: consume it as the
\ name, or close an effect wait with no effect comment.
: PEND-WORD ( -- )
   PEND-NAME @ if
      WC-O @ PEND-NO !
      WC-U @ PEND-NU !
      LINT-FALSE PEND-NAME !
      LINT-TRUE PEND-EFF !
      exit
   then
   PEND-EFF @ if
      PEND-NAME$ s" -" FINISH-TRUSTED
   then ;

\ Anything that is not a name reference or a provable literal shape falls
\ closed into the TRUST-BARE kind instead of being silently skipped.
: ADD-BARE ( ptr u8 n -- )
   ROW-NAME!
   s" -" ROW-EFFECT!
   CL-BARE WC-LINE @ ROW+ ;

: CUR-TRUSTED ( -- )
   DEFINER-REF? if exit then
   PEND-EFF @ if PEND-NAME$ s" -" FINISH-TRUSTED then
   LINT-TRUE PEND-NAME !
   WC-LINE @ PEND-LINE ! ;

: CUR-TRUST ( -- )
   DEFINER-REF? if exit then
   W1-SQ @ if
      W2-SQ @ if
         P2-BODY$ ROW-NAME!
         P1-BODY$ LINT-TRIM DASH-IF-EMPTY ROW-EFFECT!
         CL-TRUST WC-LINE @ ROW+ exit
      then
   then
   s" TRUST" ADD-BARE ;

\ ' NAME set-check / ['] NAME set-check installs a checker hook. The shape is
\ counted and ratcheted as its own HOOK-INSTALL kind, named by the installed
\ hook, so a rogue install is visible in the TSV and grows the baseline.
\ Static hook-identity policing in checked-boundary-lint is
\ habu-police-set-check-850bc543.
: ADD-HOOK ( -- )
   P1$ ROW-NAME!
   s" -" ROW-EFFECT!
   CL-HOOK WC-LINE @ ROW+ ;

: CUR-SETCHECK ( -- )
   DEFINER-REF? if exit then
   W1-U @ 0 > if
      P1$ s" 0" LINT-STR= if
         s" 0 set-check" ROW-NAME!
         s" -" ROW-EFFECT!
         CL-SETCHECK WC-LINE @ ROW+ exit
      then
      W2-U @ 0 > if
         P2$ s" '" LINT-STR= if ADD-HOOK exit then
         P2$ s" [']" LINT-STR= if ADD-HOOK exit then
      then
   then
   s" set-check" ADD-BARE ;

: CLASSIFY-CUR ( -- )
   WC$ s" TRUSTED:" LINT-STR=CI if CUR-TRUSTED exit then
   WC$ s" TRUST" LINT-STR=CI if CUR-TRUST exit then
   WC$ s" set-check" LINT-STR=CI if CUR-SETCHECK then ;

: SCAN-TOKEN ( -- )
   READ-WORD
   PEND-WORD
   CLASSIFY-CUR
   RING-ROTATE ;

: DO-PAREN ( -- )
   PAREN-BODY$
   PEND-EFF @ 0= if 2drop exit then
   LINT-TRIM DASH-IF-EMPTY {: ea:ptr eu:n :}
   PEND-NAME$ ea eu FINISH-TRUSTED ;

: FLUSH-PENDING ( -- )
   PEND-NAME @ if
      s" -" s" -" FINISH-TRUSTED exit
   then
   PEND-EFF @ if
      PEND-NAME$ s" -" FINISH-TRUSTED
   then ;

: SCAN-RESET ( -- )
   0 SPOS !
   1 SLINE !
   0 W1-U !
   0 W2-U !
   LINT-FALSE W1-SQ !
   LINT-FALSE W2-SQ !
   LINT-FALSE PEND-NAME !
   LINT-FALSE PEND-EFF ! ;

\ ---- deterministic order ----------------------------------------------------
: STR< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   0 SI !
   begin SI @ u < while
      SI @ v >= if LINT-FALSE exit then
      a SI @ + c@  b SI @ + c@  < if LINT-TRUE exit then
      a SI @ + c@  b SI @ + c@  > if LINT-FALSE exit then
      SI @ 1+ SI !
   repeat
   u v < ;

: COUNT-CLASS ( n -- n ) {: c:n :}
   0 CC-N !
   0 CC-I !
   begin CC-I @ ROW# @ < while
      R-CLASS CC-I @ TAB@ c = if CC-N @ 1+ CC-N ! then
      CC-I @ 1+ CC-I !
   repeat CC-N @ ;

public

: INIT ( -- )
   TAB-A @ 0= if TAB# ROW-MAX * >COUNT MEM-ALLOC-CELLS TAB-A-FIELD ! then
   ARENA-A @ 0= if STR-CAP MEM-ALLOC-BYTES drop ARENA-A-FIELD ! then
   FILE-A @ 0= if FILE-CAP MEM-ALLOC-BYTES drop FILE-A-FIELD ! then
   CTAB-A @ 0= if CTAB# CMAX * >COUNT MEM-ALLOC-CELLS CTAB-A-FIELD ! then
   CARENA-A @ 0= if CSTR-CAP MEM-ALLOC-BYTES drop CARENA-A-FIELD ! then
   0 ROW# !
   0 ARENA-END !
   0 CM# !
   0 C-END !
   COUNT-UNSET FB-COUNT ! ;

: RESET ( -- )
   0 ROW# !
   0 ARENA-END ! ;

: ROWS ( -- n )
   ROW# @ ;

: ROW-CLASS ( n -- n )
   R-CLASS swap TAB@ ;

: ROW-LINE ( n -- n )
   R-LINE swap TAB@ ;

: ROW-PATH$ ( n -- ptr u8 n ) {: k:n :}
   R-PO k TAB@ R-PU k TAB@ O$ ;

: ROW-NAME$ ( n -- ptr u8 n ) {: k:n :}
   R-NO k TAB@ R-NU k TAB@ O$ ;

: ROW-EFFECT$ ( n -- ptr u8 n ) {: k:n :}
   R-EO k TAB@ R-EU k TAB@ O$ ;

: IX-ROW ( n -- n )
   R-IX swap TAB@ ;

: ROW-BEFORE? ( n n -- bool ) {: i:n j:n :}
   i ROW-PATH$ j ROW-PATH$ STR< if LINT-TRUE exit then
   j ROW-PATH$ i ROW-PATH$ STR< if LINT-FALSE exit then
   i ROW-LINE j ROW-LINE < if LINT-TRUE exit then
   j ROW-LINE i ROW-LINE < if LINT-FALSE exit then
   i j < ;

: TRUSTED# ( -- n )
   CL-TRUSTED COUNT-CLASS ;

: TRUST# ( -- n )
   CL-TRUST COUNT-CLASS ;

: SETCHECK# ( -- n )
   CL-SETCHECK COUNT-CLASS ;

: BARE# ( -- n )
   CL-BARE COUNT-CLASS ;

: HOOK# ( -- n )
   CL-HOOK COUNT-CLASS ;

\ Record every trust site in one source buffer under the given path.
: SCAN-SOURCE ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n sa:ptr su:n :}
   pa pu STORE$ CUR-PATH-U ! CUR-PATH-O !
   sa SRC!
   su SRC-LEN !
   SCAN-RESET
   begin SEND? 0= while
      SCUR LINT-WS? if SADV
      else SCUR CH-BSLASH = if SKIP-LINE-COMMENT
      else SCUR CH-LPAREN = PAREN-STANDALONE? and if DO-PAREN
      else SCAN-TOKEN then then then
   repeat
   FLUSH-PENDING ;

: SORT-ROWS ( -- )
   0 SR-I !
   begin SR-I @ ROW# @ < while
      SR-I @ R-IX SR-I @ TAB!
      SR-I @ 1+ SR-I !
   repeat
   R-IX ROW# @ [: ROW-BEFORE? ;] SORT! ;

\ ---- committed classification mapping (class + owning dot per site) ---------
private

: CMAP-FILE$ ( n -- ptr u8 n ) {: k:n :}
   K-FO k TAB@ K-FU k TAB@ CO$ ;

: CMAP-NAME$ ( n -- ptr u8 n ) {: k:n :}
   K-NO k TAB@ K-NU k TAB@ CO$ ;

: CMAP-CLASS$ ( n -- ptr u8 n ) {: k:n :}
   K-CO k TAB@ K-CU k TAB@ CO$ ;

: CMAP-DOT$ ( n -- ptr u8 n ) {: k:n :}
   K-DO k TAB@ K-DU k TAB@ CO$ ;

: CMAP-NAMED? ( n -- bool )
   K-NU swap TAB@ 0 > ;

: CMAP+ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- )
   {: fa:ptr fu:n na:ptr nu:n ca:ptr cu:n da:ptr du:n cnt:n :}
   CM# @ CMAX >= if s" trusted-inventory: classification overflow" 1 die then
   fa fu CSTORE$ K-FU CM# @ TAB! K-FO CM# @ TAB!
   na nu CSTORE$ K-NU CM# @ TAB! K-NO CM# @ TAB!
   ca cu CSTORE$ K-CU CM# @ TAB! K-CO CM# @ TAB!
   da du CSTORE$ K-DU CM# @ TAB! K-DO CM# @ TAB!
   cnt K-CNT CM# @ TAB!
   0 K-MATCHED CM# @ TAB!
   0 K-UNNAME CM# @ TAB!
   CM# @ 1+ CM# ! ;

: CMAP-COUNT@ ( n -- n )
   K-CNT swap TAB@ ;

: CMAP-MATCHED@ ( n -- n )
   K-MATCHED swap TAB@ ;

\ Split a mapping key into file plus optional site name after the first colon.
: KEY-SPLIT ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u CH-COLON LINT-INDEX-OF MATCH option
     none OF a u a 0 ENDOF
     some OF KS-I !
        a KS-I @  a KS-I @ 1+ +  u KS-I @ 1+ - ENDOF
   ;MATCH ;

: CLASS-VALID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" builder-emit" LINT-STR= if LINT-TRUE exit then
   a u s" stdlib-boundary" LINT-STR= if LINT-TRUE exit then
   a u s" test-metaprog" LINT-STR= if LINT-TRUE exit then
   a u s" prim-axiom" LINT-STR= if LINT-TRUE exit then
   a u s" discharge-candidate" LINT-STR= ;

: PARSE-COUNT ( ptr u8 n -- option<n> )   \ SOME ratchet count >= 1, else NONE
   STR>NUMBER? MATCH option
     none OF OPTION:NONE ENDOF
     some OF dup 1 < if drop OPTION:NONE else OPTION:SOME then ENDOF
   ;MATCH ;

\ One classification row `key class dot [count]`. `key` is `file:name` (names the
\ site(s) called `name` in `file`) or bare `file` (a file-level row covering the
\ sites no named row owns). The count is the derived ratchet ceiling for the row:
\ a three-token named row defaults to 1 (override with an explicit count when a
\ name appears at more than one trust site, e.g. a definition plus its install);
\ a three-token file-level row is COUNT-UNSET so the block still loads while the
\ ratchet reports the missing count fail-closed. A bad count rejects.
: CROW-PARSE ( ptr u8 n -- bool )
   SPLIT-WHITESPACE
   SN# @ 0= if LINT-TRUE exit then
   0 S@ s" fold-baseline" STR= if          \ ratchet directive: separable folds allowed
      SN# @ 2 = 0= if LINT-FALSE exit then
      1 S@ PARSE-COUNT MATCH option
        none OF LINT-FALSE exit ENDOF
        some OF FB-COUNT ! ENDOF
      ;MATCH LINT-TRUE exit
   then
   0 S@ KEY-SPLIT {: fa:ptr fu:n na:ptr nu:n :}
   SN# @ 3 = if
      1 S@ CLASS-VALID? 0= if LINT-FALSE exit then
      nu 0 > if 1 else COUNT-UNSET then {: cnt:n :}
      fa fu na nu 1 S@ 2 S@ cnt CMAP+ LINT-TRUE exit
   then
   SN# @ 4 = if
      1 S@ CLASS-VALID? 0= if LINT-FALSE exit then
      3 S@ PARSE-COUNT MATCH option
        none OF LINT-FALSE exit ENDOF
        some OF ENDOF
      ;MATCH {: cnt:n :}
      fa fu na nu 1 S@ 2 S@ cnt CMAP+ LINT-TRUE exit
   then
   LINT-FALSE ;

: CLASSES-LINES ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LINT-TRUE CP-OK !
   0 CP-S !
   0 CP-X !
   begin CP-X @ u < while
      a CP-X @ + c@ CH-LF = if
         a CP-S @ +  CP-X @ CP-S @ -  CROW-PARSE 0= if LINT-FALSE CP-OK ! then
         CP-X @ 1+ CP-S !
      then
      CP-X @ 1+ CP-X !
   repeat
   CP-S @ u < if
      a CP-S @ +  u CP-S @ -  CROW-PARSE 0= if LINT-FALSE CP-OK ! then
   then
   CP-OK @ ;

\ Row lookup: exact file:name row first, then the file-level row.
: CLASS-FIND ( n -- n ) {: rowk:n :}
   0 CF-I !
   begin CF-I @ CM# @ < while
      CF-I @ CMAP-NAMED? if
         CF-I @ CMAP-FILE$ rowk ROW-PATH$ LINT-STR= if
            CF-I @ CMAP-NAME$ rowk ROW-NAME$ LINT-STR=CI if CF-I @ exit then
         then
      then
      CF-I @ 1+ CF-I !
   repeat
   0 CF-I !
   begin CF-I @ CM# @ < while
      CF-I @ CMAP-NAMED? 0= if
         CF-I @ CMAP-FILE$ rowk ROW-PATH$ LINT-STR= if CF-I @ exit then
      then
      CF-I @ 1+ CF-I !
   repeat -1 ;

public

: CLASSES-RESET ( -- )
   0 CM# !
   0 C-END ! ;

: CLASSES# ( -- n )
   CM# @ ;

: CLASSES-MARKER? ( ptr u8 n -- bool )
   s" trusted-inventory-classes" LINT-CONTAINS? ;

\ Parse the machine-readable block: one `file[:name] class dot` row per line.
\ A second marker occurrence rejects so a shadowed block cannot win silently.
: PC-DUP-MARKER? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ a second marker after PC-M shadows the block
   a PC-M @ 1+ +  u PC-M @ 1+ -  s" trusted-inventory-classes" LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF drop LINT-TRUE ENDOF
   ;MATCH ;

: PC-ROWS ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ rows between the first LF and the --> end
   a PC-M @ + PC-E @ CH-LF LINT-INDEX-OF MATCH option
     none OF LINT-TRUE ENDOF
     some OF PC-L !
        a PC-M @ + PC-L @ 1+ +  PC-E @ PC-L @ 1+ -  CLASSES-LINES ENDOF
   ;MATCH ;

: PC-END ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ locate the --> terminator, then parse rows
   a PC-M @ + u PC-M @ - s" -->" LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF PC-E !  a u PC-ROWS ENDOF
   ;MATCH ;

: PARSE-CLASSES$ ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CLASSES-RESET
   a u s" trusted-inventory-classes" LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF PC-M !
        a u PC-DUP-MARKER? if LINT-FALSE else a u PC-END then ENDOF
   ;MATCH ;

: CLASSES-FROM ( ptr u8 n -- ) {: a:ptr u:n :}
   CLASSES-RESET
   a u FILE? 0= if exit then
   a u FILE-BUF FILE-CAP READ-FILE
   2dup CLASSES-MARKER? 0= if 2drop exit then
   PARSE-CLASSES$ 0= if
      s" trusted-inventory: invalid classification block in " OUT a u OUT OUT-NL
      E-TINV-CLASSES throw
   then ;

: ROW-CLASS$ ( n -- ptr u8 n )
   CLASS-FIND dup 0 < if drop s" -" exit then
   CMAP-CLASS$ ;

: ROW-DOT$ ( n -- ptr u8 n )
   CLASS-FIND dup 0 < if drop s" -" exit then
   CMAP-DOT$ ;

: UNCLASSIFIED# ( -- n )
   0 UC-N !
   0 UC-I !
   begin UC-I @ ROW# @ < while
      UC-I @ CLASS-FIND 0 < if UC-N @ 1+ UC-N ! then
      UC-I @ 1+ UC-I !
   repeat UC-N @ ;

private

: DOTP-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   DOTP-U @ u + DOTP-CAP > if s" trusted-inventory: dot path overflow" 1 die then
   a DOTP-BUF DOTP-U @ + u LINT-BMOVE
   DOTP-U @ u + DOTP-U ! ;

: DOTP$ ( -- ptr u8 n )
   DOTP-BUF DOTP-U @ ;

: DOTS-ROOT$ ( -- ptr u8 n )
   DOTS-ROOT-BUF DOTS-ROOT-U @ ;

public

\ Point the owning-dot lookup at a different dots root (must end in '/'); tests
\ set a scratch fixture tree here and DOTS-ROOT-RESET back to the default.
: DOTS-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u DOTS-ROOT-CAP > if s" trusted-inventory: dots root overflow" 1 die then
   a DOTS-ROOT-BUF u LINT-BMOVE
   u DOTS-ROOT-U ! ;

: DOTS-ROOT-RESET ( -- )
   s" .dots/" DOTS-ROOT! ;

DOTS-ROOT-RESET

\ An owning dot exists as <root><id>.md or, for a parent dot with children,
\ <root><id>/<id>.md, where <root> defaults to .dots/ (DOTS-ROOT!/-RESET). Closed
\ dots move to the ignored .dots/archive/, so a mapping row referencing a closed
\ or never-minted dot fails here.
: DOT-EXISTS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 DOTP-U !
   DOTS-ROOT$ DOTP-APPEND a u DOTP-APPEND s" .md" DOTP-APPEND
   DOTP$ FILE? if LINT-TRUE exit then
   0 DOTP-U !
   DOTS-ROOT$ DOTP-APPEND a u DOTP-APPEND s" /" DOTP-APPEND
   a u DOTP-APPEND s" .md" DOTP-APPEND
   DOTP$ FILE? ;

private

: DOT-BEFORE? ( n -- bool ) {: k:n :}
   0 DB-I !
   begin DB-I @ k < while
      DB-I @ CMAP-DOT$ k CMAP-DOT$ LINT-STR= if LINT-TRUE exit then
      DB-I @ 1+ DB-I !
   repeat LINT-FALSE ;

public

\ Report each distinct owning dot referenced by the mapping that is missing
\ from .dots/; strict mode fails while any remain.
: DOTS-MISSING# ( -- n )
   0 DV-N !
   0 DV-I !
   begin DV-I @ CM# @ < while
      DV-I @ DOT-BEFORE? 0= if
         DV-I @ CMAP-DOT$ DOT-EXISTS? 0= if
            s" trusted-inventory: missing owning dot " OUT
            DV-I @ CMAP-DOT$ OUT OUT-NL
            DV-N @ 1+ DV-N !
         then
      then
      DV-I @ 1+ DV-I !
   repeat DV-N @ ;

private

: CMAP-MATCHES-ROW? ( n n -- bool ) {: k:n r:n :}
   k CMAP-FILE$ r ROW-PATH$ LINT-STR= 0= if LINT-FALSE exit then
   k CMAP-NAMED? 0= if LINT-TRUE exit then
   k CMAP-NAME$ r ROW-NAME$ LINT-STR=CI ;

: CMAP-DEAD? ( n -- bool ) {: k:n :}
   0 MR-J !
   begin MR-J @ ROW# @ < while
      k MR-J @ CMAP-MATCHES-ROW? if LINT-FALSE exit then
      MR-J @ 1+ MR-J !
   repeat LINT-TRUE ;

: CMAP-KEY= ( n n -- bool ) {: i:n j:n :}
   i CMAP-FILE$ j CMAP-FILE$ LINT-STR= 0= if LINT-FALSE exit then
   i CMAP-NAMED? if
      j CMAP-NAMED? 0= if LINT-FALSE exit then
      i CMAP-NAME$ j CMAP-NAME$ LINT-STR=CI exit
   then
   j CMAP-NAMED? 0= ;

: CMAP-DUP-BEFORE? ( n -- bool ) {: k:n :}
   0 MD-J !
   begin MD-J @ k < while
      MD-J @ k CMAP-KEY= if LINT-TRUE exit then
      MD-J @ 1+ MD-J !
   repeat LINT-FALSE ;

: CMAP-KEY. ( n -- ) {: k:n :}
   k CMAP-FILE$ OUT
   k CMAP-NAMED? if s" :" OUT k CMAP-NAME$ OUT then
   OUT-NL ;

public

\ A mapping row is dead when no scanned site matches it (named rows by
\ file+name, file rows by file); dead rows are stale classification debt.
: CMAP-DEAD# ( -- n )
   0 MR-N !
   0 MR-I !
   begin MR-I @ CM# @ < while
      MR-I @ CMAP-DEAD? if
         s" trusted-inventory: dead mapping row " OUT MR-I @ CMAP-KEY.
         MR-N @ 1+ MR-N !
      then
      MR-I @ 1+ MR-I !
   repeat MR-N @ ;

\ Duplicate keys silently shadow each other in row lookup; report every
\ repeat occurrence of a file[:name] key.
: CMAP-DUP# ( -- n )
   0 MD-N !
   0 MD-I !
   begin MD-I @ CM# @ < while
      MD-I @ CMAP-DUP-BEFORE? if
         s" trusted-inventory: duplicate mapping row " OUT MD-I @ CMAP-KEY.
         MD-N @ 1+ MD-N !
      then
      MD-I @ 1+ MD-I !
   repeat MD-N @ ;

private

: SRC-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

public

: SKIP-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" /.jj-ws/" LINT-CONTAINS? if LINT-TRUE exit then
   a u s" /.dots/" LINT-CONTAINS? if LINT-TRUE exit then
   a u s" .jj-ws/" LINT-STARTS-WITH? if LINT-TRUE exit then
   a u s" .dots/" LINT-STARTS-WITH? ;

private

: REL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" ./" LINT-STARTS-WITH? if a 2 + u 2 - exit then
   a u ;

: SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SRC-FILE? 0= if exit then
   a u SKIP-PATH? if exit then
   a u REL$
   a u FILE-BUF FILE-CAP READ-FILE
   SCAN-SOURCE ;

: CLASS$ ( n -- ptr u8 n ) {: c:n :}
   c CL-TRUSTED = if s" TRUSTED" exit then
   c CL-TRUST = if s" TRUST" exit then
   c CL-SETCHECK = if s" SETCHECK" exit then
   c CL-HOOK = if s" HOOK-INSTALL" exit then
   s" TRUST-BARE" ;

: ROW. ( n -- ) {: k:n :}
   k ROW-CLASS CLASS$ OUT OUT-TAB
   k ROW-PATH$ OUT OUT-TAB
   k ROW-LINE OUT-U OUT-TAB
   k ROW-NAME$ OUT OUT-TAB
   k ROW-EFFECT$ OUT OUT-TAB
   k ROW-CLASS$ OUT OUT-TAB
   k ROW-DOT$ OUT OUT-NL ;

: FOOTER ( -- )
   s" trusted-inventory: TRUSTED " OUT TRUSTED# OUT-U
   s"  TRUST " OUT TRUST# OUT-U
   s"  SETCHECK " OUT SETCHECK# OUT-U
   s"  TRUST-BARE " OUT BARE# OUT-U
   s"  HOOK-INSTALL " OUT HOOK# OUT-U
   s"  total " OUT ROWS OUT-U
   s"  unclassified " OUT UNCLASSIFIED# OUT-U OUT-NL ;

\ ---- per-file per-class classification summary (strict mode) ----------------
variable PF-BE  variable PF-SB  variable PF-TM  variable PF-PA  variable PF-DC  variable PF-UN
variable PF-K
variable PF-HAVE
variable PF-I

: PF-ZERO ( -- )
   0 PF-BE ! 0 PF-SB ! 0 PF-TM ! 0 PF-PA ! 0 PF-DC ! 0 PF-UN ! ;

\ Increment the counter for a resolved class string; unknown falls to unclassified.
: PF-BUMP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" builder-emit" LINT-STR= if PF-BE @ 1+ PF-BE ! exit then
   a u s" stdlib-boundary" LINT-STR= if PF-SB @ 1+ PF-SB ! exit then
   a u s" test-metaprog" LINT-STR= if PF-TM @ 1+ PF-TM ! exit then
   a u s" prim-axiom" LINT-STR= if PF-PA @ 1+ PF-PA ! exit then
   a u s" discharge-candidate" LINT-STR= if PF-DC @ 1+ PF-DC ! exit then
   PF-UN @ 1+ PF-UN ! ;

: PF-PAIR ( ptr u8 n n -- ) {: na:ptr nu:n c:n :}
   c 0= if exit then
   s"  " OUT na nu OUT s"  " OUT c OUT-U ;

: PF-FLUSH ( -- )
   PF-HAVE @ 0= if exit then
   s" trusted-inventory: by-file " OUT PF-K @ ROW-PATH$ OUT
   s" builder-emit" PF-BE @ PF-PAIR
   s" stdlib-boundary" PF-SB @ PF-PAIR
   s" test-metaprog" PF-TM @ PF-PAIR
   s" prim-axiom" PF-PA @ PF-PAIR
   s" discharge-candidate" PF-DC @ PF-PAIR
   s" unclassified" PF-UN @ PF-PAIR
   OUT-NL ;

\ Sorted rows are contiguous per path; k continues the current file when its
\ path matches the group anchor PF-K, otherwise it opens a new group.
: PF-SAME? ( n -- bool ) {: k:n :}
   PF-HAVE @ 0= if LINT-FALSE exit then
   k ROW-PATH$ PF-K @ ROW-PATH$ LINT-STR= ;

: PF-STEP ( n -- ) {: k:n :}
   k PF-SAME? 0= if
      PF-FLUSH PF-ZERO k PF-K ! LINT-TRUE PF-HAVE !
   then
   k ROW-CLASS$ PF-BUMP ;

public

\ Emit one summary line per source file with its non-zero per-class site counts.
: CLASS-BY-FILE-REPORT ( -- )
   LINT-FALSE PF-HAVE !
   PF-ZERO
   0 PF-I !
   begin PF-I @ ROWS < while
      PF-I @ IX-ROW PF-STEP
      PF-I @ 1+ PF-I !
   repeat
   PF-FLUSH ;

: SCAN-REPO ( -- )
   RESET
   s" ." [: SCAN-FILE ;] WALK-FILES ;

: REPORT ( -- )
   0 RI !
   begin RI @ ROWS < while
      RI @ IX-ROW ROW.
      RI @ 1+ RI !
   repeat
   FOOTER ;

\ ---- derived ratchet: the classification block is the ceiling ----------------
\ Each `file:name` row covers exactly one site; each `file class dot N`
\ file-level row covers the sites in its file that no named row owns, bounded by
\ its committed count N. RATCHET-TALLY resolves every scanned site to its winning
\ row (CLASS-FIND) and tallies the per-row matched count, so the ratchet compares
\ committed vs live per row -- no separate global count block to hand-edit and
\ conflict on every parallel merge. A file-level row with no committed count is
\ COUNT-UNSET and fails fail-closed.
: RATCHET-TALLY ( -- )
   0 RT-K !
   begin RT-K @ CM# @ < while
      0 K-MATCHED RT-K @ TAB!
      RT-K @ 1+ RT-K !
   repeat
   0 RT-R !
   begin RT-R @ ROW# @ < while
      RT-R @ CLASS-FIND {: mk:n :}
      mk 0 >= if mk CMAP-MATCHED@ 1+ K-MATCHED mk TAB! then
      RT-R @ 1+ RT-R !
   repeat ;

: ROW-UNSET? ( n -- bool )
   CMAP-COUNT@ COUNT-UNSET = ;

: ROW-GREW? ( n -- bool ) {: k:n :}
   k ROW-UNSET? if LINT-FALSE exit then
   k CMAP-MATCHED@ k CMAP-COUNT@ > ;

: ROW-STALE? ( n -- bool ) {: k:n :}
   k ROW-UNSET? if LINT-FALSE exit then
   k CMAP-MATCHED@ k CMAP-COUNT@ < ;

: RATCHET-UNSET# ( -- n )
   0 RD-BAD !
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ ROW-UNSET? if RD-BAD @ 1+ RD-BAD ! then
      RD-I @ 1+ RD-I !
   repeat RD-BAD @ ;

: RATCHET-GREW# ( -- n )
   0 RD-BAD !
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ ROW-GREW? if RD-BAD @ 1+ RD-BAD ! then
      RD-I @ 1+ RD-I !
   repeat RD-BAD @ ;

: RATCHET-STALE# ( -- n )
   0 RD-STALE !
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ ROW-STALE? if RD-STALE @ 1+ RD-STALE ! then
      RD-I @ 1+ RD-I !
   repeat RD-STALE @ ;

\ A ratchet failure (fail) is an uncovered site (a new trust site with no row),
\ a file-level row that grew, or a file-level row missing its count. A stale is a
\ file-level row (or dead named row) whose sites shrank below the committed count.
: RATCHET-BAD# ( -- n )
   UNCLASSIFIED# RATCHET-UNSET# + RATCHET-GREW# + ;

: RATCHET-ROW. ( n -- ) {: k:n :}
   s" trusted-inventory: " OUT
   k CMAP-FILE$ OUT
   k CMAP-NAMED? if s" :" OUT k CMAP-NAME$ OUT then
   k ROW-UNSET? if
      s"  missing count (set to " OUT k CMAP-MATCHED@ OUT-U s" )" OUT OUT-NL exit
   then
   s"  expected " OUT k CMAP-COUNT@ OUT-U
   s"  got " OUT k CMAP-MATCHED@ OUT-U
   k ROW-GREW? if s"  GREW" OUT OUT-NL exit then
   s"  STALE" OUT OUT-NL ;

: RATCHET-DETAIL ( -- )
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ ROW-UNSET? RD-I @ ROW-GREW? or RD-I @ ROW-STALE? or if
         RD-I @ RATCHET-ROW.
      then
      RD-I @ 1+ RD-I !
   repeat ;

: RATCHET-UNCOVERED. ( -- )
   0 RD-I !
   begin RD-I @ ROW# @ < while
      RD-I @ CLASS-FIND 0 < if
         s" trusted-inventory: uncovered site " OUT
         RD-I @ ROW-PATH$ OUT s" :" OUT RD-I @ ROW-LINE OUT-U
         s"  " OUT RD-I @ ROW-NAME$ OUT
         s"  (add a classification row to TRUSTED.md)" OUT OUT-NL
      then
      RD-I @ 1+ RD-I !
   repeat ;

: RATCHET-REPORT ( -- )
   RATCHET-TALLY
   RATCHET-UNCOVERED.
   RATCHET-DETAIL
   RATCHET-BAD# 0 > if
      s" trusted-inventory: ratchet FAILED - trust site(s) added without an audited classification row/count" OUT OUT-NL
      E-TINV-RATCHET throw
   then
   RATCHET-STALE# 0 > if
      s" trusted-inventory: ratchet STALE - lower the file-level count(s) to the current sites" OUT OUT-NL
      E-TINV-RATCHET throw
   then
   s" trusted-inventory: ratchet ok" OUT OUT-NL ;

: RATCHET-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if
      s" trusted-inventory: classification file not found: " OUT a u OUT OUT-NL
      E-TINV-BASELINE throw
   then
   a u CLASSES-FROM
   CLASSES# 0= if
      s" trusted-inventory: no classification block in " OUT a u OUT OUT-NL
      E-TINV-BASELINE throw
   then ;

: RATCHET-ARGS? ( -- bool )
   ARGV-POS# 2 = 0= if LINT-FALSE exit then
   0 ARGV-POS$ s" baseline" STR= ;

\ ---- separable-fold ratchet (strict) ----------------------------------------
\ A file-level row is SEPARABLE when every site it covers is nameable (a
\ non-empty, whitespace-free word name), i.e. the row could be `file:name`
\ rows attributing trust drift to WORDS. The committed `fold-baseline N`
\ directive in the classes block is the allowed count (contested files whose
\ sites churn under another lane keep folds deliberately); strict fails when
\ separable folds EXCEED the baseline, so new coarse rows cannot creep in,
\ and the printed count prompts lowering the baseline as folds are split.
: NAME-UNNAMEABLE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if LINT-TRUE exit then
   0 begin dup u < while
      a over + c@ CH-BLANK = if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: SEP-TALLY ( -- )   \ tally file-level matches + mark rows hit by an unnameable site
   0 RD-I !
   begin RD-I @ CM# @ < while
      0 K-MATCHED RD-I @ TAB!
      0 K-UNNAME RD-I @ TAB!
      RD-I @ 1+ RD-I !
   repeat
   0 RD-I !
   begin RD-I @ ROW# @ < while
      RD-I @ CLASS-FIND dup 0 >= if
         {: mk:n :}
         mk CMAP-NAMED? 0= if
            mk CMAP-MATCHED@ 1+ K-MATCHED mk TAB!
            RD-I @ ROW-NAME$ NAME-UNNAMEABLE? if -1 K-UNNAME mk TAB! then
         then
      else drop then
      RD-I @ 1+ RD-I !
   repeat ;

: SEP-FOLD? ( n -- bool ) {: k:n :}
   k CMAP-NAMED? if LINT-FALSE exit then
   k CMAP-MATCHED@ 0 > 0= if LINT-FALSE exit then
   K-UNNAME k TAB@ 0= ;

: SEP-FOLD# ( -- n )
   0 DV-SEP !
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ SEP-FOLD? if DV-SEP @ 1+ DV-SEP ! then
      RD-I @ 1+ RD-I !
   repeat DV-SEP @ ;

: SEP-FOLD-DETAIL ( -- )
   0 RD-I !
   begin RD-I @ CM# @ < while
      RD-I @ SEP-FOLD? if
         s" trusted-inventory: separable fold " OUT
         RD-I @ CMAP-FILE$ OUT
         s"  (" OUT RD-I @ CMAP-MATCHED@ OUT-U s"  nameable site(s))" OUT OUT-NL
      then
      RD-I @ 1+ RD-I !
   repeat ;

: STRICT-ARGS? ( -- bool )
   ARGV-POS# 1 = 0= if LINT-FALSE exit then
   0 ARGV-POS$ s" strict" STR= ;

: MANIFEST$ ( -- ptr u8 n )
   s" TRUSTED.md" ;

: MAIN-REPORT ( -- )
   SCAN-REPO SORT-ROWS
   MANIFEST$ CLASSES-FROM
   REPORT ;

: MAIN-STRICT ( -- )
   MAIN-REPORT
   CLASS-BY-FILE-REPORT
   UNCLASSIFIED# 0 > if
      s" trusted-inventory: strict FAILED - " OUT UNCLASSIFIED# OUT-U
      s"  unclassified site(s); add classification rows to TRUSTED.md" OUT OUT-NL
      E-TINV-STRICT throw
   then
   DOTS-MISSING# DV-MISS !
   DV-MISS @ 0 > if
      s" trusted-inventory: strict FAILED - " OUT DV-MISS @ OUT-U
      s"  missing owning dot(s); mint them in .dots/ or reassign the rows" OUT OUT-NL
      E-TINV-STRICT throw
   then
   CMAP-DEAD# DV-DEAD !
   CMAP-DUP# DV-DUP !
   DV-DEAD @ DV-DUP @ + 0 > if
      s" trusted-inventory: strict FAILED - " OUT DV-DEAD @ OUT-U
      s"  dead and " OUT DV-DUP @ OUT-U
      s"  duplicate mapping row(s); prune the TRUSTED.md classes block" OUT OUT-NL
      E-TINV-STRICT throw
   then
   FB-COUNT @ COUNT-UNSET = if
      s" trusted-inventory: strict FAILED - missing fold-baseline row in the classes block" OUT OUT-NL
      E-TINV-STRICT throw
   then
   SEP-TALLY
   SEP-FOLD# drop
   s" trusted-inventory: separable fold(s) " OUT DV-SEP @ OUT-U
   s"  (baseline " OUT FB-COUNT @ OUT-U s" )" OUT OUT-NL
   DV-SEP @ FB-COUNT @ > if
      SEP-FOLD-DETAIL
      s" trusted-inventory: strict FAILED - separable fold(s) exceed the fold-baseline; split them into file:name rows" OUT OUT-NL
      E-TINV-STRICT throw
   then ;

: MAIN ( -- )
   s" tools/trusted-inventory.f [-- strict | baseline PATH]" ARGV-USAGE!
   ARGV-PARSE
   INIT
   ARGV-POS# 0= if MAIN-REPORT exit then
   STRICT-ARGS? if MAIN-STRICT exit then
   RATCHET-ARGS? 0= if s" expected no arguments, strict, or: baseline PATH" ARGV-FAIL then
   SCAN-REPO SORT-ROWS
   1 ARGV-POS$ RATCHET-LOAD
   RATCHET-REPORT ;

MAIN

end-package
