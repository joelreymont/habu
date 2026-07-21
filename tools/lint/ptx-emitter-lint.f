\ ptx-emitter-lint.f — the PTX SAXPY text-emitter surface has exactly one
\ global definition across the source tree.
\
\ src/arch/ptx/emit.f owns the checked PTX text encoder (PTX-L line sink,
\ PTX-HEADER, the PTX-SAXPY-* body words, PTX-EMIT-SAXPY). A second GLOBAL-scope
\ definition of any of those words anywhere is a duplicate emitter copy: it
\ invites load-order shadowing and fixes landing in only one copy (the exact
\ defect dot habu-remove-stale-duplicate-347b8ec6 removed — the never-required
\ tools/ptx/emit.f). This lint reds the gate the moment such a copy reappears.
\
\ Reachability, not just anti-duplicate: each surface word must resolve to
\ EXACTLY one definer, so count 0 (surface deleted/renamed out from under
\ tools/ptx/saxpy.f) fails too.
\
\ Only GLOBAL-scope definitions are counted. A package-scoped word is namespaced
\ (docs/forth.md § Packages: unqualified global lookup never finds it) and cannot
\ clobber the global surface, so it is a different word, not a second copy. Per
\ file the string-aware source-lex consumes s" / s\" / ." / c" bodies as opaque
\ spans, so a surface name written in prose or PTX text is never a false match.
\ Fail-closed: an unterminated string literal (the lexer would swallow later
\ definitions) is reported as a finding rather than silently trusted.
\
\ Run: bin/hb --load tools/lint/ptx-emitter-lint.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package PTX-EMITTER-LINT-TOOL

$80000 constant PE-FB-CAP                       \ one file at a time; headroom past the largest scanned source (checker.f crossed $60000 with the field-projection window)
create PE-FB PE-FB-CAP allot

\ ---- emitter-surface family: name -> abs ptr / len / per-run count ----------
$20 constant PE-MAX
create PE-NAMES 1024 allot   variable PE-NEND
create PE-OFF PE-MAX cells allot
create PE-LEN PE-MAX cells allot
create PE-CNT PE-MAX cells allot
variable PE-N#

create PE-FILE-BUF FS-PATH-CAP allot   variable PE-FILE-U
variable PE-IN-PKG   variable PE-LI   variable PE-BAD

: PE-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   a  PE-NAMES PE-NEND @ +  u BYTE-COPY
   PE-NAMES PE-NEND @ +  PE-OFF PE-N# @ cells + !
   u PE-LEN PE-N# @ cells + !
   0 PE-CNT PE-N# @ cells + !
   PE-NEND @ u + PE-NEND !
   PE-N# @ 1+ PE-N# ! ;

: PE-NAME$ ( n -- ptr u8 n ) {: idx:n :}
   PE-OFF idx cells + @   PE-LEN idx cells + @ ;

: PE-CNT@ ( n -- n ) {: idx:n :}
   PE-CNT idx cells + @ ;

: PE-CNT+ ( n -- ) {: idx:n :}
   idx PE-CNT@ 1+ PE-CNT idx cells + ! ;

: PE-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ family index, or -1
   0 begin dup PE-N# @ < while
      dup PE-NAME$ a u LINT-STR=CI if exit then
      1+
   repeat drop -1 ;

\ Definition order in src/arch/ptx/emit.f.
: PE-FAMILY! ( -- )
   s" PTX-L" PE-ADD
   s" PTX-NL" PE-ADD
   s" PTX-HEADER" PE-ADD
   s" PTX-SAXPY-ENTRY" PE-ADD
   s" PTX-SAXPY-REGS" PE-ADD
   s" PTX-SAXPY-PARAMS" PE-ADD
   s" PTX-SAXPY-TID" PE-ADD
   s" PTX-SAXPY-BOUNDS" PE-ADD
   s" PTX-SAXPY-LOADS" PE-ADD
   s" PTX-SAXPY-MATH" PE-ADD
   s" PTX-SAXPY-RET" PE-ADD
   s" PTX-SAXPY-BODY" PE-ADD
   s" PTX-EMIT-SAXPY" PE-ADD ;

: PE-FILE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PE-FILE-BUF u BYTE-COPY  u PE-FILE-U ! ;

: PE-FILE$ ( -- ptr u8 n )   PE-FILE-BUF PE-FILE-U @ ;

\ ---- token scan: only GLOBAL-scope definers, only surface names -------------
: PE-LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LK@ L-WORD <> if LINT-FALSE exit then
   k LEX-TOK a u LINT-STR=CI ;

\ Offset from a definer token to the name it defines; 0 when k is not a definer.
: PE-DEF-OFFSET ( n -- n )
   dup s" :" PE-LEX-WORD= if drop 1 exit then
   dup s" constant" PE-LEX-WORD= if drop 1 exit then
   dup s" variable" PE-LEX-WORD= if drop 1 exit then
   dup s" create" PE-LEX-WORD= if drop 1 exit then
   drop 0 ;

: PE-DUP-REPORT ( ptr u8 n n -- ) {: a:ptr u:n line:n :}
   s" ptx-emitter-lint: DUPLICATE `" type a u type
   s" ` redefined at " type PE-FILE$ type s" :" type line .
   s"  (canonical owner src/arch/ptx/emit.f)" type cr
   PE-BAD @ 1+ PE-BAD ! ;

: PE-CHECK-DEF ( n -- ) {: k:n :}
   k PE-DEF-OFFSET dup 0= if drop exit then
   k + dup L# @ >= if drop exit then {: ni:n :}
   ni LEX-TOK {: na:ptr nu:n :}
   na nu PE-FIND dup 0 < if drop exit then {: idx:n :}
   idx PE-CNT@ 0 > if na nu ni LL@ PE-DUP-REPORT then
   idx PE-CNT+ ;

\ package/;package toggle scope; the language rejects nesting, so one flag holds.
: PE-TOKEN ( n -- ) {: k:n :}
   k s" package" PE-LEX-WORD= if LINT-TRUE PE-IN-PKG ! exit then
   k s" ;package" PE-LEX-WORD= if LINT-FALSE PE-IN-PKG ! exit then
   PE-IN-PKG @ LINT-NOT if k PE-CHECK-DEF then ;

: PE-UNTERM-FAIL ( -- )
   s" ptx-emitter-lint: UNTERMINATED string literal in " type PE-FILE$ type
   s"  (line " type LEX-UNTERM-LINE @ . s" )" type cr
   PE-BAD @ 1+ PE-BAD ! ;

: PE-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LEX-SOURCE
   LEX-UNTERM-QUOTE? if PE-UNTERM-FAIL exit then
   LINT-FALSE PE-IN-PKG !
   0 PE-LI !
   begin PE-LI @ L# @ < while
      PE-LI @ PE-TOKEN
      PE-LI @ 1+ PE-LI !
   repeat ;

: PE-POLICY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

: PE-MENTIONS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup PE-N# @ < while
      dup PE-NAME$ {: na:ptr nu:n :}
      a u na nu LINT-CONTAINS? if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: PE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PE-POLICY? 0= if exit then
   a u PE-FILE!
   a u PE-FB PE-FB-CAP READ-FILE {: ba:ptr bu:n :}
   ba bu PE-MENTIONS? 0= if exit then
   ba bu PE-SCAN ;

: PE-VERIFY ( -- )
   0 begin dup PE-N# @ < while
      dup PE-CNT@ 0= if
         dup PE-NAME$ {: na:ptr nu:n :}
         s" ptx-emitter-lint: MISSING `" type na nu type s" ` defined nowhere" type cr
         PE-BAD @ 1+ PE-BAD !
      then
      1+
   repeat drop ;

: PE-WALK ( -- )
   s" src"       [: PE-FILE ;] WALK-FILES
   s" tools"     [: PE-FILE ;] WALK-FILES
   s" test"      [: PE-FILE ;] WALK-FILES
   s" lib"       [: PE-FILE ;] WALK-FILES
   s" bootstrap" [: PE-FILE ;] WALK-FILES ;

: PTX-EMITTER-LINT ( -- )
   0 PE-BAD !
   PE-WALK
   PE-VERIFY
   PE-BAD @ 0 > if
      s" ptx-emitter-lint: " type PE-BAD @ . s"  finding(s)" type cr
      s" ptx-emitter-lint: duplicate/missing PTX emitter surface" 1 die
   else
      s" ptx-emitter-lint: clean (" type PE-N# @ . s"  surface words, single-defined)" type cr
   then ;

PE-FAMILY!
PTX-EMITTER-LINT
;package
