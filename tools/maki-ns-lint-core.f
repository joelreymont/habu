\ maki-ns-lint-core.f - maki wordlist-namespace guard.
\
\ Enforces the maki namespace adoption (docs/forth.md § Naming/Packages): every maki
\ application word lives in a package wordlist, so a bare reference does not resolve
\ from global/habu. The default and required package is `package MAKI`. A top-level
\ definition (`:`, `+:`, `TRUSTED:`, `KERNEL:`, `create`, `variable`, `constant`,
\ `defer`) that is NOT inside a `package MAKI` block is a finding, with two exceptions:
\   1. Cross-cutting error codes: a top-level `constant` whose name starts `E-` stays
\      global (E-MK-*, E-FUSE, ...), exactly as the dot mandates.
\   2. Documented subsystem-boundary files (the pre-existing CUDA / FUSION / MAKI-GRADE
\      internal packages, per docs/forth.md § Packages "internal module packages plus
\      one public-interface package"). Such a file must carry a machine marker
\        \ maki-ns-lint: boundary <PKG> - <reason>
\      whose <PKG> MUST match the file's own `package <PKG>` token; a stale/mismatched
\      marker is itself a finding, so markers cannot rot. Without a marker a non-MAKI
\      package inside maki/ is NOT silently OK - it is flagged.
\
\ Scan: TOKENIZE strips `\` line comments and `( )` stack comments, so package/definer
\ detection sees code tokens only. The boundary marker lives in a comment, so it is
\ found by a raw-byte scan of the file; the file's real `package` token comes from the
\ tokenized stream (comment prose that mentions "package MAKI" cannot spoof it).
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, tools/lint/text.f,
\ and tools/lint/token.f.

$40000 constant MNL-CAP
64      constant MNL-NCAP
10      constant MNL-LF
48      constant MNL-ZERO

create MNL-BUF  MNL-CAP allot
create MNL-NBUF MNL-NCAP allot
create MNL-PATH 512 allot
create MNL-MPKG 64 allot
create MNL-APKG 64 allot

variable MNL-PATHU
variable MNL-MPKG-U
variable MNL-APKG-U
variable MNL-BAD
variable MNL-FILES
variable MNL-REPORT?
variable MNL-NL#
variable MNL-IN-DEF
variable MNL-IN-MAKI
variable MNL-I

: MNL-NL ( -- ) MNL-LF emit ;

: MNL-TRUE ( -- bool )
   0 0= ;

: MNL-FALSE ( -- bool )
   0 0= 0= ;

: MNL-REPORT! ( bool -- )
   MNL-REPORT? ! ;

: MNL-REPORT-ON ( -- )
   MNL-TRUE MNL-REPORT! ;

: MNL-REPORT-OFF ( -- )
   MNL-FALSE MNL-REPORT! ;

\ ---- decimal print for the summary counts ----
: MNL-U. ( n -- )
   0 MNL-NL# !
   dup 0= IF drop MNL-ZERO emit exit THEN
   begin dup 0 > while
      dup 10 mod MNL-ZERO + MNL-NBUF MNL-NL# @ + c!
      10 / MNL-NL# @ 1+ MNL-NL# !
   repeat drop
   begin MNL-NL# @ 0 > while
      MNL-NL# @ 1- MNL-NL# !
      MNL-NBUF MNL-NL# @ + c@ emit
   repeat ;

: MNL-BAD+ ( -- ) MNL-BAD @ 1+ MNL-BAD ! ;

: MNL-SRC? ( ptr u8 n -- bool )
   s" .f" HAS-EXT? ;

: MNL-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a MNL-PATH u LINT-BMOVE  u MNL-PATHU ! ;

: MNL-CLAMP ( n -- n )  dup 64 > if drop 64 then ;

\ store a package-name token into a bounded buffer
: MNL-MPKG! ( ptr u8 n -- ) {: a:ptr u:n :}
   a MNL-MPKG u MNL-CLAMP LINT-BMOVE  u MNL-CLAMP MNL-MPKG-U ! ;

: MNL-APKG! ( ptr u8 n -- ) {: a:ptr u:n :}
   a MNL-APKG u MNL-CLAMP LINT-BMOVE  u MNL-CLAMP MNL-APKG-U ! ;

\ ---- boundary-marker (raw byte scan; comments are stripped by TOKENIZE) ----
: MNL-MARKER? ( ptr u8 n -- bool )
   s" maki-ns-lint: boundary" LINT-CONTAINS? ;

\ Extract the <PKG> token that follows the marker phrase into MNL-MPKG.
: MNL-READ-MARKER-PKG ( ptr u8 n -- ) {: a:ptr u:n :}
   0 MNL-MPKG-U !
   a u s" maki-ns-lint: boundary" LINT-FIND-SUB {: idx:n :}
   idx 0 < IF exit THEN
   idx s" maki-ns-lint: boundary" nip + {: off:n :}
   a off +  u off -  SPLIT-WHITESPACE
   SN# @ 0= IF exit THEN
   0 S@ MNL-MPKG! ;

\ First real `package` token's name (tokenized, so comment prose cannot spoof it).
: MNL-READ-ACTUAL-PKG ( ptr u8 n -- )
   0 MNL-APKG-U !
   LINT-TRUE PARENS? !
   TOKENIZE
   0 begin dup TN# @ < while
      dup TOK s" package" LINT-STR=CI IF
         dup 1+ TN# @ < IF dup 1+ TOK MNL-APKG! THEN
         drop exit
      THEN
      1+
   repeat drop ;

: MNL-STALE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u MNL-READ-MARKER-PKG
   a u MNL-READ-ACTUAL-PKG
   MNL-MPKG MNL-MPKG-U @  MNL-APKG MNL-APKG-U @  LINT-STR=CI 0= ;

\ ---- findings ----
: MNL-HIT ( ptr u8 n -- ) {: t:ptr tu:n :}
   MNL-REPORT? @ if
      s" MAKI-NS " type
      MNL-PATH MNL-PATHU @ type
      s" : top-level definition '" type
      t tu type
      s" ' outside package MAKI (wrap in `package MAKI`, or mark a subsystem boundary with `\ maki-ns-lint: boundary <PKG>`)" type
      MNL-NL
   then
   MNL-BAD+ ;

: MNL-STALE-HIT ( -- )
   MNL-REPORT? @ if
      s" MAKI-NS " type
      MNL-PATH MNL-PATHU @ type
      s" : stale boundary marker names package '" type
      MNL-MPKG MNL-MPKG-U @ type
      s" ' but file declares '" type
      MNL-APKG MNL-APKG-U @ type
      s" '" type MNL-NL
   then
   MNL-BAD+ ;

\ ---- token classification ----
: MNL-COLON-DEF? ( n -- bool ) {: k:n :}
   k TOK s" :" LINT-STR= if MNL-TRUE exit then
   k TOK s" +:" LINT-STR= if MNL-TRUE exit then
   k TOK s" TRUSTED:" LINT-STR=CI if MNL-TRUE exit then
   k TOK s" KERNEL:" LINT-STR=CI ;

: MNL-DATA-DEF? ( n -- bool ) {: k:n :}
   k TOK s" create" LINT-STR=CI if MNL-TRUE exit then
   k TOK s" variable" LINT-STR=CI if MNL-TRUE exit then
   k TOK s" constant" LINT-STR=CI if MNL-TRUE exit then
   k TOK s" defer" LINT-STR=CI ;

\ definer at index k, name at k+1: is it an exempt cross-cutting error constant?
: MNL-EXEMPT? ( n -- bool ) {: k:n :}
   k TOK s" constant" LINT-STR=CI 0= if MNL-FALSE exit then
   k 1+ TN# @ >= if MNL-FALSE exit then
   k 1+ TOK s" E-" LINT-PREFIX? ;

\ flag the definition name (k+1) when it is not inside package MAKI and not exempt
: MNL-CHECK-DEF ( n -- ) {: k:n :}
   MNL-IN-MAKI @ if exit then
   k MNL-EXEMPT? if exit then
   k 1+ TN# @ >= if exit then
   k 1+ TOK MNL-HIT ;

: MNL-STEP-PACKAGE ( n -- ) {: k:n :}
   k 1+ TN# @ < if
      k 1+ TOK s" MAKI" LINT-STR=CI MNL-IN-MAKI !
   else
      MNL-FALSE MNL-IN-MAKI !
   then ;

: MNL-STEP ( -- )
   MNL-I @ {: k:n :}
   MNL-IN-DEF @ if
      k TOK s" ;" LINT-STR= if MNL-FALSE MNL-IN-DEF ! then
      exit
   then
   k TOK s" package" LINT-STR=CI if k MNL-STEP-PACKAGE exit then
   k TOK s" end-package" LINT-STR=CI if MNL-FALSE MNL-IN-MAKI ! exit then
   k MNL-COLON-DEF? if
      k MNL-CHECK-DEF
      MNL-TRUE MNL-IN-DEF !
      exit
   then
   k MNL-DATA-DEF? if k MNL-CHECK-DEF then ;

: MNL-SCAN-TOKENS ( -- )
   0 MNL-I !
   MNL-FALSE MNL-IN-DEF !
   MNL-FALSE MNL-IN-MAKI !
   begin MNL-I @ TN# @ < while
      MNL-STEP
      MNL-I @ 1+ MNL-I !
   repeat ;

: MNL-SCAN-STR ( ptr u8 n -- )
   LINT-TRUE PARENS? !
   TOKENIZE
   MNL-SCAN-TOKENS ;

\ scan a source string in isolation (reset -> scan -> count), ignoring markers
: MNL-COUNT ( ptr u8 n -- n )
   MNL-REPORT? @ {: report:bool :}
   MNL-REPORT-OFF
   0 MNL-BAD !
   MNL-SCAN-STR
   report MNL-REPORT!
   MNL-BAD @ ;

: MNL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MNL-SRC? 0= IF exit THEN
   a u MNL-PATH!
   MNL-FILES @ 1+ MNL-FILES !
   a u MNL-BUF MNL-CAP READ-FILE {: fa:ptr fu:n :}
   fa fu MNL-MARKER? if
      fa fu MNL-STALE? if MNL-STALE-HIT then
      exit
   then
   fa fu MNL-SCAN-STR ;

: MAKI-NS-LINT ( -- )
   MNL-REPORT-ON
   0 MNL-BAD !  0 MNL-FILES !
   s" maki/" [: MNL-SCAN-FILE ;] WALK-FILES
   s" maki-ns-lint: " type
   MNL-FILES @ MNL-U. s"  maki file(s), " type
   MNL-BAD @ MNL-U.   s"  finding(s)" type MNL-NL
   MNL-BAD @ 0 > IF 1 throw THEN ;
