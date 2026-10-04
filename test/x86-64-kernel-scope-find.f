\ Real x86-64 kernel images exercise scope-find with and without the name
\ index. The emitted ELFs remain in HB_TMP for replay on the x86-64 peer.
require test/x86-64-boot-harness.f
require src/os/linux-x86-64/target-layout.f

package X64K-SCOPE-TEST
using X64ASM
using X64CODE
using X64LAYOUT

: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: SCOPE, ( ptr u8 n -- ) X64HARNESS:PUSH-TEXT,  s" scope-find" ROW ;

\ -1 means the null record; every other expected value is a record index.
: REC?, ( n -- ) dup 0 < if drop 0 X64HARNESS:EXPECT-POP, else X64HARNESS:EXPECT-ROW, then ;
: ANSWER, ( n n n n -- ) {: bound:n used:n used2:n flags:n :}
   flags X64HARNESS:EXPECT-POP,
   used2 REC?,  used REC?,  bound REC?, ;

\ Pkg is record 6, so its harness code cell is public wid 7. Its private
\ wordlist is wid 8; two other public wordlists are wids 9 and 10.
: SEED, ( -- )
   s" Alpha" 0 0 X64HARNESS:RECORD,                     \ 0
   s" ALPHA" 7 0 X64HARNESS:RECORD,                     \ 1
   s" alpha" 8 0 X64HARNESS:RECORD,                     \ 2
   s" Both" 9 0 X64HARNESS:RECORD,                      \ 3
   s" BOTH" 10 0 X64HARNESS:RECORD,                     \ 4
   s" both" 0 0 X64HARNESS:RECORD,                      \ 5
   s" Pkg" DICT-WL:NAMESPACE 0 X64HARNESS:RECORD,     \ 6: public wid 7
   s" OnlyPub" 7 0 X64HARNESS:RECORD,                   \ 7
   s" OnlyPri" 8 0 X64HARNESS:RECORD,                   \ 8
   s" OnlyUsed" 9 0 X64HARNESS:RECORD,                  \ 9
   s" ONLYUSED" 10 0 X64HARNESS:RECORD, ;              \ 10

: USE, ( n n -- ) {: first:n second:n :}
   first USE-WIDS-OFF X64HARNESS:CELL!,
   second USE-WIDS-OFF CELL + X64HARNESS:CELL!,
   2 USE-DEPTH-CELL X64HARNESS:CELL!, ;

: CASE, ( n -- )
   {: kind:n :}
   kind 0 = if s" aLPHA" SCOPE,  0 -1 -1 1 ANSWER, then
   kind 1 = if
      7 PKG-PUB-CELL X64HARNESS:CELL!,  8 PKG-PRI-CELL X64HARNESS:CELL!,
      s" ALPHA" SCOPE,  2 -1 -1 1 ANSWER,
   then
   kind 2 = if
      7 PKG-PUB-CELL X64HARNESS:CELL!,  8 PKG-PRI-CELL X64HARNESS:CELL!,
      s" OnlyPub" SCOPE,  7 -1 -1 1 ANSWER,
   then
   kind 3 = if s" pKg:ONLYPUB" SCOPE,  7 -1 -1 1 ANSWER, then
   kind 4 = if s" Pkg:OnlyPri" SCOPE,  -1 -1 -1 0 ANSWER, then
   kind 5 = if
      9 10 USE,
      s" Both" SCOPE,  5 3 4 3 ANSWER,
   then
   kind 6 = if
      9 10 USE,
      s" OnlyUsed" SCOPE,  -1 9 10 2 ANSWER,
   then
   kind 7 = if
      9 10 USE,
      s" Pkg:OnlyPub:extra" SCOPE,  -1 -1 -1 0 ANSWER,
   then
   kind 8 = if
      9 9 USE,
      s" OnlyUsed" SCOPE,  9 9 -1 1 ANSWER,
   then
   kind 9 = if
      7 PKG-PUB-CELL X64HARNESS:CELL!,  8 PKG-PRI-CELL X64HARNESS:CELL!,
      s" Pkg:Both" SCOPE,  5 -1 -1 1 ANSWER,
   then
   kind 10 = if s" Pkg:Both" SCOPE,  -1 -1 -1 0 ANSWER, then
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

\ A real primitive registered after CONTROL still owns a seeded record. Put
\ the registry's last row at its actual seed ordinal, not in the low fixture
\ range where the old partial count happened to work.
: BUILD-LATE ( -- )
   false X64HARNESS:BOOT-OPEN,
   ENGINE-PRIMS:COUNT 1- {: row:n :}
   ENGINE-GPR:X64-NDICT >R64 row >IMM64 ASM-SINK ENC-MOV-RI64
   row ENGINE-PRIMS:NAME$ row ENGINE-PRIMS:HELPER-WID 0 X64HARNESS:RECORD,
   row ENGINE-PRIMS:NAME$ SCOPE,  row -1 -1 1 ANSWER,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-scope-late-seed" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

: BUILD ( bool n ptr u8 n -- ) {: indexed:bool kind:n path:ptr u:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,
   indexed if X64KERNEL:HIDX-BUILD, then
   kind CASE,
   path u X64HARNESS:BOOT-CLOSE, ;

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false 0 s" hb-x64-scope-global-scan" TMP-PATH BUILD
   false 1 s" hb-x64-scope-private-scan" TMP-PATH BUILD
   false 2 s" hb-x64-scope-public-scan" TMP-PATH BUILD
   false 3 s" hb-x64-scope-qualified-scan" TMP-PATH BUILD
   false 4 s" hb-x64-scope-private-qual-scan" TMP-PATH BUILD
   false 5 s" hb-x64-scope-global-used-scan" TMP-PATH BUILD
   false 6 s" hb-x64-scope-ambiguous-scan" TMP-PATH BUILD
   false 7 s" hb-x64-scope-bad-qual-scan" TMP-PATH BUILD
   false 8 s" hb-x64-scope-duplicate-used-scan" TMP-PATH BUILD
   false 9 s" hb-x64-scope-open-qual-scan" TMP-PATH BUILD
   false 10 s" hb-x64-scope-closed-qual-scan" TMP-PATH BUILD
   true 0 s" hb-x64-scope-global-index" TMP-PATH BUILD
   true 1 s" hb-x64-scope-private-index" TMP-PATH BUILD
   true 2 s" hb-x64-scope-public-index" TMP-PATH BUILD
   true 3 s" hb-x64-scope-qualified-index" TMP-PATH BUILD
   true 4 s" hb-x64-scope-private-qual-index" TMP-PATH BUILD
   true 5 s" hb-x64-scope-global-used-index" TMP-PATH BUILD
   true 6 s" hb-x64-scope-ambiguous-index" TMP-PATH BUILD
   true 7 s" hb-x64-scope-bad-qual-index" TMP-PATH BUILD
   true 8 s" hb-x64-scope-duplicate-used-index" TMP-PATH BUILD
   true 9 s" hb-x64-scope-open-qual-index" TMP-PATH BUILD
   true 10 s" hb-x64-scope-closed-qual-index" TMP-PATH BUILD
   BUILD-LATE
   X64HARNESS:DISPOSE
   T-REPORT ;

RUN

;using
;using
;using
;package
