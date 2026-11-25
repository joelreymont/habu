\ x86-64-kernel-policy.f - policy-admit and policy-seal in a booted x86 ELF.
\ Each rejected image writes an `error[E-POLICY]` record or die line to stderr:
\
\ hb-x64-kernel-policy-sealed    record "the vocabulary is sealed"
\ hb-x64-kernel-policy-reseal    record "the vocabulary is sealed"
\ hb-x64-kernel-policy-missing   record "no package has the name the
\                                program admits", package NOPE
\ hb-x64-kernel-policy-bound     die line "E-POLICY habu: policy: package
\                                wid above the bound"
\ hb-x64-kernel-policy-keyword   record "a package publishes a word
\                                spelled like a keyword", package PKW,
\                                keyword DUP
\
\ These cases distinguish a missing namespace, a bad wordlist bound, a missed
\ keyword callback, early bit publication, an ineffective seal and an unbalanced
\ caller stack. The real vocabulary suite loads src/lib/vocabulary.f through the captured
\ interpreter; these ELFs isolate the primitive's machine boundary.
require src/core/exit-status.f
require test/x86-64-boot-harness.f

package X64K-POLICY
using X64ASM
using X64CODE
using X64RT

5 constant PUB-WID

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: ROW, ( str -- ) X64HARNESS:CALL-ROW, ;

\ The callback has the captured INTERPRETER policy check's effect: a public wid
\ in, the first colliding spelling and a flag out. The normal arm returns no
\ name.
: NO-KEYWORD ( -- label )
   [: 0 G-POP
      0 X64HARNESS:PUSH,  0 X64HARNESS:PUSH,  0 X64HARNESS:PUSH, ;]
   X64HARNESS:ROUTINE, ;

: DUP-KEYWORD ( -- label )
   [: 0 G-POP
      s" DUP" X64HARNESS:PUSH-TEXT,  -1 X64HARNESS:PUSH, ;]
   X64HARNESS:ROUTINE, ;

: INSTALL ( label -- )
   POLICY-ABI:KEYWORD-CELL X64HARNESS:LABEL-CELL!, ;

\ A namespace row's first cell is its public wid; the harness's RECORD, gives
\ an ordinary code entry there, so replace only that field before indexing.
: PACKAGE, ( str n -- ) {: name:str wid:n :}
   name WL:NAMESPACE 0 X64HARNESS:RECORD,
   wid 0 X64KERNEL:REC-CODE + X64HARNESS:REGION!, ;

: ADMIT, ( str -- )
   X64HARNESS:PUSH-TEXT,  s" policy-admit" ROW, ;

: READY, ( str n -- ) {: name:str wid:n :}
   name wid PACKAGE,
   s" SAFE" wid 0 X64HARNESS:RECORD,
   X64KERNEL:HIDX-BUILD, ;

: ADMITTED, ( -- )
   NO-KEYWORD INSTALL
   s" PDEP" PUB-WID READY,
   s" PDEP" ADMIT,
   1 PUB-WID lshift POLICY-BITS-OFF X64HARNESS:EXPECT-CELL,
   s" policy-seal" ROW,
   2 POLICY-NDICT-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: SEALED-ADMIT, ( -- )
   NO-KEYWORD INSTALL
   s" PDEP" PUB-WID READY,
   s" PDEP" ADMIT,
   s" policy-seal" ROW,
   s" PDEP" ADMIT, ;

: SEALED-SEAL, ( -- )
   NO-KEYWORD INSTALL
   s" PDEP" PUB-WID READY,
   s" policy-seal" ROW,
   s" policy-seal" ROW, ;

: MISSING, ( -- )
   s" NOPE" ADMIT, ;

: BOUND, ( -- )
   s" PBIG" POLICY-WID-MAX READY,
   s" PBIG" ADMIT, ;

: KEYWORD, ( -- )
   DUP-KEYWORD INSTALL
   s" PKW" PUB-WID PACKAGE,
   s" DUP" PUB-WID 0 X64HARNESS:RECORD,
   X64KERNEL:HIDX-BUILD,
   s" PKW" ADMIT, ;

: IMAGE ( [ -- ] n str -- ) {: status:n path:str :}
   false X64HARNESS:BOOT-OPEN,
   status X64HARNESS:EXPECT-EXIT
   execute
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: ADMITTED, ;] EXIT-STATUS:SUCCESS s" hb-x64-kernel-policy-admit" TMP-PATH IMAGE
   [: SEALED-ADMIT, ;] EXIT-STATUS:POLICY s" hb-x64-kernel-policy-sealed" TMP-PATH IMAGE
   [: SEALED-SEAL, ;] EXIT-STATUS:POLICY s" hb-x64-kernel-policy-reseal" TMP-PATH IMAGE
   [: MISSING, ;] EXIT-STATUS:POLICY s" hb-x64-kernel-policy-missing" TMP-PATH IMAGE
   [: BOUND, ;] EXIT-STATUS:POLICY s" hb-x64-kernel-policy-bound" TMP-PATH IMAGE
   [: KEYWORD, ;] EXIT-STATUS:POLICY s" hb-x64-kernel-policy-keyword" TMP-PATH IMAGE
   X64HARNESS:DISPOSE T-REPORT ;

;using
;using
;using
;package

X64K-POLICY:RUN
