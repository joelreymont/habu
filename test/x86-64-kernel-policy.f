\ x86-64-kernel-policy.f - policy-admit and policy-seal in a booted x86 ELF.
\ The peer runs these images and checks their exact status and first stderr line:
\
\ hb-x64-kernel-policy-admit       0   PDEP's bit and the NDICT watermark hold
\ hb-x64-kernel-policy-sealed    107   a second admission is refused
\ hb-x64-kernel-policy-reseal    107   a second seal is refused
\ hb-x64-kernel-policy-missing   107   an absent package is named
\ hb-x64-kernel-policy-bound     107   a package wid without a bit is refused
\ hb-x64-kernel-policy-keyword   107   a public dispatch keyword is named
\
\ These cases distinguish a missing namespace, a bad wordlist bound, a missed
\ keyword callback, early bit publication, an ineffective seal and an unbalanced
\ caller stack. The real policy suite loads lib/policy.f through the captured
\ interpreter; these ELFs isolate the primitive's machine boundary.
require test/x86-64-boot-harness.f

package X64K-POLICY
using X64ASM
using X64CODE
using X64RT

5 constant PUB-WID

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: ROW, ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;

\ The callback has the captured OUTER policy check's effect: a public wid in,
\ the first colliding spelling and a flag out. The normal arm returns no name.
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
: PACKAGE, ( ptr u8 n n -- ) {: name:ptr size:n wid:n :}
   name size DICT-WL:NAMESPACE 0 X64HARNESS:RECORD,
   wid 0 X64KERNEL:REC-CODE + X64HARNESS:REGION!, ;

: ADMIT, ( ptr u8 n -- )
   X64HARNESS:PUSH-TEXT,  s" policy-admit" ROW, ;

: READY, ( ptr u8 n n -- ) {: name:ptr size:n wid:n :}
   name size wid PACKAGE,
   s" SAFE" wid 0 X64HARNESS:RECORD,
   X64KERNEL:HIDX-BUILD,
   X64HARNESS:REST, ;

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
   X64HARNESS:REST,
   s" NOPE" ADMIT, ;

: BOUND, ( -- )
   s" PBIG" PROT-WID-MAX READY,
   s" PBIG" ADMIT, ;

: KEYWORD, ( -- )
   DUP-KEYWORD INSTALL
   s" PKW" PUB-WID PACKAGE,
   s" DUP" PUB-WID 0 X64HARNESS:RECORD,
   X64KERNEL:HIDX-BUILD,
   X64HARNESS:REST,
   s" PKW" ADMIT, ;

: IMAGE ( [ -- ] ptr u8 n -- ) {: path:ptr size:n :}
   false X64HARNESS:BOOT-OPEN,
   execute
   path size X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   X64HARNESS:INIT
   [: ADMITTED, ;] s" hb-x64-kernel-policy-admit" TMP-PATH IMAGE
   [: SEALED-ADMIT, ;] s" hb-x64-kernel-policy-sealed" TMP-PATH IMAGE
   [: SEALED-SEAL, ;] s" hb-x64-kernel-policy-reseal" TMP-PATH IMAGE
   [: MISSING, ;] s" hb-x64-kernel-policy-missing" TMP-PATH IMAGE
   [: BOUND, ;] s" hb-x64-kernel-policy-bound" TMP-PATH IMAGE
   [: KEYWORD, ;] s" hb-x64-kernel-policy-keyword" TMP-PATH IMAGE ;

;using
;using
;using
;package

X64K-POLICY:RUN
