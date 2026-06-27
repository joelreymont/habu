\ bootstrap-codegen-test.f - native source regression for bootstrap codegen hard cutover.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f

$20000 constant BCG-CAP

create BCG-BUF BCG-CAP allot
variable BCG-LEN

: BCG-SOURCE ( -- ptr u8 n )
   BCG-BUF BCG-LEN @ ;

: BCG-LOAD ( ptr u8 n -- )
   BCG-BUF BCG-CAP READ-ALL BCG-LEN ! ;

: BCG-HAS? ( ptr u8 n -- bool )
   BCG-SOURCE 2swap CONTAINS? ;

: BCG-MUST-HAVE ( ptr u8 n -- )
   BCG-HAS? TTRUE ;

: BCG-MUST-LACK ( ptr u8 n -- )
   BCG-HAS? 0= TTRUE ;

: BCG-POS ( ptr u8 n -- n )
   BCG-SOURCE 2swap FIND-SUB ;

: BCG-POS-FOUND ( ptr u8 n -- n )
   BCG-POS dup 0 >= TTRUE ;

: BCG-MUST-BEFORE ( ptr u8 n ptr u8 n -- ) {: earlier:ptr earlieru later:ptr lateru :}
   earlier earlieru BCG-POS-FOUND
   later lateru BCG-POS-FOUND
   < TTRUE ;

: BCG-FIND-AFTER ( n ptr u8 n -- n ) {: start needle:ptr nu :}
   BCG-SOURCE {: src:ptr srcu :}
   start 0 < if -1 exit then
   start srcu >= if -1 exit then
   src start + srcu start - needle nu FIND-SUB
   dup 0 < if exit then
   start + ;

: BCG-MUST-NOT-FIND-BEFORE ( n n ptr u8 n -- ) {: start end needle:ptr nu :}
   start needle nu BCG-FIND-AFTER {: pos :}
   pos 0 < if exit then
   pos end >= TTRUE ;

: BCG-MUST-FIND-BEFORE ( n n ptr u8 n -- )
   {: start end needle:ptr nu :}
   start needle nu BCG-FIND-AFTER dup 0 >= TTRUE
   end < TTRUE ;

: BCG-TEST-INSTALL-FAIL-CLOSED ( -- )
   s" bootstrap/cg/install.fs" BCG-LOAD
   s" : BODY-ARITY ( -- n )  ['] TRY-ARITY CG-CATCH ;" BCG-MUST-HAVE
   s" ['] TRY-EFFECT CG-CATCH" BCG-MUST-HAVE
   s" catch if 1" BCG-MUST-LACK
   s" catch if 0" BCG-MUST-LACK
   s" NM@ CAP$ BODY-ARITY EFFECT-FLAGS CG-RECORD" BCG-MUST-HAVE ;

: BCG-TEST-FORTH-SDQ-COMMENT ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" C-ADR PC-relative" BCG-MUST-HAVE
   s" push abs-addr" BCG-MUST-LACK
   s" absolute address is known" BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-COMMON ( -- )
   s" PFX-LOAD-FILES" BCG-MUST-HAVE
   s" PFX-PATH-FILES" BCG-MUST-HAVE
   s" PFX-FILES" BCG-MUST-LACK
   s" PFX-ROW" BCG-MUST-LACK
   s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-HAVE
   s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-HAVE
   s" a u ZBYTES," BCG-MUST-HAVE
   s" LPUTIL @ ADR" BCG-MUST-LACK
   s" LSRCRD @ BL then" BCG-MUST-LACK
   s" a u ZBYTES ;" BCG-MUST-LACK
   s" LPLINUXTARGET @ LBL, s" BCG-MUST-LACK
   s" PFX-COMMON LPCHECKER" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPCHECKER" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-COMMON LPROLES" BCG-MUST-BEFORE
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" PFX-COMMON LPINCLUDE" s" PFX-COMMON LPSTRUCTURES" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPENUMS" BCG-MUST-BEFORE
   s" PFX-COMMON LPENUMS" s" PFX-COMMON LPCOMBINATORS" BCG-MUST-BEFORE ;

: BCG-TEST-PREFIX-LIST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   s" LSRCRD @ BL," BCG-MUST-HAVE
   s" LSRCRD LABEL@ BL," BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-NATIVE ( -- )
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   s" LSRCRD LABEL@ BL," BCG-MUST-HAVE ;

: BCG-TEST-PREFIX-LIST ( -- )
   BCG-TEST-PREFIX-LIST-BOOTSTRAP
   BCG-TEST-PREFIX-LIST-NATIVE ;

: BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT ( -- )
   s" : C-SOURCE-BAKED" BCG-POS-FOUND {: start :}
   start s" : EMIT-SOURCE" BCG-FIND-AFTER dup 0 >= TTRUE {: end :}
   start end s" EMIT-COLD-PREFIX" BCG-MUST-FIND-BEFORE ;

: BCG-TEST-BAKED-SOURCE-PREFIX ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT ;

: BCG-TEST-TRUST-CALLS-CURRENT ( -- )
   s" : C-PUSH-DATA-CELL ( n -- )" BCG-MUST-HAVE
   s" : C-PUSH-TRUST-SIG ( n n -- )" BCG-MUST-HAVE
   s" : C-CALL-X11-SAVED ( -- )" BCG-MUST-HAVE
   s" CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG" BCG-MUST-HAVE
   s" 9 DATA CRSIG-A-CELL LDR,  9 G-PUSH" BCG-MUST-LACK
   s" 9 DATA CRSIG-U-CELL LDR,  9 G-PUSH" BCG-MUST-LACK ;

: BCG-TEST-TRUST-CALLS ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-TRUST-CALLS-CURRENT
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-TRUST-CALLS-CURRENT
   s" TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG" BCG-MUST-HAVE
   s" 9 DATA TSIG-A-CELL LDR,  9 G-PUSH" BCG-MUST-LACK
   s" 9 DATA TSIG-U-CELL LDR,  9 G-PUSH" BCG-MUST-LACK ;

: BCG-TEST-IMAGE-BUFFER-CURRENT ( -- )
   s" require image.fs" BCG-MUST-HAVE
   s" $90000 constant MSIZE" BCG-MUST-LACK
   s" create MBUF MSIZE allot" BCG-MUST-LACK
   s" variable MP" BCG-MUST-LACK
   s" variable MLEN" BCG-MUST-LACK
   s" : M8" BCG-MUST-LACK
   s" : M16" BCG-MUST-LACK
   s" : M32" BCG-MUST-LACK
   s" : M64" BCG-MUST-LACK
   s" SCODE CODELEN @ M-BYTES" BCG-MUST-HAVE ;

: BCG-TEST-IMAGE-BUFFER ( -- )
   s" bootstrap/cg/image.fs" BCG-LOAD
   s" create MBUF MSIZE allot" BCG-MUST-HAVE
   s" : M-BYTES ( addr u -- )" BCG-MUST-HAVE
   s" : M-NAME16 ( addr u -- )" BCG-MUST-HAVE
   s" bootstrap/cg/elf.fs" BCG-LOAD
   BCG-TEST-IMAGE-BUFFER-CURRENT
   s" bootstrap/cg/macho.fs" BCG-LOAD
   BCG-TEST-IMAGE-BUFFER-CURRENT ;

: BCG-TEST-ASM-CHECKED ( -- )
   s" bootstrap/cg/asm-checked.fs" BCG-LOAD
   s" : A-RRR16 ( reg reg n n -- n )" BCG-MUST-HAVE
   s" : A-RRI10 ( reg reg n n -- n )" BCG-MUST-HAVE
   s" : A-MOVW ( reg n n n -- n )" BCG-MUST-HAVE
   s" : A-LS-UOFF ( reg reg off n -- n )" BCG-MUST-HAVE
   s" 2332033024 A-RRR16" BCG-MUST-HAVE
   s" $9AC00C00 A-RRR16" BCG-MUST-HAVE
   s" $D63F0000 A-R1-5" BCG-MUST-HAVE
   s" 16 lshift swap 5 lshift or swap or" BCG-MUST-LACK
   s" 10 lshift swap 5 lshift or swap or" BCG-MUST-LACK ;

: BCG-MAIN ( -- )
   T-RESET
   BCG-TEST-INSTALL-FAIL-CLOSED
   BCG-TEST-FORTH-SDQ-COMMENT
   BCG-TEST-PREFIX-LIST
   BCG-TEST-BAKED-SOURCE-PREFIX
   BCG-TEST-TRUST-CALLS
   BCG-TEST-IMAGE-BUFFER
   BCG-TEST-ASM-CHECKED
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

BCG-MAIN
