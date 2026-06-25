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

: BCG-TEST-PREFIX-LIST-FILE ( ptr u8 n -- )
   BCG-LOAD
   s" ['] PFX-LOAD-ROW PFX-FILES" BCG-MUST-HAVE
   s" ['] PFX-PATH-ROW PFX-FILES" BCG-MUST-HAVE
   s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-HAVE
   s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-HAVE
   s" LSRCRD @ BL," BCG-MUST-HAVE
   s" a u ZBYTES," BCG-MUST-HAVE
   s" LPUTIL @ ADR" BCG-MUST-LACK
   s" LSRCRD @ BL then" BCG-MUST-LACK
   s" a u ZBYTES ;" BCG-MUST-LACK
   s" LPLINUXTARGET @ LBL, s" BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST ( -- )
   s" bootstrap/cg/forth.fs" BCG-TEST-PREFIX-LIST-FILE
   s" src/habu/habu2.f" BCG-TEST-PREFIX-LIST-FILE ;

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
   s" : A-RRR16 ( a b c d -- e )" BCG-MUST-HAVE
   s" : A-RRI10 ( a b c d -- e )" BCG-MUST-HAVE
   s" : A-MOVW ( a b c d -- e )" BCG-MUST-HAVE
   s" : A-LS-UOFF ( a b c d -- e )" BCG-MUST-HAVE
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
   BCG-TEST-TRUST-CALLS
   BCG-TEST-IMAGE-BUFFER
   BCG-TEST-ASM-CHECKED
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

BCG-MAIN
