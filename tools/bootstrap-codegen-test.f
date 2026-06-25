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

: BCG-MAIN ( -- )
   T-RESET
   BCG-TEST-INSTALL-FAIL-CLOSED
   BCG-TEST-FORTH-SDQ-COMMENT
   BCG-TEST-PREFIX-LIST
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

BCG-MAIN
