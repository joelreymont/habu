\ match-factor-pin.f - construct/MATCH execution and diagnostic regressions.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/subject.f

package MATCH-FACTOR-PIN

$4000 constant CAP
30000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: CAP-LENS ( len len -- )
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: OUT$ ( -- ptr u8 n )   OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )   ERR ERR-U @ ;

: RUN! ( ptr u8 n n -- ) {: src:ptr srcu:n rc:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   outu erru CAP-LENS
   src srcu OUT$ ERR$ oc rc T-OUTCOME-EXITED= ;

\ Positive: exit 0, empty stderr, stdout equals the expected output.
: POS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n want:ptr wantu:n :}
   label labelu T-LABEL   src srcu 0 RUN!
   label labelu T-LABEL   ERR$ s" " T$=
   label labelu T-LABEL   OUT$ want wantu T$= ;

\ Negative: exact exit code + a stderr diagnostic substring; the armed-line
\ stdout (if any) is pinned exactly so a fail-open (silent accept) trips too.
: NEG ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n rc:n out:ptr outu:n diag:ptr diagu:n :}
   label labelu T-LABEL   src srcu rc RUN!
   label labelu T-LABEL   OUT$ out outu T$=
   label labelu T-LABEL   ERR$ diag diagu CONTAINS? TTRUE ;

\ On Linux x86-64 the native checker refuses a malformed definition before the
\ construct/MATCH diagnostic tail; both name the same bad token and reason, and
\ the native form also locates it inside the definition.
: DIAG$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: old:ptr oldu:n native:ptr nativeu:n :}
   HB-TARGET-LINUX-X86-64? if native nativeu else old oldu then ;

\ Construct then MATCH inside one word, scalar and wide in one fork: exit 0 with
\ the pinned stdout proves the shared legs store code only through the region's
\ writable alias (a store into the executable view faults).

: RT-CASES ( -- )
   s" rt/roundtrip"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: RT ( n -- n ) construct mfp ok MATCH mfp ok OF 1 + ENDOF err OF negate ENDOF ;MATCH ;\n: RTE ( n -- n ) construct mfp err MATCH mfp ok OF 1 + ENDOF err OF negate ENDOF ;MATCH ;\nSUMTYPE mfpw\n  VARIANT small n ;VARIANT\n  VARIANT big ptr u8 n n ;VARIANT\n;SUMTYPE\n: RW ( ptr u8 n n -- n ) construct mfpw big MATCH mfpw small OF 100 * ENDOF big OF nip nip ENDOF ;MATCH ;\n41 RT . 7 RTE . s\" zz\" 9 RW . cr\ns\" ok\" type cr\n"
      s\" 42\n-7\n9\n\nok\n" POS ;

\ After ;package the package's private word must be gone: the shared-leg BL/RET
\ traffic and LBCAP token capture must not corrupt the compile-time wordlist.
\ The public runner returns 45 (4 + PRIV 41); a bare PRIV after ;package dies
\ E-UNDEFINED at exit 70.

: PKG-CASE ( -- )
   s" pkg/restore"
      s\" package MFPK\nSUMTYPE pk\n  VARIANT ok n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PRIV ( -- n ) 41 ;\n: RP ( n -- n ) construct pk ok MATCH pk ok OF PRIV + ENDOF err OF PRIV - ENDOF ;MATCH ;\npublic\n: RUN ( -- n ) 4 RP ;\n;package\nMFPK:RUN . cr\ns\" armed\" type cr\nPRIV . cr\n"
      70 s\" 45\n\narmed\n" s" E-UNDEFINED: PRIV" NEG ;

\ Every factored failure branch dies at its own token: construct legs (LADTDIE
\ via B) on an unknown family or variant, rc 70; match legs on an unknown
\ family, variant or expected-of, rc 70; a runtime tag mismatch (C-DIE-BAD-TAG
\ inline), rc 85; a certified word's interpret underdepth, rc 70, through the
\ same fail path the ADT legs feed.

: DIAG-CASES ( -- )
   s" diag/con-fam"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( n -- n ) construct nofam ok ;\n"
      70 s" " s" hb: construct: unknown family: nofam"
      s" at 'nofam' bad construct: family not declared in the active package" DIAG$ NEG
   s" diag/con-var"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( n -- mfp ) construct mfp novar ;\n"
      70 s" " s" hb: construct: unknown variant: novar"
      s" at 'novar' bad construct: unknown variant" DIAG$ NEG
   s" diag/match-fam"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH nofam ok OF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: unknown family: nofam"
      s" at 'nofam' bad match: unknown type family" DIAG$ NEG
   s" diag/match-var"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH mfp novar OF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: unknown variant: novar"
      s" at 'novar' bad match: unknown variant" DIAG$ NEG
   s" diag/match-of"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH mfp ok NOTOF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: expected of: NOTOF"
      s" at 'NOTOF' bad match: variant token must be followed by OF" DIAG$ NEG
   s" diag/bad-tag"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PW ( mfp -- n ) MATCH mfp ok OF ENDOF err OF ENDOF ;MATCH ;\nTRUSTED: FORGE ( n -- mfp ) 99 ;\n: BAD ( -- n ) 7 FORGE PW ;\ns\" armed\" type cr\nBAD . cr\n"
      85 s\" armed\n" s" hb: bad mfp tag" NEG
   s" diag/underdepth"
      s\" SUMTYPE mfp\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PW ( mfp -- n ) MATCH mfp ok OF ENDOF err OF ENDOF ;MATCH ;\n: PWN ( n -- n ) construct mfp ok PW ;\ns\" armed\" type cr\nPWN . cr\n"
      70 s\" armed\n" s" hb: interpret stack underdepth: PWN" NEG ;

public

: RUN ( -- )
   T-RESET
   RT-CASES
   PKG-CASE
   DIAG-CASES
   T-REPORT
   s" match-factor-pin: ok" type cr ;

;package

MATCH-FACTOR-PIN:RUN
