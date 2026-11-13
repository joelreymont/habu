\ enum-decl-suite.f - behavior and rollback suite for the ENUM declaration front
\ end, driven through ENUM-DECL:ED-RUN, which the global `ENUM` keyword calls:
\ `ENUM` and ED-RUN rows run one path. It asserts reject codes, rendered
\ diagnostics, field-row counts and the generated constructors.

require src/habu/verify-source.f
require test/replay-scope.f
require test/decl-diag-capture.f
require src/lib/test.f

DECL-DIAG:PROSE

T-RESET

variable PFB

\ A declared family name is reserved in the variant-name rejects below.
s" ENUM-DECL:ED-RUN color red green blue ;ENUM" evaluate-closed

TYPE-FIELD:COUNT PFB !
s" ENUM-DECL:ED-RUN dupf VARIANT alpha FIELD z n FIELD z n ;VARIANT ;ENUM" TEST-EVAL:RC 7102 T=
TYPE-FIELD:COUNT PFB @ T=

s" ENUM-DECL:ED-RUN dupv red red ;ENUM" TEST-EVAL:RC 7102 T=

s" ENUM-DECL:ED-RUN emix red VARIANT alpha ;VARIANT ;ENUM" TEST-EVAL:RC 7107 T=  \ mixed modes (block token in compact)
s" ENUM-DECL:ED-RUN eac<a,b> red ;ENUM" TEST-EVAL:RC 7107 T=                \ parameterized compact form
s" ENUM-DECL:ED-RUN ech red POLICY packed-tag ;ENUM" TEST-EVAL:RC 7107 T=  \ header clause after a compact variant
s" ENUM-DECL:ED-RUN ecp red FIELD y n ;ENUM" TEST-EVAL:RC 7107 T=        \ positional/named payload in compact
s" ENUM-DECL:ED-RUN emv VARIANT alpha FIELD x n ;ENUM" TEST-EVAL:RC 7107 T=  \ missing ;VARIANT
s" ENUM-DECL:ED-RUN eme red green" TEST-EVAL:RC 7107 T=                  \ missing ;ENUM
s" ENUM-DECL:ED-RUN ear<a,> VARIANT alpha ;VARIANT ;ENUM" TEST-EVAL:RC 7108 T=  \ trailing comma in parameter list
s" ENUM-DECL:ED-RUN eempty ;ENUM" TEST-EVAL:RC 7107 T=                   \ an enum needs a variant
s" ENUM-DECL:ED-RUN enum red ;ENUM" TEST-EVAL:RC 7110 T=                 \ reserved opener keyword as a name
s" ENUM-DECL:ED-RUN Bad red ;ENUM" TEST-EVAL:RC 7101 T=                  \ upper-case family name (case)
s" ENUM-DECL:ED-RUN n red ;ENUM" TEST-EVAL:RC 7110 T=                    \ single-letter family name
\ An effect reads a bare `ptr` as the pointer constructor, never as a family, so
\ the tail is reserved ahead of the global `ptr` family's duplicate check (7102).
s" ENUM-DECL:ED-RUN ptr red ;ENUM" TEST-EVAL:RC 7110 T=
s" ENUM-DECL:ED-RUN erf VARIANT alpha FIELD make n ;VARIANT ;ENUM" TEST-EVAL:RC 7125 T=  \ reserved field name
s" ENUM-DECL:ED-RUN ecf VARIANT alpha FIELD Zed n ;VARIANT ;ENUM" TEST-EVAL:RC 7101 T=  \ upper-case field name (case)
s" ENUM-DECL:ED-RUN ebs VARIANT alpha FIELD x nope ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=  \ unresolved field type
s" ENUM-DECL:ED-RUN euc VARIANT alpha FIELD x Q ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=  \ upper-case single-letter type
s" ENUM-DECL:ED-RUN epa VARIANT alpha FIELD x a ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=  \ parameter outside declared arity
s" ENUM-DECL:ED-RUN epg<a,b,c,d,e,g> VARIANT alpha FIELD x h ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=  \ parameter 6 is outside arity 6

s" ENUM-DECL:ED-RUN twice red ;ENUM" evaluate-closed
s" ENUM-DECL:ED-RUN twice red ;ENUM" TEST-EVAL:RC 7102 T=

\ Reserved variant names reject before publication; a family owned only by another
\ package does not reserve its tails, while a global or active-package family does.
package other-enum-name-test
s" ENUM-DECL:ED-RUN foreign-variant member ;ENUM" evaluate-closed
;package

package enum-name-test

VALUE-RECORD enum-record payload n END-VALUE-RECORD
s" ENUM-DECL:ED-RUN local-variant member ;ENUM" evaluate-closed

public

\ REJECT-CODE asserts a declaration's reject code, public so section 24
\ (control-word names) reuses it.
: REJECT-CODE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u TEST-EVAL:RC want T= ;

s" ENUM-DECL:ED-RUN compact-dup-policy POLICY packed-tag POLICY stack-cell-tag alpha ;ENUM" 7163 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-dup-feature DERIVE eq eq alpha ;ENUM" 7164 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-dup-split DERIVE eq DERIVE eq alpha ;ENUM" 7164 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-missing-policy POLICY ;ENUM" 7116 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-missing-derive DERIVE ;ENUM" 7119 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-bad-policy POLICY unknown alpha ;ENUM" 7116 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-bad-feature DERIVE unknown alpha ;ENUM" 7119 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-order DERIVE order alpha ;ENUM" 7119 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-no-variant POLICY packed-tag ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-late-policy alpha POLICY packed-tag ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-late-derive alpha DERIVE eq ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-policy-name alpha policy ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN compact-derive-name alpha derive ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN full-niche<a> POLICY niche-null VARIANT none ;VARIANT VARIANT some FIELD value ptr a ;VARIANT ;ENUM" 0 REJECT-CODE

private

s" ENUM-DECL:ED-RUN reject-compact ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact n ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact q ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact if ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact alpha variant ;ENUM" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact bool ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact enum-record ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact space-x ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact color ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-compact local-variant ;ENUM" 7110 REJECT-CODE

s" ENUM-DECL:ED-RUN reject-full VARIANT" 7107 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT n ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT q ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT if ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT variant ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT bool ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT enum-record ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT space-x ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT color ;VARIANT ;ENUM" 7110 REJECT-CODE
s" ENUM-DECL:ED-RUN reject-full VARIANT local-variant ;VARIANT ;ENUM" 7110 REJECT-CODE

s" ENUM-DECL:ED-RUN allowed-compact foreign-variant ;ENUM" evaluate-closed
s" ENUM-DECL:ED-RUN allowed-full VARIANT foreign-variant ;VARIANT ;ENUM" evaluate-closed
s" ENUM-DECL:ED-RUN duplicate-order ready ready ;ENUM" 7102 REJECT-CODE
\ A package may shadow a global family, but not `ptr`: an effect in this
\ package would still read `ptr` as the pointer constructor.
s" ENUM-DECL:ED-RUN ptr ready ;ENUM" 7110 REJECT-CODE

;package

\ 20. A public ENUM family with variants owns one sealed checked
\     FAMILY:VARIANT constructor per variant, generated inside the declaration's
\     own transaction.
package enum-ctor-test
public

variable CT-DICT   variable CT-CP

: PENDING ( -- n ) CTOR-PEND-COUNT @ ;
: DICT-RECS ( -- n ) ndict@ ;
: DICT-CODE ( -- n ) cp@ ;

: DICT-MARK ( -- ) DICT-RECS CT-DICT !  DICT-CODE CT-CP ! ;
: DICT-SAME ( -- ) DICT-RECS CT-DICT @ T=  DICT-CODE CT-CP @ T= ;
: DICT-MOVED ( -- ) DICT-RECS CT-DICT @ > TTRUE ;

\ A generation failure must leave the native dictionary byte-identical: what
\ the generator had defined is truncated with the rest of the declaration.
: CTOR-REJECT ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   DICT-MARK
   a u TEST-EVAL:RC want T=
   DICT-SAME ;

private
;package

\ 20a. Generation moves the native dictionary, which is what the dictionary
\      participant's savepoint has to be able to undo.
enum-ctor-test:DICT-MARK
s" ENUM-DECL:ED-RUN msgctor VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" evaluate-closed
enum-ctor-test:DICT-MOVED

s" C1 ( -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! -1 T=
s" C2 ( n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! -1 T=

s" C3 ( n -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! 0 T=
s" C4 ( n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C5 ( n n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C6 ( n n -- n ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=

\ 20b. DERIVE rides the same commit.
s" ENUM-DECL:ED-RUN dctor DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two ;VARIANT ;ENUM" evaluate-closed
s" D1 ( n -- dctor ) DCTOR:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" D2 ( -- dctor ) DCTOR:TWO" CHECK-QUIET-CANDIDATE! -1 T=
s" D3 ( dctor -- n ) DCTOR:TAG" CHECK-QUIET-CANDIDATE! -1 T=
s" D4 ( dctor dctor -- f ) DCTOR:EQ" CHECK-QUIET-CANDIDATE! -1 T=

s" ENUM lgpar red green blue ;ENUM" evaluate-closed
s" ENUM-DECL:ED-RUN fepar red green blue ;ENUM" evaluate-closed
s" P1 ( -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P2 ( -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P3 ( -- lgpar ) LGPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P4 ( -- fepar ) FEPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P5 ( n -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=
s" P6 ( n -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=

\ 20d. A PRODUCT family is not owned here: its definer generates its own
\      make/unmake.
s" PRODUCT prodctor FIELD a n FIELD b n ;PRODUCT" evaluate-closed
s" G1 ( n n -- prodctor ) PRODCTOR:MAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ 20f. A failure inside the generation commit (a payload with no derived equality,
\      a variant named like a derived word) rolls the whole declaration back.
s" ENUM-DECL:ED-RUN rollctor DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two FIELD b r ;VARIANT ;ENUM"
   7119 enum-ctor-test:CTOR-REJECT
s" ENUM-DECL:ED-RUN rollctor2 DERIVE eq VARIANT tag FIELD a n ;VARIANT ;ENUM"
   7110 enum-ctor-test:CTOR-REJECT

\ A later constructor collides after the first has entered the generation plan.
: ROLLCOLL:TAKEN ( -- n ) 37 ;
s" ENUM-DECL:ED-RUN rollcoll fresh taken ;ENUM"
   TYPE-DECL:E-TDECL-NAME enum-ctor-test:CTOR-REJECT
enum-ctor-test:PENDING 0 T=
DECLARATION-TRANSACTION:ACTIVE? 0= TTRUE
s" RC-FRESH ( -- n ) ROLLCOLL:FRESH" CHECK-QUIET-CANDIDATE! 0 T=
ROLLCOLL:TAKEN 37 T=
\ Rollback also removes protection of the generated package and leaves the
\ namespace available for a corrected declaration using the same family name.
undefine ROLLCOLL:TAKEN
s" ENUM-DECL:ED-RUN rollcoll fresh taken ;ENUM" evaluate-closed
s" RC-VALID ( -- rollcoll ) ROLLCOLL:FRESH" CHECK-QUIET-CANDIDATE! -1 T=

\ PRODUCT make/unmake uses the pending constructor authority queue. A collision
\ on UNMAKE occurs after MAKE was queued, and must clear that authority too.
: ROLLP:UNMAKE ( -- n ) 41 ;
s" STRUCTURE rollp FIELD value n ;STRUCTURE"
   TYPE-DECL:E-TDECL-NAME enum-ctor-test:CTOR-REJECT
enum-ctor-test:PENDING 0 T=
DECLARATION-TRANSACTION:ACTIVE? 0= TTRUE
s" RP-MAKE ( n -- n ) ROLLP:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
ROLLP:UNMAKE 41 T=
undefine ROLLP:UNMAKE
s" STRUCTURE rollp FIELD value n ;STRUCTURE" evaluate-closed
s" RP-VALID ( n -- rollp ) ROLLP:MAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ 21. POLICY packed-tag with DERIVE eq: an n payload declares, and a pointer payload
\     is refused at the constructor commit (no derived equality, 7119).
s" ENUM-DECL:ED-RUN lay-keep POLICY packed-tag DERIVE eq VARIANT one FIELD p n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM-DECL:ED-RUN lay-drop POLICY packed-tag DERIVE eq VARIANT one FIELD p ptr n ;VARIANT ;ENUM" TEST-EVAL:RC 7119 T=

\ 22. Each case asserts the whole rendered line, so text the declaration
\     supplies cannot satisfy it by appearing elsewhere in the buffer.

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgempty ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgempty': empty enum at 'dgempty'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN Dgcase red ;ENUM" TEST-EVAL:RC 7101 T=
s" habu: bad enum declaration 'Dgcase': name must be a lowercase family tail at 'Dgcase'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN n red ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'n': reserved name at 'n'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdup red red ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgdup': duplicate variant at 'red'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgar<a,a> VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 7108 T=
s" habu: bad enum declaration 'dgar<a,a>': invalid type parameter header at 'dgar<a,a>'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgpay VARIANT vv FIELD payfld nosuchtype ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'dgpay': unknown payload type at 'nosuchtype'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgpol POLICY nosuch red ;ENUM" TEST-EVAL:RC 7116 T=
s" habu: bad enum declaration 'dgpol': unknown layout policy at 'nosuch'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdrv DERIVE nosuch red ;ENUM" TEST-EVAL:RC 7119 T=
s" habu: bad enum declaration 'dgdrv': unknown derive feature at 'nosuch'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgord DERIVE order red ;ENUM" TEST-EVAL:RC 7119 T=
s" habu: bad enum declaration 'dgord': derive feature not yet supported at 'order'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgfn VARIANT vv FIELD make n ;VARIANT ;ENUM" TEST-EVAL:RC 7125 T=
s" habu: bad enum declaration 'dgfn': reserved field name at 'make'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgkw alpha variant ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgkw': block keyword in a compact enum at 'variant'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgnoend red" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgnoend': missing ;ENUM at 'dgnoend'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dglegacy red red ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dglegacy': duplicate variant at 'red'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgunified red red ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgunified': duplicate variant at 'red'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dglegacy2 ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dglegacy2': empty enum at 'dglegacy2'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgunified2 ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgunified2': empty enum at 'dgunified2'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgnest VARIANT nestvar FIELD nestfld nosuchtype ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'dgnest': unknown payload type at 'nosuchtype'" DECL-DIAG:HAS? TTRUE
s" declaration 'nestvar'" DECL-DIAG:HAS? TFALSE
s" declaration 'nestfld'" DECL-DIAG:HAS? TFALSE

\ 22d. The front end names the family from the token it just read,
\      unconditionally and before validating it, so the empty token replaces the
\      previous family.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration '': missing name" DECL-DIAG:HAS? TTRUE
s" dgnest" DECL-DIAG:HAS? TFALSE

\ 22e. Hostile declarations. Text supplied BY the declaration cannot forge or
\      displace the packet: a variant spelled like a fragment of the message is
\      reported as the token it is, the reason still comes from the reject, and
\      exactly one line is emitted (length = the line plus its newline).
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgforge duplicate duplicate ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgforge': duplicate variant at 'duplicate'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:LEN 71 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgat at at ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgat': duplicate variant at 'at'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:LEN 61 T=

\      A family name spelled like one of the JSON packet's own keys keeps key and
\      value distinguishable, and a token carrying a double quote is escaped
\      rather than closing the JSON string early.
DECL-DIAG:JSON
s" ENUM-DECL:ED-RUN reason red red ;ENUM" TEST-EVAL:RC 7102 T=
s\" \"family\":\"reason\"" DECL-DIAG:HAS? TTRUE
s\" \"reason\":\"duplicate variant\"" DECL-DIAG:HAS? TTRUE
s\" \"token\":\"red\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:JSON
s\" ENUM-DECL:ED-RUN dgquote aq\"b red ;ENUM" TEST-EVAL:RC 7101 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? TTRUE
s\" \"family\":\"dgquote\"" DECL-DIAG:HAS? TTRUE
s\" \"code\":\"E-BAD-DECLARATION\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgok1 red green ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM-DECL:ED-RUN dgok2 VARIANT okvar FIELD okfld n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM-DECL:ED-RUN dgok3 POLICY packed-tag redd greend ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM-DECL:ED-RUN dgok4 DERIVE eq rede greene ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgroll red red ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgroll': duplicate variant at 'red'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:OFF

\ 22g. An arming names a fault expected AT a token, so once the front end has
\      moved on it can no longer explain anything.
s" enum" DECL-REJECT:OPEN
s" armed-for-7102" 7102 DECL-REJECT:EXPECT
7102 DECL-REJECT:REASON$ s" armed-for-7102" DECL-DIAG:SAME? TTRUE
7107 DECL-REJECT:REASON$ s" malformed declaration" DECL-DIAG:SAME? TTRUE
s" moved-on" DECL-REJECT:TOKEN!
7102 DECL-REJECT:REASON$ s" duplicate name in this package" DECL-DIAG:SAME? TTRUE

s" enum" DECL-REJECT:OPEN
s" leaked-family" DECL-REJECT:FAMILY!
s" leaked-token" DECL-REJECT:TOKEN!
s" leaked-reason" 7102 DECL-REJECT:EXPECT
s" enum" DECL-REJECT:OPEN
DECL-REJECT:FAMILY$ nip 0 T=
DECL-REJECT:TOKEN$ nip 0 T=
7102 DECL-REJECT:REASON$ s" duplicate name in this package" DECL-DIAG:SAME? TTRUE

\ 22h. A span longer than the 96-byte slot is capped and MARKED with a
\      trailing "...", so the packet never presents a prefix as if it were
\      the whole name.
\      The token is written FIRST here and the over-long family SECOND, because
\      S-FAMILY sits immediately below S-TOKEN: an unclamped copy would
\      overwrite the token that was already there.
s" enum" DECL-REJECT:OPEN
s" survivor" DECL-REJECT:TOKEN!
s" qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop"
DECL-REJECT:FAMILY!
DECL-REJECT:TOKEN$ s" survivor" DECL-DIAG:SAME? TTRUE
DECL-REJECT:FAMILY$ nip 96 T=
DECL-REJECT:FAMILY$
s" qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwe..."
DECL-DIAG:SAME? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop ;ENUM"
TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwe...': empty enum"
DECL-DIAG:HAS? TTRUE

\ 22i. The reasons no case above reaches: the armed duplicate-family text, the
\      declaration-event clause duplicates, the variant-block and header-order
\      grammar faults, and the three codes whose reason can only come from the
\      table because the owner that raises them is past the front end's last
\      token.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgtwice red ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgtwice blue ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgtwice': duplicate family at 'dgtwice'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdupfld VARIANT vv FIELD zz n FIELD zz n ;VARIANT ;ENUM" TEST-EVAL:RC 7102 T=
s" habu: bad enum declaration 'dgdupfld': duplicate field name at 'zz'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dg2pol POLICY packed-tag POLICY packed-tag red ;ENUM" TEST-EVAL:RC 7163 T=
s" habu: bad enum declaration 'dg2pol': a second POLICY clause in one declaration at 'packed-tag'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dg2drv DERIVE eq DERIVE eq red ;ENUM" TEST-EVAL:RC 7164 T=
s" habu: bad enum declaration 'dg2drv': the same DERIVE feature twice in one declaration at 'eq'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dghdr red POLICY packed-tag ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dghdr': header clause after the first variant at 'POLICY'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvb VARIANT vv stray ;VARIANT ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgvb': unexpected token in variant block at 'stray'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvn VARIANT" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'dgvn': missing variant name" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdgd<a> DERIVE eq VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 7119 T=
s" habu: bad enum declaration 'dgdgd': derive requires a concrete (arity 0) family at 'eq'"
DECL-DIAG:HAS? TTRUE

\      7119 raised by the constructor participant's payload-role check, two
\      phases after the body: no reason is armed for it, so the table answers.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgroleq DERIVE eq VARIANT vv FIELD p ptr n ;VARIANT ;ENUM" TEST-EVAL:RC 7119 T=
s" habu: bad enum declaration 'dgroleq': a payload type or role has no derived equality at 'dgroleq'"
DECL-DIAG:HAS? TTRUE

\      7110 raised by the variant-name gate, and 7101 raised by the field record:
\      both are deeper owners, both answered from the table.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvrsv VARIANT n ;VARIANT ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'dgvrsv': name is reserved or already taken at 'n'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgfcase VARIANT vv FIELD Zed n ;VARIANT ;ENUM" TEST-EVAL:RC 7101 T=
s" habu: bad enum declaration 'dgfcase': name must be a lowercase tail at 'Zed'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:OFF

\ 22j. A reason explained in a text that throws is taken back with it, so
\      storage laid over its row is never walked as a reason.
package enum-reason-test
: ED-ONE ( -- ) s" reason one" 990001 DECL-REJECT:EXPLAIN ;
: ED-FILL ( n -- ) 0 ?do 777 , loop ;
s" ED-ONE 1 throw" TEST-EVAL:RC 1 T=
create ED-JUNK 16 ED-FILL
s" reason two" 990002 DECL-REJECT:EXPLAIN
990002 DECL-REJECT:REASON$ s" reason two" DECL-DIAG:SAME? TTRUE
;package

\ 23. ED-REPLAY - registering an ENUM from tokens a tool already lexed.
package enum-replay-test
public

: RP-EV ( ptr u8 n ptr u8 n -- ) ENUM-DECL:ED-REPLAY ;
: RP-TRY ( ptr u8 n ptr u8 n -- n )
   [: 2over 2over RP-EV ;] catch {: rc:n :} 2drop 2drop rc ;

\ Force the replay stream open so the re-entry guard can be reached at all; the
\ production callers never nest.
: RP-FORCE-OPEN ( -- ) s" x" s" y" DECL-REPLAY:RP-CLAIM ;
: RP-FORCE-CLOSE ( -- ) DECL-REPLAY:RP-RELEASE ;

private
;package

enum-ctor-test:DICT-MARK
CHECKER-SCOPE-START-NEUTRAL
s" rpcompact" s" red green blue ;ENUM" enum-replay-test:RP-TRY 0 T=
s" R1 ( -- rpcompact ) RPCOMPACT:RED" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" R1BAD ( -- n ) RPCOMPACT:RED" VERIFY:CANDIDATE-IN-SCOPE 0 T=
CHECKER-SCOPE-FINALIZE
enum-ctor-test:DICT-SAME

s" rpfull" s" VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM"
enum-replay-test:RP-TRY 0 T=

\ 23c. POLICY and DERIVE replay; derived EQ calls rows only the replay recorded, so
\      it runs in the check tool's replay scope.
TOOL-REPLAY:OPEN
s" rppol" s" POLICY packed-tag DERIVE eq hash red green ;ENUM"
enum-replay-test:RP-TRY 0 T=
TOOL-REPLAY:CLOSE

DECL-DIAG:PROSE
s" rpbadv" s" VARIANT vv stray ;VARIANT ;ENUM" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration 'rpbadv': unexpected token in variant block at 'stray'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" rpbadname" s" red red ;ENUM" enum-replay-test:RP-TRY 7102 T=
s" habu: bad enum declaration 'rpbadname': duplicate variant at 'red'"
DECL-DIAG:HAS? TTRUE

\ 23e. A buffer missing its terminator rejects through the front end's own
\      end-of-input gate, as a truncated live declaration does.
DECL-DIAG:PROSE
s" rpnoend" s" red green" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration 'rpnoend': missing ;ENUM" DECL-DIAG:HAS? TTRUE

\ 23f. A zero-length name reaches the missing-name gate instead of silently
\      promoting the first body token to the family name.
DECL-DIAG:PROSE
s" " s" red green ;ENUM" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration '': missing name" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ 23g. A REJECTED replay leaves the stream closed, so the next LIVE declaration
\      reads the input source again rather than a spent buffer.
DECL-DIAG:PROSE
s" rpdangle" s" VARIANT" enum-replay-test:RP-TRY 7107 T=
DECL-DIAG:OFF
s" ENUM-DECL:ED-RUN rpafterbad VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

\ 23h. Re-entry is refused with its own named code rather than retargeting a
\      stream that is already installed.
enum-replay-test:RP-FORCE-OPEN
s" rpbusy" s" red ;ENUM" enum-replay-test:RP-TRY 7177 T=
enum-replay-test:RP-FORCE-CLOSE
s" ENUM-DECL:ED-RUN rpafterbusy VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

package rp-copy-test
public
s" shape" s" POLICY packed-tag VARIANT alpha ;VARIANT VARIANT beta FIELD px n ;VARIANT ;ENUM"
enum-replay-test:RP-TRY 0 T=
;package

\ 24. Control words are reserved in every declaration name position: a family,
\     variant or field named `if` would compile as the control word wherever
\     generated code names it.
package enum-control-test
public

\ 24a. Every word on the shared list, in the family-name position. Three of them
\      (`?do`, `+loop`, `;match`) never reach the reserved-name gate: their
\      leading byte is not a lowercase letter, so the canonical-tail gate refuses
\      them first with 7101.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN if red ;ENUM"        7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN then red ;ENUM"      7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN else red ;ENUM"      7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN begin red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN until red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN again red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN while red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN repeat red ;ENUM"    7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN case red ;ENUM"      7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN of red ;ENUM"        7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN endof red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN endcase red ;ENUM"   7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN do red ;ENUM"        7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN loop red ;ENUM"      7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN leave red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN unloop red ;ENUM"    7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN exit red ;ENUM"      7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN recurse red ;ENUM"   7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN construct red ;ENUM" 7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN match red ;ENUM"     7110 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN ?do red ;ENUM"       7101 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN +loop red ;ENUM"     7101 enum-name-test:REJECT-CODE
s" ENUM-DECL:ED-RUN ;match red ;ENUM"    7101 enum-name-test:REJECT-CODE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN if red green ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM if red green ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwvc do loop ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'cwvc': name is reserved or already taken at 'do'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwvf VARIANT loop ;VARIANT ;ENUM" TEST-EVAL:RC 7110 T=
s" habu: bad enum declaration 'cwvf': name is reserved or already taken at 'loop'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ 24d. Field-name position: the field row's gate answers 7125, the
\      code and wording a reserved generated-operation name such as `make` gets.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwfld VARIANT vv FIELD then n ;VARIANT ;ENUM" TEST-EVAL:RC 7125 T=
s" habu: bad enum declaration 'cwfld': reserved field name at 'then'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

s" ENUM-DECL:ED-RUN iffy dolly matcher constructor elsewhere ;ENUM" evaluate-closed
s" ENUM-DECL:ED-RUN doing VARIANT looping FIELD ifs n FIELD thence n ;VARIANT ;ENUM" evaluate-closed

;package

\ 25. A 700-variant declaration is accepted: the front end buffers
\     no body. The private package keeps constructor generation out of it.
package enum-long-test

$2000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U
variable LI

700 constant LONG-N

: SRC$ ( -- ptr u8 n ) SRC-BUF SRC-U @ ;
: PUT-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF s" enum-decl-suite: long-enum buffer overflow" 1 die THEN
   c SRC-BUF SRC-U @ + c!  SRC-U @ 1 + SRC-U ! ;
: PUT$ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LI !
   BEGIN LI @ u < WHILE  a LI @ + c@ PUT-C  LI @ 1 + LI !  REPEAT ;
: PUT-DIGIT ( n -- ) 48 + PUT-C ;
: PUT-VARIANT ( n -- ) {: i:n :}
   118 PUT-C
   i 100 / 10 mod PUT-DIGIT
   i 10 / 10 mod PUT-DIGIT
   i 10 mod PUT-DIGIT
   32 PUT-C ;
: BUILD ( -- )
   0 SRC-U !
   s" ENUM-DECL:ED-RUN longenum " PUT$
   0 LI !
   LONG-N 0 ?DO I PUT-VARIANT LOOP
   s" ;ENUM" PUT$ ;

private
BUILD
SRC$ TEST-EVAL:RC 0 T=
;package

\ A variant payload may name a linear family at any depth, and the enum then owns
\ it.
package EDLIN
public
DEFLINEAR EDLIN:tok
;package

s" ENUM edlone VARIANT hold FIELD t EDLIN:tok ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" STRUCTURE edlmodel FIELD res EDLIN:tok FIELD nl n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" ENUM edl-load-result VARIANT loaded FIELD model edlmodel ;VARIANT VARIANT rejected FIELD code n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" STRUCTURE edl-via-load FIELD result edl-load-result ;STRUCTURE" TEST-EVAL:RC 0 T=
s" ENUM edlnest VARIANT loaded FIELD result edl-via-load ;VARIANT VARIANT rejected FIELD code n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" STRUCTURE edlplain FIELD v n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" ENUM edlctl VARIANT hold FIELD m edlplain ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" ENUM edlrole VARIANT hold FIELD edlmodel n ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

DECL-DIAG:PROSE
s" ENUM edlfwd VARIANT hold FIELD m edllater ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlfwd': unknown payload type at 'edllater'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

s" ENUM edlgen<a> VARIANT hold FIELD m a ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM edlgenuse VARIANT hold FIELD m edlgen ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlgenuse': payload type is parametric and needs type arguments at 'edlgen'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A variant payload may not name a pointer to a linear value: a pointer is a
\ non-owning boundary, so the linearity walk stops at the pointer node and the
\ enum would read non-linear, copying and dropping freely with a linear
\ resource behind the address.

s" ENUM edlowns VARIANT hold FIELD m edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

DECL-DIAG:PROSE
s" ENUM edlptr VARIANT hold FIELD p ptr edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptr': payload type is a pointer to a linear value and cannot own it at 'edlmodel'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrcon VARIANT hold FIELD p ptr EDLIN:tok ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptrcon': payload type is a pointer to a linear value and cannot own it at 'EDLIN:tok'"
DECL-DIAG:HAS? TTRUE

\ The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" ENUM edlptr2 VARIANT hold FIELD p ptr ptr edlmodel ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptr2': payload type is a pointer to a linear value and cannot own it at 'edlmodel'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrsum VARIANT hold FIELD p ptr edl-load-result ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptrsum': payload type is a pointer to a linear value and cannot own it at 'edl-load-result'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrlate VARIANT good FIELD c n ;VARIANT VARIANT bad FIELD p ptr edlmodel ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptrlate': payload type is a pointer to a linear value and cannot own it at 'edlmodel'"
DECL-DIAG:HAS? TTRUE

\ A pointer to a family that owns nothing declares, so the
\ rejects above answer the POINTEE's linearity and not the word `ptr`.
s" ENUM edlptrok VARIANT hold FIELD p ptr edlplain ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrok2 VARIANT hold FIELD p ptr ptr edlplain ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrn VARIANT hold FIELD p ptr n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" ENUM edlptrrole VARIANT hold FIELD ptr n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrrole2 VARIANT hold FIELD edlmodel n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

\ A variant block has no comment syntax: `(` and `\` are
\ ordinary tokens, so the declaration is malformed and rejects 7107, not this
\ rule's 7109.
DECL-DIAG:PROSE
s" ENUM edlptrpar VARIANT hold FIELD v n ( FIELD p ptr edlmodel ) ;VARIANT ;ENUM" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'edlptrpar': unexpected token in variant block at '('"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM edlptrbsl VARIANT hold FIELD v n \ FIELD p ptr edlmodel" TEST-EVAL:RC 7107 T=
s" habu: bad enum declaration 'edlptrbsl': unexpected token in variant block at '\'"
DECL-DIAG:HAS? TTRUE
\ and trailing text after the offending payload cannot suppress the reject,
\ because the payload is resolved before anything following it is read.
DECL-DIAG:PROSE
s" ENUM edlptrtail VARIANT hold FIELD p ptr edlmodel ( note ) ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edlptrtail': payload type is a pointer to a linear value and cannot own it at 'edlmodel'"
DECL-DIAG:HAS? TTRUE

\ A payload names its own family only inside a `ptr` pointee, and that pointer is
\ a pointer to a linear value when the enum holds one: judged at ;ENUM.
DECL-DIAG:PROSE
s" ENUM edsval VARIANT hold FIELD x edsval ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edsval': payload type cannot recursively name its owner at 'edsval'"
DECL-DIAG:HAS? TTRUE
s" ENUM edsptr VARIANT hold FIELD next ptr edsptr ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM edslin VARIANT hold FIELD next ptr edslin ;VARIANT VARIANT own FIELD t EDLIN:tok ;VARIANT ;ENUM" TEST-EVAL:RC 7109 T=
s" habu: bad enum declaration 'edslin': payload type is a pointer to a linear value and cannot own it at 'edslin'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

T-REPORT

