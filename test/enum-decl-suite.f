\ enum-decl-suite.f - behavior and rollback suite for the ENUM declaration front
\ end, driven through the global `ENUM`. It asserts reject codes, rendered
\ diagnostics, field-row counts and the generated constructors.

require src/habu/verify-source.f
require test/replay-scope.f
require test/decl-diag-capture.f
require src/lib/test.f
require src/lib/test/rejection-text.f
require src/lib/adt/option.f

DECL-DIAG:PROSE

T-RESET

variable PFB

\ A declared family name is reserved in the variant-name rejects below.
s" ENUM color red green blue ;ENUM" evaluate-closed

TYPE-FIELD:COUNT PFB !
DECL-DIAG:PROSE
s" ENUM dupf VARIANT alpha FIELD z n FIELD z n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dupf`\n" DECL-DIAG:HAS? TTRUE
s" z" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the variant its own name\n" DECL-DIAG:HAS? TTRUE
TYPE-FIELD:COUNT PFB @ T=

DECL-DIAG:PROSE
s" ENUM dupv red red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dupv`\n" DECL-DIAG:HAS? TTRUE
s" red" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM emix red VARIANT alpha ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ mixed modes (block token in compact)
s\" error[E-BAD-DECLARATION]: bad enum declaration `emix`\n" DECL-DIAG:HAS? TTRUE
s" VARIANT" s" block keyword in a compact enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM eac<a,b> red ;ENUM" TEST-EVAL:RC 70 T=                \ parameterized compact form
s\" error[E-BAD-DECLARATION]: bad enum declaration `eac`\n" DECL-DIAG:HAS? TTRUE
s" red" s" unexpected token in enum declaration" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write only `POLICY`, `DERIVE` and `VARIANT` clauses before `;ENUM`, with no comments\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ech red POLICY packed-tag ;ENUM" TEST-EVAL:RC 70 T=  \ header clause after a compact variant
s\" error[E-BAD-DECLARATION]: bad enum declaration `ech`\n" DECL-DIAG:HAS? TTRUE
s" POLICY" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ecp red FIELD y n ;ENUM" TEST-EVAL:RC 70 T=        \ positional/named payload in compact
s\" error[E-BAD-DECLARATION]: bad enum declaration `ecp`\n" DECL-DIAG:HAS? TTRUE
s" FIELD" s" block keyword in a compact enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM emv VARIANT alpha FIELD x n ;ENUM" TEST-EVAL:RC 70 T=  \ missing ;VARIANT
s\" error[E-BAD-DECLARATION]: bad enum declaration `emv`\n" DECL-DIAG:HAS? TTRUE
s" ;ENUM" s" unexpected token in variant block" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM eme red green" TEST-EVAL:RC 70 T=                  \ missing ;ENUM
s\" error[E-BAD-DECLARATION]: bad enum declaration `eme`\n" DECL-DIAG:HAS? TTRUE
s" eme" s" missing ;ENUM" DECL-DIAG:CAUSE? TTRUE
s\"   = help: end the declaration with `;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ear<a,> VARIANT alpha ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ trailing comma in parameter list
s\" error[E-BAD-DECLARATION]: bad enum declaration `ear<a,>`\n" DECL-DIAG:HAS? TTRUE
s" ear<a,>" s" invalid type parameter header" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list distinct parameter letters in angle brackets, as in `box<a>` or `result<a,b>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM eempty ;ENUM" TEST-EVAL:RC 70 T=                   \ an enum needs a variant
s\" error[E-BAD-DECLARATION]: bad enum declaration `eempty`\n" DECL-DIAG:HAS? TTRUE
s" eempty" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM enum red ;ENUM" TEST-EVAL:RC 70 T=                 \ reserved opener keyword as a name
s\" error[E-BAD-DECLARATION]: bad enum declaration `enum`\n" DECL-DIAG:HAS? TTRUE
s" enum" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM Bad red ;ENUM" TEST-EVAL:RC 70 T=                  \ upper-case family name (case)
s\" error[E-BAD-DECLARATION]: bad enum declaration `Bad`\n" DECL-DIAG:HAS? TTRUE
s" Bad" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name as a lowercase tail with no package qualifier, as in `color`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM n red ;ENUM" TEST-EVAL:RC 70 T=                    \ single-letter family name
s\" error[E-BAD-DECLARATION]: bad enum declaration `n`\n" DECL-DIAG:HAS? TTRUE
s" n" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
\ An effect reads the token `ptr` ahead of any family, never as this one, so the
\ tail is reserved ahead of the global `ptr` family's duplicate check (7102).
DECL-DIAG:PROSE
s" ENUM ptr red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `ptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM erf VARIANT alpha FIELD make n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ reserved field name
s\" error[E-BAD-DECLARATION]: bad enum declaration `erf`\n" DECL-DIAG:HAS? TTRUE
s" make" s" reserved field name" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM ecf VARIANT alpha FIELD Zed n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ upper-case field name (case)
s\" error[E-BAD-DECLARATION]: bad enum declaration `ecf`\n" DECL-DIAG:HAS? TTRUE
s" Zed" s" name must be a lowercase tail" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM ebs VARIANT alpha FIELD x nope ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ unresolved field type
s\" error[E-BAD-DECLARATION]: bad enum declaration `ebs`\n" DECL-DIAG:HAS? TTRUE
s" nope" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM euc VARIANT alpha FIELD x Q ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ upper-case single-letter type
s\" error[E-BAD-DECLARATION]: bad enum declaration `euc`\n" DECL-DIAG:HAS? TTRUE
s" Q" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: use `n`, `f` or `r`, or a type parameter the header declares, as in `ENUM box<a>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM epa VARIANT alpha FIELD x a ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ parameter outside declared arity
s\" error[E-BAD-DECLARATION]: bad enum declaration `epa`\n" DECL-DIAG:HAS? TTRUE
s" a" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: use `n`, `f` or `r`, or a type parameter the header declares, as in `ENUM box<a>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM epg<a,b,c,d,e,g> VARIANT alpha FIELD x h ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=  \ parameter 6 is outside arity 6
s\" error[E-BAD-DECLARATION]: bad enum declaration `epg`\n" DECL-DIAG:HAS? TTRUE
s" h" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: use `n`, `f` or `r`, or a type parameter the header declares, as in `ENUM box<a>`\n" DECL-DIAG:HAS? TTRUE

s" ENUM twice red ;ENUM" evaluate-closed
DECL-DIAG:PROSE
s" ENUM twice red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `twice`\n" DECL-DIAG:HAS? TTRUE
s" twice" s" duplicate family" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the enum: this package already declares a family by that name\n" DECL-DIAG:HAS? TTRUE

\ Reserved variant names reject before publication; a family owned only by another
\ package does not reserve its tails, while a global or active-package family does.
package other-enum-name-test
s" ENUM foreign-variant member ;ENUM" evaluate-closed
;package

package enum-name-test

VALUE-RECORD enum-record payload n END-VALUE-RECORD
s" ENUM local-variant member ;ENUM" evaluate-closed

public

\ REJECT-CODE asserts a declaration's reject code, public so section 24
\ (control-word names) reuses it.
: REJECT-CODE ( ^u8 n n -- ) {: a:^ u:n want:n :}
   a u TEST-EVAL:RC want T= ;

DECL-DIAG:PROSE
s" ENUM compact-dup-policy POLICY packed-tag POLICY stack-cell-tag alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-dup-policy`\n" DECL-DIAG:HAS? TTRUE
s" stack-cell-tag" s" a second POLICY clause in one declaration" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-dup-feature DERIVE eq eq alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-dup-feature`\n" DECL-DIAG:HAS? TTRUE
s" eq" s" the same DERIVE feature twice in one declaration" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-dup-split DERIVE eq DERIVE eq alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-dup-split`\n" DECL-DIAG:HAS? TTRUE
s" eq" s" the same DERIVE feature twice in one declaration" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-missing-policy POLICY ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-missing-policy`\n" DECL-DIAG:HAS? TTRUE
s" ;ENUM" s" unknown layout policy" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-missing-derive DERIVE ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-missing-derive`\n" DECL-DIAG:HAS? TTRUE
s" ;ENUM" s" unknown derive feature" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name `eq`, `hash` or `tag` after `DERIVE`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-bad-policy POLICY unknown alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-bad-policy`\n" DECL-DIAG:HAS? TTRUE
s" unknown" s" unknown layout policy" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-bad-feature DERIVE unknown alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-bad-feature`\n" DECL-DIAG:HAS? TTRUE
s" unknown" s" unknown derive feature" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name `eq`, `hash` or `tag` after `DERIVE`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-order DERIVE order alpha ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-order`\n" DECL-DIAG:HAS? TTRUE
s" order" s" derive feature not yet supported" DECL-DIAG:CAUSE? TTRUE
s\"   = help: derive `eq`, `hash` or `tag` instead: `order` is not implemented\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-no-variant POLICY packed-tag ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-no-variant`\n" DECL-DIAG:HAS? TTRUE
s" compact-no-variant" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-late-policy alpha POLICY packed-tag ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-late-policy`\n" DECL-DIAG:HAS? TTRUE
s" POLICY" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-late-derive alpha DERIVE eq ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-late-derive`\n" DECL-DIAG:HAS? TTRUE
s" DERIVE" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-policy-name alpha policy ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-policy-name`\n" DECL-DIAG:HAS? TTRUE
s" policy" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM compact-derive-name alpha derive ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `compact-derive-name`\n" DECL-DIAG:HAS? TTRUE
s" derive" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE
s" ENUM full-niche<a> POLICY niche-null VARIANT none ;VARIANT VARIANT some FIELD value ^a ;VARIANT ;ENUM" 0 REJECT-CODE

private

DECL-DIAG:PROSE
s" ENUM reject-compact ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" reject-compact" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact n ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" n" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact q ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" q" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact if ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" if" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact alpha variant ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" variant" s" block keyword in a compact enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact bool ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" bool" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact enum-record ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" enum-record" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact space-x ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" space-x" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact color ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" color" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-compact local-variant ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-compact`\n" DECL-DIAG:HAS? TTRUE
s" local-variant" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM reject-full VARIANT" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" " s" missing variant name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write a name after `VARIANT`, as in `VARIANT quit ;VARIANT`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT n ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" n" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT q ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" q" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT if ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" if" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT variant ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" variant" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT bool ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" bool" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT enum-record ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" enum-record" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT space-x ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" space-x" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT color ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" color" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM reject-full VARIANT local-variant ;VARIANT ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `reject-full`\n" DECL-DIAG:HAS? TTRUE
s" local-variant" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE

s" ENUM allowed-compact foreign-variant ;ENUM" evaluate-closed
s" ENUM allowed-full VARIANT foreign-variant ;VARIANT ;ENUM" evaluate-closed
DECL-DIAG:PROSE
s" ENUM duplicate-order ready ready ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `duplicate-order`\n" DECL-DIAG:HAS? TTRUE
s" ready" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE
\ A package may shadow a global family, but not `ptr`: an effect in this
\ package would still read the token `ptr` ahead of any family.
DECL-DIAG:PROSE
s" ENUM ptr ready ;ENUM" 70 REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `ptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE

;package

\ 20. A public ENUM family with variants owns one sealed checked
\     FAMILY:VARIANT constructor per variant, generated inside the declaration's
\     own transaction.
package enum-ctor-test
public

variable CT-DICT   variable CT-CP

: DICT-RECS ( -- n ) ndict@ ;
: DICT-CODE ( -- n ) cp@ ;

: DICT-MARK ( -- ) DICT-RECS CT-DICT !  DICT-CODE CT-CP ! ;
: DICT-SAME ( -- ) DICT-RECS CT-DICT @ T=  DICT-CODE CT-CP @ T= ;
: DICT-MOVED ( -- ) DICT-RECS CT-DICT @ > TTRUE ;

\ A generation failure must leave the native dictionary byte-identical: what
\ the generator had defined is truncated with the rest of the declaration.
: CTOR-REJECT ( ^u8 n n -- ) {: a:^ u:n want:n :}
   DICT-MARK
   a u TEST-EVAL:RC want T=
   DICT-SAME ;

private
;package

\ 20a. Generation moves the native dictionary, which is what the dictionary
\      participant's savepoint has to be able to undo.
enum-ctor-test:DICT-MARK
s" ENUM msgctor VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" evaluate-closed
enum-ctor-test:DICT-MOVED

s" C1 ( -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! -1 T=
s" C2 ( n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! -1 T=

s" C3 ( n -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! 0 T=
s" C4 ( n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C5 ( n n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C6 ( n n -- n ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=

\ 20b. DERIVE rides the same commit.
s" ENUM dctor DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two ;VARIANT ;ENUM" evaluate-closed
s" D1 ( n -- dctor ) DCTOR:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" D2 ( -- dctor ) DCTOR:TWO" CHECK-QUIET-CANDIDATE! -1 T=
s" D3 ( dctor -- n ) DCTOR:TAG" CHECK-QUIET-CANDIDATE! -1 T=
s" D4 ( dctor dctor -- f ) DCTOR:EQ" CHECK-QUIET-CANDIDATE! -1 T=

\ 20c. DERIVE tag adds TAG> ( n -- fam ), TAG's inverse: each tag decodes to
\      its own variant, and any other number throws E-ENUM-TAG.
ENUM tagconv DERIVE eq tag low mid high ;ENUM
: TAGCONV-ARM ( n -- n )
   TAGCONV:TAG>
   MATCH tagconv low of 10 endof mid of 20 endof high of 30 endof ;MATCH ;
: TAGCONV-THROWS ( n -- ) {: k:n :}
   [: k TAGCONV-ARM drop ;] TYPE-DECL:E-ENUM-TAG TTHROWSQ ;
0 TAGCONV-ARM 10 T=  1 TAGCONV-ARM 20 T=  2 TAGCONV-ARM 30 T=
-1 TAGCONV-THROWS  3 TAGCONV-THROWS  $7FFFFFFFFFFFFFFF TAGCONV-THROWS

\      TAG> builds a value from its tag alone, so a payload variant rejects.
DECL-DIAG:PROSE
s" ENUM tagpay DERIVE tag VARIANT one FIELD a n ;VARIANT VARIANT two ;VARIANT ;ENUM"
   TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `tagpay`\n" DECL-DIAG:HAS? TTRUE
s" tagpay" s" derive tag requires variants without payload" DECL-DIAG:CAUSE? TTRUE

\      A private ENUM or SUMTYPE generates its variants and derived words into
\      its package's private wordlist, protected there as a public family's are.
package enum-tag-test
private
ENUM tagpriv DERIVE eq tag one two ;ENUM
s" ETPI ( n -- ) TAGPRIV-TAG> drop" CHECK-QUIET-CANDIDATE! -1 T=
DECL-DIAG:PROSE
s" undefine TAGPRIV-ONE" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `TAGPRIV-ONE`" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" undefine TAGPRIV-TAG>" TEST-EVAL:RC 70 T=
s" error[E-CTOR-PROTECTED]: `TAGPRIV-TAG>`" DECL-DIAG:HAS? TTRUE
SUMTYPE sumpriv DERIVE eq
  VARIANT bare ;VARIANT
  VARIANT held n ;VARIANT
;SUMTYPE
public
: TPRT ( n -- n ) TAGPRIV-TAG> TAGPRIV-TAG ;
: TPEQT ( -- bool ) TAGPRIV-ONE TAGPRIV-ONE TAGPRIV-EQ ;
: TPEQF ( -- bool ) TAGPRIV-ONE TAGPRIV-TWO TAGPRIV-EQ ;
: TPSEQT ( -- bool ) 5 SUMPRIV-HELD 5 SUMPRIV-HELD SUMPRIV-EQ ;
: TPSEQF ( -- bool ) 5 SUMPRIV-HELD SUMPRIV-BARE SUMPRIV-EQ ;
;package
1 enum-tag-test:TPRT 1 T=
enum-tag-test:TPEQT TTRUE
enum-tag-test:TPEQF TFALSE
enum-tag-test:TPSEQT TTRUE
enum-tag-test:TPSEQF TFALSE

\      Another package reaches the derived TAG> under no spelling.
package enum-tag-other
private
s" ETPO ( n -- ) TAGPRIV-TAG> drop" CHECK-QUIET-CANDIDATE! 0 T=
s" ETPQ ( n -- ) enum-tag-test:TAGPRIV-TAG> drop" CHECK-QUIET-CANDIDATE! 0 T=
public
;package

s" ENUM lgpar red green blue ;ENUM" evaluate-closed
s" ENUM fepar red green blue ;ENUM" evaluate-closed
s" P1 ( -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P2 ( -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P3 ( -- lgpar ) LGPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P4 ( -- fepar ) FEPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P5 ( n -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=
s" P6 ( n -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=

\ 20d. A STRUCTURE family is not owned here: its definer generates its own
\      make/unmake.
s" STRUCTURE prodctor FIELD a n FIELD b n ;STRUCTURE" evaluate-closed
s" G1 ( n n -- prodctor ) PRODCTOR:MAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ A later constructor collides after the first has entered the generation plan.
: ROLLCOLL:TAKEN ( -- n ) 37 ;
DECL-DIAG:PROSE
s" ENUM rollcoll fresh taken ;ENUM"
   70 enum-ctor-test:CTOR-REJECT
s\" error[E-BAD-DECLARATION]: bad enum declaration `rollcoll`\n" DECL-DIAG:HAS? TTRUE
s" rollcoll" s" generated declaration already defined" DECL-DIAG:CAUSE? TTRUE
s" RC-FRESH ( -- n ) ROLLCOLL:FRESH" CHECK-QUIET-CANDIDATE! 0 T=
ROLLCOLL:TAKEN 37 T=
\ Rollback also removes protection of the generated package and leaves the
\ namespace available for a corrected declaration using the same family name.
undefine ROLLCOLL:TAKEN
s" ENUM rollcoll fresh taken ;ENUM" evaluate-closed
s" RC-VALID ( -- rollcoll ) ROLLCOLL:FRESH" CHECK-QUIET-CANDIDATE! -1 T=

\ A STRUCTURE collision on UNMAKE also rolls back the MAKE planned before it.
: ROLLP:UNMAKE ( -- n ) 41 ;
DECL-DIAG:PROSE
s" STRUCTURE rollp FIELD value n ;STRUCTURE"
   70 enum-ctor-test:CTOR-REJECT
s\" error[E-BAD-DECLARATION]: bad structure declaration `rollp`\n" DECL-DIAG:HAS? TTRUE
s" rollp" s" generated declaration already defined" DECL-DIAG:CAUSE? TTRUE
s" RP-MAKE ( n -- n ) ROLLP:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
ROLLP:UNMAKE 41 T=
undefine ROLLP:UNMAKE
s" STRUCTURE rollp FIELD value n ;STRUCTURE" evaluate-closed
s" RP-VALID ( n -- rollp ) ROLLP:MAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ 21. POLICY packed-tag with DERIVE eq declares.
s" ENUM lay-keep POLICY packed-tag DERIVE eq VARIANT one FIELD p n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

\ 22. Each case asserts whole rendered lines and the token the caret marks,
\     so text the declaration supplies cannot satisfy it elsewhere in the buffer.

DECL-DIAG:PROSE
s" ENUM dgempty ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgempty`\n" DECL-DIAG:HAS? TTRUE
s" dgempty" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM Dgcase red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `Dgcase`\n" DECL-DIAG:HAS? TTRUE
s" Dgcase" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name as a lowercase tail with no package qualifier, as in `color`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM n red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `n`\n" DECL-DIAG:HAS? TTRUE
s" n" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgdup red red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgdup`\n" DECL-DIAG:HAS? TTRUE
s" red" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgar<a,a> VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgar<a,a>`\n" DECL-DIAG:HAS? TTRUE
s" dgar<a,a>" s" invalid type parameter header" DECL-DIAG:CAUSE? TTRUE
s\"   = help: list distinct parameter letters in angle brackets, as in `box<a>` or `result<a,b>`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgpay VARIANT vv FIELD payfld nosuchtype ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgpay`\n" DECL-DIAG:HAS? TTRUE
s" nosuchtype" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`\n" DECL-DIAG:HAS? TTRUE

\ A pointer payload is spelled ^T: `ptr` is an unknown payload type.
DECL-DIAG:PROSE
s" ENUM dgptr VARIANT vv FIELD payfld ptr u8 ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgptr`\n" DECL-DIAG:HAS? TTRUE
s" ptr" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^list`\n" DECL-DIAG:HAS? TTRUE

\ A bare `^` is a pointer with no pointee, never a missing type.
DECL-DIAG:PROSE
s" ENUM dgbare VARIANT vv FIELD payfld ^ ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgbare`\n" DECL-DIAG:HAS? TTRUE
s" ^" s" pointer without a pointee" DECL-DIAG:CAUSE? TTRUE
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^list`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgpol POLICY nosuch red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgpol`\n" DECL-DIAG:HAS? TTRUE
s" nosuch" s" unknown layout policy" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM dgdrv DERIVE nosuch red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgdrv`\n" DECL-DIAG:HAS? TTRUE
s" nosuch" s" unknown derive feature" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name `eq`, `hash` or `tag` after `DERIVE`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgord DERIVE order red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgord`\n" DECL-DIAG:HAS? TTRUE
s" order" s" derive feature not yet supported" DECL-DIAG:CAUSE? TTRUE
s\"   = help: derive `eq`, `hash` or `tag` instead: `order` is not implemented\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgfn VARIANT vv FIELD make n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgfn`\n" DECL-DIAG:HAS? TTRUE
s" make" s" reserved field name" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM dgkw alpha variant ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgkw`\n" DECL-DIAG:HAS? TTRUE
s" variant" s" block keyword in a compact enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgnoend red" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgnoend`\n" DECL-DIAG:HAS? TTRUE
s" dgnoend" s" missing ;ENUM" DECL-DIAG:CAUSE? TTRUE
s\"   = help: end the declaration with `;ENUM`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dglegacy red red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dglegacy`\n" DECL-DIAG:HAS? TTRUE
s" red" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM dgunified red red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgunified`\n" DECL-DIAG:HAS? TTRUE
s" red" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dglegacy2 ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dglegacy2`\n" DECL-DIAG:HAS? TTRUE
s" dglegacy2" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM dgunified2 ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgunified2`\n" DECL-DIAG:HAS? TTRUE
s" dgunified2" s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgnest VARIANT nestvar FIELD nestfld nosuchtype ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgnest`\n" DECL-DIAG:HAS? TTRUE
s" nosuchtype" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`\n" DECL-DIAG:HAS? TTRUE
s" declaration `nestvar`" DECL-DIAG:HAS? TFALSE
s" declaration `nestfld`" DECL-DIAG:HAS? TFALSE

\ 22d. The front end names the family from the token it just read,
\      unconditionally and before validating it, so the empty token replaces the
\      previous family.
DECL-DIAG:PROSE
s" ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration\n" DECL-DIAG:HAS? TTRUE
s" " s" missing name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name after `ENUM`, as in `ENUM color red green ;ENUM`\n" DECL-DIAG:HAS? TTRUE
s" dgnest" DECL-DIAG:HAS? TFALSE

\ 22e. Hostile declarations. Text supplied BY the declaration cannot forge or
\      displace the packet: a variant spelled like a fragment of the message is
\      reported as the token it is, the reason still comes from the reject, and
\      exactly one record is emitted (no second `error[` line).
DECL-DIAG:PROSE
s" ENUM dgforge duplicate duplicate ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgforge`\n" DECL-DIAG:HAS? TTRUE
s" duplicate" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE
s\" \nerror[" DECL-DIAG:HAS? TFALSE

DECL-DIAG:PROSE
s" ENUM dgat at at ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgat`\n" DECL-DIAG:HAS? TTRUE
s" at" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE
s\" \nerror[" DECL-DIAG:HAS? TFALSE

\      A family name spelled like one of the JSON packet's own keys keeps key and
\      value distinguishable, and a token carrying a double quote is escaped
\      rather than closing the JSON string early.
DECL-DIAG:JSON
s" ENUM reason red red ;ENUM" TEST-EVAL:RC 70 T=
s\" \"message\":\"bad enum declaration `reason`\"" DECL-DIAG:HAS? TTRUE
s\" \"token\":\"red\"" DECL-DIAG:HAS? TTRUE
s\" \"label\":\"duplicate variant\"" DECL-DIAG:HAS? TTRUE
s\" \"help\":\"give each variant of the enum its own name\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:JSON
s\" ENUM dgquote aq\"b red ;ENUM" TEST-EVAL:RC 70 T=
s\" \"message\":\"bad enum declaration `dgquote`\"" DECL-DIAG:HAS? TTRUE
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? TTRUE
s\" \"label\":\"name must be a lowercase tail\"" DECL-DIAG:HAS? TTRUE
s\" \"help\":\"write the name as a lowercase tail with no package qualifier, as in `point`\"" DECL-DIAG:HAS? TTRUE
s\" \"code\":\"E-BAD-DECLARATION\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgok1 red green ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM dgok2 VARIANT okvar FIELD okfld n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM dgok3 POLICY packed-tag redd greend ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM dgok4 DERIVE eq rede greene ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE

DECL-DIAG:PROSE
s" ENUM dgroll red red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgroll`\n" DECL-DIAG:HAS? TTRUE
s" red" s" duplicate variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:OFF

\ 22g. An arming names a fault expected AT a token, so once the front end has
\      moved on it can no longer explain anything.
s" enum" DECL-REJECT:OPEN
s" armed-for-7102" s" fix-for-7102" 7102 DECL-REJECT:EXPECT
7102 DECL-REJECT:REASON$ s" armed-for-7102" DECL-DIAG:SAME? TTRUE
7107 DECL-REJECT:REASON$ s" malformed declaration" DECL-DIAG:SAME? TTRUE
s" moved-on" DECL-REJECT:TOKEN!
7102 DECL-REJECT:REASON$ s" duplicate name in this package" DECL-DIAG:SAME? TTRUE

s" enum" DECL-REJECT:OPEN
s" leaked-family" DECL-REJECT:FAMILY!
s" leaked-token" DECL-REJECT:TOKEN!
s" leaked-reason" s" leaked-fix" 7102 DECL-REJECT:EXPECT
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
s" ENUM qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop ;ENUM"
TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwe...`\n" DECL-DIAG:HAS? TTRUE
s" qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop"
   s" empty enum" DECL-DIAG:CAUSE? TTRUE
s\"   = help: declare at least one variant before `;ENUM`\n" DECL-DIAG:HAS? TTRUE

\ 22i. The reasons no case above reaches: the armed duplicate-family text, the
\      declaration-event clause duplicates, the variant-block and header-order
\      grammar faults, and the three codes whose reason can only come from the
\      table because the owner that raises them is past the front end's last
\      token.
DECL-DIAG:PROSE
s" ENUM dgtwice red ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM dgtwice blue ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgtwice`\n" DECL-DIAG:HAS? TTRUE
s" dgtwice" s" duplicate family" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the enum: this package already declares a family by that name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgdupfld VARIANT vv FIELD zz n FIELD zz n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgdupfld`\n" DECL-DIAG:HAS? TTRUE
s" zz" s" duplicate field name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each field of the variant its own name\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dg2pol POLICY packed-tag POLICY packed-tag red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dg2pol`\n" DECL-DIAG:HAS? TTRUE
s" packed-tag" s" a second POLICY clause in one declaration" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM dg2drv DERIVE eq DERIVE eq red ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dg2drv`\n" DECL-DIAG:HAS? TTRUE
s" eq" s" the same DERIVE feature twice in one declaration" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM dghdr red POLICY packed-tag ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dghdr`\n" DECL-DIAG:HAS? TTRUE
s" POLICY" s" header clause after the first variant" DECL-DIAG:CAUSE? TTRUE
s\"   = help: move the `POLICY` and `DERIVE` clauses before the first variant\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgvb VARIANT vv stray ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgvb`\n" DECL-DIAG:HAS? TTRUE
s" stray" s" unexpected token in variant block" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgvn VARIANT" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgvn`\n" DECL-DIAG:HAS? TTRUE
s" " s" missing variant name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write a name after `VARIANT`, as in `VARIANT quit ;VARIANT`\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgdgd<a> DERIVE eq VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgdgd`\n" DECL-DIAG:HAS? TTRUE
s" eq" s" derive requires a concrete (arity 0) family" DECL-DIAG:CAUSE? TTRUE
s\"   = help: drop the type parameters or the `DERIVE` clause: only an arity-0 family derives\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" ENUM dgroleq DERIVE eq VARIANT vv FIELD p ^n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgroleq`\n" DECL-DIAG:HAS? TTRUE
s" ^n" s" pointer payloads have no derived equality" DECL-DIAG:CAUSE? TTRUE

\      7110 raised by the variant-name gate, and 7101 raised by the field record:
\      both are deeper owners, both answered from the table.
DECL-DIAG:PROSE
s" ENUM dgvrsv VARIANT n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgvrsv`\n" DECL-DIAG:HAS? TTRUE
s" n" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM dgfcase VARIANT vv FIELD Zed n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `dgfcase`\n" DECL-DIAG:HAS? TTRUE
s" Zed" s" name must be a lowercase tail" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:OFF

\ 22j. A reason explained in a text that throws is taken back with it, so
\      storage laid over its row is never walked as a reason.
package enum-reason-test
: ED-ONE ( -- ) s" reason one" s" fix one" 990001 DECL-REJECT:EXPLAIN ;
: ED-FILL ( n -- ) 0 ?do 777 , loop ;
s" ED-ONE 1 throw" TEST-EVAL:RC 1 T=
create ED-JUNK 16 ED-FILL
s" reason two" s" fix two" 990002 DECL-REJECT:EXPLAIN
990002 DECL-REJECT:REASON$ s" reason two" DECL-DIAG:SAME? TTRUE
;package

\ 23. TYPE-DECL:REPLAY-ENUM - registering an ENUM from tokens a tool already lexed.
package enum-replay-test
public

: RP-EV ( ^u8 n ^u8 n -- ) TYPE-DECL:REPLAY-ENUM TFALSE 2drop ;
: RP-TRY ( ^u8 n ^u8 n -- n )
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
s" rpbadv" s" VARIANT vv stray ;VARIANT ;ENUM" enum-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `rpbadv`\n" DECL-DIAG:HAS? TTRUE
s\" ^ unexpected token in variant block\n" DECL-DIAG:HAS? TTRUE
s\" = word: stray\n" DECL-DIAG:HAS? TTRUE
s\"   = help: write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body\n" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" rpbadname" s" red red ;ENUM" enum-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `rpbadname`\n" DECL-DIAG:HAS? TTRUE
s\" ^ duplicate variant\n" DECL-DIAG:HAS? TTRUE
s\" = word: red\n" DECL-DIAG:HAS? TTRUE
s\"   = help: give each variant of the enum its own name\n" DECL-DIAG:HAS? TTRUE

\ 23e. A buffer missing its terminator rejects through the front end's own
\      end-of-input gate, as a truncated live declaration does.
DECL-DIAG:PROSE
s" rpnoend" s" red green" enum-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `rpnoend`\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing ;ENUM\n" DECL-DIAG:HAS? TTRUE
s\" = word: rpnoend\n" DECL-DIAG:HAS? TTRUE
s\"   = help: end the declaration with `;ENUM`\n" DECL-DIAG:HAS? TTRUE

\ 23f. A zero-length name reaches the missing-name gate instead of silently
\      promoting the first body token to the family name.
DECL-DIAG:PROSE
s" " s" red green ;ENUM" enum-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing name\n" DECL-DIAG:HAS? TTRUE
s\"   = help: write the family name after `ENUM`, as in `ENUM color red green ;ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ 23g. A REJECTED replay leaves the stream closed, so the next LIVE declaration
\      reads the input source again rather than a spent buffer.
DECL-DIAG:PROSE
s" rpdangle" s" VARIANT" enum-replay-test:RP-TRY 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `rpdangle`\n" DECL-DIAG:HAS? TTRUE
s\" ^ missing variant name\n" DECL-DIAG:HAS? TTRUE
s\"   = help: write a name after `VARIANT`, as in `VARIANT quit ;VARIANT`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF
s" ENUM rpafterbad VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

\ 23h. Re-entry is refused with its own named code rather than retargeting a
\      stream that is already installed.
enum-replay-test:RP-FORCE-OPEN
s" rpbusy" s" red ;ENUM" enum-replay-test:RP-TRY 7177 T=
enum-replay-test:RP-FORCE-CLOSE
s" ENUM rpafterbusy VARIANT vv ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

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
s" ENUM if red ;ENUM"        70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `if`\n" DECL-DIAG:HAS? TTRUE
s" if" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM then red ;ENUM"      70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `then`\n" DECL-DIAG:HAS? TTRUE
s" then" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM else red ;ENUM"      70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `else`\n" DECL-DIAG:HAS? TTRUE
s" else" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM begin red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `begin`\n" DECL-DIAG:HAS? TTRUE
s" begin" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM until red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `until`\n" DECL-DIAG:HAS? TTRUE
s" until" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM again red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `again`\n" DECL-DIAG:HAS? TTRUE
s" again" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM while red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `while`\n" DECL-DIAG:HAS? TTRUE
s" while" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM repeat red ;ENUM"    70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `repeat`\n" DECL-DIAG:HAS? TTRUE
s" repeat" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM case red ;ENUM"      70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `case`\n" DECL-DIAG:HAS? TTRUE
s" case" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM of red ;ENUM"        70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `of`\n" DECL-DIAG:HAS? TTRUE
s" of" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM endof red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `endof`\n" DECL-DIAG:HAS? TTRUE
s" endof" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM endcase red ;ENUM"   70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `endcase`\n" DECL-DIAG:HAS? TTRUE
s" endcase" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM do red ;ENUM"        70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `do`\n" DECL-DIAG:HAS? TTRUE
s" do" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM loop red ;ENUM"      70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `loop`\n" DECL-DIAG:HAS? TTRUE
s" loop" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM leave red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `leave`\n" DECL-DIAG:HAS? TTRUE
s" leave" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM unloop red ;ENUM"    70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `unloop`\n" DECL-DIAG:HAS? TTRUE
s" unloop" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM exit red ;ENUM"      70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `exit`\n" DECL-DIAG:HAS? TTRUE
s" exit" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM recurse red ;ENUM"   70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `recurse`\n" DECL-DIAG:HAS? TTRUE
s" recurse" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM construct red ;ENUM" 70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `construct`\n" DECL-DIAG:HAS? TTRUE
s" construct" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM match red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `match`\n" DECL-DIAG:HAS? TTRUE
s" match" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ?do red ;ENUM"       70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `?do`\n" DECL-DIAG:HAS? TTRUE
s" ?do" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name as a lowercase tail with no package qualifier, as in `color`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM +loop red ;ENUM"     70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `+loop`\n" DECL-DIAG:HAS? TTRUE
s" +loop" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name as a lowercase tail with no package qualifier, as in `color`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ;match red ;ENUM"    70 enum-name-test:REJECT-CODE
s\" error[E-BAD-DECLARATION]: bad enum declaration `;match`\n" DECL-DIAG:HAS? TTRUE
s" ;match" s" name must be a lowercase family tail" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write the family name as a lowercase tail with no package qualifier, as in `color`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM if red green ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `if`\n" DECL-DIAG:HAS? TTRUE
s" if" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM if red green ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `if`\n" DECL-DIAG:HAS? TTRUE
s" if" s" reserved name" DECL-DIAG:CAUSE? TTRUE
s\"   = help: rename the family: keywords, control words, single letters, `ptr` and value records are reserved\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM cwvc do loop ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `cwvc`\n" DECL-DIAG:HAS? TTRUE
s" do" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM cwvf VARIANT loop ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `cwvf`\n" DECL-DIAG:HAS? TTRUE
s" loop" s" name is reserved or already taken" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

\ 24d. Field-name position: the field row's gate answers 7125, the
\      code and wording a reserved generated-operation name such as `make` gets.
DECL-DIAG:PROSE
s" ENUM cwfld VARIANT vv FIELD then n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `cwfld`\n" DECL-DIAG:HAS? TTRUE
s" then" s" reserved field name" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

s" ENUM iffy dolly matcher constructor elsewhere ;ENUM" evaluate-closed
s" ENUM doing VARIANT looping FIELD ifs n FIELD thence n ;VARIANT ;ENUM" evaluate-closed

;package

\ 25. A 700-variant declaration is accepted: the front end buffers
\     no body. The private package keeps constructor generation out of it.
package enum-long-test

$2000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U
variable LI

700 constant LONG-N

: SRC$ ( -- ^u8 n ) SRC-BUF SRC-U @ ;
: PUT-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF s" enum-decl-suite: long-enum buffer overflow" 1 die THEN
   c SRC-BUF SRC-U @ + c!  SRC-U @ 1 + SRC-U ! ;
: PUT$ ( ^u8 n -- ) {: a:^ u:n :}
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
   s" ENUM longenum " PUT$
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
s" ENUM edlfwd VARIANT hold FIELD m edllater ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlfwd`\n" DECL-DIAG:HAS? TTRUE
s" edllater" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

s" ENUM edlgen<a> VARIANT hold FIELD m a ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM edlgenuse VARIANT hold FIELD m edlgen ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlgenuse`\n" DECL-DIAG:HAS? TTRUE
s" edlgen" s" payload type is parametric and needs type arguments" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give the family its type arguments, as in `box<n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A variant payload may not name a pointer to a linear value: a pointer is a
\ non-owning boundary, so the linearity walk stops at the pointer node and the
\ enum would read non-linear, copying and dropping freely with a linear
\ resource behind the address.

s" ENUM edlowns VARIANT hold FIELD m edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

DECL-DIAG:PROSE
s" ENUM edlptr VARIANT hold FIELD p ^edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptr`\n" DECL-DIAG:HAS? TTRUE
s" edlmodel" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrcon VARIANT hold FIELD p ^EDLIN:tok ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrcon`\n" DECL-DIAG:HAS? TTRUE
s" EDLIN:tok" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" ENUM edlptr2 VARIANT hold FIELD p ^^edlmodel ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptr2`\n" DECL-DIAG:HAS? TTRUE
s" edlmodel" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrsum VARIANT hold FIELD p ^edl-load-result ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrsum`\n" DECL-DIAG:HAS? TTRUE
s" edl-load-result" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrlate VARIANT good FIELD c n ;VARIANT VARIANT bad FIELD p ^edlmodel ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrlate`\n" DECL-DIAG:HAS? TTRUE
s" edlmodel" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

DECL-DIAG:PROSE
s" ENUM edlptrapp VARIANT hold FIELD p option<^edlmodel> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrapp`\n" DECL-DIAG:HAS? TTRUE
s" edlmodel" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ A pointer to a family that owns nothing declares, so the
\ rejects above answer the POINTEE's linearity and not the word `^`.
s" ENUM edlptrok VARIANT hold FIELD p ^edlplain ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrok2 VARIANT hold FIELD p ^^edlplain ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrn VARIANT hold FIELD p ^n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

s" ENUM edlptrrole VARIANT hold FIELD ptr n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" ENUM edlptrrole2 VARIANT hold FIELD edlmodel n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=

\ A variant block has no comment syntax: `(` and `\` are
\ ordinary tokens, so the declaration is malformed and rejects 7107, not this
\ rule's 7109.
DECL-DIAG:PROSE
s" ENUM edlptrpar VARIANT hold FIELD v n ( FIELD p ^edlmodel ) ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrpar`\n" DECL-DIAG:HAS? TTRUE
s" (" s" unexpected token in variant block" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM edlptrbsl VARIANT hold FIELD v n \ FIELD p ^edlmodel" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrbsl`\n" DECL-DIAG:HAS? TTRUE
s\" \\" s" unexpected token in variant block" DECL-DIAG:CAUSE? TTRUE
s\"   = help: write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body\n" DECL-DIAG:HAS? TTRUE
\ and trailing text after the offending payload cannot suppress the reject,
\ because the payload is resolved before anything following it is read.
DECL-DIAG:PROSE
s" ENUM edlptrtail VARIANT hold FIELD p ^edlmodel ( note ) ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edlptrtail`\n" DECL-DIAG:HAS? TTRUE
s" edlmodel" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE

\ A payload names its own family only inside a `^` pointee, and that pointer is
\ a pointer to a linear value when the enum holds one: judged at ;ENUM.
DECL-DIAG:PROSE
s" ENUM edsval VARIANT hold FIELD x edsval ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edsval`\n" DECL-DIAG:HAS? TTRUE
s" edsval" s" payload type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
s\"   = help: name the enum being declared only beneath `^`, as in `FIELD next ^list`\n" DECL-DIAG:HAS? TTRUE
s" ENUM edsptr VARIANT hold FIELD next ^edsptr ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM edslin VARIANT hold FIELD next ^edslin ;VARIANT VARIANT own FIELD t EDLIN:tok ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edslin`\n" DECL-DIAG:HAS? TTRUE
s" edslin" s" payload type is a pointer to a linear value and cannot own it" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:OFF

\ 26. A payload may be a family application, read by STRUCTURE's node parser,
\     and is as wide as STRUCTURE counts it.
s" package edapp-pkg public STRUCTURE edapps FIELD v option<n> ;STRUCTURE  ENUM edappe VARIANT yy FIELD v option<n> ;VARIANT ;ENUM ;package"
   TEST-EVAL:RC 0 T=

ENUM edapcol red green ;ENUM
ENUM edapopt
   VARIANT num FIELD v option<n> ;VARIANT
   VARIANT col FIELD c option<edapcol> ;VARIANT
   VARIANT bare ;VARIANT
;ENUM
: EDAP-COL ( option<edapcol> -- n )
   MATCH option
      some of MATCH edapcol red of 1 endof green of 2 endof ;MATCH endof
      none of -1 endof
   ;MATCH ;
: EDAP-READ ( edapopt -- n )
   MATCH edapopt
      num of MATCH option some of endof none of -1 endof ;MATCH endof
      col of EDAP-COL endof
      bare of -2 endof
   ;MATCH ;
: EDAP-NUM ( n -- n ) OPTION:SOME EDAPOPT:NUM EDAP-READ ;
: EDAP-NUM-NONE ( -- n ) OPTION:NONE EDAPOPT:NUM EDAP-READ ;
: EDAP-GREEN ( -- n ) EDAPCOL:GREEN OPTION:SOME EDAPOPT:COL EDAP-READ ;
: EDAP-COL-NONE ( -- n ) OPTION:NONE EDAPOPT:COL EDAP-READ ;
: EDAP-BARE ( -- n ) EDAPOPT:BARE EDAP-READ ;
7 EDAP-NUM 7 T=
EDAP-NUM-NONE -1 T=
EDAP-GREEN 2 T=
EDAP-COL-NONE -1 T=
EDAP-BARE -2 T=

ENUM edapgen<a>
   VARIANT held FIELD v option<a> ;VARIANT
   VARIANT bare ;VARIANT
;ENUM
: EDAPGEN-READ ( edapgen<n> -- n )
   MATCH edapgen
      held of MATCH option some of endof none of -1 endof ;MATCH endof
      bare of -2 endof
   ;MATCH ;
: EDAPGEN-SOME ( n -- n ) OPTION:SOME EDAPGEN:HELD EDAPGEN-READ ;
: EDAPGEN-NONE ( -- n ) OPTION:NONE EDAPGEN:HELD EDAPGEN-READ ;
9 EDAPGEN-SOME 9 T=
EDAPGEN-NONE -1 T=

\     ENUM's rejections hold inside an application, in its own words.
DECL-DIAG:PROSE
s" ENUM edapunk VARIANT hold FIELD v nosuch<n> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edapunk`\n" DECL-DIAG:HAS? TTRUE
s" nosuch" s" unknown payload type" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM edapself VARIANT hold FIELD v option<edapself> ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edapself`\n" DECL-DIAG:HAS? TTRUE
s" edapself" s" payload type cannot recursively name its owner" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:PROSE
s" ENUM edapscope VARIANT hold FIELD x read-view<n,n,n> ;VARIANT VARIANT none ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edapscope`\n" DECL-DIAG:HAS? TTRUE
s" n" s" type argument must be a scope parameter" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pass a parameter the header lists, as in `ENUM box<p,q>` with `FIELD v read-view<p,q,u8>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM edapregion<p,q> VARIANT hold FIELD x mut-view<p,q,n,u8> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edapregion`\n" DECL-DIAG:HAS? TTRUE
s" n" s" type argument must be a region parameter" DECL-DIAG:CAUSE? TTRUE
s\"   = help: pass a parameter the header lists, as in `ENUM box<p,q,s>` with `FIELD v mut-view<p,q,s,u8>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM edkind<a,q> VARIANT vone FIELD x a ;VARIANT VARIANT wtwo FIELD y read-view<a,q,n> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edkind`\n" DECL-DIAG:HAS? TTRUE
s" a" s" `a` is a scope here, but field `x` uses it as a value type" DECL-DIAG:CAUSE? TTRUE
s\"   = help: give each kind its own header parameter, as in `ENUM box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM ednode<q> VARIANT one FIELD next ^ednode<n> FIELD w read-view<q,q,n> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `ednode`\n" DECL-DIAG:HAS? TTRUE
s" ednode" s" a pointer cannot reach `ednode`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
DECL-DIAG:TEXT$ s\" = evaluated: ENUM ednode<q> VARIANT one FIELD next ^ednode<n> FIELD w read-view<q,q,n> ;VARIANT ;ENUM\n                                                    ^^^^^^" REJECTION-TEXT:HAS? TTRUE
s\"   = help: keep the view out of the family a pointer reaches, as in `ENUM node<a>` with `FIELD next ^node<a>`, and `ENUM walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" ENUM edopt<p,q> VARIANT one FIELD x ^option<read-view<p,q,n>> ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
DECL-DIAG:TEXT$ s" error[" COUNT-SUB 1 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edopt`\n" DECL-DIAG:HAS? TTRUE
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region" DECL-DIAG:CAUSE? TTRUE
s\"   = help: hold the view in the field itself, as in `ENUM box<p,q>` with `FIELD x read-view<p,q,n>`\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\     An application is as wide as its arguments make it: option<edapw> takes
\     three cells, so big's payload takes four.
STRUCTURE edapw FIELD a n FIELD b n ;STRUCTURE
ENUM edapwide
   VARIANT big FIELD v option<edapw> FIELD t n ;VARIANT
   VARIANT small FIELD x n ;VARIANT
;ENUM
: EDAPW-SUM ( option<edapw> -- n )
   MATCH option some of EDAPW:UNMAKE + endof none of -1 endof ;MATCH ;
: EDAPWIDE-BIG ( option<edapw> n -- n ) {: t:n :} EDAPW-SUM t 1000 * + ;
: EDAPWIDE-READ ( edapwide -- n )
   MATCH edapwide
      big of EDAPWIDE-BIG endof
      small of 100 + endof
   ;MATCH ;
: EDAPWIDE-SOME ( n n n -- n ) {: a:n b:n t:n :}
   a b EDAPW:MAKE OPTION:SOME t EDAPWIDE:BIG EDAPWIDE-READ ;
: EDAPW-NONE ( -- option<edapw> ) OPTION:NONE ;
: EDAPWIDE-NONE ( n -- n ) {: t:n :} EDAPW-NONE t EDAPWIDE:BIG EDAPWIDE-READ ;
: EDAPWIDE-SMALL ( n -- n ) EDAPWIDE:SMALL EDAPWIDE-READ ;
3 4 7 EDAPWIDE-SOME 7007 T=
9 EDAPWIDE-NONE 8999 T=
5 EDAPWIDE-SMALL 105 T=
TFAM:TFAM-ACTIVE-PKG$ s" edapwide" TFAM:TFAM-SIG-RESOLVE TFAM:SR-FOUND T= TFAM:TFAM-SLOTS@ 4 T=

\     A payload type's tokens may hold blanks and stand on two lines, as a field
\     type's do; one left open at `;VARIANT` is unclosed.
STRUCTURE edappair<a,b> FIELD x a FIELD y b ;STRUCTURE
s" ENUM edapspaced VARIANT vq FIELD v edappair<n, n> ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s\" ENUM edapline VARIANT vq FIELD v edappair<n,\n   n> ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" ENUM edapopen VARIANT vq FIELD v edappair<n, ;VARIANT ;ENUM" TEST-EVAL:RC 70 T=
s\" error[E-BAD-DECLARATION]: bad enum declaration `edapopen`\n" DECL-DIAG:HAS? TTRUE
s" edappair<n," s" unclosed payload type application" DECL-DIAG:CAUSE? TTRUE
s\"   = help: close each `<` of the type with `>` before the next clause\n" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

T-REPORT

