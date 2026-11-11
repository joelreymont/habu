\ arena.f — the ARENA definer.
\
\ `ARENA NAME t`, inside a package, declares two words that make zeroed records
\ of `t` in an arena the caller holds (src/lib/arena.f): NAME-NEW
\ ( ptr ARENA:arena -- ptr t ) and NAME-NEW-DISPOSE
\ ( ptr ARENA:arena [ ptr t -- ] -- ptr t ), whose action runs once when the
\ arena is reset or released. The width is the declared type's, fixed per
\ declaration. NAME-NEW types ARENA:ALLOC's bytes through the cast NAME-NEW-PTR;
\ NAME-NEW-DISPOSE also hands its action to ARENA:ALLOC-DISPOSE through the cast
\ NAME-NEW-ACT. Both casts mint, which only a private section admits
\ (E-CAST-MINT), so the definer makes them in the declaring package's private
\ section and refuses a declaration outside any package (STG-NO-PACKAGE) or by a
\ qualified name, whose casts would land in another package's public wordlist
\ (STG-QUALIFIED-NAME).

package ARENA-DECL

private

\ Appends `NAME<suffix>`.
: NAMED, ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   name nameu LBUF-APP  sfx sfxu LBUF-APP ;


\ The casts' text and the words' text, built before either is evaluated so a
\ refusal while building defines nothing.
: SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n width:n :}
   LBUF-CLEAR
   s" CAST: " LBUF-APP  name nameu s" -NEW-PTR" NAMED,
   s"  ( ptr u8 -- ptr " LBUF-APP  type typeu LBUF-APP  s"  )" LBUF-APP  10 LBUF-C,
   s" CAST: " LBUF-APP  name nameu s" -NEW-ACT" NAMED,
   s"  ( [ ptr " LBUF-APP  type typeu LBUF-APP  s"  -- ] -- [ ptr u8 -- ] )" LBUF-APP
   LBUF-GEN-U @ {: wat:n :}
   s" : " LBUF-APP  name nameu s" -NEW" NAMED,
   s"  ( ptr ARENA:arena -- ptr " LBUF-APP  type typeu LBUF-APP  s"  ) " LBUF-APP
   width LBUF-DEC,  s"  ARENA:ALLOC drop " LBUF-APP  name nameu s" -NEW-PTR ;" NAMED,
   10 LBUF-C,
   s" : " LBUF-APP  name nameu s" -NEW-DISPOSE" NAMED,
   s"  ( ptr ARENA:arena [ ptr " LBUF-APP  type typeu LBUF-APP
   s"  -- ] -- ptr " LBUF-APP  type typeu LBUF-APP  s"  ) " LBUF-APP
   name nameu s" -NEW-ACT " NAMED,  width LBUF-DEC,
   s"  swap ARENA:ALLOC-DISPOSE drop " LBUF-APP  name nameu s" -NEW-PTR ;" NAMED,
   LBUF-GEN wat
   LBUF-GEN wat +  LBUF-GEN-U @ wat - ;


: EVAL ( ptr u8 n -- ) LBUF-GEN 0 LBUF-EVAL! drop ;


: SECTION ( n -- )
   CHECKER-PACKAGE-PUBLIC = if s" public" else s" private" then
   TYPE-DECL:TDECL-EVAL-XT ;
: OTHER ( n -- n )
   CHECKER-PACKAGE-PUBLIC = if CHECKER-PACKAGE-PRIVATE else CHECKER-PACKAGE-PUBLIC then ;


\ Runs Q in the section a declaration in MODE is not in, and returns to MODE's
\ section on a throw too.
: IN-OTHER ( R [ R -- R ] n -- R )
   {: q mode:n :}
   mode OTHER SECTION
   q catch {: rc:n :}
   mode SECTION
   rc 0 <> if rc throw then ;


\ Evaluates the casts in the private section.
: CASTS-EVAL ( ptr u8 n n -- )
   {: src:ptr srcu:n mode:n :}
   mode CHECKER-PACKAGE-PUBLIC <> if src srcu EVAL exit then
   [: src srcu EVAL ;] mode IN-OTHER ;


\ Whether none of NAME's four words is taken in the current section; a taken
\ one throws E-DUP-DEFINITION.
: FREE? ( ptr u8 n -- bool )
   {: name:ptr nameu:n :}
   name nameu s" -NEW" LBUF-SUFFIX-OK? dup 0= if exit then drop
   name nameu s" -NEW-DISPOSE" LBUF-SUFFIX-OK? dup 0= if exit then drop
   name nameu s" -NEW-PTR" LBUF-SUFFIX-OK? dup 0= if exit then drop
   name nameu s" -NEW-ACT" LBUF-SUFFIX-OK? ;

public

\ The four words must be free in both sections: the casts land in the private
\ one, and the package's bare calls resolve private-first.
: RUN ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" ARENA" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if exit then
   name nameu CHECKER-QUALIFIED? if
      name nameu name nameu STG-QUALIFIED-NAME CHECKER-STORAGE-REFUSE exit
   then
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   mode CHECKER-PACKAGE-NONE = if
      name nameu name nameu STG-NO-PACKAGE CHECKER-STORAGE-REFUSE exit
   then
   name nameu FREE? {: own:bool :}
   own 0= if exit then
   own [: drop name nameu FREE? ;] mode IN-OTHER 0= if exit then
   type typeu CHECKER-ARENA-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then {: width:n :}
   name nameu type typeu width SOURCE {: casts:ptr castsu:n words:ptr wordsu:n :}
   casts castsu mode CASTS-EVAL
   words wordsu EVAL ;

;package

: ARENA ( -- ) ARENA-DECL:RUN ;

PRIM: ARENA PRIM;
