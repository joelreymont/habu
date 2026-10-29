\ arena.f — the ARENA definer.
\
\ `ARENA NAME t` declares the family name, NAME folded to lower case as DEFTYPE
\ folds, whose one field is an arena head (DYNAMIC-STORAGE:arena-head), and two
\ words over a `ptr name`: NAME-NEW answers a fresh zeroed `ptr t` and
\ NAME-RELEASE frees every record. src/core/dynamic-storage.f holds the chunks.
\ The family derives no accessor, and the checker refuses to fetch or store a
\ head, or any layout holding one, whole (checker.f IN-PLACE?), and any cast
\ that reaches one outside DYNAMIC-STORAGE (checker.f CAST-REACHES-HEAD?), so
\ no typed access and no such cast copies or forges a head; address arithmetic
\ (`+`, `record-at` and other raw offsets) still reaches it, as it reaches any
\ record (docs/forth.md "Raw storage never holds an address"). The two words reach the
\ head through `field-project`, its window armed for each with the head field's
\ committed id; they spell it unqualified, and a sealed engine refuses that
\ name as a definition (STRUCTURE-MAKE:PROJECT-NAME$), so it is always the
\ engine's word. Each is evaluated through LBUF-EVAL with its own name pending,
\ and only inside that armed window may checked code call or tick
\ DYNAMIC-STORAGE:ARENA-NEW or ARENA-RELEASE (checker.f ARENA-CALL-REFUSE?,
\ E-ARENA-CALL), so a record's size always comes from the declared type.
\
\ ARENA is a definer over STRUCTURE, so it loads right after
\ src/core/structure-decl.f: it runs STRUCTURE's own family-name gate
\ (STRUCTURE-DECL:NAME-OK?) on the folded name before it spells anything from
\ that name, and builds the generated text in layout-buffer.f's LBUF-GEN.

package ARENA-DECL

private

\ Appends the name folded to lower case.
: LOWER, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CORE-FOLD-C LBUF-C, loop ;


\ Appends `: NAME<suffix> ( ptr <tail> -- ` and answers the span of the word's
\ name.
: HEAD-ROW ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n sfx:ptr sfxu:n tail:ptr tailu:n :}
   s" : " LBUF-APP
   LBUF-GEN-U @ {: start:n :}
   name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN start +  LBUF-GEN-U @ start -
   s"  ( ptr " LBUF-APP tail tailu LBUF-APP s"  -- " LBUF-APP ;


\ The head field's id in the family the declaration's STRUCTURE committed in
\ the package it is made in. A lookup that finds no family or no head field
\ throws E-PF-ID: there is no committed field id to arm.
: HEAD-FIELD ( ptr u8 n -- n )
   {: tail:ptr tailu:n :}
   CHECKER-AUTH-PACKAGE$ tail tailu TFAM:TFAM-FIND-IN 0= if TFAM:E-PF-ID throw then
   TYPE-FIELD:NO-VARIANT s" head" TYPE-FIELD:FIND 0= if TFAM:E-PF-ID throw then ;


\ Evaluates one generated word with the field-projection window armed for it.
: ARMED-EVAL ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: src:ptr srcu:n word:ptr wordu:n pend:ptr pendu:n fid:n :}
   word wordu fid 0 TYPE-DECL:FIELD-ARM
   src srcu pend pendu LBUF-EVAL! drop ;

public

\ Starts the declaration's text with `STRUCTURE ` and the folded name, and
\ answers the folded name's span within it.
: TAIL ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   LBUF-CLEAR
   s" STRUCTURE " LBUF-APP
   LBUF-GEN LBUF-GEN-U @ + nameu
   name nameu LOWER, ;


\ The rest of the declaration's text after TAIL, built before any of it is
\ evaluated so a refusal while building defines nothing: the family's
\ STRUCTURE, evaluated first so its own refusal speaks, then NEW and RELEASE,
\ each evaluated alone since the field-projection window is single-shot, then
\ the spans of their names. The package the declaration is made in names the
\ arena in a capture refusal.
: SOURCE ( ptr u8 n ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n )
   {: name:ptr nameu:n tail:ptr tailu:n type:ptr typeu:n width:n :}
   CHECKER-AUTH-PACKAGE$ {: pkg:ptr pkgu:n :}
   s"  FIELD head DYNAMIC-STORAGE:arena-head ;STRUCTURE" LBUF-APP
   LBUF-GEN-U @ {: famu:n :}
   name nameu s" -NEW" tail tailu HEAD-ROW {: pna:ptr pnu:n :}
   s" ptr " LBUF-APP type typeu LBUF-APP
   s"  ) 0 field-project " LBUF-APP width LBUF-DEC,
   s"  s" LBUF-APP 34 LBUF-C, 32 LBUF-C,
   pkgu 0 > if pkg pkgu LBUF-APP 58 LBUF-C, then
   name nameu LBUF-APP 34 LBUF-C,
   s"  DYNAMIC-STORAGE:ARENA-NEW ; " LBUF-APP
   LBUF-GEN-U @ {: relat:n :}
   name nameu s" -RELEASE" tail tailu HEAD-ROW {: rna:ptr rnu:n :}
   s" ) 0 field-project DYNAMIC-STORAGE:ARENA-RELEASE ;" LBUF-APP
   LBUF-GEN famu
   LBUF-GEN famu +  relat famu -
   LBUF-GEN relat +  LBUF-GEN-U @ relat -
   pna pnu  rna rnu ;


\ A multi-error load counts the family's refusal and goes on, so ARENA then
\ defines no NEW or RELEASE, as when NAME-OK? answers false.
: RUN ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" ARENA" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if exit then
   name nameu s" -NEW" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -RELEASE" LBUF-SUFFIX-OK? 0= if exit then
   type typeu CHECKER-DYNAMIC-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then {: width:n :}
   name nameu TAIL {: tail:ptr tailu:n :}
   tail tailu STRUCTURE-DECL:NAME-OK? 0= if exit then
   name nameu tail tailu type typeu width SOURCE
   {: fam:ptr famu:n new:ptr newu:n rel:ptr relu:n pna:ptr pnu:n rna:ptr rnu:n :}
   MULTI-ERR-N @ {: refused:n :}
   fam famu LBUF-GEN 0 LBUF-EVAL! drop
   MULTI-ERR-N @ refused <> if exit then
   tail tailu HEAD-FIELD {: fid:n :}
   new newu pna pnu pna pnu fid ARMED-EVAL
   rel relu rna rnu rna rnu fid ARMED-EVAL ;

;package

: ARENA ( -- ) ARENA-DECL:RUN ;

PRIM: ARENA PRIM;
