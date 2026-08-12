\ judge/corpus4.f - the judged rows of tools/codegen-compare-corpus4.f: twelve
\ shapes chosen because somebody had a reason to believe the native chain
\ handles them WORSE than the engine's emitter. One concern: which subject is
\ judged, and which C symbol is its twin.
\
\ WHAT THIS FILE STATES AND WHAT IT DOES NOT. It states the twelve subjects and
\ the C symbol that is each one's twin. It states NOTHING about the programs:
\ the engine compiled them when the corpus file was loaded, and the chain's
\ copies are derived from that same file's bytes by tools/judge/src.f and
\ compiled by tools/judge/chain.f. There is no retyped body here.
\
\ AND IT STATES NOTHING ABOUT WHAT THE CHAIN CANNOT COMPILE. The comparison this
\ replaces kept a hand-written list of those subjects, and a list cannot notice a
\ row that started refusing or one that stopped. Here the chain is asked, every
\ run, and what it answers - the routine, or the code it declined with - is the
\ row's verdict. The two rows that are refused today are refused with
\ E-A64RA-SPILL and appear in the committed artifact as REFUSED with that code;
\ if the allocator gains the capability they become ordinary rows and the
\ artifact disagrees until it is regenerated, which is the whole point.
\
\ THE TWIN IS NAMED AND NOT DERIVED, because it is a fact about tools/clang/twins.c
\ and there is nothing in the corpus file that says which C function stands for
\ which subject. A subject with no twin is a row with no reference column, which
\ is a fact about that C file rather than about either code generator.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f
require tools/codegen-compare-corpus4.f
require tools/codegen-tail-probe.f
require tools/judge/src.f
require tools/judge/chain.f
require tools/judge/row.f

package JUDGE-CORPUS4

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus4.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J4" ;

\ The name a row is printed under: the subject as the corpus publishes it, which
\ is also the name the engine's word is found by.
create QUAL $60 allot

: QUAL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   s" CODEGEN-CORPUS4:" {: pa:ptr pu:n :}
   pa QUAL pu STR-LEN BYTE-COPY-LEN
   a QUAL pu + u STR-LEN BYTE-COPY-LEN
   QUAL pu u + ;

\ ---- one judged subject ------------------------------------------------------

: OLD-COLUMN ( n ptr u8 n -- ) {: k:n a:ptr u:n :}
   k  a u QUAL$ NTAILPROBE:CODE-BYTES  JUDGE-ROW:OLD! ;

\ The chain is asked here, and its answer is the column. A refusal is recorded
\ with its own code; anything else is the routine it published.
: NEW-COLUMN ( n ptr u8 n -- ) {: k:n a:ptr u:n :}
   a u JUDGE-SRC:FIND {: d:n :}
   d 0 < if E-JUDGE-SRC-ROW throw then
   d JUDGE-CHAIN:PUBLISH-CALLING {: rc:n :}
   rc 0<> if k rc JUDGE-ROW:REFUSED! exit then
   k  d JUDGE-CHAIN:SIZE  JUDGE-ROW:NEW! ;

: REF-COLUMN ( n ptr u8 n -- ) {: k:n ta:ptr tu:n :}
   CODEGEN-CLANG:PRESENT? 0= if exit then
   k  ta tu CODEGEN-MACHO:BYTES  JUDGE-ROW:REF! ;

\ One subject: the name the corpus publishes it under and the C symbol that is
\ its twin. The row is opened first, so a refusal further down still leaves a
\ row in the table rather than a shorter column.
: SUBJECT ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n ta:ptr tu:n :}
   a u QUAL$ JUDGE-ROW:OPEN {: k:n :}
   k a u OLD-COLUMN
   k a u NEW-COLUMN
   k ta tu REF-COLUMN ;

public

\ Judge every subject of this corpus. The corpus source is read once, the chain
\ is asked about each subject in the order the corpus file defines them - which
\ is the order a call site's callee has to be published in - and each row is
\ opened as it is measured.
: JUDGE ( -- )
   SUFFIX$ JUDGE-CHAIN:SUFFIX!
   SOURCE$ JUDGE-SRC:LOAD
   s" CALL-FAN"       s" hc4_call_fan"       SUBJECT
   s" CALL-FAN-BIG"   s" hc4_call_fan_big"   SUBJECT
   s" CALL-LOOP-3"    s" hc4_call_loop_3"    SUBJECT
   s" WIDE-ARITY"     s" hc4_wide_arity"     SUBJECT
   s" LADDER"         s" hc4_ladder"         SUBJECT
   s" PRESSURE-LOOP"  s" hc4_pressure_loop"  SUBJECT
   s" CALL-PRESSURE"  s" hc4_call_pressure"  SUBJECT
   s" BIG-CONSTS"     s" hc4_big_consts"     SUBJECT
   s" MANY-LOCALS"    s" hc4_many_locals"    SUBJECT
   s" TINY-CALLEE"    s" hc4_tiny_callee"    SUBJECT
   s" FLOAT-MIX"      s" hc4_float_mix"      SUBJECT
   s" STORE-LOAD"     s" hc4_store_load"     SUBJECT ;

;package
