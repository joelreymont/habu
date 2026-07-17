\ maki/db/diagnostic-render.f - the TWO diagnostic renderers over ONE value
\ (MODEL-CAD-V2-PLAN.md § 23.9 "Renderers produce human text and canonical JSON
\ from the same value"; dot habu-v2-structured-diagnostic-18d24536).
\
\ CONCERN: presentation only. Both DIAG:RENDER (human text) and DIAG:RENDER-JSON
\ (canonical JSON) consume the SAME `diagnostic` handle produced by
\ maki/db/diagnostic.f and read every field through that file's typed accessors;
\ neither renderer can construct, mutate, or reinterpret the value. Enum labels
\ come from the single-source-of-truth NAME words in diagnostic.f, so the two
\ renderers never disagree on a class/severity/phase/repair spelling.
\
\ JSON is emitted through the checked lib/json-write.f string builder (JW-*): no
\ host tooling, all escaping and number formatting is checked Habu. The human
\ renderer builds into a package-owned text buffer with the same digit recursion.
\ Both return a span valid until the next call of the same renderer.
\
\ This file reopens `package DIAG`, so it reaches the private slot/list helpers
\ (DIAG>, SL-*) by bare name and the public accessors likewise.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/json-write.f
require maki/db/diagnostic.f
require maki/producer.f
require maki/config.f
require maki/rev.f
require maki/artifact.f

package DIAG
private

\ ---- package-owned human-text builder -----------------------------------------
$2000 constant RB-CAP
create RB RB-CAP allot
variable RB-LEN

: TB-RESET ( -- )   0 RB-LEN ! ;
: TB-ROOM ( n -- ) {: k:n :}
   RB-LEN @ k + RB-CAP > if E-DIAG-BUF throw then ;
: TB-C ( n -- ) {: c:n :}
   1 TB-ROOM  c RB RB-LEN @ + c!  RB-LEN @ 1+ RB-LEN ! ;
: TB-S ( ptr u8 n -- ) {: a:ptr u:n :}
   u TB-ROOM  a RB RB-LEN @ + u BYTE-COPY  RB-LEN @ u + RB-LEN ! ;
: TB-NL ( -- )   10 TB-C ;
: TB-U ( n -- )
   dup 10 >= if dup 10 / recurse then
   10 mod 48 + TB-C ;
: TB-INT ( n -- )
   dup 0 < if 45 TB-C negate then TB-U ;
: TB$ ( -- ptr u8 n )   RB RB-LEN @ ;

\ ---- signed JSON number (lib/json-write.f JW-U rejects negatives) --------------
: JW-INT ( n -- )
   dup 0 < if s" -" JW-RAW negate then JW-U ;

\ ---- human-text field helpers -------------------------------------------------
: TB-LABEL ( ptr u8 n -- )   TB-S s" : " TB-S ;         \ "label: "
: TB-KV ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n va:ptr vu:n :}
   la lu TB-LABEL  va vu TB-S  TB-NL ;

: TB-STRLIST ( diagnostic n ptr u8 n -- ) {: d:diagnostic list:n la:ptr lu:n :}
   la lu TB-S s" :" TB-S TB-NL
   d DIAG> list SL-N@ {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      s"   - " TB-S  d DIAG> list k SL-GET TB-S  TB-NL
      1+
   repeat drop ;

: TB-CONE ( diagnostic -- ) {: d:diagnostic :}
   s" dependency-cone:" TB-S TB-NL
   d CONE-COUNT {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      s"   - " TB-S  d k CONE@ ARTIFACT:KEY$ TB-S  TB-NL
      1+
   repeat drop ;

: TB-REPAIRS ( diagnostic -- ) {: d:diagnostic :}
   s" legal-repairs:" TB-S TB-NL
   d REPAIR-COUNT {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      s"   - " TB-S  d k REPAIR@ REPAIR-NAME TB-S  TB-NL
      1+
   repeat drop ;

: TB-SUBJECT ( diagnostic -- ) {: d:diagnostic :}
   d HAS-SUBJECT? if s" subject" d SUBJECT@ ARTIFACT:KEY$ TB-KV then ;
: TB-CX ( diagnostic -- ) {: d:diagnostic :}
   d HAS-COUNTEREXAMPLE? if s" counterexample" d COUNTEREXAMPLE@ ARTIFACT:KEY$ TB-KV then ;
: TB-REV ( diagnostic -- ) {: d:diagnostic :}
   d HAS-REVISION? if s" revision" d REVISION@ REV:CONTENT$ TB-KV then ;
: TB-ENV ( diagnostic -- ) {: d:diagnostic :}
   d HAS-ENVIRONMENT? if s" environment" d ENVIRONMENT@ CONFIG:FACTS$ TB-KV then ;
: TB-PARENT ( diagnostic -- ) {: d:diagnostic :}
   d HAS-PARENT? if s" parent" TB-LABEL d PARENT@ TB-INT TB-NL then ;
: TB-PROGRESS ( diagnostic -- ) {: d:diagnostic :}
   d HAS-PROGRESS? if s" progress" TB-LABEL d PROGRESS@ TB-INT TB-NL then ;

\ ---- JSON field helpers -------------------------------------------------------
: JSON-STRLIST ( diagnostic n -- ) {: d:diagnostic list:n :}
   JW-ARRAY-START
   d DIAG> list SL-N@ {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      k 0 > if JW-COMMA then
      d DIAG> list k SL-GET JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

: JSON-CONE ( diagnostic -- ) {: d:diagnostic :}
   JW-ARRAY-START
   d CONE-COUNT {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      k 0 > if JW-COMMA then
      d k CONE@ ARTIFACT:KEY$ JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

: JSON-REPAIRS ( diagnostic -- ) {: d:diagnostic :}
   JW-ARRAY-START
   d REPAIR-COUNT {: cnt:n :}
   0 begin dup cnt < while
      dup {: k:n :}
      k 0 > if JW-COMMA then
      d k REPAIR@ REPAIR-NAME JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

: JSON-SUBJECT ( diagnostic -- ) {: d:diagnostic :}
   d HAS-SUBJECT? if d SUBJECT@ ARTIFACT:KEY$ JW-STRING else JW-NULL then ;
: JSON-CX ( diagnostic -- ) {: d:diagnostic :}
   d HAS-COUNTEREXAMPLE? if d COUNTEREXAMPLE@ ARTIFACT:KEY$ JW-STRING else JW-NULL then ;
: JSON-REV ( diagnostic -- ) {: d:diagnostic :}
   d HAS-REVISION? if d REVISION@ REV:CONTENT$ JW-STRING else JW-NULL then ;
: JSON-ENV ( diagnostic -- ) {: d:diagnostic :}
   d HAS-ENVIRONMENT? if d ENVIRONMENT@ CONFIG:FACTS$ JW-STRING else JW-NULL then ;
: JSON-PARENT ( diagnostic -- ) {: d:diagnostic :}
   d HAS-PARENT? if d PARENT@ JW-INT else JW-NULL then ;
: JSON-PROGRESS ( diagnostic -- ) {: d:diagnostic :}
   d HAS-PROGRESS? if d PROGRESS@ JW-INT else JW-NULL then ;

public

\ ---- human-readable text renderer ---------------------------------------------
: RENDER ( diagnostic -- ptr u8 n ) {: d:diagnostic :}
   TB-RESET
   s" diagnostic " TB-S  d CODE@ TB-INT  TB-NL
   s" class"    d CLASS@ CLASS-NAME       TB-KV
   s" severity" d SEVERITY@ SEVERITY-NAME TB-KV
   s" phase"    d PHASE@ PHASE-NAME       TB-KV
   s" owner"    d OWNER@ PRODUCER:NAME$   TB-KV
   d TB-SUBJECT
   d TB-REV
   s" location" d LOCATION@ TB-KV
   d TB-ENV
   d SL-EXP   s" expected"             TB-STRLIST
   d SL-OBS   s" observed"             TB-STRLIST
   d SL-INVAL s" invalidated-evidence" TB-STRLIST
   d TB-CONE
   d TB-REPAIRS
   d TB-CX
   d TB-PARENT
   d TB-PROGRESS
   s" reproduction" d REPRODUCTION@ TB-KV
   TB$ ;

\ ---- canonical JSON renderer --------------------------------------------------
: RENDER-JSON ( diagnostic -- ptr u8 n ) {: d:diagnostic :}
   JW-RESET
   JW-OBJECT-START
   s" code" JW-KEY  d CODE@ JW-INT
   JW-COMMA s" class"    JW-KEY  d CLASS@ CLASS-NAME       JW-STRING
   JW-COMMA s" severity" JW-KEY  d SEVERITY@ SEVERITY-NAME JW-STRING
   JW-COMMA s" phase"    JW-KEY  d PHASE@ PHASE-NAME       JW-STRING
   JW-COMMA s" owner"    JW-KEY  d OWNER@ PRODUCER:NAME$   JW-STRING
   JW-COMMA s" subject"  JW-KEY  d JSON-SUBJECT
   JW-COMMA s" revision" JW-KEY  d JSON-REV
   JW-COMMA s" location" JW-KEY  d LOCATION@ JW-STRING
   JW-COMMA s" environment" JW-KEY  d JSON-ENV
   JW-COMMA s" expected" JW-KEY  d SL-EXP JSON-STRLIST
   JW-COMMA s" observed" JW-KEY  d SL-OBS JSON-STRLIST
   JW-COMMA s" invalidated_evidence" JW-KEY  d SL-INVAL JSON-STRLIST
   JW-COMMA s" dependency_cone" JW-KEY  d JSON-CONE
   JW-COMMA s" legal_repairs"   JW-KEY  d JSON-REPAIRS
   JW-COMMA s" counterexample"  JW-KEY  d JSON-CX
   JW-COMMA s" parent"   JW-KEY  d JSON-PARENT
   JW-COMMA s" progress" JW-KEY  d JSON-PROGRESS
   JW-COMMA s" reproduction" JW-KEY  d REPRODUCTION@ JW-STRING
   JW-OBJECT-END
   JW$ ;

;package
