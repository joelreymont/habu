\ gpt2-pin-test.f - pinned GPT-2 artifact identity tests.

require lib/test.f
require lib/fs.f
require maki/infer/gpt2-pin.f

package GPT2-PIN-TEST

64 constant SHA256-LEN

create PATH FS-PATH-CAP allot
create DIGEST SHA256-LEN allot

using GPT2PIN

: ASSERT$ ( ptr u8 n ptr u8 n -- )
   {: got:ptr gotu:n want:ptr wantu:n :}
   got gotu want wantu T$= ;

: PRESENT? ( ptr u8 n -- bool )
   {: name:ptr nameu:n :}
   s" gpt2-model" name nameu PATH JOIN-PATH
   PATH swap EXISTS? ;

: ASSERT-PATH ( ptr u8 n n ptr u8 n -- )
   {: path:ptr pathu:n size:n hash:ptr hashu:n :}
   path pathu FILE-SIZE size T=
   path pathu DIGEST SHA256-FILE-HEX 0 T=
   DIGEST SHA256-LEN hash hashu T$= ;

: ASSERT-FILE ( ptr u8 n n ptr u8 n -- )
   {: name:ptr nameu:n size:n hash:ptr hashu:n :}
   s" gpt2-model" name nameu PATH JOIN-PATH
   PATH swap size hash hashu ASSERT-PATH ;

: SNAPSHOT? ( -- bool )
   CONFIG-NAME$ PRESENT?
   MODEL-NAME$ PRESENT? and
   VOCAB-NAME$ PRESENT? and
   MERGES-NAME$ PRESENT? and ;

: CONSTANTS ( -- )
   DTYPE MAKI-DATATYPE:DF32 MAKI-DATATYPE:EQ TTRUE
   TIED? TTRUE
   ATTN-SCALE? TTRUE

   REVISION$
   s" 607a30d783dfa663caf39e06633721c8d4cfcd7e" ASSERT$

   CONFIG-NAME$ s" config.json" ASSERT$
   CONFIG-LEN 665 T=
   CONFIG-SHA256$
   s" 0daed7749b4f02b8f76240d5444551d7b08712dab4d0adb8239c56ba823bb7b4" ASSERT$

   MODEL-NAME$ s" model.safetensors" ASSERT$
   MODEL-LEN 548105171 T=
   MODEL-SHA256$
   s" 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707" ASSERT$

   VOCAB-NAME$ s" vocab.json" ASSERT$
   VOCAB-LEN 1042301 T=
   VOCAB-SHA256$
   s" 196139668be63f3b5d6574427317ae82f612a97c5d1cdaf36ed2256dbf636783" ASSERT$

   MERGES-NAME$ s" merges.txt" ASSERT$
   MERGES-LEN 456318 T=
   MERGES-SHA256$
   s" 1ce1664773c50f3e0cc8842619a93edc4624525b728b188a9e0be33b7726adc5" ASSERT$ ;

: ARTIFACTS ( -- )
   CONFIG-NAME$ CONFIG-LEN CONFIG-SHA256$ ASSERT-FILE
   MODEL-NAME$ MODEL-LEN MODEL-SHA256$ ASSERT-FILE
   VOCAB-NAME$ VOCAB-LEN VOCAB-SHA256$ ASSERT-FILE
   MERGES-NAME$ MERGES-LEN MERGES-SHA256$ ASSERT-FILE ;

;using

public

: RUN ( -- )
   T-RESET
   CONSTANTS
   SNAPSHOT? if
      ARTIFACTS
   else
      s" gpt2-pin: local snapshot absent -> artifact leg SKIPPED; constants checked" type cr
   then
   T-REPORT ;

;package

GPT2-PIN-TEST:RUN
