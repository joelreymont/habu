\ hf-config-test.f - focused pinned GPT-2 config tests.

require lib/prelude.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require maki/infer/hf-config.f

package HF

using GPT2
using GPT2PIN

create ROOT FS-PATH-CAP allot
create CONFIG FS-PATH-CAP allot
create FIXTURE CONFIG-LEN 1+ allot
create NUL-ROOT 47 c, 0 c, 120 c,
create LONG-ROOT FS-PATH-CAP 1+ allot

variable ROOT-U
variable CONFIG-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: CONFIG$ ( -- ptr u8 n )
   CONFIG CONFIG-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-hf-config" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ CONFIG-NAME$ CONFIG JOIN-PATH CONFIG-U ! ;

: PAD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u CONFIG-LEN > if E-FS-CAPACITY throw then
   a FIXTURE u BYTE-COPY
   u begin dup CONFIG-LEN < while
      dup FIXTURE + 32 swap c!
      1+
   repeat drop
   FIXTURE CONFIG-LEN ;

: WRITE-PADDED ( ptr u8 n -- )
   PAD$ {: a:ptr u:n :}
   CONFIG$ a u WRITE-ALL ;

: WRITE-SHORT ( ptr u8 n -- ) {: a:ptr u:n :}
   CONFIG$ a u WRITE-ALL ;

: WRITE-EXTRA ( ptr u8 n -- )
   PAD$ 2drop
   33 FIXTURE CONFIG-LEN + c!
   CONFIG$ FIXTURE CONFIG-LEN 1+ WRITE-ALL ;

: PINNED$ ( -- ptr u8 n )
   S\" {\n  \qactivation_function\q: \qgelu_new\q,\n  \qarchitectures\q: [\n    \qGPT2LMHeadModel\q\n  ],\n  \qattn_pdrop\q: 0.1,\n  \qbos_token_id\q: 50256,\n  \qembd_pdrop\q: 0.1,\n  \qeos_token_id\q: 50256,\n  \qinitializer_range\q: 0.02,\n  \qlayer_norm_epsilon\q: 1e-05,\n  \qmodel_type\q: \qgpt2\q,\n  \qn_ctx\q: 1024,\n  \qn_embd\q: 768,\n  \qn_head\q: 12,\n  \qn_layer\q: 12,\n  \qn_positions\q: 1024,\n  \qresid_pdrop\q: 0.1,\n  \qsummary_activation\q: null,\n  \qsummary_first_dropout\q: 0.1,\n  \qsummary_proj_to_labels\q: true,\n  \qsummary_type\q: \qcls_index\q,\n  \qsummary_use_proj\q: true,\n  \qtask_specific_params\q: {\n    \qtext-generation\q: {\n      \qdo_sample\q: true,\n      \qmax_length\q: 50\n    }\n  },\n  \qvocab_size\q: 50257\n}" ;

: GOOD$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:1024,\qn_embd\q:768,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: REORDERED$ ( -- ptr u8 n )
   S\" {\qvocab_size\q:50257,\qn_positions\q:1024,\qn_layer\q:12,\qn_head\q:12,\qn_embd\q:768,\qn_ctx\q:1024,\qmodel_type\q:\qgpt2\q,\qlayer_norm_epsilon\q:1e-05,\qeos_token_id\q:50256,\qbos_token_id\q:50256,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qactivation_function\q:\qgelu_new\q}" ;

: DUPLICATE$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:1024,\qn_ctx\q:1024,\qn_embd\q:768,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: WRONG-ROLE$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:1024,\qn_embd\q:\q768\q,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: BAD-ARCH$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qNotGPT2\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:1024,\qn_embd\q:768,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: BAD-COMMENT$ ( -- ptr u8 n )
   S\" {\qmodel_type\q:\qgpt2\q,/* \qn_ctx\q:1024 */\qarchitectures\q:[\qGPT2LMHeadModel\q]}" ;

: HIDDEN$ ( -- ptr u8 n )
   S\" {\qmodel_type\q:\qgpt2\q,\qnote\q:\qarchitectures n_ctx n_embd n_head n_layer n_positions vocab_size bos_token_id eos_token_id layer_norm_epsilon activation_function\q}" ;

: OVERFLOW$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:9223372036854775808,\qn_embd\q:768,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: BAD-SEMANTIC$ ( -- ptr u8 n )
   S\" {\qactivation_function\q:\qgelu_new\q,\qarchitectures\q:[\qGPT2LMHeadModel\q],\qbos_token_id\q:50256,\qeos_token_id\q:50256,\qlayer_norm_epsilon\q:1e-05,\qmodel_type\q:\qgpt2\q,\qn_ctx\q:1024,\qn_embd\q:769,\qn_head\q:12,\qn_layer\q:12,\qn_positions\q:1024,\qvocab_size\q:50257}" ;

: RESULT-CODE ( result<GPT2:config,n> -- n )
   MATCH result
      ok OF drop false TTRUE 0 ENDOF
      err OF dup 0<> TTRUE ENDOF
   ;MATCH ;

: ROOT-CODE ( -- n )
   ROOT$ FS-PATH:MAKE OPEN-GPT2 RESULT-CODE ;

: ROOT-CODE-TWICE ( -- n )
   ROOT-CODE {: first:n :}
   ROOT-CODE first T=
   first ;

: ASSERT-CONFIG ( GPT2:config -- )
   DATATYPE@ DTYPE MAKI-DATATYPE:EQ TTRUE
   NCTX@ 1024 T=
   NVOCAB@ 50257 T=
   NLAYER@ 12 T=
   NEMBD@ 768 T=
   NHEAD@ 12 T=
   GPT2:TIED? TTRUE
   BOS@ 50256 T=
   EOS@ 50256 T=
   LN-EPS@ 0.00001 f= TTRUE
   GPT2:ATTN-SCALE? TTRUE
   drop ;

: ASSERT-OPEN ( result<GPT2:config,n> -- )
   MATCH result
      ok OF ASSERT-CONFIG ENDOF
      err OF drop false TTRUE ENDOF
   ;MATCH ;

: TEST-PINNED ( -- )
   PINNED$ WRITE-SHORT
   ROOT$ FS-PATH:MAKE OPEN-GPT2 ASSERT-OPEN
   ROOT$ FS-PATH:MAKE OPEN-GPT2 ASSERT-OPEN ;

: PARSE-FRAME ( ptr u8 n -- ptr u8 n )
   2dup PARSE BUILD-FIELDS drop ;

: PARSE-CODE ( ptr u8 n -- n )
   [: PARSE-FRAME ;] catch {: code:n :}
   2drop
   code ;

: TEST-PARSER ( -- )
   GOOD$ PARSE-CODE 0 T=
   REORDERED$ PARSE-CODE 0 T=
   DUPLICATE$ PARSE-CODE E-FIELD T=
   WRONG-ROLE$ PARSE-CODE E-FIELD T=
   BAD-ARCH$ PARSE-CODE E-FIELD T=
   BAD-COMMENT$ PARSE-CODE E-JR-MALFORMED T=
   HIDDEN$ PARSE-CODE E-FIELD T=
   OVERFLOW$ PARSE-CODE E-JR-NUMBER T=
   BAD-SEMANTIC$ PARSE-CODE E-HEAD T= ;

: TEST-FILE-FAILURES ( -- )
   BAD-COMMENT$ WRITE-PADDED
   ROOT-CODE-TWICE E-DIGEST T=
   GOOD$ WRITE-SHORT
   ROOT-CODE-TWICE E-SIZE T=
   GOOD$ WRITE-EXTRA
   ROOT-CODE-TWICE E-FS-CAPACITY T=
   CONFIG$ REMOVE-FILE
   ROOT-CODE-TWICE E-FS-OPEN T= ;

: TEST-ROOTS ( -- )
   ROOT 0 FS-PATH:MAKE OPEN-GPT2 RESULT-CODE E-ROOT T=
   NUL-ROOT 3 FS-PATH:MAKE OPEN-GPT2 RESULT-CODE E-ROOT T=
   FS-PATH-CAP 1+ 0 ?do 120 LONG-ROOT i + c! loop
   LONG-ROOT FS-PATH-CAP FS-PATH:MAKE OPEN-GPT2 RESULT-CODE E-FS-CAPACITY T=
   LONG-ROOT FS-PATH-CAP 1+ FS-PATH:MAKE OPEN-GPT2 RESULT-CODE E-FS-CAPACITY T= ;

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-PARSER
   TEST-FILE-FAILURES
   TEST-ROOTS
   TEST-PINNED
   CLEANUP-RUN
   T-REPORT
   s" hf-config-test: ok" type cr ;

RUN

;using
;using

;package
