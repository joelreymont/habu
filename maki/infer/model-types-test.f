\ model-types-test.f - MODEL enum acceptance (dot habu-add-shared-inference).
\
\ Three legs, all through the PUBLIC package surface (this file never reopens
\ MODEL):
\   1. every variant constructs and renders through an exhaustive MATCH (a
\      missing arm would be a load-time checker reject, so the render words are
\      themselves the exhaustiveness pin);
\   2. DERIVE eq compares typed identity per family;
\   3. swapping ANY two of the five families is a checker reject even though
\      the runtime tags coincide (every family numbers its variants from 0) -
\      all 20 ordered cross-family pairs, each beside its positive control.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require maki/infer/model-types.f

package MODEL-TYPES-TEST

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- exhaustive MATCH renders, one per family ( value -- n ) -----------------
: FAM-N ( MODEL:family -- n )
   MATCH MODEL:family
      gpt2  OF 1 ENDOF
      llama OF 2 ENDOF
   ;MATCH ;

: POS-N ( MODEL:position -- n )
   MATCH MODEL:position
      learned OF 1 ENDOF
      rope    OF 2 ENDOF
   ;MATCH ;

: NORM-N ( MODEL:normalization -- n )
   MATCH MODEL:normalization
      layer-norm OF 1 ENDOF
      rms-norm   OF 2 ENDOF
   ;MATCH ;

: ACT-N ( MODEL:activation -- n )
   MATCH MODEL:activation
      gelu-new OF 1 ENDOF
      silu     OF 2 ENDOF
   ;MATCH ;

: ADPT-N ( MODEL:adapter -- n )
   MATCH MODEL:adapter
      hf-gpt2 OF 1 ENDOF
   ;MATCH ;

\ ---- every variant constructs and MATCHes; EQ is identity per family ---------
: RUN ( -- )
   MODEL-FAMILY:GPT2 FAM-N 1 T=
   MODEL-FAMILY:LLAMA FAM-N 2 T=
   MODEL-POSITION:LEARNED POS-N 1 T=
   MODEL-POSITION:ROPE POS-N 2 T=
   MODEL-NORMALIZATION:LAYER-NORM NORM-N 1 T=
   MODEL-NORMALIZATION:RMS-NORM NORM-N 2 T=
   MODEL-ACTIVATION:GELU-NEW ACT-N 1 T=
   MODEL-ACTIVATION:SILU ACT-N 2 T=
   MODEL-ADAPTER:HF-GPT2 ADPT-N 1 T=
   MODEL-FAMILY:GPT2 MODEL-FAMILY:GPT2 MODEL-FAMILY:EQ TTRUE
   MODEL-FAMILY:GPT2 MODEL-FAMILY:LLAMA MODEL-FAMILY:EQ TFALSE
   MODEL-POSITION:LEARNED MODEL-POSITION:LEARNED MODEL-POSITION:EQ TTRUE
   MODEL-POSITION:LEARNED MODEL-POSITION:ROPE MODEL-POSITION:EQ TFALSE
   MODEL-NORMALIZATION:RMS-NORM MODEL-NORMALIZATION:RMS-NORM MODEL-NORMALIZATION:EQ TTRUE
   MODEL-NORMALIZATION:LAYER-NORM MODEL-NORMALIZATION:RMS-NORM MODEL-NORMALIZATION:EQ TFALSE
   MODEL-ACTIVATION:SILU MODEL-ACTIVATION:SILU MODEL-ACTIVATION:EQ TTRUE
   MODEL-ACTIVATION:GELU-NEW MODEL-ACTIVATION:SILU MODEL-ACTIVATION:EQ TFALSE
   MODEL-ADAPTER:HF-GPT2 MODEL-ADAPTER:HF-GPT2 MODEL-ADAPTER:EQ TTRUE ;

T-RESET
RUN

\ ---- positive controls: each render certifies on its own family --------------
s" MTP-F ( MODEL:family -- n ) FAM-N" YES
s" MTP-P ( MODEL:position -- n ) POS-N" YES
s" MTP-N ( MODEL:normalization -- n ) NORM-N" YES
s" MTP-A ( MODEL:activation -- n ) ACT-N" YES
s" MTP-D ( MODEL:adapter -- n ) ADPT-N" YES

\ ---- cross-family swaps: all 20 ordered pairs reject (tags coincide at 0) ----
s" MTN-FP ( MODEL:position -- n ) FAM-N" NO
s" MTN-FN ( MODEL:normalization -- n ) FAM-N" NO
s" MTN-FA ( MODEL:activation -- n ) FAM-N" NO
s" MTN-FD ( MODEL:adapter -- n ) FAM-N" NO
s" MTN-PF ( MODEL:family -- n ) POS-N" NO
s" MTN-PN ( MODEL:normalization -- n ) POS-N" NO
s" MTN-PA ( MODEL:activation -- n ) POS-N" NO
s" MTN-PD ( MODEL:adapter -- n ) POS-N" NO
s" MTN-NF ( MODEL:family -- n ) NORM-N" NO
s" MTN-NP ( MODEL:position -- n ) NORM-N" NO
s" MTN-NA ( MODEL:activation -- n ) NORM-N" NO
s" MTN-ND ( MODEL:adapter -- n ) NORM-N" NO
s" MTN-AF ( MODEL:family -- n ) ACT-N" NO
s" MTN-AP ( MODEL:position -- n ) ACT-N" NO
s" MTN-AN ( MODEL:normalization -- n ) ACT-N" NO
s" MTN-AD ( MODEL:adapter -- n ) ACT-N" NO
s" MTN-DF ( MODEL:family -- n ) ADPT-N" NO
s" MTN-DP ( MODEL:position -- n ) ADPT-N" NO
s" MTN-DN ( MODEL:normalization -- n ) ADPT-N" NO
s" MTN-DA ( MODEL:activation -- n ) ADPT-N" NO

\ ---- a constructed wrong-family value cannot flow into a family slot ---------
s" MTN-VAL ( -- n ) MODEL-POSITION:LEARNED FAM-N" NO
s" MTN-RAW ( n -- n ) FAM-N" NO

T-REPORT

;package
