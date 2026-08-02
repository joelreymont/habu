\ model-types.f - canonical shared inference model semantics (package MODEL).
\
\ CONCERN: the five closed payload-free enum families shared by normalized
\ configuration and the compiled-pack manifest, so both ends name ONE authority
\ instead of translating tags (dot habu-add-shared-inference-0dad1107; inference
\ design rev 4, blackboard 20260724-191041.846). Adapter tensor names and
\ orientation conventions stay GPT2TENSOR-owned semantics of the variant.
\ There is deliberately NO dtype family here: MAKI:datatype
\ (maki/tensor.f:123, package MAKI public) is the sole dtype authority.
\ No parser, storage, JSON, or target identity belongs in this package.
\
\ DERIVE eq gives each family its typed identity compare (MODEL-FAMILY:EQ,
\ MODEL-POSITION:EQ, ...) so consumers compare values without raw tags; the
\ generated constructors (MODEL-FAMILY:GPT2, ...) and exhaustive MATCH are the
\ only value surface - no raw-n crossing exists.

package MODEL

public

\ architecture family: which transformer lineage the weights implement.
ENUM family DERIVE eq
   gpt2
   llama
;ENUM

\ position encoding: learned absolute table vs rotary embedding.
ENUM position DERIVE eq
   learned
   rope
;ENUM

\ normalization flavor: LayerNorm vs RMSNorm.
ENUM normalization DERIVE eq
   layer-norm
   rms-norm
;ENUM

\ feed-forward activation: tanh-approximated "new" GELU vs SiLU.
ENUM activation DERIVE eq
   gelu-new
   silu
;ENUM

\ weight-format adapter vocabulary (never text, never a bare number).
ENUM adapter DERIVE eq
   hf-gpt2
;ENUM

;package
