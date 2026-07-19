I recently came across the kind of GPU bug I want to eliminate: a kernel that runs without an error and returns the wrong numbers.

In Triton issue #10927, a tensor-descriptor GEMM taken from a vLLM MoE path produced incorrect values for about 96% of its output on Blackwell when (K=511) in bf16. Changing (K) to 512 made the kernel work. The descriptors were accepted, the kernel launched successfully, and the ragged final K-tile silently corrupted the result.

The problem is that GPU programmers still have to write too much implementation detail and too little of the math. The descriptor path requires explicit bases, shapes, strides, block shapes, tiling, and data movement. Each additional choice is another place where a compiler bug or programmer mistake can produce a wrong result.

In Habu, the same indexed GEMM can be written as one line:

`O[m,n] = Σk A[ix[m],k] · B[n,k]`

The programmer should state what to compute. The compiler should determine how to compute it: choose the tile sizes, memory layout, pipeline depth, tensor-core instruction, fusion strategy, and numerical precision; generate the data movement; handle partial tiles; and reject the program at compile time when it cannot guarantee valid bounds, alignment, compatible layouts, or the required accumulated precision.

The programmer should not have to tune these choices manually. The compiler should calculate the arithmetic intensity of each operation, fuse operations when memory traffic is the bottleneck, and tile them for tensor cores when computation is the bottleneck.

Habu’s concatenative representation makes whole-program fusion direct. A Habu program is a sequence of typed words, and composing two operations means concatenating their words. The compiler can analyze and transform the whole program rather than reconstructing it from Python calls or treating a fused kernel as a separate programming construct. Values passed directly from one word to the next can remain in registers or shared memory instead of being written to global memory and loaded again.

The same representation supports automatic differentiation for the backward pass. The compiler can traverse the forward words in reverse order and replace each word with the word that computes its derivative. It can determine at compile time which forward values the backward pass needs and whether to preserve or recompute them.

This approach is partially implemented in Habu. So far, I have tested it only on a Jetson Orin NX and a DGX Spark. The next step is to prove it on production training and inference hardware: first H100 and H200, then B200 and B300 GPUs, and eventually multi-GPU systems such as the GB300 NVL72. It must also be tested on realistic transformer and MoE workloads rather than isolated demonstrations.

My proposed research would implement indexed GEMMs, reductions, fused epilogues, attention, and MoE kernels, then compare Habu with PyTorch and Triton on performance, source-code size, compile time, correctness across irregular shapes, and portability between GPU generations.

The larger question is whether writing GPU kernels can become unnecessary for most machine-learning code. A programmer should be able to write the computation once while the compiler derives the tiling, fusion, data movement, tensor-core instructions, precision, and backward pass; checks that every transformation preserves the computation for every supported shape; and generates code competitive with hand-written kernels across GPU generations. A case such as (K=511) should not require a regression test to discover silent corruption. The compiler should either generate a correct implementation or refuse to compile it.
