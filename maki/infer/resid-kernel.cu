// resid-kernel.cu - GPU read-bandwidth probe kernel for the UMA weight-residency
// measurement (dot habu-infer-safetensors-loader-0b58e06a). resid_read streams
// every element of `in` (n4 float4s = n4*16 bytes) exactly once via a grid-stride
// loop of coalesced 128-bit loads, reduces per block through shared memory, and
// folds one atomicAdd per block into `out` - so the loads cannot be elided and
// the kernel is memory-bandwidth bound (not atomic bound). The SAME kernel is
// timed over (a) an mmap'd host pointer accessed directly (GB10 UMA/ATS
// coherence) and (b) a cuMemAlloc device buffer; the only difference between the
// two runs is the value of `in`. Regenerable: compiled to a private cubin at
// probe time (never committed) by maki/infer/residency-probe.f.
//   nvcc -arch=sm_121a -cubin resid-kernel.cu -o <cubin>
extern "C" __global__ void resid_read(const float4* __restrict__ in, float* out, unsigned n4) {
    unsigned i = blockIdx.x * blockDim.x + threadIdx.x;
    unsigned stride = gridDim.x * blockDim.x;
    float acc = 0.0f;
    for (; i < n4; i += stride) {
        float4 v = in[i];
        acc += v.x + v.y + v.z + v.w;
    }
    __shared__ float s[256];
    s[threadIdx.x] = acc;
    __syncthreads();
    for (unsigned k = blockDim.x >> 1; k > 0; k >>= 1) {
        if (threadIdx.x < k) s[threadIdx.x] += s[threadIdx.x + k];
        __syncthreads();
    }
    if (threadIdx.x == 0) atomicAdd(out, s[0]);
}
