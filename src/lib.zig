//! Habu - A Zig-based Lisp with gradual typing
//!
//! Architecture:
//! - Types-first design with Racket-style contracts
//! - Bytecode interpreter (works on WASM)
//! - Copy-and-patch JIT (native platforms)
//! - Cheney copying GC

pub const diagnostic = @import("diagnostic.zig");
pub const types = @import("types/types.zig");
pub const runtime = @import("runtime/runtime.zig");
pub const reader = @import("reader/reader.zig");
pub const compiler = @import("compiler/compiler.zig");
pub const bytecode = @import("bytecode/bytecode.zig");
pub const interp = @import("interp/interp.zig");
pub const jit = @import("jit/mod.zig");
pub const ir = @import("ir/ir.zig");
pub const tests = @import("tests/tests.zig");
pub const testutil = @import("testing/compile_chunk.zig");

test {
    _ = diagnostic;
    _ = types;
    _ = runtime;
    _ = reader;
    _ = compiler;
    _ = bytecode;
    _ = interp;
    _ = jit;
    _ = ir;
    _ = tests;
    _ = testutil;
}
