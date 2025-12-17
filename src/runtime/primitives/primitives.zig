//! Habu Runtime Primitives
//!
//! Re-exports all primitive operations:
//! - List operations (cons, car, cdr, etc.)
//! - Arithmetic operations (+, -, *, /, etc.)
//! - String operations
//! - Vector operations
//! - I/O operations

pub const list = @import("list.zig");
pub const arith = @import("arith.zig");
pub const string = @import("string.zig");
pub const vector = @import("vector.zig");
pub const io = @import("io.zig");

// Re-export commonly used functions
pub const cons = list.cons;
pub const car = list.car;
pub const cdr = list.cdr;
pub const length = list.length;

pub const add = arith.add;
pub const sub = arith.sub;
pub const mul = arith.mul;
pub const div = arith.div;

test {
    _ = list;
    _ = arith;
    _ = string;
    _ = vector;
    _ = io;
}
