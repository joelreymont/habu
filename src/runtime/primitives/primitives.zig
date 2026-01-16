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
pub const rational = @import("rational.zig");
pub const complex = @import("complex.zig");
pub const clos = @import("clos.zig");
pub const stream = @import("stream.zig");
pub const hash = @import("hash.zig");
pub const char = @import("char.zig");
pub const package = @import("package.zig");
pub const symbol = @import("symbol.zig");
pub const pathname = @import("pathname.zig");
pub const ty = @import("type.zig");

// Re-export commonly used functions
pub const cons = list.cons;
pub const car = list.car;
pub const cdr = list.cdr;
pub const length = list.length;

pub const add = arith.add;
pub const sub = arith.sub;
pub const mul = arith.mul;
pub const div = arith.div;

pub const makeInstance = clos.makeInstance;
pub const slotValue = clos.slotValue;
pub const setSlotValue = clos.setSlotValue;

pub const typep = ty.typep;
pub const typeOf = ty.typeOf;

test {
    _ = list;
    _ = arith;
    _ = string;
    _ = vector;
    _ = io;
    _ = rational;
    _ = complex;
    _ = clos;
    _ = stream;
    _ = hash;
    _ = char;
    _ = package;
    _ = symbol;
    _ = pathname;
    _ = ty;
}
