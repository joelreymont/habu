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
pub const macro = @import("macro.zig");
pub const condition = @import("condition.zig");

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
pub const classOf = clos.classOf;
pub const slotExistsP = clos.slotExistsP;
pub const slotBoundp = clos.slotBoundp;
pub const slotMakunbound = clos.slotMakunbound;
pub const callNextMethod = clos.callNextMethod;
pub const classp = clos.classp;
pub const findClass = clos.findClass;
pub const setFindClass = clos.setFindClass;
pub const className = clos.className;
pub const classDirectSuperclasses = clos.classDirectSuperclasses;
pub const classPrecedenceList = clos.classPrecedenceList;
pub const classDirectSlots = clos.classDirectSlots;
pub const classSlots = clos.classSlots;
pub const slotDefinitionName = clos.slotDefinitionName;
pub const slotDefinitionInitform = clos.slotDefinitionInitform;
pub const slotDefinitionInitargs = clos.slotDefinitionInitargs;
pub const slotDefinitionReaders = clos.slotDefinitionReaders;
pub const slotDefinitionWriters = clos.slotDefinitionWriters;
pub const slotDefinitionAllocation = clos.slotDefinitionAllocation;
pub const slotDefinitionType = clos.slotDefinitionType;
pub const makeGenericFunction = clos.makeGenericFunction;
pub const makeMethod = clos.makeMethod;
pub const addMethod = clos.addMethod;
pub const genericFunctionName = clos.genericFunctionName;
pub const genericFunctionMethods = clos.genericFunctionMethods;
pub const genericFunctionLambdaList = clos.genericFunctionLambdaList;
pub const methodQualifiers = clos.methodQualifiers;
pub const methodSpecializers = clos.methodSpecializers;
pub const methodFunction = clos.methodFunction;
pub const makeUnbound = clos.makeUnbound;

pub const typep = ty.typep;
pub const subtypep = ty.subtypep;
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
    _ = macro;
    _ = condition;
}
