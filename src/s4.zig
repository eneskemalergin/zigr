//! Registered S4 class construction and slot access.
//!
//! New objects use the class prototype but do not run `initialize` or validity methods.
//! Returned objects and slot values are unprotected. Construction and assignment
//! can allocate and longjmp; callers keep their SEXP inputs reachable across calls.
//! Slot access and assignment do not request ALTREP payload storage from slot values.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const r_error = @import("error");
const protect = @import("protect.zig");
const symbols = @import("symbols.zig");

const max_name_len = @import("sexp.zig").max_symbol_name;

pub const S4Error = error{
    InvalidClassName,
    InvalidSlotName,
    MissingClass,
    MissingSlot,
    NotS4,
    NullValue,
};

pub fn isS4(sexp: R.SEXP) bool {
    return sexp != null and R.Rf_isS4(sexp) != 0;
}

/// The returned object may differ from `sexp`.
pub fn setS4Object(sexp: R.SEXP, value: bool) R.SEXP {
    return R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 1);
}

pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    if (!isS4(sexp) or !validName(name)) return false;
    return R.R_has_slot(sexp, symbols.install(name)) != 0;
}

/// The returned value borrows from `sexp`; a missing slot raises an R error.
pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    return R.R_do_slot(sexp, symbols.install(name));
}

pub fn getSlotChecked(sexp: R.SEXP, name: []const u8) S4Error!R.SEXP {
    if (!isS4(sexp)) return error.NotS4;
    if (!validName(name)) return error.InvalidSlotName;
    const slot = symbols.install(name);
    if (R.R_has_slot(sexp, slot) == 0) return error.MissingSlot;
    return R.R_do_slot(sexp, slot);
}

/// The returned object may differ from `sexp`, notably for `.Data` assignment.
pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) R.SEXP {
    return R.R_do_slot_assign(sexp, symbols.install(name), value);
}

pub fn setSlotChecked(sexp: R.SEXP, name: []const u8, value: R.SEXP) S4Error!R.SEXP {
    if (!isS4(sexp)) return error.NotS4;
    if (value == null) return error.NullValue;
    if (!validName(name)) return error.InvalidSlotName;
    const slot = symbols.install(name);
    if (R.R_has_slot(sexp, slot) == 0) return error.MissingSlot;
    return R.R_do_slot_assign(sexp, slot, value);
}

/// Resolves the class through R's methods registry and returns its unprotected prototype object.
pub fn newObjectChecked(class_name: []const u8) S4Error!R.SEXP {
    if (!validName(class_name)) return error.InvalidClassName;
    var name: [max_name_len:0]u8 = undefined;
    @memcpy(name[0..class_name.len], class_name);
    name[class_name.len] = 0;

    const result = cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const class_name_ptr: [*c]const u8 = @ptrCast(data.?);
            const definition = R.R_getClassDef(class_name_ptr);
            if (definition == R.R_NilValue) return R.R_NilValue;
            var protected_definition = protect.scoped(definition);
            defer protected_definition.deinit();
            return R.R_do_new_object(protected_definition.get());
        }
    }.call, @ptrCast(&name));
    if (result == R.R_NilValue) return error.MissingClass;
    return result;
}

fn s4ErrorMessage(s4_error: S4Error) []const u8 {
    return switch (s4_error) {
        error.InvalidClassName => "S4 class name is invalid",
        error.InvalidSlotName => "S4 slot name is invalid",
        error.MissingClass => "S4 class is not registered",
        error.MissingSlot => "S4 slot does not exist",
        error.NotS4 => "expected an S4 object",
        error.NullValue => "S4 slot value is null",
    };
}

/// Builds an S4 object or raises an R error when validation fails.
pub fn newObject(class_name: []const u8) R.SEXP {
    return newObjectChecked(class_name) catch |s4_error| r_error.signal(s4ErrorMessage(s4_error));
}

fn validName(name: []const u8) bool {
    return name.len != 0 and name.len < max_name_len and std.mem.indexOfScalar(u8, name, 0) == null;
}
