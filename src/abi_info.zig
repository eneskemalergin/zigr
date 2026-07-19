const std = @import("std");
const builtin = @import("builtin");
const sexp = @import("sexp");

pub fn main() void {
    std.debug.print("sexp_abi={s} r_version={d} target={s}-{s}-{s} endian={s} pointer_bits={d} cpu={s} optimize={s}\n", .{
        @tagName(sexp.active_abi_contract),
        sexp.r_header_version,
        @tagName(builtin.target.cpu.arch),
        @tagName(builtin.target.os.tag),
        @tagName(builtin.target.abi),
        @tagName(builtin.target.cpu.arch.endian()),
        @bitSizeOf(usize),
        builtin.target.cpu.model.name,
        @tagName(builtin.mode),
    });
}
