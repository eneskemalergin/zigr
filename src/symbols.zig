//! Cache interned R symbols.
//!
//! R owns symbols for the session. The fixed thread-local cache avoids native
//! allocation and stops accepting entries when full.

const std = @import("std");
const R = @import("R");
const err = @import("error");
const max_name_len = @import("sexp.zig").max_symbol_name;

const cap = 64;
const mask = cap - 1;
threadlocal var hashes: [cap]u64 = [_]u64{0} ** cap;
threadlocal var names: [cap][max_name_len:0]u8 = undefined;
threadlocal var lengths: [cap]usize = undefined;
threadlocal var sexps: [cap]R.SEXP = undefined;
threadlocal var count: usize = 0;

/// Calls `Rf_install`; invalid fixed-cache names become R errors.
pub fn install(name: []const u8) R.SEXP {
    if (name.len >= max_name_len) err.signal("symbol name exceeds 255 bytes");
    if (std.mem.indexOfScalar(u8, name, 0) != null) err.signal("symbol name contains a NUL byte");

    const hash = std.hash.Wyhash.hash(0, name) | 1;
    var idx = @as(usize, @truncate(hash)) & mask;

    for (0..cap) |_| {
        if (hashes[idx] == 0) break;
        if (hashes[idx] == hash and lengths[idx] == name.len and
            std.mem.eql(u8, names[idx][0..name.len], name))
        {
            return sexps[idx];
        }
        idx = (idx + 1) & mask;
    }

    var buf: [max_name_len:0]u8 = undefined;
    @memcpy(buf[0..name.len], name);
    buf[name.len] = 0;
    const sxp = R.Rf_install(@ptrCast(&buf));

    if (count < cap and hashes[idx] == 0) {
        hashes[idx] = hash;
        lengths[idx] = name.len;
        @memcpy(names[idx][0..name.len], name);
        names[idx][name.len] = 0;
        sexps[idx] = sxp;
        count += 1;
    }

    return sxp;
}
