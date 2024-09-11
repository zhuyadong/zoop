const std = @import("std");
const StructField = std.builtin.Type.StructField;
const FieldType = std.meta.FieldType;
const FieldEnum = std.meta.FieldEnum;
const Tuple = type;
const compfmt = std.fmt.comptimePrint;
const assert = std.debug.assert;

//===== public content ======
pub const type_id = *const anyopaque;
pub const VtableGetFunc = fn (iface_id: type_id) ?*anyopaque;
pub const ClassCheckFunc = fn (class_id: type_id) bool;
pub const TypeInfoGetFunc = fn () *const TypeInfo;

pub const TypeInfo = struct {
    typename: []const u8,
    typeid: type_id,
    getVtable: ?*const VtableGetFunc,
    isClass: ?*const ClassCheckFunc,
};

pub const Nil = struct {
    const payload: usize = 0;
    pub fn ptr() *anyopaque {
        return @ptrCast(@constCast(&payload));
    }

    pub fn of(comptime I: type) I {
        return .{ .ptr = @ptrCast(ptr()), .vptr = @alignCast(@ptrCast(ptr())) };
    }
};

pub const IObject = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,
};

pub fn Klass(comptime T: type) type {
    if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.", .{@typeName(T)}));

    const kmagic: u32 = 0xaabbccdd;
    const Header = struct {
        getTypeInfo: *const TypeInfoGetFunc,
        deinit: *const fn (*anyopaque) void,
        deallocator: ?std.mem.Allocator = null,
        magic: u32 = kmagic,
    };
    return struct {
        pub const @"#klass" = true;
        header: Header,
        class: T,

        pub fn new(allocator: std.mem.Allocator) !*@This() {
            var self = try allocator.create(@This());
            self.header = .{ .getTypeInfo = typeInfoGetter(T), .deallocator = allocator, .deinit = @ptrCast(&deinit) };
            return self;
        }

        pub fn make(class: ?T) @This() {
            return .{
                .header = .{ .getTypeInfo = typeInfoGetter(T), .deinit = @ptrCast(&deinit) },
                .class = if (class) |v| v else undefined,
            };
        }

        pub fn ptr(self: *@This()) *T {
            return &self.class;
        }

        pub fn from(pclass: *const T) *@This() {
            const self: *@This() = @constCast(@alignCast(@fieldParentPtr("class", pclass)));
            assert(self.header.magic == kmagic);
            return self;
        }

        fn deinit(self: *@This()) void {
            assert(self.header.magic == kmagic);
            assert(self.header.getTypeInfo == typeInfoGetter(T));
            inline for (classes(T).items) |V| {
                if (@hasDecl(V, "deinit")) {
                    var p: *V = @ptrCast(self.ptr());
                    p.deinit();
                }
            }
            if (self.header.deallocator) |dtor| {
                dtor.destroy(self);
            }
        }
    };
}

pub fn Vtable(comptime I: type) type {
    const ifaces = interfaces(I);
    comptime var vtables: [ifaces.items.len]type = undefined;
    comptime var nfield: comptime_int = 0;

    inline for (ifaces.items, 0..) |iface, i| {
        vtables[i] = VtableDirect(iface);
        nfield += @typeInfo(vtables[i]).Struct.fields.len;
    }

    comptime var allfields: [nfield + 1]StructField = undefined;
    // 0 for VtableHeader
    allfields[0] = StructField{
        .alignment = @alignOf(VtableHeader),
        .default_value = null,
        .is_comptime = false,
        .name = "#header",
        .type = VtableHeader,
    };

    // 1.. for vtables
    comptime var idx: comptime_int = 1;
    for (vtables) |vt| {
        const fields = @typeInfo(vt).Struct.fields;
        for (fields) |field| {
            allfields[idx] = field;
            idx += 1;
        }
    }
    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .decls = &.{},
            .is_tuple = false,
            .fields = allfields[0..idx],
        },
    });
}

pub fn tupleInit(any: anytype) Tuple {
    if (isTuple(any)) return any;

    if (@TypeOf(any) == @TypeOf(.{})) {
        return struct {
            pub const items = .{};
        };
    }

    return struct {
        pub const items = .{any};
    };
}

pub fn tupleAppend(tuple: Tuple, any: anytype) Tuple {
    if (isTuple(any)) {
        return struct {
            pub const items = tuple.items ++ any.items;
        };
    } else {
        return struct {
            pub const items = tuple.items ++ .{any};
        };
    }
}

pub fn tupleAppendUnique(tuple: Tuple, any: anytype) Tuple {
    comptime {
        var ret = tuple;
        for (tupleInit(any).items) |item| {
            if (!tupleHas(ret, item)) {
                ret = tupleAppend(ret, item);
            }
        }
        return ret;
    }
}

pub inline fn tupleHas(tuple: Tuple, any: anytype) bool {
    comptime {
        for (tuple.items) |item| {
            if (item == any) return true;
        }
        return false;
    }
}

pub fn typeInfo(any: anytype) *const TypeInfo {
    const T = @TypeOf(any);
    if (T == type) {
        return makeTypeInfo(any);
    } else if (isInterfaceType(T)) {
        const head: *VtableHeader = @ptrCast(@alignCast(any.vptr));
        return head.getTypeInfo();
    } else {
        return makeTypeInfo(@TypeOf(any));
    }
}

pub fn typeId(any: anytype) type_id {
    return typeInfo(any).typeid;
}

pub fn new(allocator: std.mem.Allocator, comptime T: type) !*T {
    if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.", .{@typeName(T)}));
    var self = try Klass(T).new(allocator);
    return self.ptr();
}

pub fn destroy(any: anytype) void {
    const T = @TypeOf(any);
    if (isInterfaceType(T)) {
        var pclass = Klass(struct {}).from(@ptrCast(@alignCast(any.ptr)));
        pclass.header.deinit(@ptrCast(pclass));
    } else {
        switch (@typeInfo(T)) {
            else => @compileError(compfmt("'{s}' is not a pointer to class/klass.", .{@typeName(@TypeOf(any))})),
            .Pointer => |p| {
                switch (p.size) {
                    else => @compileError(compfmt("'{s}' is not a pointer to class/klass.", .{@typeName(@TypeOf(any))})),
                    .One => {
                        if (isKlassType(p.child)) {
                            any.header.deinit(@ptrCast(any));
                        } else if (isClassType(p.child)) {
                            var self = Klass(p.child).from(any);
                            self.header.deinit(@ptrCast(self));
                        } else @compileError(compfmt("'{s}' is not a pointer to class/klass.", .{@typeName(@TypeOf(any))}));
                    },
                }
            },
        }
    }
}

pub fn make(comptime T: type, val: ?T) Klass(T) {
    return Klass(T).make(val);
}

pub fn vptr(iface: anytype) t: {
    if (!isInterfaceType(@TypeOf(iface))) @compileError(compfmt("'{s}' is not an interface type.", .{@typeName(@TypeOf(iface))}));
    break :t *const Vtable(@TypeOf(iface));
} {
    return @ptrCast(@alignCast(iface.vptr));
}

pub fn cast(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) T else switch (pointerType(@TypeOf(any))) {
        .read => *const T,
        .write => *T,
        else => @compileError(compfmt("zoop.cast(any, T): any must be interface or pointer to class but '{}'.", .{@typeName(@TypeOf(any))})),
    };
} {
    const V = @TypeOf(any);
    if (isInterfaceType(V)) {
        if (V == T) return any;

        if (isInterfaceType(T)) {
            // interface -> interface
            if (tupleHas(interfaces(V), T)) {
                return T{ .ptr = any.ptr, .vptr = typeInfo(any).getVtable.?(makeTypeId(T)).? };
            }
        }
    } else switch (@typeInfo(V)) {
        else => {},
        .Pointer => |p| {
            if (isClassType(p.child)) {
                if (isInterfaceType(T)) {
                    // class -> interface
                    if (tupleHas(interfaces(p.child), T)) {
                        return T{ .ptr = @ptrCast(@constCast(any)), .vptr = makeVtable(p.child, T) };
                    }
                } else if (isClassType(T)) {
                    // class -> class
                    if (p.child == T) return any;

                    if (tupleHas(classes(p.child), T)) {
                        return @ptrCast(@alignCast(any));
                    }
                }
            } else if (isKlassType(p.child)) {
                return cast(any.ptr(), T);
            }
        },
    }
    @compileError(compfmt("'{s}' can not cast to '{s}'", .{ @typeName(V), @typeName(T) }));
}

pub fn as(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) ?T else switch (pointerType(@TypeOf(any))) {
        .read => ?*const T,
        .write => ?*T,
        else => @compileError(compfmt("zoop.cast(any, T): any must be interface or pointer to class/klass but '{}'.", .{@typeName(@TypeOf(any))})),
    };
} {
    const V = @TypeOf(any);
    const ptr: *anyopaque = blk: {
        if (isInterfaceType(V)) break :blk any.ptr;
        if (isKlassType(std.meta.Child(V))) break :blk @ptrCast(any.ptr());
        if (isClassType(std.meta.Child(V))) break :blk @ptrCast(any);
        unreachable;
    };
    const typeinfo = Klass(struct {}).from(@ptrCast(ptr)).header.getTypeInfo();

    if (isInterfaceType(T)) {
        if (typeinfo.getVtable) |get| {
            if (get(makeTypeId(T))) |pvtable| {
                return T{ .ptr = ptr, .vptr = pvtable };
            }
        }
    }
    if (isClassType(T)) {
        if (typeinfo.isClass) |is| {
            if (is(makeTypeId(T))) {
                return @ptrCast(@alignCast(ptr));
            }
        }
    }

    return null;
}

pub fn nil(comptime I: type) I {
    comptime {
        if (!isInterfaceType(I)) @compileError(@typeName(I) ++ " is not interface.");
    }
    return Nil.of(I);
}

pub fn isNil(any: anytype) bool {
    comptime {
        if (!isInterfaceType(@TypeOf(any))) @compileError("zoop.isNil(any): any must be interface.");
    }
    return any.ptr == Nil.ptr();
}

//===== private content ======
fn StackBuf(comptime N: usize) type {
    return struct {
        buf: [N]u8 = undefined,
        pub fn print(self: *const @This(), comptime fmt: []const u8, args: anytype) []const u8 {
            var this = @constCast(self);
            return std.fmt.bufPrint(&this.buf, fmt, args) catch @panic("FMT ERROR");
        }
    };
}

const VtableHeader = struct {
    getTypeInfo: *const TypeInfoGetFunc,
};

fn vtableGetter(comptime T: type) ?*const VtableGetFunc {
    if (!isClassType(T)) return null;
    const ifaces = interfaces(T);
    const KV = struct { typeid: type_id, vtable: *anyopaque };
    comptime var kvs: [ifaces.items.len]KV = undefined;
    inline for (ifaces.items, 0..) |iface, i| {
        kvs[i] = .{ .typeid = makeTypeId(iface), .vtable = @ptrCast(makeVtable(T, iface)) };
    }
    return (struct {
        pub fn func(iface_id: type_id) ?*anyopaque {
            for (kvs) |kv| {
                if (kv.typeid == iface_id) return kv.vtable;
            }
            return null;
        }
    }).func;
}

fn classChecker(comptime T: type) ?*const ClassCheckFunc {
    if (!isClassType(T)) return null;
    return (struct {
        pub fn func(class_id: type_id) bool {
            inline for (classes(T).items) |class| {
                if (class_id == makeTypeId(class)) return true;
            }
            return false;
        }
    }).func;
}

inline fn makeTypeInfo(comptime T: type) *const TypeInfo {
    return &(struct {
        pub const info: TypeInfo = .{
            .typename = @typeName(T),
            .typeid = makeTypeId(T),
            .getVtable = vtableGetter(T),
            .isClass = classChecker(T),
        };
    }).info;
}

fn typeInfoGetter(comptime T: type) *const TypeInfoGetFunc {
    return &(struct {
        pub fn func() *const TypeInfo {
            return makeTypeInfo(T);
        }
    }).func;
}

fn makeTypeId(comptime T: type) type_id {
    return @ptrCast(&(struct {
        pub const val: ?*T = null;
    }).val);
}

fn makeVtable(comptime T: type, comptime I: type) *anyopaque {
    const VT = Vtable(I);
    return @constCast(&(struct {
        pub const vt: VT = blk: {
            var val: VT = undefined;
            @field(val, "#header") = VtableHeader{ .getTypeInfo = typeInfoGetter(T) };
            for (std.meta.fields(VT)[1..]) |field| {
                const MT = MethodType(T, field.name);
                if (MT != void) {
                    checkApi(T, I, field.name);
                    @field(val, field.name) = @ptrCast(&getMethod(T, field.name));
                } else if (field.default_value) |def_ptr| {
                    const defval = @as(*align(1) const field.type, @ptrCast(def_ptr)).*;
                    @field(val, field.name) = defval;
                } else {
                    @compileError(compfmt("{s} must implement method '{s}:{}'", .{ @typeName(T), field.name, field.type }));
                }
            }
            break :blk val;
        };
    }).vt);
}

/// Check whether the type of the function named field in T and the pointer of the function with the same name in VT match
fn checkApi(comptime T: type, comptime I: type, comptime field: []const u8) void {
    const VT = Vtable(I);
    const vtinfo = @typeInfo(@typeInfo(FieldType(VT, std.enums.nameCast(FieldEnum(VT), field))).Pointer.child);
    const tinfo = @typeInfo(MethodType(T, field));
    if (vtinfo.Fn.return_type.? != tinfo.Fn.return_type.?) @compileError(compfmt("'{s}.{s}' must return '{}' as same as '{s}.{s}'.", .{ @typeName(T), field, vtinfo.Fn.return_type.?, @typeName(I), field }));
    if (vtinfo.Fn.params.len != tinfo.Fn.params.len) @compileError(compfmt("parameters number of '{s}.{s}' must as same as '{s}.{s}'.", .{ @typeName(T), field, @typeName(I), field }));
    if (vtinfo.Fn.params.len < 2) return;
    for (vtinfo.Fn.params[1..], tinfo.Fn.params[1..], 1..) |vt, t, i| {
        const idx = i;
        if (vt.type.? != t.type.?) {
            @compileError(compfmt("The {d}th parameter of {s}.{s} must be '{}'.", .{ idx + 1, @typeName(T), field, vt.type.? }));
        }
    }
}

inline fn isInterfaceType(comptime T: type) bool {
    comptime {
        const fields = @typeInfo(IObject).Struct.fields;
        return switch (@typeInfo(T)) {
            else => false,
            .Struct => |s| s.fields.len == fields.len and
                s.fields[0].type == fields[0].type and
                s.fields[1].type == fields[1].type and
                std.mem.eql(u8, s.fields[0].name, fields[0].name) and
                std.mem.eql(u8, s.fields[1].name, fields[1].name),
        };
    }
}

inline fn isClassType(comptime T: type) bool {
    if (isKlassType(T)) return false;
    if (isInterfaceType(T)) return false;
    return switch (@typeInfo(T)) {
        else => false,
        .Struct => |s| !s.is_tuple,
    };
}

inline fn isKlassType(comptime T: type) bool {
    if (@typeInfo(T) == .Struct) {
        return @hasDecl(T, "#klass");
    }
    return false;
}

fn MethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        var Cur = T;
        while (Cur != void) {
            if (@hasDecl(Cur, name)) {
                const FT = @TypeOf(@field(Cur, name));
                if (@typeInfo(FT) == .Fn) {
                    return FT;
                }
            }
            const fields = @typeInfo(Cur).Struct.fields;
            if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct) {
                Cur = fields[0].type;
            } else {
                Cur = void;
            }
        }
        return void;
    }
}

fn getMethod(comptime T: type, comptime name: []const u8) MethodType(T, name) {
    comptime var Cur = T;
    while (Cur != void) {
        if (@hasDecl(Cur, name)) {
            const FT = @TypeOf(@field(Cur, name));
            if (@typeInfo(FT) == .Fn) {
                return @field(Cur, name);
            }
        }
        const fields = @typeInfo(Cur).Struct.fields;
        if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct) {
            Cur = fields[0].type;
        } else {
            Cur = void;
        }
    }
    return void;
}

inline fn isTuple(any: anytype) bool {
    if (@TypeOf(any) == type and @typeInfo(any) == .Struct) {
        if (!@hasDecl(any, "items")) return false;
        const T = @TypeOf(@field(any, "items"));
        return switch (@typeInfo(T)) {
            .Struct => |s| s.is_tuple,
            else => false,
        };
    } else {
        return false;
    }
}

fn interfaces(comptime T: type) Tuple {
    comptime var ret = tupleInit(IObject);

    if (isInterfaceType(T)) {
        ret = tupleAppendUnique(ret, T);
        if (@hasDecl(T, "extends")) {
            const extends = @field(T, "extends");
            switch (@typeInfo(@TypeOf(extends))) {
                else => {},
                .Struct => |s| {
                    if (s.is_tuple) {
                        inline for (extends) |iface| {
                            if (isInterfaceType(iface)) {
                                ret = tupleAppendUnique(ret, interfaces(iface));
                            } else @compileError(compfmt("{s} in {s}.extends but is not an interface type.", .{ @typeName(iface), @typeName(T) }));
                        }
                    }
                },
            }
        }
    } else if (isClassType(T)) {
        if (@hasDecl(T, "extends")) {
            const extends = @field(T, "extends");
            switch (@typeInfo(@TypeOf(extends))) {
                else => {},
                .Struct => |s| {
                    if (s.is_tuple) {
                        inline for (extends) |iface| {
                            if (isInterfaceType(iface)) {
                                ret = tupleAppend(ret, interfaces(iface));
                            } else @compileError(compfmt("{s} in {s}.extends but is not an interface type.", .{ @typeName(iface), @typeName(T) }));
                        }
                    }
                },
            }
        }
        const fields = std.meta.fields(T);
        if (fields.len > 0 and isClassType(fields[0].type)) {
            ret = tupleAppendUnique(ret, interfaces(fields[0].type));
        }
    }

    return ret;
}

fn classes(comptime T: type) Tuple {
    comptime var ret = tupleInit(.{});
    if (!isClassType(T)) return ret;

    comptime var Cur = T;
    while (Cur != void) {
        ret = tupleAppend(ret, Cur);
        const fields = std.meta.fields(Cur);
        if (fields.len > 0 and isClassType(fields[0].type)) {
            Cur = fields[0].type;
        } else {
            Cur = void;
        }
    }

    return ret;
}

fn VtableDirect(comptime I: type) type {
    if (I == IObject) return struct {};

    const decls = std.meta.declarations(I);
    comptime var fields: [decls.len]StructField = undefined;
    comptime var idx = 0;
    for (decls) |decl| {
        const info = @typeInfo(@TypeOf(@field(I, decl.name)));
        switch (info) {
            .Fn => |f| {
                if (f.params.len > 0 and f.params[0].type == I) {
                    fields[idx] = StructField{
                        .alignment = @alignOf(VtableFieldType(f)),
                        .default_value = null,
                        .is_comptime = false,
                        .name = decl.name,
                        .type = VtableFieldType(f),
                    };
                    idx += 1;
                }
            },
            else => {},
        }
    }
    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .decls = &.{},
            .is_tuple = false,
            .fields = fields[0..idx],
        },
    });
}

fn VtableFieldType(comptime F: std.builtin.Type.Fn) type {
    comptime var params: [F.params.len]std.builtin.Type.Fn.Param = undefined;
    params[0] = .{
        .is_generic = F.params[0].is_generic,
        .is_noalias = F.params[0].is_noalias,
        .type = *anyopaque,
    };
    for (1..F.params.len) |i| {
        params[i] = F.params[i];
    }
    return *const @Type(.{ .Fn = .{
        .params = params[0..],
        .return_type = F.return_type,
        .is_var_args = F.is_var_args,
        .is_generic = F.is_generic,
        .calling_convention = F.calling_convention,
    } });
}
inline fn pointerType(any: anytype) enum {
    no,
    read,
    write,
} {
    comptime {
        const T = @TypeOf(any);
        const info = if (T == type) @typeInfo(any) else @typeInfo(T);
        return switch (info) {
            else => .no,
            .Pointer => |p| if (p.is_const) .read else .write,
        };
    }
}
