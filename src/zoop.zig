const std = @import("std");
const builtin = @import("builtin");
const StructField = std.builtin.Type.StructField;
const FieldType = std.meta.FieldType;
const FieldEnum = std.meta.FieldEnum;
const Tuple = type;
const nameCast = std.enums.nameCast;
const compfmt = std.fmt.comptimePrint;
const assert = std.debug.assert;
const zoop = @This();

//===== public content ======
pub const alignment = @alignOf(KlassHeader);
pub const type_id = [*]const u8;
pub const ClassCheckFunc = fn (class_id: type_id) bool;

pub const HookFunc = *const fn (obj: IObject) void;
var destroy_hook_func: ?HookFunc = null;
var new_hook_func: ?HookFunc = null;

pub const ClassInfo = struct {
    pub const VtableInfo = struct {
        typeid: type_id,
        vtable: *anyopaque,
    };
    vtables: []const VtableInfo,
    /// class's typeinfo
    typeinfo: *const TypeInfo,
    /// offset to class in klass
    offset: usize,
    /// check if can cast to other class
    isClass: *const ClassCheckFunc,
    /// call class and all super classes's deinit() and free mem if need
    deinit: *const fn (pklass: *anyopaque) void,

    pub fn getVtable(self: *const ClassInfo, iface_typeid: type_id) ?*anyopaque {
        for (self.vtables) |*item| {
            if (item.typeid == iface_typeid) return item.vtable;
        }
        return null;
    }

    pub fn getVtableOf(self: *const ClassInfo, comptime T: type, comptime I: type) *Vtable(I) {
        comptime {
            if (!tupleHas(interfaces(T), I)) @compileError(compfmt("{s} don't support interface:{s}", .{ @typeName(T), @typeName(I) }));
        }
        assert((comptime interfaceIndex(T, I)) < self.vtables.len);
        return @ptrCast(@alignCast(self.vtables[comptime interfaceIndex(T, I)].vtable));
    }
};

pub const TypeInfo = struct {
    /// @typeNmae()
    typename: []const u8,
    /// typeid of the type
    typeid: type_id,
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

    pub fn formatAny(self: IObject, writer: std.io.AnyWriter) anyerror!void {
        try icall(self, .formatAny, .{writer});
    }

    pub fn format(self: *const IObject, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try (self.*).formatAny(if (@TypeOf(writer) == std.io.AnyWriter) writer else writer.any());
    }

    pub fn Default(comptime Class: type) type {
        return struct {
            pub fn formatAny(self: *Class, writer: std.io.AnyWriter) anyerror!void {
                try writer.print("{}", .{self});
            }
        };
    }
};

pub const IRaw = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn cast(self: IRaw, comptime I: type) I {
        return I{ .ptr = self.ptr, .vptr = self.vptr };
    }
};

/// Set up hooks to monitor the creation and destruction of objects on the heap
pub fn setHook(new_hook: ?HookFunc, destroy_hook: ?HookFunc) void {
    new_hook_func = new_hook;
    destroy_hook_func = destroy_hook;
}

/// get field of special type from any's inherit tree
pub fn getField(any: anytype, comptime name: []const u8, comptime T: type) t: {
    switch (pointerType(@TypeOf(any))) {
        else => {},
        .read => break :t *const T,
        .write => break :t *T,
    }
    @compileError(compfmt("zoop.getField(any): any must be *Class/*Klass", .{}));
} {
    const V = std.meta.Child(@TypeOf(any));
    if (comptime isKlassType(V)) return getField(&any.class, name, T);
    if (comptime isClassType(V)) {
        const offset = comptime fieldOffset(V, name, T);
        return @ptrFromInt(@intFromPtr(any) + offset);
    }
    @compileError(compfmt("{s} is not Class/Klass.", .{@typeName(V)}));
}

/// get method of Klass/Class
pub fn getMethod(comptime T: type, comptime name: []const u8) MethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an Klass/Class type.", .{@typeName(T)}));

        return (struct {
            pub const method = blk: {
                var Cur = if (isKlassType(T)) T.Class else T;
                while (Cur != void and isClassType(Cur)) {
                    if (@hasDecl(Cur, name)) {
                        const FT = @TypeOf(@field(Cur, name));
                        if (@typeInfo(FT) == .Fn) {
                            break :blk @field(Cur, name);
                        }
                    }
                    const fields = @typeInfo(Cur).Struct.fields;
                    if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct) {
                        Cur = fields[0].type;
                    } else {
                        Cur = void;
                    }
                }
                break :blk void;
            };
        }).method;
    }
}

/// get method of Klass/Class' super class
pub fn getUpMethod(comptime T: type, comptime name: []const u8) UpMethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an Klass/Class type.", .{@typeName(T)}));

        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const fields = @typeInfo(Class).Struct.fields;
            if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct and isClassType(fields[0].type)) {
                break :blk fields[0].type;
            } else {
                break :blk void;
            }
        };
        return getMethod(Super, name);
    }
}

pub const KlassHeader = if (builtin.mode == .Debug) packed struct {
    const kmagic: u32 = 0xaabbccdd;
    magic: u32 = kmagic,
    info: *const ClassInfo,
    allocator: *const fn (*anyopaque) ?std.mem.Allocator,
} else packed struct {
    info: *const ClassInfo,
    allocator: *const fn (*anyopaque) ?std.mem.Allocator,
};

pub fn Klass(comptime T: type) type {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.(check if @alignOf({s}) == zoop.alignment)", .{ @typeName(T), @typeName(T) }));

        return struct {
            pub const @"#klass" = true;
            pub const class_offset: usize = blk: {
                const pklass: *allowzero @This() = @ptrFromInt(0);
                break :blk @intFromPtr(&pklass.class);
            };
            pub const Class = T;
            header: KlassHeader,
            allocator: ?std.mem.Allocator = null,
            class: T,

            pub fn new(allocator: std.mem.Allocator, init: ?T) !*@This() {
                var self = try allocator.create(@This());
                self.header = .{ .info = comptime makeClassInfo(T), .allocator = @ptrCast(&getAlly) };
                self.allocator = allocator;
                if (init) |v| {
                    self.class = v;
                } else {
                    initClass(&self.class);
                }
                if (new_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                return self;
            }

            pub fn make(init: ?T) @This() {
                var self: @This() = undefined;
                self.header = .{ .info = comptime makeClassInfo(T), .allocator = @ptrCast(&getAlly) };
                self.allocator = null;
                if (init) |v| {
                    self.class = v;
                } else {
                    initClass(&self.class);
                }
                if (new_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                return self;
            }

            pub fn from(pclass: *const T) *@This() {
                const self: *@This() = @ptrFromInt(@intFromPtr(pclass) - class_offset);
                if (comptime builtin.mode == .Debug) {
                    assert(self.header.magic == KlassHeader.kmagic);
                }
                return self;
            }

            pub fn ptr(self: *@This()) *Class {
                return &self.class;
            }

            pub fn deinit(self: *@This()) void {
                if (comptime builtin.mode == .Debug) {
                    assert(self.header.magic == KlassHeader.kmagic);
                    assert(self.header.info == comptime makeClassInfo(T));
                }
                if (destroy_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                inline for (comptime classes(T).items) |V| {
                    if (comptime @hasDecl(V, "deinit")) {
                        var p: *V = @ptrCast(&self.class);
                        p.deinit();
                    }
                }
                if (self.allocator) |dtor| {
                    dtor.destroy(self);
                }
            }

            fn getAlly(self: *@This()) ?std.mem.Allocator {
                return self.allocator;
            }
        };
    }
}

pub fn ApiEnum(comptime I: type) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));

        const decls = std.meta.declarations(I);
        var apis: [decls.len]std.builtin.Type.Declaration = undefined;
        var idx = 0;
        for (std.meta.declarations(I)) |decl| {
            const info = @typeInfo(@TypeOf(@field(I, decl.name)));
            if (info == .Fn and info.Fn.params.len > 0 and !info.Fn.is_generic) {
                const first = info.Fn.params[0].type orelse unreachable;
                const Self = switch (@typeInfo(first)) {
                    else => void,
                    .Struct => first,
                    .Pointer => std.meta.Child(first),
                };
                if (Self == I) {
                    if (!hasDecl(apis[0..idx], decl)) {
                        apis[idx] = decl;
                        idx += 1;
                    }
                }
            }
        }

        if (idx == 0) {
            return @Type(.{
                .Enum = .{
                    .tag_type = u0,
                    .fields = &.{},
                    .decls = &.{},
                    .is_exhaustive = true,
                },
            });
        }

        var fields: [idx]std.builtin.Type.EnumField = undefined;
        for (0..idx) |i| {
            fields[i] = .{
                .name = apis[i].name,
                .value = i,
            };
        }
        return @Type(.{
            .Enum = .{
                .tag_type = std.math.IntFittingRange(0, fields.len - 1),
                .fields = &fields,
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }
}

pub fn MethodEnum(comptime T: type) type {
    comptime {
        const Class = if (isKlassType(T)) T.Class else if (isClassType(T) or isInterfaceType(T)) T else @compileError(compfmt("{s} is not Klass/Class/Interface.", .{@typeName(T)}));

        const supers = if (isInterfaceType(T)) interfaces(T) else classes(Class);
        const count = blk: {
            var n = 0;
            for (supers.items) |super| {
                n += std.meta.declarations(super).len;
            }
            break :blk n;
        };
        var methods: [count]std.builtin.Type.Declaration = undefined;
        var idx = 0;
        for (supers.items) |super| {
            for (std.meta.declarations(super)) |decl| {
                const info = @typeInfo(@TypeOf(@field(super, decl.name)));
                if (info == .Fn and info.Fn.params.len > 0 and !info.Fn.is_generic) {
                    const first = info.Fn.params[0].type.?;
                    const Self = switch (@typeInfo(first)) {
                        else => void,
                        .Struct => first,
                        .Pointer => std.meta.Child(first),
                    };
                    if (Self == super and !hasDecl(methods[0..idx], decl)) {
                        methods[idx] = decl;
                        idx += 1;
                    }
                }
            }
        }

        if (idx == 0) {
            return @Type(.{
                .Enum = .{
                    .tag_type = u0,
                    .fields = &.{},
                    .decls = &.{},
                    .is_exhaustive = true,
                },
            });
        }

        var fields: [idx]std.builtin.Type.EnumField = undefined;
        for (0..idx) |i| {
            fields[i] = .{
                .name = methods[i].name,
                .value = i,
            };
        }
        return @Type(.{
            .Enum = .{
                .tag_type = std.math.IntFittingRange(0, fields.len - 1),
                .fields = &fields,
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }
}

pub fn Vtable(comptime I: type) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));
        const ifaces = interfaces(I);
        var vtables: [ifaces.items.len]type = undefined;
        // ifaces.items.len - 1 for saving pointer to super interfaces vtable, -1 for I itself,
        // for making interface to interface casting faster.
        var nfield: comptime_int = ifaces.items.len - 1;

        for (ifaces.items, 0..) |iface, i| {
            vtables[i] = VtableDirect(iface);
            nfield += @typeInfo(vtables[i]).Struct.fields.len;
        }

        var allfields: [nfield]StructField = undefined;
        var idx: comptime_int = 0;
        for (ifaces.items) |iface| {
            if (iface != I) {
                allfields[idx] = StructField{
                    .alignment = @alignOf(*Vtable(iface)),
                    .default_value = null,
                    .is_comptime = false,
                    .name = @typeName(iface),
                    .type = *Vtable(iface),
                };
                idx += 1;
            }
        }
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
}

pub fn tupleInit(comptime any: anytype) Tuple {
    comptime {
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
}

pub fn tupleAppend(comptime tuple: Tuple, comptime any: anytype) Tuple {
    comptime {
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
}

pub fn tupleAppendUnique(comptime tuple: Tuple, comptime any: anytype) Tuple {
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

pub inline fn tupleHas(comptime tuple: Tuple, comptime any: anytype) bool {
    comptime {
        @setEvalBranchQuota(3000);
        for (tuple.items) |item| {
            if (@TypeOf(item) == @TypeOf(any)) {
                if (@TypeOf(any) == [:0]const u8 or @TypeOf(any) == []const u8) {
                    if (std.mem.eql(u8, item, any)) return true;
                } else if (item == any) return true;
            }
        }
        return false;
    }
}

pub fn typeInfo(any: anytype) *const TypeInfo {
    if (comptime @TypeOf(any) == type) {
        return comptime makeTypeInfo(any);
    } else {
        return comptime makeTypeInfo(@TypeOf(any));
    }
}

pub fn classInfo(any: anytype) *const ClassInfo {
    return switch (@typeInfo(@TypeOf(any))) {
        else => ClassInfoGetter(@TypeOf(any)).get(any),
        .Type => comptime makeClassInfo(any),
    };
}

pub fn typeId(any: anytype) type_id {
    if (comptime @TypeOf(any) == type) {
        return comptime makeTypeId(any);
    } else {
        return comptime makeTypeId(@TypeOf(any));
    }
}

pub fn new(allocator: std.mem.Allocator, comptime T: type, init: ?T) !*align(alignment) T {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.", .{@typeName(T)}));
    }
    var self = try Klass(T).new(allocator, init);
    return &self.class;
}

pub fn destroy(any: anytype) void {
    const T = comptime @TypeOf(any);
    if (comptime isInterfaceType(T)) {
        var header: *KlassHeader = @ptrCast(@alignCast(any.ptr));
        return header.info.deinit(@ptrCast(header));
    } else {
        switch (comptime @typeInfo(T)) {
            else => {},
            .Pointer => |p| {
                switch (p.size) {
                    else => {},
                    .One => {
                        if (comptime isKlassType(p.child)) {
                            return any.header.info.deinit(@ptrCast(any));
                        } else if (comptime isClassType(p.child)) {
                            var klass = Klass(p.child).from(any);
                            return klass.header.info.deinit(@ptrCast(klass));
                        }
                    },
                }
            },
        }
    }
    @compileError(compfmt("'{s}' is not a *Klass/*Class.", .{@typeName(@TypeOf(any))}));
}

pub fn make(comptime T: type, init: ?T) Klass(T) {
    return Klass(T).make(init);
}

/// call interface method
/// example: zoop.icall(ihuman, .getAge, .{})
pub fn icall(iface: anytype, comptime api_enum: ApiEnum(@TypeOf(iface)), args: anytype) ReturnType(@TypeOf(iface), api_enum) {
    comptime {
        if (!isInterfaceType(@TypeOf(iface))) @compileError(compfmt("'{s}' is not an interface type.", .{@typeName(@TypeOf(iface))}));
    }
    const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(iface.ptr));
    const vptr: *const Vtable(@TypeOf(iface)) = @ptrFromInt(@intFromPtr(iface.vptr));
    const ptr: *anyopaque = @ptrFromInt(@intFromPtr(pklass) + pklass.header.info.offset);
    return @call(.auto, @field(vptr, @tagName(api_enum)), .{ptr} ++ args);
}

/// call interface method of Klass/Class. (virtual call)
/// example: zoop.vcall(pclass, IHuman.getName, .{})
pub fn vcall(any: anytype, comptime method: anytype, args: anytype) ApiInfo(method).Return {
    comptime {
        var pass = false;
        switch (@typeInfo(@TypeOf(any))) {
            else => {},
            .Struct => pass = isInterfaceType(@TypeOf(any)),
            .Pointer => |p| {
                pass = isKlassType(p.child) or isClassType(p.child);
            },
        }
        if (!pass) @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
        if (!isInterfaceType(ApiInfo(method).Iface)) @compileError(compfmt("'{s}'' is not Interface.", .{@typeName(ApiInfo(method).Iface)}));
    }
    const info = comptime ApiInfo(method);
    if (comptime canCast(@TypeOf(any), info.Iface)) {
        return icall(zoop.cast(any, info.Iface), info.name, args);
    }
    if (zoop.as(any, info.Iface)) |iface| {
        return icall(iface, info.name, args);
    }
    @panic(StackBuf(2048).init().print("{s} don't support method: {s}.{s}", .{ @typeName(@TypeOf(any)), @typeName(info.Iface), @tagName(info.name) }));
}

/// call super class's method.
/// example: zoop.upcall(pclass, .methodOfSuperClass, .{});
pub fn upcall(any: anytype, comptime method_enum: MethodEnum(std.meta.Child(@TypeOf(any))), args: anytype) t: {
    const T = @TypeOf(any);
    switch (@typeInfo(T)) {
        else => {},
        .Pointer => |p| {
            if (isKlassType(p.child) or isClassType(p.child)) {
                const Class = if (isKlassType(p.child)) p.child.Class else p.child;
                const MT = UpMethodType(Class, @tagName(method_enum));
                if (MT != void) {
                    break :t @typeInfo(MT).Fn.return_type orelse void;
                } else {
                    @compileError(compfmt("no method named '{s}' founded in inherit tree of {s}.", .{ @tagName(method_enum), @typeName(Class) }));
                }
            }
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class.", .{@typeName(@TypeOf(any))}));
} {
    const T = comptime std.meta.Child(@TypeOf(any));
    const Class = comptime if (isKlassType(T)) T.Class else T;
    const method = comptime getUpMethod(Class, @tagName(method_enum));
    const Ptr = comptime @typeInfo(@TypeOf(method)).Fn.params[0].type.?;
    const ptr: Ptr = @ptrCast(if (comptime isKlassType(T)) &any.class else any);
    return @call(.auto, method, .{ptr} ++ args);
}

pub inline fn isRootPtr(ptr: anytype) bool {
    const T = comptime std.meta.Child(@TypeOf(ptr));
    if (comptime isClassType(T)) {
        return classInfo(ptr) == comptime makeClassInfo(T);
    } else if (comptime isKlassType(T)) {
        return classInfo(ptr) == comptime makeClassInfo(T.Class);
    }
    @compileError(compfmt("{s} is not *Klass/*Class.", .{@typeName(T)}));
}

pub fn getAllocator(any: anytype) ?std.mem.Allocator {
    const T = comptime @TypeOf(any);
    switch (comptime @typeInfo(T)) {
        else => {},
        .Struct => {
            if (comptime isInterfaceType(T)) {
                const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(any.ptr));
                return pklass.header.allocator(any.ptr);
            }
        },
        .Pointer => |p| {
            if (comptime p.size == .One) {
                if (comptime isKlassType(p.child)) {
                    return any.allocator;
                } else if (comptime isClassType(p.child)) {
                    const pklass = Klass(p.child).from(any);
                    return pklass.header.allocator(@ptrFromInt(@intFromPtr(pklass)));
                }
            }
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
}

pub fn cast(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) T else switch (pointerType(@TypeOf(any))) {
        .read => *const T,
        .write => *T,
        else => @compileError(compfmt("'{s}' is not *Class/interface.", .{@typeName(@TypeOf(any))})),
    };
} {
    const caster = comptime Caster(@TypeOf(any), T);
    if (comptime caster != void) {
        return caster.cast(any, T);
    }
    @compileError(compfmt("'{s}' can not cast to '{s}'", .{ @typeName(@TypeOf(any)), @typeName(T) }));
}

pub fn as(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) ?T else switch (pointerType(@TypeOf(any))) {
        .read => ?*const T,
        .write => ?*T,
        else => if (isInterfaceType(@TypeOf(any))) ?*T else @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))})),
    };
} {
    const V = comptime @TypeOf(any);
    const ptr: *anyopaque = blk: {
        if (comptime isInterfaceType(V)) break :blk @alignCast(any.ptr);
        if (comptime isKlassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(any));
        if (comptime isClassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(Klass(std.meta.Child(V)).from(any)));
        unreachable;
    };
    const info = classInfo(any);

    if (comptime isInterfaceType(T)) {
        if (info.getVtable(comptime makeTypeId(T))) |pvtable| {
            return T{ .ptr = ptr, .vptr = pvtable };
        }
    }
    if (comptime isClassType(T)) {
        if (info.isClass(comptime makeTypeId(T))) {
            const klass: *Klass(T) = @ptrCast(@alignCast(ptr));
            return &klass.class;
        }
    }

    return null;
}

pub fn asptr(any: anytype) *anyopaque {
    switch (comptime @typeInfo(@TypeOf(any))) {
        else => {},
        .Struct => {
            if (comptime isInterfaceType(@TypeOf(any))) {
                return any.ptr;
            }
        },
        .Pointer => |p| {
            if (comptime isKlassType(p.child)) return @ptrCast(any);
            if (comptime isClassType(p.child)) return @ptrCast(Klass(p.child).from(any));
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
}

pub fn nil(comptime I: type) I {
    comptime {
        if (!isInterfaceType(I)) @compileError(@typeName(I) ++ " is not Interface.");
    }
    return Nil.of(I);
}

pub inline fn isNil(any: anytype) bool {
    comptime {
        if (!isInterfaceType(@TypeOf(any))) @compileError(compfmt("'{s}' is not interface", .{@typeName(@TypeOf(any))}));
    }
    return any.ptr == Nil.ptr();
}

//===== private content ======
inline fn hasDecl(comptime decls: []std.builtin.Type.Declaration, comptime decl: std.builtin.Type.Declaration) bool {
    comptime {
        @setEvalBranchQuota(5000);
        for (decls) |d| {
            if (std.mem.eql(u8, d.name, decl.name)) return true;
        }
        return false;
    }
}

inline fn isMethod(comptime T: type, comptime name: []const u8) bool {
    comptime {
        if (@hasDecl(T, name)) {
            const FT = @TypeOf(@field(T, name));
            switch (@typeInfo(FT)) {
                else => {},
                .Fn => |f| {
                    if (f.params.len > 0) {
                        return RealType(f.params[0].type.?) == T;
                    }
                },
            }
        }
        return false;
    }
}

fn RealType(comptime T: type) type {
    comptime {
        return switch (@typeInfo(T)) {
            else => T,
            .Pointer => std.meta.Child(T),
        };
    }
}

fn DefaultMethodType(comptime T: type, comptime I: type, comptime name: []const u8) type {
    comptime {
        const ifaces = interfaces(I);
        var all = tupleInit(.{});
        for (ifaces.items) |iface| {
            if (@hasDecl(iface, "Default")) {
                const def = iface.Default(T);
                if (@hasDecl(def, name)) {
                    all = tupleAppend(all, iface);
                }
            }
        }
        switch (all.items.len) {
            0 => {},
            1 => return @TypeOf(@field(all.items[0].Default(T), name)),
            else => @compileError(compfmt("multi default method '{s}' found in: {}", .{ name, all.items })),
        }

        return void;
    }
}

fn defaultMethod(comptime T: type, comptime I: type, comptime name: []const u8) DefaultMethodType(T, I, name) {
    comptime {
        const ifaces = interfaces(I);
        for (ifaces.items) |iface| {
            if (@hasDecl(iface, "Default")) {
                const def = iface.Default(T);
                if (@hasDecl(def, name)) {
                    return @field(def, name);
                }
            }
        }
        unreachable;
    }
}

fn MethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        var Cur = if (isKlassType(T)) T.Class else T;
        while (Cur != void) {
            if (isMethod(Cur, name)) return @TypeOf(@field(Cur, name));

            const fields = @typeInfo(Cur).Struct.fields;
            if (fields.len > 0 and isClassType(RealType(fields[0].type))) {
                Cur = fields[0].type;
            } else {
                Cur = void;
            }
        }
        return void;
    }
}

fn UpMethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const fields = @typeInfo(Class).Struct.fields;
            if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct and isClassType(fields[0].type)) {
                break :blk fields[0].type;
            } else {
                break :blk void;
            }
        };
        return if (Super == void) void else MethodType(Super, name);
    }
}

fn ReturnType(comptime I: type, comptime method: ApiEnum(I)) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));
        return @typeInfo(@TypeOf(@field(I, @tagName(method)))).Fn.return_type orelse void;
    }
}

fn ApiInfo(comptime method: anytype) type {
    comptime {
        const info = @typeInfo(@TypeOf(method));
        if (info != .Fn) @compileError("method is not a .Fn");
        if (info.Fn.params.len == 0 or !isInterfaceType(info.Fn.params[0].type.?))
            @compileError(compfmt("{s} is not an interface type.", .{@typeName(info.Fn.params[0].type.?)}));

        const I = info.Fn.params[0].type orelse unreachable;
        const method_name = blk: {
            for (std.meta.declarations(I)) |decl| {
                if (@TypeOf(@field(I, decl.name)) == @TypeOf(method)) {
                    if (@field(I, decl.name) == method)
                        break :blk decl.name;
                }
            }
            unreachable;
        };
        return struct {
            pub const Iface = I;
            pub const Return = @typeInfo(@TypeOf(method)).Fn.return_type orelse void;
            pub const name = nameCast(ApiEnum(I), method_name);
        };
    }
}

fn Caster(comptime V: type, comptime T: type) type {
    comptime {
        if (isInterfaceType(V)) {
            if (isInterfaceType(T)) {
                if (tupleHas(interfaces(V), T)) {
                    // interface -> interface
                    return struct {
                        pub fn cast(any: anytype, comptime I: type) I {
                            if (V == I) return any;
                            const vtable: *Vtable(V) = @ptrFromInt(@intFromPtr(any.vptr));
                            return T{ .ptr = any.ptr, .vptr = @field(vtable, @typeName(I)) };
                        }
                    };
                }
            }
        } else switch (@typeInfo(V)) {
            else => {},
            .Pointer => |p| {
                if (isClassType(p.child)) {
                    if (isInterfaceType(T)) {
                        if (tupleHas(interfaces(p.child), T)) {
                            // class -> interface
                            return struct {
                                pub fn cast(any: anytype, comptime I: type) I {
                                    const pklass = Klass(p.child).from(any);
                                    return I{ .ptr = @ptrCast(pklass), .vptr = pklass.header.info.getVtableOf(p.child, I) };
                                }
                            };
                        }
                    } else if (isClassType(T)) {
                        if (tupleHas(classes(p.child), T)) {
                            // class -> class
                            return struct {
                                pub fn cast(any: anytype, comptime C: type) t: {
                                    break :t switch (pointerType(V)) {
                                        else => unreachable,
                                        .read => *const C,
                                        .write => *C,
                                    };
                                } {
                                    if (p.child == C) {
                                        return any;
                                    } else {
                                        return @ptrFromInt(@intFromPtr(any));
                                    }
                                }
                            };
                        }
                    }
                } else if (isKlassType(p.child)) {
                    // klass -> T
                    const caster = switch (pointerType(V)) {
                        else => unreachable,
                        .read => Caster(*const p.child.Class, T),
                        .write => Caster(*p.child.Class, T),
                    };
                    return struct {
                        pub fn cast(any: anytype, comptime ANY: type) @TypeOf(caster.cast(&any.class, ANY)) {
                            return caster.cast(&any.class, ANY);
                        }
                    };
                }
            },
        }
        return void;
    }
}

fn ClassInfoGetter(comptime T: type) type {
    comptime {
        switch (@typeInfo(T)) {
            else => {},
            .Type => {
                if (isKlassType(T)) {
                    return struct {
                        pub fn get(_: anytype) *const ClassInfo {
                            return comptime makeClassInfo(T.Class);
                        }
                    };
                }
                if (isClassType(T)) {
                    return struct {
                        pub fn get(_: anytype) *const ClassInfo {
                            return comptime makeClassInfo(T);
                        }
                    };
                }
            },
            .Pointer => |p| {
                if (isKlassType(p.child)) {
                    return struct {
                        pub fn get(klass: anytype) *const ClassInfo {
                            return klass.header.info;
                        }
                    };
                }
                if (isClassType(p.child)) {
                    return struct {
                        pub fn get(class: anytype) *const ClassInfo {
                            return Klass(p.child).from(class).header.info;
                        }
                    };
                }
            },
            .Struct => {
                if (isInterfaceType(T)) {
                    return struct {
                        pub fn get(iface: anytype) *const ClassInfo {
                            const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(iface.ptr));
                            return pklass.header.info;
                        }
                    };
                }
            },
        }
        @compileError(compfmt("'{s}' is not Class/*Class/Klass/*Klass/Interface.", .{@typeName(T)}));
    }
}

fn defaultFields(comptime T: type) []StructField {
    comptime {
        const allfields = std.meta.fields(T);
        var fields: [allfields.len]StructField = undefined;
        var idx = 0;
        for (allfields) |field| {
            if (field.default_value != null) {
                fields[idx] = field;
                idx += 1;
            }
        }
        const ret = fields[0..idx];
        return ret;
    }
}

fn initDefaultFields(pclass: anytype) void {
    const T = comptime std.meta.Child(@TypeOf(pclass));
    inline for (comptime defaultFields(T)) |field| {
        const ptr: *const field.type = @ptrCast(@alignCast(field.default_value.?));
        @field(pclass, field.name) = ptr.*;
    }
}

fn initClass(pclass: anytype) void {
    const T = comptime std.meta.Child(@TypeOf(pclass));
    const supers = comptime classes(T);
    inline for (supers.items) |V| {
        const pv: *V = @ptrCast(@alignCast(pclass));
        initDefaultFields(pv);
    }
}

fn fieldOffset(comptime T: type, comptime name: []const u8, comptime FT: type) usize {
    comptime {
        const supers = classes(T);
        var owners = tupleInit(.{});
        for (supers.items) |V| {
            if (@hasField(V, name)) {
                if (FieldType(V, nameCast(FieldEnum(V), name)) == FT) {
                    owners = tupleAppend(owners, V);
                }
            }
        }
        switch (owners.items.len) {
            0 => @compileError(compfmt("no field of type '{s}' named '{s}' in '{s}'", .{ @typeName(FT), name, @typeName(T) })),
            1 => {
                const V = owners.items[0];
                const pv: *allowzero V = @ptrFromInt(0);
                const pf = &@field(pv, name);
                return @intFromPtr(pf);
            },
            else => @compileError(compfmt("more than one field of type '{s}' named '{s}' found: {}", .{ @typeName(FT), name, owners.items })),
        }
    }
}

fn classChecker(comptime T: type) *const ClassCheckFunc {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("'{s}' is not Class.", .{@typeName(T)}));

        return (struct {
            pub fn func(class_id: type_id) bool {
                inline for (comptime classes(T).items) |Class| {
                    if (class_id == comptime makeTypeId(Class)) return true;
                }
                return false;
            }
        }).func;
    }
}

fn makeClassVtables(comptime T: type) []const ClassInfo.VtableInfo {
    comptime {
        const ifaces = interfaces(T);
        return @ptrCast((struct {
            pub const val: [ifaces.items.len]ClassInfo.VtableInfo = blk: {
                var vtables: [ifaces.items.len]ClassInfo.VtableInfo = undefined;
                for (ifaces.items, 0..) |iface, i| {
                    vtables[i] = .{
                        .typeid = makeTypeId(iface),
                        .vtable = @ptrCast(makeVtable(T, iface)),
                    };
                }
                break :blk vtables;
            };
        }).val[0..]);
    }
}

fn makeClassInfo(comptime T: type) *const ClassInfo {
    comptime {
        return &(struct {
            pub const info: ClassInfo = .{
                .vtables = makeClassVtables(T),
                .offset = Klass(T).class_offset,
                .typeinfo = makeTypeInfo(T),
                .isClass = classChecker(T),
                .deinit = @ptrCast(&Klass(T).deinit),
            };
        }).info;
    }
}

fn makeTypeInfo(comptime T: type) *const TypeInfo {
    comptime {
        return &(struct {
            pub const info: TypeInfo = .{
                .typename = @typeName(T),
                .typeid = makeTypeId(T),
            };
        }).info;
    }
}

fn makeTypeId(comptime T: type) type_id {
    comptime {
        return (struct {
            pub const name = @typeName(T);
        }).name.ptr;
    }
}

fn makeVtable(comptime T: type, comptime I: type) *anyopaque {
    comptime {
        if (!isKlassType(T) and !isClassType(T)) @compileError(compfmt("{s} is not Klass/Class.", .{@typeName(T)}));
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not interface.", .{@typeName(I)}));

        const VT = Vtable(I);
        const ifaces = interfaces(I);
        const nsuper = ifaces.items.len - 1;
        return @constCast(&(struct {
            pub const vt: VT = blk: {
                var val: VT = undefined;
                for (ifaces.items) |iface| {
                    if (iface != I) {
                        @field(val, @typeName(iface)) = @ptrCast(@alignCast(makeVtable(T, iface)));
                    }
                }
                for (std.meta.fields(VT)[nsuper..]) |field| {
                    const MT = MethodType(T, field.name);
                    if (MT != void) {
                        checkApi(T, I, field.name);
                        @field(val, field.name) = @ptrCast(&getMethod(T, field.name));
                    } else if (DefaultMethodType(T, I, field.name) != void) {
                        checkInterface(I);
                        @field(val, field.name) = @ptrCast(&defaultMethod(T, I, field.name));
                    } else {
                        @compileError(compfmt("{s} must implement method '{s}: {}'", .{ @typeName(T), field.name, field.type }));
                    }
                }
                break :blk val;
            };
        }).vt);
    }
}

fn checkInterface(comptime I: type) void {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not Interface.", .{@typeName(I)}));
        if (@hasDecl(I, "Default")) {
            const field = @field(I, "Default");
            switch (@typeInfo(@TypeOf(field))) {
                else => @compileError("{s}.Default must be: fn (comptime T:type) type"),
                .Fn => |info| {
                    if (!info.is_generic or info.return_type != type)
                        @compileError("{s}.Default must be: fn (comptime T:type) type");
                },
            }
            const methods = I.Default(struct {});
            if (@typeInfo(methods) != .Struct) {
                @compileError(compfmt("{s}.Default() must return a struct but:{}", .{ @typeName(I), methods }));
            }
            for (std.meta.declarations(methods)) |decl| {
                if (!@hasDecl(I, decl.name))
                    @compileError(compfmt("'{s}' is not a '{s}' api but in '{s}.Default'.", .{ decl.name, @typeName(I), @typeName(I) }));
                const definfo = @typeInfo(@TypeOf(@field(methods, decl.name)));
                const info = @typeInfo(@TypeOf(@field(I, decl.name)));
                var fail = false;
                if (definfo == .Fn and info == .Fn) {
                    if (definfo.Fn.params.len == info.Fn.params.len and definfo.Fn.params.len > 0) {
                        for (1..definfo.Fn.params.len) |i| {
                            if (definfo.Fn.params[i].type != info.Fn.params[i].type) {
                                fail = true;
                                break;
                            }
                        }
                    } else fail = true;
                } else fail = true;
                if (fail) {
                    @compileError(compfmt("type missmatch: {s}.{s}, {s}.Default.{s}", .{ @typeName(I), decl.name, @typeName(I), decl.name }));
                }
            }
        }
    }
}

/// Check whether the type of the function named field in T and the pointer of the function with the same name in VT match
fn checkApi(comptime T: type, comptime I: type, comptime field: []const u8) void {
    comptime {
        @setEvalBranchQuota(5000);
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
}

pub inline fn isInterfaceType(comptime T: type) bool {
    const fields = comptime @typeInfo(IObject).Struct.fields;
    return comptime blk: {
        switch (@typeInfo(T)) {
            else => break :blk false,
            .Struct => |s| break :blk s.fields.len == fields.len and
                s.fields[0].type == fields[0].type and
                s.fields[1].type == fields[1].type and
                std.mem.eql(u8, s.fields[0].name, fields[0].name) and
                std.mem.eql(u8, s.fields[1].name, fields[1].name),
        }
    };
}

pub inline fn isClassType(comptime T: type) bool {
    return comptime blk: {
        if (isKlassType(T)) break :blk false;
        if (isInterfaceType(T)) break :blk false;
        break :blk switch (@typeInfo(T)) {
            else => false,
            .Struct => |s| !s.is_tuple and s.fields.len > 0 and s.fields[0].alignment == alignment,
        };
    };
}

pub inline fn isKlassType(comptime T: type) bool {
    return comptime blk: {
        if (@typeInfo(T) == .Struct) {
            break :blk @hasDecl(T, "#klass");
        } else break :blk false;
    };
}

pub inline fn isTuple(any: anytype) bool {
    return comptime blk: {
        if (@TypeOf(any) == type and @typeInfo(any) == .Struct) {
            if (!@hasDecl(any, "items")) break :blk false;
            const T = @TypeOf(@field(any, "items"));
            break :blk switch (@typeInfo(T)) {
                .Struct => |s| s.is_tuple,
                else => false,
            };
        } else {
            break :blk false;
        }
    };
}

pub inline fn interfaceIndex(comptime T: type, comptime I: type) usize {
    return comptime blk: {
        const ifaces = interfaces(T);
        for (ifaces.items, 0..) |iface, i| {
            if (iface == I) break :blk i;
        }
        @compileError(compfmt("{s} don't support interface:{s}", .{ @typeName(T), @typeName(I) }));
    };
}

fn interfaces(comptime T: type) Tuple {
    comptime {
        var ret = tupleInit(IObject);

        if (isInterfaceType(T)) {
            if (@hasDecl(T, "extends")) {
                const extends = @field(T, "extends");
                switch (@typeInfo(@TypeOf(extends))) {
                    else => {},
                    .Struct => |s| {
                        if (s.is_tuple) {
                            for (extends) |iface| {
                                if (isInterfaceType(iface)) {
                                    ret = tupleAppendUnique(ret, interfaces(iface));
                                } else @compileError(compfmt("{s} in {s}.extends but is not an interface type.", .{ @typeName(iface), @typeName(T) }));
                            }
                        }
                    },
                }
            }
            ret = tupleAppendUnique(ret, T);
        } else if (isClassType(T)) {
            const fields = std.meta.fields(T);
            if (fields.len > 0 and isClassType(fields[0].type)) {
                ret = tupleAppendUnique(ret, interfaces(fields[0].type));
            }
            if (@hasDecl(T, "extends")) {
                const extends = @field(T, "extends");
                switch (@typeInfo(@TypeOf(extends))) {
                    else => {},
                    .Struct => |s| {
                        if (s.is_tuple) {
                            for (extends) |iface| {
                                if (isInterfaceType(iface)) {
                                    ret = tupleAppendUnique(ret, interfaces(iface));
                                } else @compileError(compfmt("{s} in {s}.extends but is not Interface.", .{ @typeName(iface), @typeName(T) }));
                            }
                        }
                    },
                }
            }
        }

        return ret;
    }
}

fn classes(comptime T: type) Tuple {
    comptime {
        var ret = tupleInit(.{});
        if (!isClassType(T)) return ret;

        var Cur = T;
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
}

inline fn isExclude(comptime I: type, comptime method: []const u8) bool {
    return comptime blk: {
        if (@hasDecl(I, "excludes")) {
            for (@field(I, "excludes")) |name| {
                if (std.mem.eql(u8, method, name)) break :blk true;
            }
        }
        break :blk false;
    };
}

fn VtableDirect(comptime I: type) type {
    comptime {
        const decls = std.meta.declarations(I);
        var fields: [decls.len]StructField = undefined;
        var idx = 0;
        for (decls) |decl| {
            if (!isExclude(I, decl.name)) {
                const info = @typeInfo(@TypeOf(@field(I, decl.name)));
                switch (info) {
                    .Fn => |f| {
                        if (!f.is_generic and f.params.len > 0 and f.params[0].type == I) {
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
}

fn VtableFieldType(comptime F: std.builtin.Type.Fn) type {
    comptime {
        var params: [F.params.len]std.builtin.Type.Fn.Param = undefined;
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
}

fn StackBuf(comptime N: usize) type {
    return comptime struct {
        buf: [N]u8 = undefined,

        pub fn init() @This() {
            return @This(){};
        }

        pub fn print(self: *const @This(), comptime fmt: []const u8, args: anytype) []const u8 {
            var this = @constCast(self);
            return std.fmt.bufPrint(&this.buf, fmt, args) catch @panic("FMT ERROR");
        }
    };
}

fn pointerType(any: anytype) enum {
    none,
    read,
    write,
} {
    return comptime blk: {
        const T = switch (@typeInfo(@TypeOf(any))) {
            else => @TypeOf(any),
            .Type => any,
        };
        break :blk switch (@typeInfo(T)) {
            else => .none,
            .Pointer => |p| if (p.is_const) .read else .write,
        };
    };
}

inline fn canCast(comptime V: type, comptime T: type) bool {
    return comptime Caster(V, T) != void;
}
