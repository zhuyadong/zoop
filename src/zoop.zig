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
pub const type_id = *const anyopaque;
pub const ClassCheckFunc = fn (class_id: type_id) bool;
pub const TypeInfoGetFunc = fn () *const ClassInfo;
pub const FormatFunc = fn (*anyopaque, writer: std.io.AnyWriter) anyerror!void;
pub const ClassPtrFunc = fn (*anyopaque) *anyopaque;

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
        assert(interfaceIndex(T, I) < self.vtables.len);
        return @ptrCast(@alignCast(self.vtables[interfaceIndex(T, I)].vtable));
    }
};

pub const TypeInfo = struct {
    /// @typeNmae()
    typename: []const u8,
    /// typeid of the type
    typeid: type_id,
    /// type to string
    format: *const FormatFunc,
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

    pub fn format(self: *const @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try zoop.format(self, writer);
    }
};

pub const IRaw = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn cast(self: IRaw, comptime I: type) I {
        return I{ .ptr = self.ptr, .vptr = self.vptr };
    }
};

pub const IFormat = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn formatAny(self: IFormat, writer: std.io.AnyWriter) anyerror!void {
        try icall(self, "formatAny", .{writer});
    }
};

/// Set up hooks to monitor the creation and destruction of objects on the heap
pub fn setHook(new_hook: ?HookFunc, destroy_hook: ?HookFunc) void {
    new_hook_func = new_hook;
    destroy_hook_func = destroy_hook;
}

/// get field of special type from any's inherit tree
pub fn getField(any: anytype, comptime name: []const u8, comptime T: type) *T {
    const V = @TypeOf(any);
    switch (@typeInfo(V)) {
        else => @compileError(compfmt("zoop.getField(any): any must be a pointer to class/klass", .{})),
        .Pointer => |p| {
            if (isKlassType(p.child)) return getField(&any.class, name, T);
            if (isClassType(p.child)) {
                const offset = fieldOffset(p.child, name, T);
                return @ptrFromInt(@intFromPtr(any) + offset);
            }
        },
    }
}

/// get method of klass/class
pub fn getMethod(comptime T: type, comptime name: []const u8) MethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an klass/class type.", .{@typeName(T)}));

        return (struct {
            pub const method = blk: {
                var Cur = if (isKlassType(T)) T.Class else T;
                while (Cur != void) {
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

/// get method of klass/class' super class
pub fn getUpMethod(comptime T: type, comptime name: []const u8) UpMethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an klass/class type.", .{@typeName(T)}));

        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const fields = @typeInfo(Class).Struct.fields;
            if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct) {
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
    }

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
            self.header = .{ .info = makeClassInfo(T), .allocator = @ptrCast(&getAlly) };
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
            self.header = .{ .info = makeClassInfo(T), .allocator = @ptrCast(&getAlly) };
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

        fn getAlly(self: *@This()) ?std.mem.Allocator {
            return self.allocator;
        }

        fn deinit(self: *@This()) void {
            if (comptime builtin.mode == .Debug) {
                assert(self.header.magic == KlassHeader.kmagic);
                assert(self.header.info == makeClassInfo(T));
            }
            if (destroy_hook_func) |func| {
                func(cast(&self.class, IObject));
            }
            inline for (classes(T).items) |V| {
                if (@hasDecl(V, "deinit")) {
                    var p: *V = @ptrCast(&self.class);
                    p.deinit();
                }
            }
            if (self.allocator) |dtor| {
                dtor.destroy(self);
            }
        }
    };
}

pub fn ApiEnum(comptime I: type) type {
    if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));

    var apis = tupleInit(.{});
    inline for (std.meta.declarations(I)) |decl| {
        const info = @typeInfo(@TypeOf(@field(I, decl.name)));
        if (info == .Fn and info.Fn.params.len > 0 and !info.Fn.is_generic) {
            const first = info.Fn.params[0].type.?;
            const Self = switch (@typeInfo(first)) {
                else => void,
                .Struct => first,
                .Pointer => std.meta.Child(first),
            };
            if (Self == I) {
                apis = tupleAppendUnique(apis, decl.name);
            }
        }
    }

    if (apis.items.len == 0) {
        return @Type(.{
            .Enum = .{
                .tag_type = u0,
                .fields = &.{},
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }

    var fields: [apis.items.len]std.builtin.Type.EnumField = undefined;
    inline for (apis.items, 0..) |name, i| {
        fields[i] = .{
            .name = name ++ "",
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

pub fn MethodEnum(comptime T: type) type {
    const Class = if (isKlassType(T)) T.Class else if (isClassType(T) or isInterfaceType(T)) T else @compileError(compfmt("{s} is not klass/class/interface.", .{@typeName(T)}));

    const supers = if (isInterfaceType(T)) interfaces(T) else classes(Class);
    var methods = tupleInit(.{});
    inline for (supers.items) |super| {
        inline for (std.meta.declarations(super)) |decl| {
            const info = @typeInfo(@TypeOf(@field(super, decl.name)));
            if (info == .Fn and info.Fn.params.len > 0 and !info.Fn.is_generic) {
                const first = info.Fn.params[0].type.?;
                const Self = switch (@typeInfo(first)) {
                    else => void,
                    .Struct => first,
                    .Pointer => std.meta.Child(first),
                };
                if (Self == super) {
                    methods = tupleAppendUnique(methods, decl.name);
                }
            }
        }
    }

    if (methods.items.len == 0) {
        return @Type(.{
            .Enum = .{
                .tag_type = u0,
                .fields = &.{},
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }

    var fields: [methods.items.len]std.builtin.Type.EnumField = undefined;
    inline for (methods.items, 0..) |name, i| {
        fields[i] = .{
            .name = name ++ "",
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

pub fn Vtable(comptime I: type) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));
    }
    const ifaces = interfaces(I);
    var vtables: [ifaces.items.len]type = undefined;
    // ifaces.items.len - 1 for saving pointer to super interfaces vtable, -1 for I itself,
    // can make interface to interface casting faster.
    var nfield: comptime_int = ifaces.items.len - 1;

    inline for (ifaces.items, 0..) |iface, i| {
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

pub fn tupleInit(comptime any: anytype) Tuple {
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

pub fn tupleAppend(comptime tuple: Tuple, comptime any: anytype) Tuple {
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
    if (@TypeOf(any) == type) {
        return makeTypeInfo(any);
    } else {
        return makeTypeInfo(@TypeOf(any));
    }
}

pub fn classInfo(any: anytype) *const ClassInfo {
    return ClassInfoGetter(@TypeOf(any)).get(any);
}

pub fn typeId(any: anytype) type_id {
    if (@TypeOf(any) == type) {
        return makeTypeId(any);
    } else {
        return makeTypeId(@TypeOf(any));
    }
}

pub fn new(allocator: std.mem.Allocator, comptime T: type, init: ?T) !*T {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.", .{@typeName(T)}));
    }
    var self = try Klass(T).new(allocator, init);
    return &self.class;
}

pub fn destroy(any: anytype) void {
    const T = @TypeOf(any);
    if (isInterfaceType(T)) {
        var header: *KlassHeader = @ptrCast(@alignCast(any.ptr));
        return header.info.deinit(@ptrCast(header));
    } else {
        switch (@typeInfo(T)) {
            else => {},
            .Pointer => |p| {
                switch (p.size) {
                    else => {},
                    .One => {
                        if (isKlassType(p.child)) {
                            return any.header.info.deinit(@ptrCast(any));
                        } else if (isClassType(p.child)) {
                            var klass = Klass(p.child).from(any);
                            return klass.header.info.deinit(@ptrCast(klass));
                        }
                    },
                }
            },
        }
    }
    @compileError(compfmt("'{s}' is not a pointer to class/klass.", .{@typeName(@TypeOf(any))}));
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
    const vptr: *const Vtable(@TypeOf(iface)) = @ptrCast(@alignCast(iface.vptr));
    const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(iface.ptr));
    const ptr: *anyopaque = @ptrFromInt(@intFromPtr(pklass) + pklass.header.info.offset);
    return @call(.auto, @field(vptr, @tagName(api_enum)), .{ptr} ++ args);
}

/// call interface method of klass/class. (virtual call)
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
        if (!pass) @compileError("vcall(any) where any must be *class/*klass/interface");
    }
    const info = ApiInfo(method);
    if (canCast(@TypeOf(any), info.Iface)) {
        return icall(zoop.cast(any, info.Iface), info.name, args);
    }
    if (zoop.as(any, info.Iface)) |iface| {
        return icall(iface, info.name, args);
    }
    @panic(StackBuf(2048).init().print("{s} don't support method: {s}.{s}", .{ @typeName(@TypeOf(any)), @typeName(info.Iface), @tagName(info.name) }));
}

/// call super class's method.
/// example: zoop.upcall(pclass, "methodOfSuperClass", .{});
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
    @compileError("upcall(any, ...) where any must be *class/*klass.");
} {
    const T = comptime std.meta.Child(@TypeOf(any));
    const Class = comptime if (isKlassType(T)) T.Class else T;
    const method = comptime getUpMethod(Class, @tagName(method_enum));
    const Ptr = comptime @typeInfo(@TypeOf(method)).Fn.params[0].type.?;
    const ptr: Ptr = @ptrCast(if (isKlassType(T)) &any.class else any);
    return @call(.auto, method, .{ptr} ++ args);
}

pub fn isRootPtr(ptr: anytype) bool {
    const T = std.meta.Child(@TypeOf(ptr));
    if (isClassType(T)) {
        return classInfo(ptr) == makeClassInfo(T);
    } else if (isKlassType(T)) {
        return classInfo(ptr) == makeClassInfo(T.Class);
    }
    @compileError(compfmt("{s} is not a class/klass.", .{@typeName(T)}));
}

pub fn getAllocator(any: anytype) ?std.mem.Allocator {
    const T = @TypeOf(any);
    switch (@typeInfo(T)) {
        else => {},
        .Struct => {
            if (isInterfaceType(T)) {
                const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(any.ptr));
                return pklass.header.allocator(any.ptr);
            }
        },
        .Pointer => |p| {
            if (p.size == .One) {
                if (isKlassType(p.child)) {
                    return any.allocator;
                } else if (isClassType(p.child)) {
                    const pklass = Klass(p.child).from(any);
                    return pklass.header.allocator(@ptrFromInt(@intFromPtr(pklass)));
                }
            }
        },
    }
    @compileError(compfmt("zoop.getAllocator(any) where any must be *klass/*class/interface", .{}));
}

pub fn cast(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) T else switch (pointerType(@TypeOf(any))) {
        .read => *const T,
        .write => *T,
        else => @compileError(compfmt("zoop.cast(any, T): any must be interface or pointer to class but '{}'.", .{@typeName(@TypeOf(any))})),
    };
} {
    const caster = comptime Caster(@TypeOf(any), T);
    if (caster != void) {
        return caster.cast(any, T);
    }
    @compileError(compfmt("'{s}' can not cast to '{s}'", .{ @typeName(@TypeOf(any)), @typeName(T) }));
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
        if (isInterfaceType(V)) break :blk @alignCast(any.ptr);
        if (isKlassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(any));
        if (isClassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(Klass(std.meta.Child(V)).from(any)));
        unreachable;
    };
    const info = classInfo(any);

    if (isInterfaceType(T)) {
        if (info.getVtable(makeTypeId(T))) |pvtable| {
            return T{ .ptr = ptr, .vptr = pvtable };
        }
    }
    if (isClassType(T)) {
        if (info.isClass(makeTypeId(T))) {
            const klass: *Klass(T) = @ptrCast(@alignCast(ptr));
            return &klass.class;
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

pub fn format(ptr: anytype, writer: anytype) anyerror!void {
    comptime {
        if (@typeInfo(@TypeOf(ptr)) != .Pointer) @compileError("zoop.format(ptr) where ptr must be a pointer.");
    }
    const T = std.meta.Child(@TypeOf(ptr));
    const typeinfo = typeInfo(T);
    try typeinfo.format(@ptrCast(@constCast(ptr)), if (@TypeOf(writer) == std.io.AnyWriter) writer else writer.any());
}

//===== private content ======
fn MethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        var Cur = if (isKlassType(T)) T.Class else T;
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

fn UpMethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const fields = @typeInfo(Class).Struct.fields;
            if (fields.len > 0 and @typeInfo(fields[0].type) == .Struct) {
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
    }
    return @typeInfo(@TypeOf(@field(I, @tagName(method)))).Fn.return_type orelse void;
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

fn ClassInfoGetter(comptime T: type) type {
    switch (@typeInfo(T)) {
        else => {},
        .Type => {
            if (isKlassType(T)) {
                return struct {
                    pub fn get(_: anytype) *const ClassInfo {
                        return makeClassInfo(T.Class);
                    }
                };
            }
            if (isClassType(T)) {
                return struct {
                    pub fn get(_: anytype) *const ClassInfo {
                        return makeClassInfo(T);
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
    @compileError("zoop.classInfo(any) where any must be Class/*class/Klass/*klass/interface");
}

fn formatFunc(comptime T: type) *const FormatFunc {
    if (isInterfaceType(T)) return (struct {
        pub fn func(piface: *anyopaque, writer: std.io.AnyWriter) anyerror!void {
            const self: *T = @ptrCast(@alignCast(piface));
            if (zoop.as(self.*, IFormat)) |iformat| {
                try icall(iformat, .formatAny, .{writer});
            } else {
                try writer.print("{any}", .{self.*});
            }
        }
    }).func else if (isClassType(T) or isKlassType(T)) return (struct {
        pub fn func(pself: *anyopaque, writer: std.io.AnyWriter) anyerror!void {
            const self: *T = @ptrCast(@alignCast(pself));
            if (zoop.as(self, IFormat)) |iformat| {
                try icall(iformat, .formatAny, .{writer});
            } else {
                try writer.print("{any}", .{self});
            }
        }
    }).func else return (struct {
        pub fn func(pself: *anyopaque, writer: std.io.AnyWriter) anyerror!void {
            const self: *T = @ptrCast(@alignCast(pself));
            try writer.print("{any}", .{self});
        }
    }).func;
}

fn defaultFields(comptime T: type) []StructField {
    const allfields = std.meta.fields(T);
    var fields: [allfields.len]StructField = undefined;
    var idx = 0;
    inline for (allfields) |field| {
        if (field.default_value != null) {
            fields[idx] = field;
            idx += 1;
        }
    }
    const ret = fields[0..idx];
    return ret;
}

fn initDefaultFields(pclass: anytype) void {
    const T = std.meta.Child(@TypeOf(pclass));
    inline for (defaultFields(T)) |field| {
        const ptr: *const field.type = @ptrCast(@alignCast(field.default_value.?));
        @field(pclass, field.name) = ptr.*;
    }
}

fn initClass(pclass: anytype) void {
    const T = std.meta.Child(@TypeOf(pclass));
    const supers = classes(T);
    inline for (supers.items) |V| {
        const pv: *V = @ptrCast(@alignCast(pclass));
        initDefaultFields(pv);
    }
}

fn fieldOffset(comptime T: type, comptime name: []const u8, comptime FT: type) usize {
    return (struct {
        pub const val: usize = blk: {
            const supers = classes(T);
            for (supers.items) |V| {
                if (@hasField(V, name)) {
                    if (FieldType(V, nameCast(FieldEnum(V), name)) == FT) {
                        const pv: *allowzero V = @ptrFromInt(0);
                        const pf = &@field(pv, name);
                        break :blk @intFromPtr(pf);
                    }
                }
            }
            @compileError(compfmt("no field named '{s}' in '{s}'", .{ name, @typeName(T) }));
        };
    }).val;
}

fn classChecker(comptime T: type) *const ClassCheckFunc {
    comptime {
        if (!isClassType(T)) @compileError("unsupport type: " ++ @typeName(T));
    }
    return (struct {
        pub fn func(class_id: type_id) bool {
            inline for (classes(T).items) |class| {
                if (class_id == makeTypeId(class)) return true;
            }
            return false;
        }
    }).func;
}

fn makeClassVtables(comptime T: type) []const ClassInfo.VtableInfo {
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

fn makeClassInfo(comptime T: type) *const ClassInfo {
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

fn makeTypeInfo(comptime T: type) *const TypeInfo {
    return &(struct {
        pub const info: TypeInfo = .{
            .typename = @typeName(T),
            .typeid = makeTypeId(T),
            .format = formatFunc(T),
        };
    }).info;
}

fn makeTypeId(comptime T: type) type_id {
    return @ptrCast(&(struct {
        pub const val: *T = undefined;
    }).val);
}

fn makeVtable(comptime T: type, comptime I: type) *anyopaque {
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
    return (struct {
        pub const val = blk: {
            const fields = @typeInfo(IObject).Struct.fields;
            break :blk switch (@typeInfo(T)) {
                else => false,
                .Struct => |s| s.fields.len == fields.len and
                    s.fields[0].type == fields[0].type and
                    s.fields[1].type == fields[1].type and
                    std.mem.eql(u8, s.fields[0].name, fields[0].name) and
                    std.mem.eql(u8, s.fields[1].name, fields[1].name),
            };
        };
    }).val;
}

inline fn isClassType(comptime T: type) bool {
    return (struct {
        pub const val = blk: {
            if (isKlassType(T)) break :blk false;
            if (isInterfaceType(T)) break :blk false;
            break :blk switch (@typeInfo(T)) {
                else => false,
                .Struct => |s| !s.is_tuple and s.fields.len > 0 and s.fields[0].alignment == alignment,
            };
        };
    }).val;
}

inline fn isKlassType(comptime T: type) bool {
    return (struct {
        pub const val = blk: {
            if (@typeInfo(T) == .Struct) {
                break :blk @hasDecl(T, "#klass");
            }
            break :blk false;
        };
    }).val;
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

fn interfaceIndex(comptime T: type, comptime I: type) usize {
    return (struct {
        pub const index = blk: {
            const ifaces = interfaces(T);
            for (ifaces.items, 0..) |iface, i| {
                if (iface == I) break :blk i;
            }
            @compileError(compfmt("{s} don't support interface:{s}", .{ @typeName(T), @typeName(I) }));
        };
    }).index;
}

fn interfaces(comptime T: type) Tuple {
    return (struct {
        pub const val = blk: {
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
                                    } else @compileError(compfmt("{s} in {s}.extends but is not an interface type.", .{ @typeName(iface), @typeName(T) }));
                                }
                            }
                        },
                    }
                }
            }

            break :blk ret;
        };
    }).val;
}

fn classes(comptime T: type) Tuple {
    return (struct {
        pub const val = blk: {
            var ret = tupleInit(.{});
            if (!isClassType(T)) break :blk ret;

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

            break :blk ret;
        };
    }).val;
}

inline fn isExclude(comptime I: type, comptime method: []const u8) bool {
    if (@hasDecl(I, "excludes")) {
        inline for (@field(I, "excludes")) |name| {
            if (std.mem.eql(u8, method, name)) return true;
        }
    }
    return false;
}

fn VtableDirect(comptime I: type) type {
    if (I == IObject) return struct {};

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

fn VtableFieldType(comptime F: std.builtin.Type.Fn) type {
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

fn StackBuf(comptime N: usize) type {
    return struct {
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
    no,
    read,
    write,
} {
    const T = switch (@typeInfo(@TypeOf(any))) {
        else => @TypeOf(any),
        .Type => any,
    };
    return (struct {
        pub const val = blk: {
            const info = if (T == type) @typeInfo(any) else @typeInfo(T);
            break :blk switch (info) {
                else => .no,
                .Pointer => |p| if (p.is_const) .read else .write,
            };
        };
    }).val;
}

inline fn canCast(comptime V: type, comptime T: type) bool {
    return (struct {
        pub const val = blk: {
            break :blk Caster(V, T) != void;
        };
    }).val;
}
