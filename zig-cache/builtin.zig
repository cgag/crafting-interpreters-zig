pub const StackTrace = struct {
    index: usize,
    instruction_addresses: []usize,
};

pub const Os = enum {
    freestanding,
    ananas,
    cloudabi,
    dragonfly,
    freebsd,
    fuchsia,
    ios,
    kfreebsd,
    linux,
    lv2,
    macosx,
    netbsd,
    openbsd,
    solaris,
    windows,
    haiku,
    minix,
    rtems,
    nacl,
    cnk,
    aix,
    cuda,
    nvcl,
    amdhsa,
    ps4,
    elfiamcu,
    tvos,
    watchos,
    mesa3d,
    contiki,
    amdpal,
    zen,
};

pub const Arch = enum {
    armv8_3a,
    armv8_2a,
    armv8_1a,
    armv8,
    armv8r,
    armv8m_baseline,
    armv8m_mainline,
    armv7,
    armv7em,
    armv7m,
    armv7s,
    armv7k,
    armv7ve,
    armv6,
    armv6m,
    armv6k,
    armv6t2,
    armv5,
    armv5te,
    armv4t,
    armebv8_3a,
    armebv8_2a,
    armebv8_1a,
    armebv8,
    armebv8r,
    armebv8m_baseline,
    armebv8m_mainline,
    armebv7,
    armebv7em,
    armebv7m,
    armebv7s,
    armebv7k,
    armebv7ve,
    armebv6,
    armebv6m,
    armebv6k,
    armebv6t2,
    armebv5,
    armebv5te,
    armebv4t,
    aarch64,
    aarch64_be,
    arc,
    avr,
    bpfel,
    bpfeb,
    hexagon,
    mips,
    mipsel,
    mips64,
    mips64el,
    msp430,
    nios2,
    powerpc,
    powerpc64,
    powerpc64le,
    r600,
    amdgcn,
    riscv32,
    riscv64,
    sparc,
    sparcv9,
    sparcel,
    s390x,
    tce,
    tcele,
    thumb,
    thumbeb,
    i386,
    x86_64,
    xcore,
    nvptx,
    nvptx64,
    le32,
    le64,
    amdil,
    amdil64,
    hsail,
    hsail64,
    spir,
    spir64,
    kalimbav3,
    kalimbav4,
    kalimbav5,
    shave,
    lanai,
    wasm32,
    wasm64,
    renderscript32,
    renderscript64,
};

pub const Environ = enum {
    unknown,
    gnu,
    gnuabin32,
    gnuabi64,
    gnueabi,
    gnueabihf,
    gnux32,
    code16,
    eabi,
    eabihf,
    android,
    musl,
    musleabi,
    musleabihf,
    msvc,
    itanium,
    cygnus,
    amdopencl,
    coreclr,
    opencl,
    simulator,
};

pub const ObjectFormat = enum {
    unknown,
    coff,
    elf,
    macho,
    wasm,
};

pub const GlobalLinkage = enum {
    Internal,
    Strong,
    Weak,
    LinkOnce,
};

pub const AtomicOrder = enum {
    Unordered,
    Monotonic,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
};

pub const AtomicRmwOp = enum {
    Xchg,
    Add,
    Sub,
    And,
    Nand,
    Or,
    Xor,
    Max,
    Min,
};

pub const Mode = enum {
    Debug,
    ReleaseSafe,
    ReleaseFast,
    ReleaseSmall,
};

pub const TypeId = enum {
    Type,
    Void,
    Bool,
    NoReturn,
    Int,
    Float,
    Pointer,
    Array,
    Struct,
    ComptimeFloat,
    ComptimeInt,
    Undefined,
    Null,
    Optional,
    ErrorUnion,
    ErrorSet,
    Enum,
    Union,
    Fn,
    Namespace,
    Block,
    BoundFn,
    ArgTuple,
    Opaque,
    Promise,
};

pub const TypeInfo = union(TypeId) {
    Type: void,
    Void: void,
    Bool: void,
    NoReturn: void,
    Int: Int,
    Float: Float,
    Pointer: Pointer,
    Array: Array,
    Struct: Struct,
    ComptimeFloat: void,
    ComptimeInt: void,
    Undefined: void,
    Null: void,
    Optional: Optional,
    ErrorUnion: ErrorUnion,
    ErrorSet: ErrorSet,
    Enum: Enum,
    Union: Union,
    Fn: Fn,
    Namespace: void,
    Block: void,
    BoundFn: Fn,
    ArgTuple: void,
    Opaque: void,
    Promise: Promise,


    pub const Int = struct {
        is_signed: bool,
        bits: u8,
    };

    pub const Float = struct {
        bits: u8,
    };

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_volatile: bool,
        alignment: u32,
        child: type,

        pub const Size = enum {
            One,
            Many,
            Slice,
        };
    };

    pub const Array = struct {
        len: usize,
        child: type,
    };

    pub const ContainerLayout = enum {
        Auto,
        Extern,
        Packed,
    };

    pub const StructField = struct {
        name: []const u8,
        offset: ?usize,
        field_type: type,
    };

    pub const Struct = struct {
        layout: ContainerLayout,
        fields: []StructField,
        defs: []Definition,
    };

    pub const Optional = struct {
        child: type,
    };

    pub const ErrorUnion = struct {
        error_set: type,
        payload: type,
    };

    pub const Error = struct {
        name: []const u8,
        value: usize,
    };

    pub const ErrorSet = struct {
        errors: []Error,
    };

    pub const EnumField = struct {
        name: []const u8,
        value: usize,
    };

    pub const Enum = struct {
        layout: ContainerLayout,
        tag_type: type,
        fields: []EnumField,
        defs: []Definition,
    };

    pub const UnionField = struct {
        name: []const u8,
        enum_field: ?EnumField,
        field_type: type,
    };

    pub const Union = struct {
        layout: ContainerLayout,
        tag_type: ?type,
        fields: []UnionField,
        defs: []Definition,
    };

    pub const CallingConvention = enum {
        Unspecified,
        C,
        Cold,
        Naked,
        Stdcall,
        Async,
    };

    pub const FnArg = struct {
        is_generic: bool,
        is_noalias: bool,
        arg_type: ?type,
    };

    pub const Fn = struct {
        calling_convention: CallingConvention,
        is_generic: bool,
        is_var_args: bool,
        return_type: ?type,
        async_allocator_type: ?type,
        args: []FnArg,
    };

    pub const Promise = struct {
        child: ?type,
    };

    pub const Definition = struct {
        name: []const u8,
        is_pub: bool,
        data: Data,

        pub const Data = union(enum) {
            Type: type,
            Var: type,
            Fn: FnDef,

            pub const FnDef = struct {
                fn_type: type,
                inline_type: Inline,
                calling_convention: CallingConvention,
                is_var_args: bool,
                is_extern: bool,
                is_export: bool,
                lib_name: ?[]const u8,
                return_type: type,
                arg_names: [][] const u8,

                pub const Inline = enum {
                    Auto,
                    Always,
                    Never,
                };
            };
        };
    };
};

pub const FloatMode = enum {
    Optimized,
    Strict,
};

pub const Endian = enum {
    Big,
    Little,
};

pub const endian = Endian.Little;
pub const is_test = false;
pub const os = Os.linux;
pub const arch = Arch.x86_64;
pub const environ = Environ.gnu;
pub const object_format = ObjectFormat.elf;
pub const mode = Mode.Debug;
pub const link_libc = false;
pub const have_error_return_tracing = true;
pub const __zig_test_fn_slice = {}; // overwritten later
