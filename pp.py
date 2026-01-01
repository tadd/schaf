from gdb import Command, COMMAND_DATA, Type, \
    block_for_pc, lookup_symbol, lookup_type, parse_and_eval

def cfuncall(name, *args):
    sym = lookup_symbol(name)[0]
    if not sym:
        raise
    func = sym.value()
    return func(*args)

COLORS = {
    'green': 32,
    'blue': 34,
    'cyan': 36,
}

HIGHLIGHT = {
    'param': 'cyan',
    'immediate': 'blue',
    'expr': 'green',
}

def color(name, s):
    c = COLORS[name]
    return f'\x1b[{c}m{s}\x1b[0m'

def highlight(ty, s):
    return color(HIGHLIGHT[ty], s)

def param(s):
    return highlight('param', s)

def expr(s):
    return highlight('expr', s)

def immediate(s):
    return highlight('immediate', s)

def format_value(val, pretty=True):
    if pretty:
        pr = schaf_pp(val)
        if pr:
            return pr.to_string()
    return val.format_string()

class SchafPrinter:
    def __init__(self, val):
        self.val = val

    def display_hint(self):
        return 'map'

    def deref_as(self, tname):
        ty = lookup_type(tname).pointer()
        return self.val.cast(ty).dereference()

    def format_as(self, ty):
        return format_value(self.deref_as(ty))

    def format_casted(self, ty):
        return format_value(self.val.cast(ty))

    def format_members(self, *keys):
        s = [f'{param(k)} = {format_value(self.val[k])}' for k in keys]
        return ', '.join(s)

    def inspect(self):
        s = cfuncall('sch_inspect', self.val).string()
        return expr(s)

class ProcedurePrinter(SchafPrinter):
    TYPE = lookup_type('Procedure')

    def child_fields(self):
        return [f.name for f in self.TYPE.fields() if f.name != 'proc']

    def to_string(self):
        return self.format_members('arity', 'apply')

class CFuncPrinter(ProcedurePrinter):
    TYPE = lookup_type('CFunc')

    def format_cfunc(self):
        b = block_for_pc(self.val['cfunc'])
        if not b or not b.function:
            return None
        return b.function.value().format_string()

    def to_string(self):
        sup = self.format_casted(super().TYPE)
        s = self.format_cfunc()
        return f'{sup}, {param("cfunc")} = {s}'

class ClosurePrinter(ProcedurePrinter):
    TYPE = lookup_type('Closure')

    def to_string(self):
        sup = self.format_casted(super().TYPE)
        s = self.format_members(*self.child_fields())
        return f'{sup}, {s}'

class ContinuationPrinter(ProcedurePrinter):
    TYPE = lookup_type('Continuation')
    PRETTY_FIELDS = ['retval']

    def to_string(self):
        pf = self.PRETTY_FIELDS
        sup = self.format_casted(super().TYPE)
        l = [f'{param(k)} = {format_value(self.val[k], k in pf)}' for
             k in self.child_fields()]
        return f'{sup}, {", ".join(l)}'

class TablePrinter(SchafPrinter):
    TYPE = lookup_type('Table')

    def to_string(self):
        return self.format_members(*self.TYPE.fields())

class EnvPrinter(SchafPrinter):
    TYPE = lookup_type('Env')

    def to_string(self):
        ty = TablePrinter.TYPE.pointer()
        tab = self.val['table'].cast(ty).dereference()
        s = f'{param("table")} = {tab}'
        t = self.format_members('parent')
        return f'{s}, {t}'

class ErrorPrinter(SchafPrinter):
    TYPE = lookup_type('Error')

    def to_string(self):
        return self.format_members('call_stack')

class ValuePrinter(SchafPrinter):
    TYPE = lookup_type('SchValue')
    TAG_TO_TYPE = {
        'cfunc': 'CFunc',
        'syntax': 'CFunc',
        'closure': 'Closure',
        'continuation': 'Continuation',
        'env': 'Env',
        'error': 'Error',
        None: None
    }

    @property
    def is_immediate(self):
        return (int(self.val) & 0x7) != 0

    @property
    def tag_name(self):
        if self.is_immediate:
            return None
        # 4: omit "TAG_"
        return self.deref_as('ValueTag').format_string()[4:].lower()

    @property
    def is_internal_tag(self):
        return self.tag_name == 'error'

    @property
    def is_self_format(self):
        return not self.is_immediate and \
            (self.is_internal_tag or
             cfuncall('sch_value_is_procedure', self.val))

    @property
    def type_name(self):
        if self.is_self_format:
            return self.TAG_TO_TYPE[self.tag_name]
        return cfuncall('sch_value_to_type_name', self.val).string().title()

    @property
    def addr(self):
        h = f'{int(self.val):#x}'
        return immediate(h)

    def to_string(self):
        addr, ty = (self.addr, self.type_name)
        if self.is_self_format:
            val = f'{{{self.format_as(ty)}}}'
        else:
            val = self.inspect()
        return f'{addr} {ty}: {val}'

class PP (Command):
    def __init__(self):
        super().__init__('pp', COMMAND_DATA)

    def invoke(self, argument, from_tty):
        val = parse_and_eval(argument)
        print(format_value(val))
PP()

PRINTERS = [ValuePrinter, EnvPrinter, ProcedurePrinter,
            CFuncPrinter, ClosurePrinter, ContinuationPrinter,
            TablePrinter, ErrorPrinter]
def schaf_pp(val):
    ty = Type.unqualified(val.type)
    g = (pr(val) for pr in PRINTERS if pr.TYPE == ty)
    return next(g, None)

#gdb.pretty_printers.append(schaf_pp)
