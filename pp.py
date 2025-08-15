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

def stringify(val):
    s = cfuncall('sch_stringify', val).string()
    return highlight('expr', s)

# gdb methods:
#
# * Value.format_string
# * pretty_printer.to_string

# our own methods:
#
# * to_string
# * format_single
# * format_members
# * format
# * format_as

class MyPrinter:
    def __init__(self, val):
        self.val = val

    def display_hint(self):
        return 'map'

    def deref_as(self, tname):
        ty = lookup_type(tname).pointer()
        return self.val.cast(ty).dereference()

    def pp_val_as(self, ty):
        return pretty_string(self.val.cast(ty))

    def format_single(self, val, pretty=True):
        return pretty_string(val) if pretty else val.format_string()

    def param(self, s):
        return highlight('param', s)

    def format_members(self, *keys, pretty=True):
        s = [f'{self.param(k)} = {self.format_single(self.val[k], pretty)}' for k in keys]
        return ', '.join(s)

class ProcedurePrinter(MyPrinter):
    TYPE = lookup_type('Procedure')

    def child_fields(self):
        return [f.name for f in self.TYPE.fields() if f.name != 'proc']

    def to_string(self):
        return self.format_members('arity', 'apply')

class CFuncPrinter(ProcedurePrinter):
    TYPE = lookup_type('CFunc')

    def format(self):
        b = block_for_pc(self.val['cfunc'])
        if not b or not b.function:
            return None
        return b.function.value().format_string()

    def to_string(self):
        sup = self.pp_val_as(super().TYPE)
        s = self.format()
        return f'{sup}, {self.param("cfunc")} = {s}'

class ClosurePrinter(ProcedurePrinter):
    TYPE = lookup_type('Closure')

    def to_string(self):
        sup = self.pp_val_as(super().TYPE)
        s = self.format_members(*self.child_fields())
        return f'{sup}, {s}'

class ContinuationPrinter(ProcedurePrinter):
    TYPE = lookup_type('Continuation')
    PRETTY_FIELDS = ['call_stack', 'retval']

    def to_string(self):
        pf = self.PRETTY_FIELDS
        sup = self.pp_val_as(super().TYPE)
        l = [f'{self.param(k)} = {self.format_single(self.val[k], k in pf)}' for
             k in self.child_fields()]
        return f'{sup}, {", ".join(l)}'

class CFuncClosurePrinter(CFuncPrinter):
    TYPE = lookup_type('CFuncClosure')

    def to_string(self):
        return f'{super()}, {self.param("data")} = ...'

class TablePrinter(MyPrinter):
    TYPE = lookup_type('Table')

    def to_string(self):
        return self.format_members(*self.TYPE.fields())

class EnvPrinter(MyPrinter):
    TYPE = lookup_type('Env')

    def to_string(self):
        ty = TablePrinter.TYPE.pointer()
        tab = self.val['table'].cast(ty).dereference()
        s = f'{self.param("table")} = {tab}'
        t = self.format_members('parent')
        return f'{s}, {t}'

class ErrorPrinter(MyPrinter):
    TYPE = lookup_type('Error')

    def to_string(self):
        return self.format_members('call_stack')

class ValuePrinter(MyPrinter):
    TYPE = lookup_type('SchValue')
    TAG_TO_TYPE = {
        'cfunc': 'CFunc',
        'syntax': 'CFunc',
        'closure': 'Closure',
        'cfunc_closure': 'CFuncClosure',
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
        return self.deref_as('ValueTag').format_string()[4:].lower()

    @property
    def is_internal_tag(self):
        return self.tag_name == 'error'

    @property
    def is_self_format(self):
        return not self.is_immediate and \
            (self.is_internal_tag or
             cfuncall('value_is_procedure', self.val))

    @property
    def type_name(self):
        if self.is_self_format:
            return self.TAG_TO_TYPE[self.tag_name]
        return cfuncall('value_to_type_name', self.val).string().title()

    def format_as(self, ty):
        return f'{{{pretty_string(self.deref_as(ty))}}}'

    @property
    def addr(self):
        hex = f'{int(self.val):#x}'
        return highlight('immediate', hex)

    def to_string(self):
        addr, ty = (self.addr, self.type_name)
        if self.is_self_format:
            val = self.format_as(ty)
        else:
            val = stringify(self.val)
        return f'{addr} {ty}: {val}'

class PP (Command):
    def __init__(self):
        super().__init__('pp', COMMAND_DATA)

    def invoke(self, argument, from_tty):
        val = parse_and_eval(argument)
        print(pretty_string(val))
PP()

PRINTERS = [ValuePrinter, EnvPrinter, ProcedurePrinter,
            CFuncPrinter, ClosurePrinter, ContinuationPrinter, CFuncClosurePrinter,
            TablePrinter, ErrorPrinter]
def schaf_pp(val):
    ty = Type.unqualified(val.type)
    g = (pr(val) for pr in PRINTERS if pr.TYPE == ty)
    return next(g, None)

def pretty_string(val):
    pr = schaf_pp(val)
    return pr.to_string() if pr else val.format_string()

#gdb.pretty_printers.append(schaf_pp)
