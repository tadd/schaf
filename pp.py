from gdb import Command, COMMAND_DATA, Type, \
    block_for_pc, lookup_symbol, lookup_type, parse_and_eval

def cfuncall(name, *args):
    func = lookup_symbol(name)[0].value()
    return func(*args)

class MyPrinter:
    COLORS = {
        'green': 32,
        'blue': 34,
        'cyan': 36,
    }
    HIGHLIGHT = {
        'param': 'cyan',
    }

    def __init__(self, val):
        self.val = val

    def color(self, s, cname):
        c = self.COLORS[cname]
        return f'\x1b[{c}m{s}\x1b[0m'

    def display_hint(self):
        return 'map'

    def deref_as(self, tname):
        ty = lookup_type(tname).pointer()
        return self.val.cast(ty).dereference()

    def pp(self, val):
        pr = schaf_pp(val)
        return pr.to_string() if pr else val.format_string()

    def pp_val_as(self, ty):
        return self.pp(self.val.cast(ty))

    def format_single(self, val, pretty=True):
        return self.pp(val) if pretty else val.format_string()

    def highlight(self, s, ty):
        return self.color(s, self.HIGHLIGHT[ty])

    def param(self, s):
        return self.highlight(s, 'param')

    def format(self, *keys, pretty=True):
        s = [f'{self.param(k)} = {self.format_single(self.val[k], pretty)}' for k in keys]
        return ', '.join(s)

class ProcedurePrinter(MyPrinter):
    TYPE = lookup_type('Procedure')

    def child_fields(self):
        return [f.name for f in self.TYPE.fields() if f.name != 'proc']

    def to_string(self):
        return self.format('arity')

class CFuncPrinter(ProcedurePrinter):
    TYPE = lookup_type('CFunc')

    def format_cfunction(self, val):
        return block_for_pc(val).function.value().format_string()

    def to_string(self):
        sup = self.pp_val_as(super().TYPE)
        s = self.format_cfunction(self.val['cfunc'])
        return f'{sup}, {self.param("cfunc")} = {s}'

class ClosurePrinter(ProcedurePrinter):
    TYPE = lookup_type('Closure')

    def to_string(self):
        sup = self.pp_val_as(super().TYPE)
        s = self.format(*self.child_fields())
        return f'{sup}, {s}'

class ContinuationPrinter(ProcedurePrinter):
    TYPE = lookup_type('Continuation')
    PRETTY_FIELDS = {'call_stack', 'retval'}

    def to_string(self):
        pf = self.PRETTY_FIELDS
        sup = self.pp_val_as(super().TYPE)
        l = [f'{self.param(k)} = {self.format_single(self.val[k], k in pf)}' for
             k in self.child_fields()]
        return f'{sup}, {", ".join(l)}'

class ValuePrinter(MyPrinter):
    TYPE = lookup_type('Value')
    TAG_TO_TYPE = {
        'cfunc': 'CFunc',
        'syntax': 'CFunc',
        'closure': 'Closure',
        'continuation': 'Continuation',
    }
    HIGHLIGHT = {
        'immediate': 'blue',
        'expr': 'green',
    }

    def tag_name(self):
        return self.deref_as('ValueTag').format_string()[4:].lower()

    def type_name(self):
        if self.isproc():
            return self.TAG_TO_TYPE[self.tag_name()]
        return cfuncall('value_to_type_name', self.val).string().title()

    def stringify(self):
        s = cfuncall('stringify', self.val).string()
        return self.highlight(s, 'expr')

    def proc_string(self, ty):
        return f'{{{self.pp(self.deref_as(ty))}}}'

    def isproc(self):
        return cfuncall('value_is_procedure', self.val)

    def addr(self):
        hex = f'{int(self.val):#x}'
        return self.highlight(hex, 'immediate')

    def to_string(self):
        addr, ty = (self.addr(), self.type_name())
        val = self.proc_string(ty) if self.isproc() else self.stringify()
        return f'{addr} {ty}: {val}'

class PP (Command):
    def __init__(self):
        super().__init__('pp', COMMAND_DATA)

    def invoke(self, arg, from_tty):
        val = parse_and_eval(arg)
        pp = schaf_pp(val)
        s = pp.to_string() if pp else str(val)
        print(s)
PP()

PRINTERS = [ValuePrinter, ProcedurePrinter,
            CFuncPrinter, ClosurePrinter, ContinuationPrinter]
def schaf_pp(val):
    ty = Type.unqualified(val.type)
    g = (pr(val) for pr in PRINTERS if pr.TYPE == ty)
    return next(g, None)

#gdb.pretty_printers.append(schaf_pp)
