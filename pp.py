from gdb import Command, COMMAND_DATA, Type, \
    lookup_symbol, lookup_type, parse_and_eval

def cfuncall(name, *args):
    func = lookup_symbol(name)[0].value()
    return func(*args)

class ValuePrinter:
    TYPE = lookup_type('Value')

    def __init__(self, val):
        self.val = val

    def to_string(self):
        ty, val = [cfuncall(f, self.val).string() for
                   f in ['value_to_type_name', 'stringify']]
        return '{:#x} {}: {}'.format(int(self.val), ty.title(), val)

class PP (Command):
    def __init__(self):
        super(type(self), self).__init__('pp', COMMAND_DATA)

    def invoke(self, arg, from_tty):
        val = parse_and_eval(arg)
        pp = schaf_pp(val)
        if pp:
            val = pp.to_string()
        print(str(val))
PP()

def schaf_pp(val):
    ty = Type.unqualified(val.type)
    match ty:
        case ValuePrinter.TYPE:
            return ValuePrinter(val)
    return None

#gdb.pretty_printers.append(schaf_pp)
