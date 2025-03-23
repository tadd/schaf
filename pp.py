from gdb import (lookup_type, lookup_global_symbol, parse_and_eval,
                 Type, Command, COMMAND_DATA)

def cfuncall(name, *args):
    func = lookup_global_symbol(name).value()
    return func(*args)

class ValuePrinter:
    TYPE = lookup_type('Value')

    def __init__(self, val):
        self.val = val

    def __str__(self):
        ty = cfuncall('value_to_type_name', self.val).string()
        val = cfuncall('stringify', self.val).string()
        return '{:#x} {}: {}'.format(int(self.val), ty, val)

class PP (Command):
    def __init__(self):
        super(type(self), self).__init__('pp', COMMAND_DATA)

    def invoke(self, arg, from_tty):
        val = parse_and_eval(arg)
        pp = schaf_pp(val)
        if pp:
            val = pp
        print(str(val))
PP()

def schaf_pp(val):
    if Type.unqualified(val.type) == ValuePrinter.TYPE:
        return ValuePrinter(val)
    return None

#gdb.pretty_printers.append(schaf_pp)
