require 'enumerable/statistics'

def get_data = STDIN.gets.split[1].to_f

def percent(val, base) = (100 * val / base).round(2)
def ppoint(val, base) = percent(val, base) - 100

def print_data(prog, cpu, dcpu, mem, base)
  print "%.3f\t%.2f\t%d" % [cpu, dcpu, mem]
  if base
    bcpu, bdcpu, bmem = base[prog]
    val = [ppoint(cpu, bcpu), (dcpu - bdcpu).round(2), ppoint(mem, bmem)]
    print (["\t%+.2f"]*3).join % val
  end
  puts
end

def stat(prog, n, base)
  cpu = []
  mem = []
  print "#{prog}\t"
  STDOUT.flush
  n.times do
    cpu << get_data
    mem << get_data
  end
  mcpu, dcpu = [cpu.mean, cpu.stdev].map{ _1.round(3) }
  pdcpu = percent(dcpu, mcpu)
  mmem = mem.mean.round
  print_data(prog, mcpu, pdcpu, mmem, base)
end

def parse_base(path)
  open(path) do |f|
    return nil if f.readlines.size < 3
    f.rewind
    lines = f.readlines[1..]
    lines.map do |l|
      prog, *val = l.split
      cpu, stdev, mem = val[..2].map(&:to_f)
      [prog, [cpu.round(3), stdev.round(2), mem.round]]
    end.to_h
  end rescue nil
end

def print_header(base)
  print "Test\tTime (msec)\tstdev (%)\tMemory (KiB)"
  print "\tvs Time (pp)\tvs stdev (pp)\tvs Memory (pp)" if base
  puts
end

def main(n, basepath)
  base = parse_base(basepath)
  print_header(base)
  while (prog = STDIN.gets)
    stat(prog.chomp, n, base)
  end
end

main(ARGV[0].to_i, ARGV[1])
