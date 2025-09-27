require 'enumerable/statistics'

def get_data = STDIN.gets.split[1].to_f

def percent(val, base) = (100 * val / base).round(2)

def stat(prog, n, base)
  cpu = []
  mem = []
  print "#{prog.chomp}\t"
  STDOUT.flush
  n.times do
    cpu << get_data
    mem << get_data
  end
  mcpu, dcpu = [cpu.mean, cpu.stdev].map{ _1.round(3) }
  pdcpu = percent(dcpu, mcpu)
  mmem = mem.mean.round
  puts "%.3f\t%.2f\t%d" % [mcpu, pdcpu, mmem]
  STDOUT.flush
end

def parse_base(path)
  open(path) do |f|
    return nil if f.reaadlines.size < 3
    f.rewind
    lines = f.readlines[1..]
    lines.map do |l|
      prog, cpu, stdev, mem, * = l.split
      [prog, [cpu.round(3), stdev.round(2), mem.round]]
    end.to_h
  end rescue nil
end

def main(n, basepath)
  print "Test\tTime (msec)\tstdev (%)\tMemory (KiB)"
  base = parse_base(basepath)
  print "\tvs Time (%)\tvs stdev (pp)\tvs Memory (%)" if base
  puts
  pp base
  STDOUT.flush
  abort
  while (prog = STDIN.gets)
    stat(prog.chomp, n, base)
  end
end

main(ARGV[0].to_i, ARGV[1])
