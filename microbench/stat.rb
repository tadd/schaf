require 'enumerable/statistics'

def get_data = STDIN.gets.split[1].to_f

def percent(val, base) = (100 * val / base).round(2)

def stat(prog, n)
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

def main(n)
  puts "Test\tTime (msec)\tstdev (%)\tMemory (KiB)"
  while (prog = STDIN.gets)
    stat(prog.chomp, n)
  end
end

main(ARGV[0].to_i)
