require 'enumerable/statistics'

def values(vals, label, n = 0)
  vals.grep(/#{label}/) do |l|
    l.split[1].to_f
  end
end

def percent(val, base) = (100 * val / base).round(2)

l = ARGF.readlines
cpu = values(l, "CPU")
mem = values(l, "Vm")
mcpu, dcpu = [cpu.mean, cpu.stdev].map{ _1.round(3) }
pdcpu = percent(dcpu, mcpu)
mmem = mem.mean.round
puts "%.3f\t%.2f\t%d" % [mcpu, pdcpu, mmem]
