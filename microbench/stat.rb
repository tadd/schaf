require 'enumerable/statistics'

def mean(vals, label, n = 0)
  vals.grep(/#{label}/) do |l|
    l.split[1].to_f
  end.mean.round(n)
end

l = ARGF.readlines
cpu = mean(l, "CPU", 3)
mem = mean(l, "Vm")
puts "%.3f\t%d" % [cpu, mem]
