require 'enumerable/statistics'

def mean(vals, label, n=0) = vals.grep(/#{label}/) { _1.split[1].to_f }.mean.round(n)

l = ARGF.readlines
cpu = mean(l, "CPU", 3)
mem = mean(l, "Vm")
puts "%.3f ms\t%6d kb" % [cpu, mem]
