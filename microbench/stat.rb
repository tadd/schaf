require 'enumerable/statistics'

def base_data
  base = File.readlines('base.tsv', chomp: 1) rescue nil
  return unless base
  title = base[0].split[1]
  h = base[1..].map do |l|
    a = l.split[0..1]
    next unless a
    sec = a[1].to_f
    next if sec.zero?
    [a[0], sec]
  end.compact.to_h
  h
end

def dump(data, name, base)
  row = data[name]
  mean = row.mean.round(3)
  stdev = row.stdev.round(3)
  p_stdev = (stdev * 100 / mean).round(2)
  s = "\t%.3f (%5.2f%%)" % [mean, p_stdev]
  if base && (b = base[name])
    s << "\t%5.1f%%" % (100 * mean / b).round(1)
  end
  puts s
end

def main(base)
  puts base ? "\t\tmsec  (stdev)\ttime/base.tsv" : nil

  data = {}
  curr = nil
  ARGF.each_line(chomp: 1) do |l|
    if l.match?(/^\w+\.scm/)
      dump(data, curr, base) if curr
      curr = l
      data[curr] ||= []
      printf '%-11s' % [curr]
      next
    end
    m = /^user\s+\dm(?<sec>\d\.\d{3})s/.match(l)
    next unless m
    data[curr] << m[:sec].to_f
  end
  dump(data, curr, base)
end

main(base_data)
