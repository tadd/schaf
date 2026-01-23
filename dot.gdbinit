# sample for debugging
set follow-fork-mode child
source pp.py

define ptag
  if $arg0 & 0b111
    pp $arg0
  else
    p VALUE_TAG($arg0)
  end
end
