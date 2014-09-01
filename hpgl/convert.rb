s = File.read('yin_yang.hpgl')

tokens = s.gsub(';', ',').split(',')

def scale(x)
  (x.to_f / 10000) * 300 - 150
end

def puts_scaled(f, x, y)
  cputs f, "MV (#{scale(x)}, #{scale(y)})"
end

def cputs(f, s)
  f.puts ", #{s}"
end

i = 0;

while tokens[i] != 'PD' do
  i+=1
end

File.open('output', 'w') do |f|

  while i<tokens.size do
    token = tokens[i]
    if token =~ /^PD/ || token =~ /^PU/
      cputs f, token
      i += 1
    elsif token =~ /^PA(\d+)/ || token =~ /^(\d+)/
      puts_scaled(f, $1, tokens[i+1])
      i += 2
    else
      i += 1
    end
  end
end
