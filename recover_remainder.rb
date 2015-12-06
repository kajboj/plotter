before_crash = File.read(ARGV[0]).gsub("\n", '')
commands = File.read(ARGV[1]).gsub("\n", '')

start = commands =~ /#{before_crash}/
finish = start + before_crash.size

pen_down = 'b'
remaining_commands = commands[finish..-1]

puts (pen_down + remaining_commands).split('').join("\n")
