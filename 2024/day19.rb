#!/usr/bin/env ruby

def count_ways(target, substrings, memo = {})
  return 1 if target.empty?
  return memo[target] if memo.key?(target)

  count = 0
  substrings.each do |sub|
    if target.start_with?(sub)
      remaining = target[sub.length..]
      count += count_ways(remaining, substrings, memo)
    end
  end

  memo[target] = count
  count
end

def simple_approach(data, patterns)
  pattern = "^(#{patterns.join('|')})*$"
  data.select { |item| item.match?(pattern) }
  return ways
end

def egrep_approach(data, patterns)
  pattern = "^(#{patterns.join('|')})*$"

  matching_items = []
  IO.popen(['egrep', pattern], 'w+') do |grep|
    data.each { |item| grep.puts(item) }
    grep.close_write

    matching_items = grep.readlines.map(&:chomp)
  end

  return matching_items
end

def parse_input(filename)
  lines = File.readlines(ARGV[0], chomp: true)

  patterns = lines[0].split(',').map(&:strip)

  unless lines[1].empty?
    puts "Error: Second line must be blank"
    exit 1
  end

  data = lines[2..]

  return patterns, data
end

def main
  if ARGV.empty?
    puts "Usage: #{$0} filename"
    exit 1
  end

  patterns, data = parse_input(ARGV[0])

  # puts "Part 1 (egrep): #{egrep_approach(data, patterns).size}"

  puts "Part 1 (simple): #{simple_approach(data, patterns).size}"

  ways = data.map do |line| 
    count_ways(line, patterns)
  end

  puts "Part 1: #{ways.select { |x| x > 0 }.length}"
  puts "Part 2: #{ways.sum}"
end

if __FILE__ == $0
  main
end
