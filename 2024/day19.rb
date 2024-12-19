#!/usr/bin/env ruby

if ARGV.empty?
  puts "Usage: #{$0} filename"
  exit 1
end

begin
  lines = File.readlines(ARGV[0], chomp: true)
rescue Errno::ENOENT
  puts "Error: File '#{ARGV[0]}' not found"
  exit 1
rescue => e
  puts "Error reading file: #{e.message}"
  exit 1
end

header = lines[0].split(',').map(&:strip).sort().join('|')
pattern = "^(#{header})*$"

unless lines[1].empty?
  puts "Error: Second line must be blank"
  exit 1
end

data = lines[2..]

matching_items = []
IO.popen(['egrep', pattern], 'w+') do |grep|
  data.each { |item| grep.puts(item) }
  grep.close_write

  matching_items = grep.readlines.map(&:chomp)
end

puts "Part 1: #{matching_items.size}"

