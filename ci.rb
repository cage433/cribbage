#!/usr/bin/ruby -w

require 'find'


def latest_source_timestamp
  source_times =[]
  Find.find(".") do |f|
    if f.end_with?(".lisp") then
      source_times.push File.mtime(f)
    end
  end
  source_times.sort.last
end

def output_indicates_test_failure(output)
  output.include?("failed")
end

def output_indicates_compilation_failure(output)
  output.chomp.split("\n").find{|line| line !~ /^testing/}
end

def tmux_message(msg, bg, fg)
  `tmux set-option -g message-bg #{bg}`
  `tmux set-option -g message-fg #{fg}`
  `tmux display-message \"#{msg}\"`
end

def tmux_set_status_left(msg, bg, fg)
  tmux_message(msg, bg, fg)
  `tmux set-option -gq -t vim status-left \"\#[bg=#{bg},fg=#{fg}]#{msg}\"`
end

def tmux_clear_status_left
  `tmux set-option -gq -t vim status-left \"\"`
end

def run_tests
  result = `sbcl --noinform --disable-debugger --load test --eval '(run-tests :exit-on-termination t)' 2>&1`
  success = $?.success?
  puts result #unless success
  puts "Successful? #{success}"
  [result, success]
end


def build
  system("clear")
  tmux_message("Building", "black", "white")
  tmux_clear_status_left
  output, success = run_tests
  if success then
    test_duration=format("%0.2f(s)", Time.now - $last_test_time)
    tmux_message("Passed #{test_duration}", "green", "black")
  elsif output_indicates_test_failure(output) then
    tmux_set_status_left("Tests failed ", "red", "black")
  else
    tmux_set_status_left("Compilation failed ", "yellow", "black")
  end
end

$last_test_time = Time.now
`tmux set -gq display-time 1250`
`tmux set-option -gq status-left-length 20`

while true
  if $last_test_time < latest_source_timestamp then
    $last_test_time = Time.now
    build
  end
  sleep(0.1)
end
