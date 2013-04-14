#!/usr/bin/ruby -w

require 'find'


def latest_source_timestamp
  source_times =[]
  Find.find(".") do |f|
    if f.end_with?(".rkt") then
      source_times.push File.mtime(f)
    end
  end
  source_times.sort.last
end

def output_indicates_test_failure(output)
  output.include?("FAILURE")
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

def build
  system("clear")
  tmux_message("Building", "black", "white")
  output = `raco test . 2>&1`
  if output_indicates_test_failure(output) then
    tmux_set_status_left("Tests failed ", "red", "black")
    puts output
  elsif output_indicates_compilation_failure(output)
    puts output
    tmux_set_status_left("Compilation failed ", "yellow", "black")
  else
    test_duration=format("%0.2f(s)", Time.now - $last_test_time)
    tmux_set_status_left("Passed #{test_duration}", "green", "black")
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
  sleep(0.05)
end
