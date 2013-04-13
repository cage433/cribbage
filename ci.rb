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

def message_tmux(msg)
  `tmux #{msg}`
end

last_test_time = Time.now

puts latest_source_timestamp
while true
  if last_test_time < latest_source_timestamp then
    last_test_time = Time.now
    message_tmux("display-message \"task launched\"")
    message_tmux("set -g status-bg magenta")
    sleep(2)
    system("clear")
    test_results = `raco test . 2>&1`
    if test_results.include?("FAILURE") then
      puts test_results
      message_tmux("set -g status-bg red")
    else
      message_tmux("set -g status-bg green")
    end
    puts Time.now - last_test_time
  end
  sleep(0.01)
end
