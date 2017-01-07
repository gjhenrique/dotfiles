#!/usr/bin/env ruby

require 'rugged'
require 'date'
require 'active_support'
require 'active_support/core_ext/date'

repo = Rugged::Repository.new ENV['DIR']
commit = repo.lookup ENV['GIT_COMMIT']

work_time = (8..19)
open_source_time = (20..23)

date = DateTime.parse(commit.author[:time].to_s)
commit_hour = date.strftime('%H').to_i

if !(date.sunday? || date.saturday?) && work_time.include?(commit_hour)
  hour = [*open_source_time].sample
  date = date.change(hour: hour, min: date.minute, sec: date.second)
end

puts date.strftime('%a %b %d %H:%M:%S %Y %z')
