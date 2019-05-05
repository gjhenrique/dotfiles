#!/usr/bin/env ruby

require 'fileutils'

files = Dir.glob("#{ENV['HOME']}/.mozilla/firefox/**/places.sqlite")

files.each do |file|
  dir = File.dirname(file)
  FileUtils.cp(file, "#{dir}/places_albert.sqlite")
end
