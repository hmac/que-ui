# frozen_string_literal: true

require "active_record"
require "que"
require "que/failure"

STDOUT.sync = true

ActiveRecord::Base.establish_connection(adapter: "postgresql")

Que.connection = "ActiveRecord"

class User < ActiveRecord::Base
  self.primary_key = "id"
end

class CreateUser < Que::Job
  include Que::Failure::NoRetry
  def run(name)
    puts "Creating user #{name}"
    raise "Some error!" if rand(10) == 9
    User.create!(name: name)
    sleep 2
  end
end

class RemoveUser < Que::Job
  def run(name)
    raise "Some error!" if rand(10) == 9
    u = User.find_by(name: name)
    return if u.nil?
    puts "Removing user #{name}"
    u.touch(:removed_at)
    u.save!
    sleep 2
  end
end

Que.worker_count = 5
Que.mode = :async

Thread.new do
  while true do
    sleep 5
    Que.worker_count = rand(10)
  end
end

words = File.read("/usr/share/dict/words").lines.map(&:chomp).sample(1000)
while true do
  CreateUser.enqueue(words.sample)
  RemoveUser.enqueue(words.sample)
  sleep 1
end
