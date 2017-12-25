# frozen_string_literal: true

require "active_record"
require "que"

STDOUT.sync = true

ActiveRecord::Base.establish_connection(adapter: "postgresql")

Que.connection = "ActiveRecord"

class User < ActiveRecord::Base
  self.primary_key = "id"
end

class CreateUser < Que::Job
  def run(name)
    puts "Creating user #{name}"
    User.create!(name: name)
    sleep 0.5
  end
end

class RemoveUser < Que::Job
  def run(name)
    u = User.find_by(name: name)
    return if u.nil?
    puts "Removing user #{name}"
    u.touch(:removed_at)
    u.save!
    sleep 0.5
  end
end

Que.worker_count = 5
Que.mode = :async

words = File.read("/usr/share/dict/words").lines.map(&:chomp).sample(1000)
while true do
  CreateUser.enqueue(words.sample)
  RemoveUser.enqueue(words.sample)
  sleep 0.1
end
