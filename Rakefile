require 'rake'

desc "update submodules"
task :update do
   puts "Fetching submodules..."
   system %Q{git submodule update --init}
end

desc "creates links into _* files in user's home directory"
task :install => ["update"] do
   Dir['_*'].each do |file|
     dotfile = File.join(ENV['HOME'], "#{file.sub('_', '.')}")
     if File.exist?(dotfile)
        puts "Creating backup for #{dotfile}..."
        system %Q{mv "#{dotfile}" "#{dotfile}-`date +%s`.old"}
     end

     puts "Linking #{file}..."
     system %Q{ln -s "$PWD/#{file}" "#{dotfile}"}
   end
end
