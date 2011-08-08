require 'rake'

desc "remove untracked files in each submodule"
task :clean do
   system %Q{git submodule foreach git clean -fd}
end

desc "update submodules repositories"
task :update do
   system %Q{git submodule foreach git pull origin master}
end

desc "fetch changes"
task :fetch do
   puts "Fetching changes..."
   system %Q{git pull origin master}
   system %Q{git submodule update --init}
end

desc "create links into _* files in user's home directory"
task :install => ["fetch"] do
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

desc "remove ~/.*.old created by :install"
task :rmold do
   system %Q{rm ~/.*vim*.old}
   system %Q{rm ~/.*zsh*.old}
end
