require 'rake'

desc "remove untracked files in each submodule"
task :clean do
   system %Q{git submodule foreach git clean -fd}
end

desc "update submodules repositories"
task :update do
   system %Q{ls _vim/bundle | parallel  'sh -c "cd _vim/bundle/{}; git pull origin master"'}
end

desc "fetch changes"
task :fetch do
   puts ">>> Fetching changes"
   system %Q{git pull origin master}
   system %Q{git submodule update --init --recursive}
end

desc "create symlinks scripts into ~/.dotfiles/bin/* in /usr/local/bin"
task :binlink do
   puts
   Dir['bin/*'].each do |script|
      puts ">>> Linking #{script} into /usr/local/bin"
      system %Q{sudo ln -f -s "$PWD/#{script}" "/usr/local/bin/#{script.sub('bin/','')}"}
   end
end

desc "create symlinks into _* entries in user's home directory"
task :install => ["fetch", "binlink"] do
   puts
   Dir['_*'].each do |file|
      dotfile = File.join(ENV['HOME'], "#{file.sub('_', '.')}")

      if File.exist?(dotfile)
         puts ">>> Creating backup for #{dotfile}"
         system %Q{mv "#{dotfile}" "#{dotfile}-`date +%s`.old"}
      end

      puts ">>> Linking #{file}"
      system %Q{ln -f -s "$PWD/#{file}" "#{dotfile}"}
   end
end

desc "remove ~/.*.old created by :install"
task :rmold do
   system %Q{rm ~/.*.old}
end
