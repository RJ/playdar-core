require 'rubygems'
require 'rake'

begin  
  require 'jeweler'
  Jeweler::Tasks.new do |gem|
    gem.name = "soundcloud-ruby-api-wrapper"
    gem.summary = "A ruby wrapper for the SoundCloud API"
    gem.email = "johannes@wagener.cc"
    gem.homepage = "http://github.com/soundcloud/ruby-api-wrapper"
    gem.authors = ["Johannes Wagener"]
    gem.add_dependency "oauth-active-resource", ">= 0.4.4"   
    gem.add_dependency "oauth", ">= 0.3.6"        
  end  
rescue LoadError
  puts "Jeweler not available. Install it with: sudo gem install technicalpickles-jeweler -s http://gems.github.com"  
end


require 'spec/rake/spectask'
Spec::Rake::SpecTask.new(:spec) do |spec|
  spec.libs << 'lib' << 'spec'
  spec.spec_files = FileList['spec/**/*_spec.rb']
end

Spec::Rake::SpecTask.new(:rcov) do |spec|
  spec.libs << 'lib' << 'spec'
  spec.pattern = 'spec/**/*_spec.rb'
  spec.rcov = true
end

task :default => :spec

require 'rake/rdoctask'
Rake::RDocTask.new do |rdoc|
  if File.exist?('VERSION.yml')
    config = YAML.load(File.read('VERSION.yml'))
    version = "#{config[:major]}.#{config[:minor]}.#{config[:patch]}"
  else
    version = ""
  end

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "ruby-api-wrapper #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end
