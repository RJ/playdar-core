# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{ruby-api-wrapper}
  s.version = "0.4.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Johannes Wagener"]
  s.date = %q{2009-09-03}
  s.email = %q{johannes@wagener.cc}
  s.extra_rdoc_files = [
    "LICENSE",
     "README.html",
     "README.rdoc"
  ]
  s.files = [
    ".document",
     ".gitignore",
     "LICENSE",
     "README.html",
     "README.rdoc",
     "Rakefile",
     "VERSION.yml",
     "lib/soundcloud.rb",
     "lib/soundcloud/models/base.rb",
     "lib/soundcloud/models/comment.rb",
     "lib/soundcloud/models/event.rb",
     "lib/soundcloud/models/playlist.rb",
     "lib/soundcloud/models/track.rb",
     "lib/soundcloud/models/user.rb",
     "ruby-api-wrapper.gemspec",
     "spec/fixtures/test_artwork.gif",
     "spec/fixtures/test_track.mp3",
     "spec/soundcloud_comment_spec.rb",
     "spec/soundcloud_event_spec.rb",
     "spec/soundcloud_playlist_spec.rb",
     "spec/soundcloud_spec.rb",
     "spec/soundcloud_track_spec.rb",
     "spec/soundcloud_user_spec.rb",
     "spec/spec_helper.rb"
  ]
  s.homepage = %q{http://github.com/soundcloud/ruby-api-wrapper}
  s.rdoc_options = ["--charset=UTF-8"]
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.4}
  s.summary = %q{TODO}
  s.test_files = [
    "spec/soundcloud_comment_spec.rb",
     "spec/soundcloud_event_spec.rb",
     "spec/soundcloud_playlist_spec.rb",
     "spec/soundcloud_spec.rb",
     "spec/soundcloud_track_spec.rb",
     "spec/soundcloud_user_spec.rb",
     "spec/spec_helper.rb"
  ]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<jwagener-oauth-active-resource>, [">= 0"])
      s.add_runtime_dependency(%q<pelle-oauth>, [">= 0"])
    else
      s.add_dependency(%q<jwagener-oauth-active-resource>, [">= 0"])
      s.add_dependency(%q<pelle-oauth>, [">= 0"])
    end
  else
    s.add_dependency(%q<jwagener-oauth-active-resource>, [">= 0"])
    s.add_dependency(%q<pelle-oauth>, [">= 0"])
  end
end
