require 'spec'

$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))


require 'soundcloud'

Spec::Runner.configure do |config|
  
end

def soundcloud_site
  'http://api.sandbox-soundcloud.com'
end

def valid_oauth_access_token
  access_token = '2EXCRQykOLw7MPhzbndyg'
  access_secret = 'rCAlWbPfjG7rFOIE7LwbQ1OVfhNmHTTbNrK9zjTY'
  consumer_token = 'z9orRCWjmdrqWJZ0ly6lg'
  consumer_secret = 'PjL4H3bnNiLtmXQaaAIRaJxI6OWE2Sr5xB8ANRbhfMk'

  sc_consumer = Soundcloud.consumer(consumer_token,consumer_secret,soundcloud_site)
  return OAuth::AccessToken.new(sc_consumer, access_token, access_secret)
end

