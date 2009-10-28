Adds scrobbling support as a playdar http api.
the erlscrobbler lib is still unfinished - it works provided the server
behaves. Anything unexpected and it will just crash.

Add to your playdar.conf:

{{as, username}, "your-username"}.
{{as, password}, "your-password"}.

should "Just work" on playlick.com and anything using a reasonably recent
playdar.js
