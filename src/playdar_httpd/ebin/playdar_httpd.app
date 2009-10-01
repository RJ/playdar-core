{application, playdar_httpd,
 [{description, "playdar_httpd"},
  {vsn, "0.01"},
  {modules, [
    playdar_httpd,
    playdar_httpd_app,
    playdar_httpd_sup,
    playdar_httpd_web,
    playdar_httpd_deps
  ]},
  {registered, []},
  {mod, {playdar_httpd_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
