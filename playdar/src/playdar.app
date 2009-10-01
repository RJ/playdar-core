{application, playdar,
 [{description, "playdar"},
  {vsn, "0.01"},
  {modules, [
    playdar,
    playdar_app,
    playdar_sup,
    playdar_web,
    playdar_deps,

    resolver, qry, stream_reader, file_reader, http_reader, utils, http_broker

  ]},
  {registered, []},
  {mod, {playdar_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, inets]}]}.
