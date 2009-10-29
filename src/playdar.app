{application, playdar,
 [{description, "playdar"},
  {vsn, "0.5"},
  {modules, [
  playdar_reader_registry, playdar_http_api, playdar_script_resolver, playdar_app, playdar_logger, playdar_http_registry, playdar, playdar_reader, playdar_resolver, playdar_web, playdar_deps, playdar_modules_sup, playdar_ctl, playdar_auth, playdar_resolver, playdar_config, playdar_sup, playdar_utils, playdar_resolver_sup, lan_resolver, playdartcp_resolver, playdartcp_router, listener_impl, playdartcp_conn, playdartcp_web, fake_resolver, file_reader, http_reader, audioscrobbler_module, erlscrobbler, library_dets, scanner
  ]},
  {registered, []},
  {mod, {playdar_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
