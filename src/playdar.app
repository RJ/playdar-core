{application, playdar,
 [{description, "playdar"},
  {vsn, "0.01"},
  {modules, [
erlydtl, erlydtl_compiler, erlydtl_dateformat, erlydtl_deps, erlydtl_filters, erlydtl_parser, erlydtl_runtime, erlydtl_scanner, http_registry, mochifmt, mochifmt_records, mochifmt_std, mochihex, mochijson2, mochijson, mochinum, mochiweb_app, mochiweb, mochiweb_charref, mochiweb_cookies, mochiweb_echo, mochiweb_headers, mochiweb_html, mochiweb_http, mochiweb_multipart, mochiweb_request, mochiweb_response, mochiweb_skel, mochiweb_socket_server, mochiweb_sup, mochiweb_util, modules_sup, playdar_app, playdar_auth, playdar, playdar_config, playdar_deps, playdar_http_api, playdar_reader, playdar_reader_registry, playdar_resolver, playdar_sup, playdar_web, qry, reloader, resolver, resolver_sup, script_resolver, utils
  ]},
  {registered, []},
  {mod, {playdar_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
