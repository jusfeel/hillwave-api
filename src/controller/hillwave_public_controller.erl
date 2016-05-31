-module(hillwave_public_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

sitemap('GET', []) ->
  Sitemap = sitemap:gen(),
  {output, Sitemap}.

