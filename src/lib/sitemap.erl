-module(sitemap).
-compile(export_all).
-include("hillwave.hrl").

gen() ->
  Header = header(),
  Footer = footer(),
  Body = body(),
  Output = Header ++ Body ++ Footer,
  file:write_file(?SITEMAP_PATH, Output),
  Output.

header() ->
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">".

footer() ->
  "\n</urlset>".

body() ->
  urls(work, "work") ++ urls(person, "person") ++ urls(prose, "prose") ++ urls(quote, "quote").

urls(Entity, Path) when is_atom(Entity), is_list(Path) ->
  Entities = boss_db:find(Entity, []),
  F = fun(Elem, Acc) ->
    Id = Elem:id(),
    Url = ?SITE_URL ++ "/" ++ Path ++ "/" ++ Id,
    Acc ++ "\n<url><loc>" ++ Url ++ "</loc><priority>1.0</priority></url>"
  end,
  lists:foldl(F, "", Entities).
