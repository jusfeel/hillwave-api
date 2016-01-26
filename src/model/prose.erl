-module(prose, [Id, 
                Title::string(), 
                Description::string(),
                AuthorId::string(), % could be undefined
                MemberId::string(), % member who adds it
                Created::string()]).
-compile(export_all).

-belongs_to(member). 
-belongs_to_person(author).

-has({works, many}).
