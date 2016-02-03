-module(person, [Id,
                 Name::string(),
                 DisplayName::string(),
                 Description::string(),
                 MemberId::string(), % member who adds it
                 Created::string()]).
-compile(export_all).

-belongs_to(member).
-has({works, many, [{foreign_key, writer_id}]}).
-has({proses, many, [{foreign_key, author_id}]}).
-has({quotes, many}).
