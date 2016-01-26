-module(person, [Id, 
                 Name::string(), 
                 DisplayName::string(),
                 Description::string(),
                 MemberId::string(), % member who adds it
                 Created::string()]).
-compile(export_all).

-belongs_to(member).
-has({works, many}).
-has({proses, many}).
-had({quotes, many}).
