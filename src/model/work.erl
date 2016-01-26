-module(work, [Id, 
               Title::string(), 
               Description::string(),
               Image::string(), 
               WriterId::string(), % could be undefined
               ProseId::string(),
               MemberId::string(), % member who adds it
               Published::boolean(),
               Created::string()]).
-compile(export_all).

-belongs_to(member). 
-belongs_to(prose). 
-belongs_to_person(writer).

