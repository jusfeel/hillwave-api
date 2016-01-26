-module(quote, [Id, 
                 QuotedContent::string(), 
                 PersonId::string(), % person who says it
                 MemberId::string(),
                 Created::string()]).
-compile(export_all).

-belongs_to(member).
-belongs_to(person).
