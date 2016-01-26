-module(verification, [Id, 
                       MemberId::string(), 
                       Type::string(), 
                       Code::string(), 
                       Created::string()]).
-compile(export_all).

-belongs_to(member).
