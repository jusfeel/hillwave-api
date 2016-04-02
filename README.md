# hillwave-api

API Erlang code base serving hillwave.cn using Chicago Boss via JSONAPI

This is the API code to support [www.hillwave.cn](http://www.hillwave.cn) which I developed using [Ember.js](http://www.emberjs.com)

Database is using Riak which Boss is not fully supporting its search feature so I customize at the time when boss at v.0.8.15. It's reflected in all the rebar.config that I have also added many different modules to support many features like bcrypt, Oauth, email validation.

Could not understand the JSONAPI at the time so I developed a very small serving my purpose module because Ember.js is using JSONAPI.

Ember code would be open source soon after some issues are settled down. 

# hillwave
It's a website to show case Chinese calligrapy works plus potentially a place to commuicate about calligraphy works which I wish it be the fastest web app ( as long as you are in China ) to explore Calligraphy works and poems etc. 

So far, ember.js serves the purpose. I have no idea about Boss. The choice for boss is for the love of Erlang.


