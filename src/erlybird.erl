-module(erlybird).
-export([get_secrets/0, post/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



get_secrets()->
	{ok,[[ConsumerKey]]}=init:get_argument(consumer_key),
	{ok,[[ConsumerSecret]]}=init:get_argument(consumer_secret),
	{ok,[[AccessToken]]}=init:get_argument(access_token),
        {ok,[[AccessTokenSecret]]}=init:get_argument(access_token_secret),
        Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
	{Consumer, AccessToken, AccessTokenSecret}.



%

post(Tweet, Consumer, AccessToken, AccessTokenSecret)->
    httpc:request(get,
		  {"https://api.twitter.com/1.1/statuses/retweets/509457288717819904.json",
		   [{"Accept", "*/*"},
		    {"Host","api.twitter.com"},
		    {"Authorization",
		     lists:append("OAuth ",
				  oauth:header_params_encode(
%https://twitter.com/igb/status/769196661545377793
oauth:sign("GET", "https://api.twitter.com/1.1/statuses/retweets/509457288717819904.json", [], Consumer, AccessToken, AccessTokenSecret)
							    )
				 )
		    }
		   ]
		  }, [], [{headers_as_is, true}]).


-ifdef(TEST).

-endif.
