-module(erlybird).
-export([get_secrets/0, post/4,get_user_timeline/4]).

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



%https://api.twitter.com/1.1/statuses/user_timeline.json

get_user_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret)->
    
    Params = [lists:flatten([atom_to_list(Key), "=", Value]) || {Key, Value} <- Parameters],

    Url=lists:foldr(fun(X, Url) -> lists:flatten([Url, X, "&"]) end, "https://api.twitter.com/1.1/statuses/user_timeline.json?", Params),
    
    get_request(Url, Parameters, Consumer, AccessToken, AccessTokenSecret).

    
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



% generic wrapper for making get requests
get_request(Url, Parameters, Consumer, AccessToken, AccessTokenSecret)->
    httpc:request(get,
		  {Url,
		   [{"Accept", "*/*"},
		    {"Host","api.twitter.com"},
		    {"Authorization",
		     lists:append("OAuth ",
				  oauth:header_params_encode(oauth:sign("GET", Url, [{atom_to_list(Key), Value} || {Key, Value} <- Parameters], Consumer, AccessToken, AccessTokenSecret)))
		    }
		   ]
		  },
		  [],
		  [{headers_as_is, true}]).



-ifdef(TEST).

-endif.
