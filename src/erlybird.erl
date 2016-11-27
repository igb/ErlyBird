-module(erlybird).
-export([get_secrets/0, post/4,get_user_timeline/4, get_entire_timeline/4]).

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



get_user_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret)->
    
    Params = [lists:flatten([atom_to_list(Key), "=", Value]) || {Key, Value} <- Parameters],

    Url=lists:foldr(fun(X, Url) -> lists:flatten([Url, X, "&"]) end, "https://api.twitter.com/1.1/statuses/user_timeline.json?", Params),
    
    get_request(Url, Parameters, Consumer, AccessToken, AccessTokenSecret).

get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret)->

    get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret, [], 0).

get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret,  Acc, LastTweetId)->
   case LastTweetId of
      0 -> NewParameters = Parameters;
       _ -> NewParameters = lists:append(Parameters, [{max_id, integer_to_list(LastTweetId - 1)}])
   end,
    

	    
    io:format("last tweet ID: ~p~n", [LastTweetId]),
    Timeline = get_user_timeline(NewParameters, Consumer, AccessToken, AccessTokenSecret),
    case Timeline of
	[] ->
	    Acc;
	_ ->
	    NewAcc=lists:append(Acc, Timeline),
	    [{Tweet}|_]=lists:reverse(Timeline),
	    {_, NewLastTweetId}=lists:keyfind(<<"id">>,1, Tweet),
	    get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret,  NewAcc, NewLastTweetId)
    end.
    
 
current_time_millis()->   
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).


post(Tweet, Consumer, AccessToken, AccessTokenSecret)->
    httpc:request(get,
		  {"https://api.twitter.com/1.1/statuses/retweets/509457288717819904.json",
		   [{"Accept", "*/*"},
		    {"Host","api.twitter.com"},
		    {"Authorization",
		     lists:append("OAuth ",
				  oauth:header_params_encode(

oauth:sign("GET", "https://api.twitter.com/1.1/statuses/retweets/509457288717819904.json", [], Consumer, AccessToken, AccessTokenSecret)
							    )
				 )
		    }
		   ]
		  }, [], [{headers_as_is, true}]).



% generic wrapper for making get requests
get_request(Url, Parameters, Consumer, AccessToken, AccessTokenSecret)->
    {ok,{{"HTTP/1.1",200,"OK"},
     Headers, Body}} = httpc:request(get,
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
				     [{headers_as_is, true}]),

    jiffy:decode(Body).



-ifdef(TEST).

-endif.
