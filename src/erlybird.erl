-module(erlybird).
-export([get_secrets/0, post/4,get_user_timeline/4, get_entire_timeline/4,get_entire_timeline/5]).

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
    get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret, 100000000).

get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret, Limit)->

    get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret, Limit, [], 0).

get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret,  Limit, Acc, LastTweetId)->
   case length(Acc) > Limit of
       true ->
	   Acc;
       false ->
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
		   get_entire_timeline(Parameters, Consumer, AccessToken, AccessTokenSecret, Limit,  NewAcc, NewLastTweetId)
	   end
   end.
    
 
current_time_millis()->   
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).


post(Tweet, Consumer, AccessToken, AccessTokenSecret)->
    Status="status=test",
    io:format(Status),
    BodyHash = base64:encode_to_string(crypto:hash(sha, Status)),
    io:format(BodyHash),
    Headers =  [{"Accept", "*/*"},
		    {"Host","api.twitter.com"},
		    {"Content-Type","application/x-www-form-urlencoded"},
		    {"Authorization",
		     lists:append("OAuth ",
				  oauth:header_params_encode(

oauth:sign("POST", "https://api.twitter.com/1.1/statuses/update.json", [{"status", "test"}], Consumer, AccessToken, AccessTokenSecret)
							    )
				 )
		    }
		   ],
    
io:fwrite("~n~p~n", [Headers]),
       httpc:request(post,
		  {"http://api.twitter.com/1.1/statuses/update.json",
		    Headers,
                    "application/x-www-form-urlencoded",
                    Status
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


escape_uri([C | Cs]) when C >= $a, C =< $z ->
        [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
	[C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C > 16#7f ->
    %% This assumes that characters are at most 16 bits wide.
    escape_byte(((C band 16#c0) bsr 6) + 16#c0)
	++ escape_byte(C band 16#3f + 16#80)
	++ escape_uri(Cs);
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $A - 10.


-ifdef(TEST).
post_signature_test()->
    Status="Hello Ladies + Gentlemen, a signed OAuth request!",
    OauthConsumerKey="xvz1evFS4wEEPTGEFPHBog",
    OauthNonce="kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg",
    OauthSignatureMethod="HMAC-SHA1",
    OauthTimestamp="1318622958",
    OauthToken="370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb",
    OauthVersion="1.0",
    EscapedStatus = escape_uri(Status),
    TestEncodedValue="Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21",
    io:fwrite("~n"),
    io:fwrite("~s~n", [EscapedStatus]),
    io:fwrite("~s~n", [TestEncodedValue]),
   % EscapedStatus="Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21",
    ?assert(EscapedStatus  =:= TestEncodedValue),
    ok.
    
-endif.
