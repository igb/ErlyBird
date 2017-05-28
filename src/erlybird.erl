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




%roll yer own oauth

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


sign(Parameters, Url, ConsumerSecret, OauthTokenSecret, HttpMethod)->
    ParameterString = create_parameter_string(Parameters),
    SignatureBaseString = create_signature_base_string(ParameterString, Url, HttpMethod),
    SigningKey= get_signing_key(ConsumerSecret, OauthTokenSecret),
    base64:encode_to_string(crypto:hmac(sha, SigningKey, SignatureBaseString)).

create_parameter_string(Parameters)->
    EncodedParameters = encode_parameters(Parameters),
    SortedEncodedParamters = lists:keysort(1, EncodedParameters),
    lists:foldl(fun({X, Y}, Acc) ->
			case Acc of
			    [] ->
				string:concat(string:concat(X, "="), Y);
			    _ -> string:concat(Acc, string:concat(string:concat(string:concat("&", X), "="), Y))
			end
		end,
		[],
		SortedEncodedParamters).
			

create_signature_base_string(ParameterString, Url, HttpMethod)->

    UpperCaseHttpMethod = string:to_upper(HttpMethod),
    SignatureBaseStringPrefix = string:concat(UpperCaseHttpMethod, string:concat("&", escape_uri(Url))),
    string:concat(string:concat(SignatureBaseStringPrefix, "&"), escape_uri(ParameterString)).
    
    
get_signing_key(ConsumerSecret, OauthTokenSecret)->					      
    string:concat(string:concat(escape_uri(ConsumerSecret), "&"), escape_uri(OauthTokenSecret)).
    
  
encode_parameters(Parameters)->
    lists:map(fun({X,Y}) ->
		      {escape_uri(X), escape_uri(Y)}
	      end,
	      Parameters).


-ifdef(TEST).

get_test_parameters()->
    Parameters = [
		  {"include_entities", "true"},
		  {"status", "Hello Ladies + Gentlemen, a signed OAuth request!"},
		  {"oauth_consumer_key", "xvz1evFS4wEEPTGEFPHBog"},
		  {"oauth_nonce", "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"},
		  {"oauth_signature_method", "HMAC-SHA1"},
		  {"oauth_timestamp", "1318622958"},
		  {"oauth_token", "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"},
		  {"oauth_version", "1.0"}
		 ],
    Parameters.

get_signing_key_test()->
    ConsumerSecret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw",
    OauthTokenSecret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    ExpectedSigningKey = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    SigningKey = get_signing_key(ConsumerSecret, OauthTokenSecret),
     ?assert(SigningKey  =:= ExpectedSigningKey).
   

create_parameter_string_test()->
    ExpectedParameterString = "include_entities=true&oauth_consumer_key=xvz1evFS4wEEPTGEFPHBog&oauth_nonce=kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg&oauth_signature_method=HMAC-SHA1&oauth_timestamp=1318622958&oauth_token=370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb&oauth_version=1.0&status=Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21",
    Parameters = get_test_parameters(),
    ParameterString = create_parameter_string(Parameters),
     ?assert(ParameterString  =:= ExpectedParameterString).

create_signature_base_string_test()->
    ExpectedSignatureBaseString = "POST&https%3A%2F%2Fapi.twitter.com%2F1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521",

    Parameters = get_test_parameters(),
    SignatureBaseString = create_signature_base_string(create_parameter_string(Parameters), "https://api.twitter.com/1/statuses/update.json", "post"),
    io:format("~n~p~n", [SignatureBaseString]),
    io:format("~n~p~n", [ExpectedSignatureBaseString]),
    ?assert(SignatureBaseString  =:= ExpectedSignatureBaseString).

encode_parameters_test()->

    Parameters = get_test_parameters(),

    ExpectedParameters = [
			  {"include_entities", "true"},
			  {"status", "Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21"},
			  {"oauth_consumer_key", "xvz1evFS4wEEPTGEFPHBog"},
			  {"oauth_nonce", "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"},
			  {"oauth_signature_method", "HMAC-SHA1"},
			  {"oauth_timestamp", "1318622958"},
			  {"oauth_token", "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"},
			  {"oauth_version", "1.0"}
			 ],

    EncodedParameters=encode_parameters(Parameters),
    ?assert(EncodedParameters  =:= ExpectedParameters).
    




escape_uri_test()->
    Status="Hello Ladies + Gentlemen, a signed OAuth request!",
    EscapedStatus = escape_uri(Status),
    TestEncodedValue="Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21",
    ?assert(EscapedStatus  =:= TestEncodedValue).

sign_test()->
    Parameters = get_test_parameters(),
    ConsumerSecret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw",
    OauthTokenSecret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    Url = "https://api.twitter.com/1/statuses/update.json",
    HttpMethod = "post",
    Signature = sign(Parameters, Url, ConsumerSecret, OauthTokenSecret, HttpMethod),
    ExpectedSignature = "tnnArxj06cWHq44gCs1OSKk/jLY=",
    ?assert(Signature  =:= ExpectedSignature).
    

-endif.
