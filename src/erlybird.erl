-module(erlybird).
-export([get_secrets/0, post/4,get_user_timeline/4, get_entire_timeline/4,get_entire_timeline/5, get_tweet/1, get_tweet/2, get_tweet/5, hex_digit/1, escape_byte/1,create_signature_base_string/3,create_oauth_header_string/1,create_oauth_header/9, sign/5, create_parameter_string/1, upload/1, upload/4]).

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

current_time_seconds()->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_oauth_timestamp()->
    lists:flatten(io_lib:format("~p", [current_time_seconds()])).

get_oauth_nonce()->
    base64:encode_to_string(list_to_binary(integer_to_list(current_time_millis()))).

add_params_to_url(Url, Params)->
    append_params_to_url( string:concat(Url, "?"), Params).

append_params_to_url(Url, [H|T])->  
    case lists:last(Url) of
	63 ->
	    NewUrlBase = Url;
	_ ->
	    NewUrlBase = string:concat(Url, "&")
    end,
    {ParamName, ParamValue}=H,
    append_params_to_url(string:concat(string:concat(string:concat(NewUrlBase, escape_uri(ParamName)), "="), escape_uri(ParamValue)), T); 
append_params_to_url(Url, [])->
    Url.


append_params_to_body(Body, [H|T])->  
    case length(Body) of
	0 ->
	    NewBodyBase = [];
	_ ->
	    NewBodyBase = string:concat(Body, "&")
    end,
    {ParamName, ParamValue}=H,
    append_params_to_url(string:concat(string:concat(string:concat(NewBodyBase, escape_uri(ParamName)), "="), escape_uri(ParamValue)), T); 
append_params_to_body(Body, [])->
    Body.


upload(Media)->
    {Consumer, AccessToken, AccessTokenSecret}=get_secrets(),
    upload(Media,  Consumer, AccessToken, AccessTokenSecret). 

upload(Media, Consumer, AccessToken, AccessTokenSecret)-> 

        {ConsumerKey, ConsumerSecret, hmac_sha1}=Consumer,


    MediaId = upload_init(Media, Consumer, AccessToken, AccessTokenSecret),
    io:format("~p~n", [MediaId]),

    Chunks=chunk(Media, 512),
    upload_append(Chunks, MediaId, 0, ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret).
    
   
chunk(BinaryMedia, ChunkSize)->    
    chunk(BinaryMedia, ChunkSize, []).
chunk(BinaryMedia, ChunkSize, Chunks)->
    case byte_size(BinaryMedia) < ChunkSize of 
	true ->
	    lists:append(Chunks, [base64:encode_to_string(BinaryMedia)]);
	false ->
	    {H,T}=split_binary(BinaryMedia, ChunkSize),
	    NewChunks=lists:append(Chunks, [base64:encode_to_string(H)]), 
	    chunk(T, ChunkSize, NewChunks)
    end.

generate_multipart__body(Params, Boundary)->
    generate_multipart__body(Params, Boundary, []).

generate_multipart__body([H|T], Boundary, Acc)->
    {Name, Value}=H,
    NewAcc=string:concat(Acc,string:concat(string:concat(string:concat(string:concat(string:concat(Boundary, "\nContent-Disposition: form-data; name=\""), Name),"\"\n\n"), Value), "\n")),
    generate_multipart__body(T, Boundary, NewAcc);
generate_multipart__body([], Boundary, Acc) ->
    string:concat(Acc, Boundary).

    

upload_append([MediaData|T], MediaId, SegmentId, ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)-> 




    Params = [{"command", "APPEND"},
	      {"media_id", MediaId},
	      {"media_data", MediaData},
	      {"segment_index", integer_to_list(SegmentId)}],
    
    Url =  "https://upload.twitter.com/1.1/media/upload.json",



    Headers =  [{"Accept", "*/*"},
		{"Host","upload.twitter.com"},
		{"Content-Type","multipart/form-data; boundary=fooo"},
		{"Authorization",
		 create_oauth_header([], %remove params
				    Url,
				     ConsumerKey,
				     ConsumerSecret,
				     AccessToken, 
				     AccessTokenSecret, 
				     get_oauth_nonce(),
				     get_oauth_timestamp(), 
				     "Post")
		}],
    io:format("~s~n", [generate_multipart__body(Params, "--fooo")]),
    {ok, Response} = httpc:request(post,
				   {Url,
				    Headers,
				    "multipart/form-data",
				    generate_multipart__body(Params, "--fooo")
				   }, [], [{headers_as_is, true}]),
    io:format("~p~n", [Response]),
    "x" = Response,
    upload_append(T, MediaId, SegmentId + 1, ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret);

upload_append([], MediaId, _, _, _,_,_)-> 
    {ok, MediaId}.
    
upload_init(Media, Consumer, AccessToken, AccessTokenSecret)-> 

    {ConsumerKey, ConsumerSecret, hmac_sha1}=Consumer,
    TotalBytes =  lists:flatten(io_lib:format("~p", [byte_size(Media)])),

    
    Headers =  [{"Accept", "*/*"},
		{"Host","upload.twitter.com"},
		{"Content-Type","application/x-www-form-urlencoded"},
		{"Authorization",
		 create_oauth_header([{"command", "INIT"},
				      {"total_bytes", TotalBytes},
				      {"media_type", "img/png"}],
				     "https://upload.twitter.com/1.1/media/upload.json",
				     ConsumerKey,
				     ConsumerSecret,
				     AccessToken, 
				     AccessTokenSecret, 
				     get_oauth_nonce(),
				     get_oauth_timestamp(), 
				     "Post")
		}],

    {ok, Response} = httpc:request(post,
			     {string:concat(string:concat("https://upload.twitter.com/1.1/media/upload.json?command=INIT", string:concat("&total_bytes=", TotalBytes)), "&media_type=img%2Fpng"),
			      Headers,
			      "application/x-www-form-urlencoded",
			      []
			     }, [], [{headers_as_is, true}]),
    {_,_, Body}=Response,
    {[{_,_},{<<"media_id_string">>,MediaId}, _]}=jiffy:decode(Body),
    binary_to_list(MediaId).




post(Tweet) -> 
    {Consumer, AccessToken, AccessTokenSecret}=get_secrets(),
    post(Tweet, Consumer, AccessToken, AccessTokenSecret).

post(Tweet, Consumer, AccessToken, AccessTokenSecret)->

    EscapedTweet= escape_uri(Tweet),
    Status=string:concat("status=", EscapedTweet),
   
    {ConsumerKey, ConsumerSecret, hmac_sha1}=Consumer,

    Headers =  [{"Accept", "*/*"},
		{"Host","api.twitter.com"},
		{"Content-Type","application/x-www-form-urlencoded"},
		{"Authorization",
		 create_oauth_header([{"status", Tweet}], "https://api.twitter.com/1.1/statuses/update.json", ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret, get_oauth_nonce(), get_oauth_timestamp(), "Post")}
	       ],
            httpc:request(post,
		  {string:concat("https://api.twitter.com/1.1/statuses/update.json?", Status),
		   Headers,
		   "application/x-www-form-urlencoded",
		   []
		  }, [], [{headers_as_is, true}]).

get_tweet(TweetId)->
    get_tweet(TweetId,[]).
get_tweet(TweetId, Parameters)->
    {Consumer, AccessToken, AccessTokenSecret}=get_secrets(),
    get_tweet(TweetId, Parameters, Consumer, AccessToken, AccessTokenSecret).

get_tweet(TweetId, Parameters, Consumer, AccessToken, AccessTokenSecret)->
    ParametersWithId=lists:append(Parameters, [{"id", TweetId}]),
    Url = string:concat("https://api.twitter.com/1.1/statuses/show.json?", create_parameter_string(ParametersWithId)),
    get_request(Url, ParametersWithId, Consumer, AccessToken, AccessTokenSecret).
    
										
key_to_string(Key)->
    case is_atom(Key) of
	true ->
	    atom_to_list(Key);
	false  ->
	    case is_number(Key) of
		true ->
		    integer_to_list(Key);
		false  ->
		    Key
	    end
    end.
    
% generic wrapper for making get requests
get_request(Url, Parameters, Consumer, AccessToken, AccessTokenSecret)->
    [RequestUrl|_]=string:tokens(Url, "?"),
    {ConsumerKey, ConsumerSecret, hmac_sha1}=Consumer,

    Response= httpc:request(get,
			    {Url,
			     [{"Accept", "*/*"},
			      {"Host","api.twitter.com"},
			      {"Authorization",
			       create_oauth_header(Parameters, RequestUrl, ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret, get_oauth_nonce(), get_oauth_timestamp(), "Get")}
			    ]
			    },
    [],
    [{headers_as_is, true}]),
        case Response of 
	    {ok,{{"HTTP/1.1",200,"OK"}, Headers, Body}} -> jiffy:decode(Body);
	    {ok,{{"HTTP/1.1",StatusCode,StatusMessage}, Headers, Body}} -> jiffy:decode(Body)
	end.

    




%roll yer own oauth

escape_uri([C | Cs]) when C >= $a, C =< $z ->
%    io:format("a-z:~p~n",[C]),
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
escape_uri([C = $* | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C > 16#7f ->
    HexStr = integer_to_list(C, 16),
    lists:flatten([$%, HexStr]) ++ escape_uri(Cs);
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


create_oauth_header(RequestParameters, Url, ConsumerKey, ConsumerSecret, OauthToken, OauthTokenSecret, OauthNonce, OauthTimestamp, HttpMethod)->
    
    OauthSignatureMethod = "HMAC-SHA1",
    OauthVersion = "1.0",
    
    
    OauthParameters = [ {"oauth_consumer_key", ConsumerKey},
			{"oauth_nonce",OauthNonce},
			{"oauth_signature_method", OauthSignatureMethod},
			{"oauth_timestamp", OauthTimestamp},
			{"oauth_token", OauthToken},
			{"oauth_version",OauthVersion}],
    SigningParameters = lists:append(OauthParameters, RequestParameters),
    OauthSignature = sign(SigningParameters, Url, ConsumerSecret, OauthTokenSecret, HttpMethod),
    SignedOauthParameters = lists:append(OauthParameters, [{"oauth_signature", OauthSignature}]),
    create_oauth_header_string(SignedOauthParameters).

    


sign(Parameters, Url, ConsumerSecret, OauthTokenSecret, HttpMethod)->
    ParameterString = create_parameter_string(Parameters),
    SignatureBaseString = create_signature_base_string(ParameterString, Url, HttpMethod),
    SigningKey= get_signing_key(ConsumerSecret, OauthTokenSecret),
    io:format("~p~n",[crypto:hmac(sha, SigningKey, SignatureBaseString)]),
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


create_oauth_header_string(Parameters)->
    EncodedParameters = encode_parameters(Parameters),
    SortedEncodedParamters = lists:keysort(1, EncodedParameters),
    lists:foldl(fun({X, Y}, Acc) ->
			case Acc of
			    "OAuth " ->
				string:concat(Acc, string:concat(string:concat(X, "="), string:concat(string:concat("\"", Y), "\"")));
			    _ -> string:concat(Acc, string:concat(string:concat(string:concat(", ", X), "="), string:concat(string:concat("\"", Y), "\"")))
			end
		end,
		"OAuth ",
		SortedEncodedParamters).
			

create_signature_base_string(ParameterString, Url, HttpMethod)->

    UpperCaseHttpMethod = string:to_upper(HttpMethod),
    SignatureBaseStringPrefix = string:concat(UpperCaseHttpMethod, string:concat("&", escape_uri(Url))),
    string:concat(string:concat(SignatureBaseStringPrefix, "&"), escape_uri(ParameterString)).
    
    
get_signing_key(ConsumerSecret, OauthTokenSecret)->					      
    string:concat(string:concat(escape_uri(ConsumerSecret), "&"), escape_uri(OauthTokenSecret)).
    
  
encode_parameters(Parameters)->
    lists:map(fun({X,Y}) ->
		      {escape_uri(key_to_string(X)), escape_uri(key_to_string(Y))}
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

encode_atom_test()->
    EncodedUri = encode_parameters([{foo, bar}]),
    io:format("~p~n", [EncodedUri]),
     ?assert(EncodedUri  =:=  [{"foo","bar"}]).

encode_int_test()->
    EncodedUri = encode_parameters([{foo, 1}]),
    io:format("~p~n", [EncodedUri]),
     ?assert(EncodedUri  =:=  [{"foo","1"}]).

create_oauth_header_string_test()->
    OauthHeaderString = create_oauth_header_string([{"foo", "bar"}, {"bing", "boo"}]),
    ExpectedOauthHeaderString = "OAuth bing=\"boo\", foo=\"bar\"",
     ?assert(OauthHeaderString  =:= ExpectedOauthHeaderString).

create_oauth_header_test()->
    RequestParameters = [
			 {"include_entities", "true"},
			 {"status", "Hello Ladies + Gentlemen, a signed OAuth request!"}],
    Url = "https://api.twitter.com/1/statuses/update.json",
    ConsumerKey =  "xvz1evFS4wEEPTGEFPHBog",
    ConsumerSecret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw",
    OauthToken =  "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb",
    OauthTokenSecret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    OauthNonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg",
    OauthTimestamp = "1318622958",
    HttpMethod="Post",
    OauthHeader  = create_oauth_header(RequestParameters, Url, ConsumerKey, ConsumerSecret, OauthToken, OauthTokenSecret, OauthNonce, OauthTimestamp, HttpMethod),
    ExpectedOauthHeader = "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", oauth_signature=\"tnnArxj06cWHq44gCs1OSKk%2FjLY%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1318622958\", oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", oauth_version=\"1.0\"",
    io:format("~n~p~n", [OauthHeader]),
    io:format("~n~p~n", [ExpectedOauthHeader]),

    ?assert(OauthHeader  =:= ExpectedOauthHeader).

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
    
encode_unicode_test()->

    SamsungTm="A cropped view of a Samsung™ monitor. The visible portion of the screen displays the Windows© 7 Professional logo.",	     
    EncodedSamsungTm=escape_uri(SamsungTm),
    TestEncodedSamsungTm="A%20cropped%20view%20of%20a%20Samsung%E2%84%A2%20monitor.%20The%20visible%20portion%20of%20the%20screen%20displays%20the%20Windows%C2%A9%207%20Professional%20logo.",
    io:format("~p", [EncodedSamsungTm]),
    io:format("~s", [TestEncodedSamsungTm]),

    ?assert(EncodedSamsungTm  =:= TestEncodedSamsungTm).

encode_amp_test()->

    Amp="An inscription in the aforementioned book reads \"For Ian\nHappy reading &\nsuccess to crime\n\n*illegible signature*\"",	     
    EncodedAmp=escape_uri(Amp),
    TestEncodedAmp="An%20inscription%20in%20the%20aforementioned%20book%20reads%20%22For%20Ian%0AHappy%20reading%20%26%0Asuccess%20to%20crime%0A%0A*illegible%20signature*%22",
    io:format("~s~n", [EncodedAmp]),
    io:format("~s~n", [TestEncodedAmp]),

    ?assert(EncodedAmp  =:= TestEncodedAmp).
    


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
    
add_params_to_url_test()->
    ?assert(add_params_to_url("http://www.hccp.org",[{"foo", "bar/png"}, {"blue", "red"}, {"glarg", "bling"}]) =:= "http://www.hccp.org?foo=bar%2Fpng&blue=red&glarg=bling").
    
-endif.
