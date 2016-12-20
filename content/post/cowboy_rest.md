+++
date = "2016-12-20T19:40:44+08:00"
title = "HTTP¿¿¿¿cowboy_rest"
draft = true
description = "HTTP Request parser"
tags = ["Erlang/OTP", "code", "cowboy"]
topics = ["Erlang/OTP", "code", "cowboy"]
+++

cowboy_rest¿REST¿¿¿¿¿¿¿¿¿¿¿HTTP¿¿¿¿¿¿¿nginx¿¿¿¿¿¿¿¿¿nginx¿¿¿¿¿¿¿¿http¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿cowboy¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿middlewares¿¿¿¿¿execute¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿nginx¿¿¿¿¿¿¿¿¿markdown¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿cowboy_rest¿¿¿¿¿¿¿¿¿¿

cowboy_handler¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿{cowboy_rest, Req, State}¿¿¿¿¿cowboy_rest:upgrade/6¿

```
execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts}) ->
	try Handler:init(Req, HandlerOpts) of
		...
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		...
```

<!--more-->

¿¿¿Mod¿cowboy_rest¿

cowboy_rest:upgrade¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿HTTP¿¿¿¿¿¿¿¿¿¿¿¿¿¿expect¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿OnTrue¿OnFalse¿

```
expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
	case call(Req, State, Callback) of
		no_call ->
			next(Req, State, OnTrue);
		{stop, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{Expected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnTrue);
		{_Unexpected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnFalse)
	end.

call(Req, State=#state{handler=Handler, handler_state=HandlerState},
		Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true ->
			try
				Handler:Callback(Req, HandlerState)
			catch Class:Reason ->
				error_terminate(Req, State, Class, Reason)
			end;
		false ->
			no_call
	end.
	
next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).
```

¿¿¿¿¿¿¿¿¿¿¿¿expect¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿service_avaliable¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿503¿

```
-callback service_available(Req, State) when Req::cowboy_req:req(), State::any() -> 
	{boolean(), Req, State} | {stop, Req, State}.
```

¿¿¿¿¿known_methods/2¿¿¿¿¿¿¿¿¿¿¿¿¿¿<<"HEAD">> <<"GET">> <"POST">> <<"PUT">> <<"PATCH">> <<"DELETE">> <<"OPTIONS">>¿¿¿501¿¿¿¿uri_too_long:

```
uri_too_long(Req, State) ->
	expect(Req, State, uri_too_long, false, fun allowed_methods/2, 414).
```

¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿false¿¿414¿¿HTTP¿¿¿¿¿allowed_methods¿¿¿¿¿¿OPTIONS¿¿¿¿¿¿¿¿¿¿¿¿¿405¿¿¿¿¿¿¿¿¿¿malformed_request¿

```
malformed_request(Req, State) ->
	expect(Req, State, malformed_request, false, fun is_authorized/2, 400).
```

malformed_request¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿is_authorized¿is_authorized¿forbidden¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿options¿¿¿

```
forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun valid_content_headers/2, 403).

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true, fun valid_entity_length/2, 501).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).
```

options¿¿ <<"OPTIONS">> ¿¿¿¿¿allow¿¿¿¿¿¿200¿¿¿¿¿¿¿content_types_provided/2¿¿¿

content_types_provided¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿ *MIME¿¿¿¿¿¿¿¿* ¿¿¿¿¿¿¿¿¿cowboy_rest¿¿¿¿¿content_types_p¿¿¿¿¿¿¿Accept¿¿¿¿¿prioritize_accept/1¿¿¿¿¿¿¿¿¿¿¿choose_media_type/3¿¿¿¿¿¿¿¿¿¿¿¿¿content_type_a¿media_type¿¿¿¿¿languages_provided/2¿

languages_provided/2¿¿¿¿¿¿¿¿¿¿¿accept-language¿¿¿¿languages_p¿languages_a¿¿¿content-language¿¿¿¿¿¿charsets_provided/2¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿accept-charset¿¿¿¿¿¿¿¿set_content_type¿¿¿¿¿¿¿content-type¿¿

¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿variances¿¿¿¿Vary¿¿¿¿¿¿¿¿¿¿¿¿¿¿resource_exists/2¿¿¿¿¿¿¿¿¿¿¿

```
resource_exists(Req, State) ->
	expect(Req, State, resource_exists, true, fun if_match_exists/2, fun if_match_must_not_exist/2).
```

if_match_exists/2 ¿ if_match_must_not_exist/2 ¿¿¿¿¿ if-match ¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿if-match¿etag¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿if-unmodified-since¿if-modified-since¿if-match¿if-none-match¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿

if-match¿¿¿¿¿¿¿¿etag¿¿¿¿¿

```
if_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		%% Strong Etag comparison: weak Etag never matches.
		{{weak, _}, Req2, State2} ->
			precondition_failed(Req2, State2);
		{Etag, Req2, State2} ->
			case lists:member(Etag, EtagsList) of
				true -> if_none_match_exists(Req2, State2);
				%% Etag may be `undefined' which cannot be a member.
				false -> precondition_failed(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

precondition_failed ¿¿ 412 ¿¿¿¿¿¿if_none_match_exists¿¿if-none-match¿¿¿¿

```
if_none_match_exists(Req, State) ->
	case cowboy_req:parse_header(<<"if-none-match">>, Req) of
		undefined ->
			if_modified_since_exists(Req, State);
		'*' ->
			precondition_is_head_get(Req, State);
		EtagsList ->
			if_none_match(Req, State, EtagsList)
	end.

if_none_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		{Etag, Req2, State2} ->
			case Etag of
				undefined ->
					precondition_failed(Req2, State2);
				Etag ->
					case is_weak_match(Etag, EtagsList) of
						true -> precondition_is_head_get(Req2, State2);
						false -> method(Req2, State2)
					end
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
	
precondition_is_head_get(Req, State=#state{method=Method})
		when Method =:= <<"HEAD">>; Method =:= <<"GET">> ->
	not_modified(Req, State);
precondition_is_head_get(Req, State) ->
	precondition_failed(Req, State).
```

precondition_is_head_get¿¿etag¿¿¿¿¿¿¿¿if_modified_since_exists¿¿if-modified-since¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿not_modified¿¿304¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿method¿¿¿¿¿

```
if_modified_since_exists(Req, State) ->
	try cowboy_req:parse_header(<<"if-modified-since">>, Req) of
		undefined ->
			method(Req, State);
		IfModifiedSince ->
			if_modified_since_now(Req, State, IfModifiedSince)
	catch _:_ ->
		method(Req, State)
	end.

if_modified_since_now(Req, State, IfModifiedSince) ->
	case IfModifiedSince > erlang:universaltime() of
		true -> method(Req, State);
		false -> if_modified_since(Req, State, IfModifiedSince)
	end.

if_modified_since(Req, State, IfModifiedSince) ->
	try last_modified(Req, State) of
		{undefined, Req2, State2} ->
			method(Req2, State2);
		{LastModified, Req2, State2} ->
			case LastModified > IfModifiedSince of
				true -> method(Req2, State2);
				false -> not_modified(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

not_modified¿¿etag¿expires¿¿¿¿304¿¿¿

```
not_modified(Req, State) ->
	Req2 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
	try set_resp_etag(Req2, State) of
		{Req3, State2} ->
			try set_resp_expires(Req3, State2) of
				{Req4, State3} ->
					respond(Req4, State3, 304)
			catch Class:Reason ->
				error_terminate(Req, State2, Class, Reason)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

if_unmodified_since_exists¿¿if-unmodified-since¿¿¿¿

```
if_unmodified_since(Req, State, IfUnmodifiedSince) ->
	try last_modified(Req, State) of
		{LastModified, Req2, State2} ->
			case LastModified > IfUnmodifiedSince of
				true -> precondition_failed(Req2, State2);
				false -> if_none_match_exists(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

if-unmodified-since ¿¿¿¿¿¿¿¿¿¿¿ 412 ¿¿¿¿¿¿¿¿¿¿¿¿¿ if-none-match ¿¿¿¿¿¿¿¿¿¿¿¿

¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿

```
is_put_to_missing_resource(Req, State=#state{method= <<"PUT">>}) ->
	moved_permanently(Req, State, fun is_conflict/2);
is_put_to_missing_resource(Req, State) ->
	previously_existed(Req, State).
```

moved_permanently¿¿¿¿PUT¿¿¿¿¿¿¿location¿¿¿¿¿¿¿¿¿¿¿¿¿¿accept_resource¿¿¿¿¿

```
accept_resource(Req, State) ->
	case call(Req, State, content_types_accepted) of
		no_call ->
			respond(Req, State, 415);
		{stop, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{CTA, Req2, HandlerState} ->
			CTA2 = [normalize_content_types(P) || P <- CTA],
			State2 = State#state{handler_state=HandlerState},
			try cowboy_req:parse_header(<<"content-type">>, Req2) of
				ContentType ->
					choose_content_type(Req2, State2, ContentType, CTA2)
			catch _:_ ->
				respond(Req2, State2, 415)
			end
	end.
```

process_content_type¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿content_types_accepted¿¿¿MIME¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿method¿¿¿¿¿¿¿¿¿¿¿¿

```
process_content_type(Req, State=#state{method=Method, exists=Exists}, Fun) ->
	try case call(Req, State, Fun) of
		{stop, Req2, HandlerState2} ->
			terminate(Req2, State#state{handler_state=HandlerState2});
		{true, Req2, HandlerState2} when Exists ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun has_resp_body/2);
		{true, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun maybe_created/2);
		{false, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			respond(Req2, State2, 400);
		{{true, ResURL}, Req2, HandlerState2} when Method =:= <<"POST">> ->
			State2 = State#state{handler_state=HandlerState2},
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			if
				Exists -> respond(Req3, State2, 303);
				true -> respond(Req3, State2, 201)
			end
	end catch Class:Reason = {case_clause, no_call} ->
		error_terminate(Req, State, Class, Reason)
	end.
```

¿¿¿¿¿¿¿¿¿¿method¿¿¿¿¿¿¿¿¿

```
method(Req, State=#state{method= <<"DELETE">>}) ->
	delete_resource(Req, State);
method(Req, State=#state{method= <<"PUT">>}) ->
	is_conflict(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"POST">>; Method =:= <<"PATCH">> ->
	accept_resource(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"GET">>; Method =:= <<"HEAD">> ->
	set_resp_body_etag(Req, State);
method(Req, State) ->
	multiple_choices(Req, State).
```

¿¿¿¿¿¿set_resp_body_etag¿¿¿GET¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿set_resp_body¿¿¿content_types_provided¿¿¿¿¿¿

upgrade¿¿¿response¿¿¿¿

```
respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).

terminate(Req, #state{handler=Handler, handler_state=HandlerState}) ->
	Result = cowboy_handler:terminate(normal, Req, HandlerState, Handler),
	{ok, Req, Result}.
```

