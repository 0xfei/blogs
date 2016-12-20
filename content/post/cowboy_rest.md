+++
date = "2016-12-20T19:40:44+08:00"
title = "HTTP������cowboy_rest"
draft = true
description = "HTTP Request parser"
tags = ["Erlang/OTP", "code", "cowboy"]
topics = ["Erlang/OTP", "code", "cowboy"]
+++

cowboy_rest��REST��ʽ�������û�ģ�����HTTP����������nginxԴ���ͬѧ��֪����nginx�ص�ģ����Խ���http������������������л��ֵ�ʮ�����׶Σ�ʵ����Դ�ض���Ȩ�޿��Ƶȡ�cowboy���������в�ͬ��ǰһƪ�ᵽ���м���ĸ����middlewares����ʵ����execute�ص���ģ�飬���յ�����ͷ�����ݰ�����봦��ʵ��nginx���ƹ��ܣ��ٷ��и�markdown�����ӣ���ͷ������һ����˵�����ǹ�ע�����������û��ص�ģ����ʵ��cowboy_restģ��Ŀ�ѡ�ص����ɡ�

cowboy_handler��Ϊ����������һ��������·�ɹ���ص��û�ģ�飬�������{cowboy_rest, Req, State}���ͻ����cowboy_rest:upgrade/6��

```
execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts}) ->
	try Handler:init(Req, HandlerOpts) of
		...
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		...
```

<!--more-->

�����Mod��cowboy_rest��

cowboy_rest:upgrade����β�ݹ����ʽ�������ÿ�ѡ�Ļص�ģ�飬���δ���HTTP����������ȿ�һЩ����������expect�ص�ģ�鵼�������������ݷ���ֵ����OnTrue��OnFalse��

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

���������еĴ���������expect���ûص�������ʹ��Ĭ�ϴ���������service_avaliable���жϷ�����״̬���������Ƿ���503��

```
-callback service_available(Req, State) when Req::cowboy_req:req(), State::any() -> 
	{boolean(), Req, State} | {stop, Req, State}.
```

����������known_methods/2�����ؿ��õķ���ͷ��Ĭ��֧��<<"HEAD">> <<"GET">> <"POST">> <<"PUT">> <<"PATCH">> <<"DELETE">> <<"OPTIONS">>������501����ִ��uri_too_long:

```
uri_too_long(Req, State) ->
	expect(Req, State, uri_too_long, false, fun allowed_methods/2, 414).
```

�����������Щģ����������������Ҳû����ϸ˵�������֮�����ڻص������ܶ����飬����falseʱ��414��ֹHTTP������ִ��allowed_methods������ᴦ��OPTIONSѡ����ÿ��÷�����������405��ֹ���󡣽���������malformed_request��

```
malformed_request(Req, State) ->
	expect(Req, State, malformed_request, false, fun is_authorized/2, 400).
```

malformed_requestӦ���������������߰�������������Ȼ�����is_authorized��is_authorized��forbidden�ص�һ��ͬʱ�ṩ��Ĭ�����ִ����������̣�����options������

```
forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun valid_content_headers/2, 403).

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true, fun valid_entity_length/2, 501).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).
```

options���� <<"OPTIONS">> ѡ�����allowͷ����������200��������������content_types_provided/2����

content_types_provided�ص��û�ģ���ͬ���������ûص�����һ�� *MIME�����Լ�������* ��ɵ��б�����Ϊcowboy_rest�ڲ�״̬��content_types_p������Ȼ�����Acceptͷ����ͨ��prioritize_accept/1ѡ�������Ƚ��ܵ����͡�choose_media_type/3ȷ��ʵ��ѡ������ͣ�������content_type_a��media_type��Ȼ�����languages_provided/2��

languages_provided/2ͬ���ص�ͬ������������accept-languageͷ������languages_p��languages_a������content-language��Ӧͷ������charsets_provided/2������ǰ��Ĵ������̣�ֻ�����accept-charset����ͷ�����ŵ���set_content_type���ú���Ӧ����content-typeͷ��

������������ش���֮���ǻ�����ء�����variances��������Varyͷ�����ڻ�����ƣ���Ȼ�����resource_exists/2���жϻ����Ƿ���ڵȣ�

```
resource_exists(Req, State) ->
	expect(Req, State, resource_exists, true, fun if_match_exists/2, fun if_match_must_not_exist/2).
```

if_match_exists/2 �� if_match_must_not_exist/2 ��������ͷ if-match �Ƿ���ڣ�ѡ���෴�Ĵ����߼���if-match��etag��ϣ���һ�ֲ�ͬ���޸�ʱ��Ļ�����ԣ�����ʵʩ�ɷ����ȷ����if-unmodified-since��if-modified-since��if-match��if-none-match�ĸ�ͷ���ƻ��档������߼���΢��Щ���ң�������ʺ�һЩ���ȿ��������жϺʹ����߼���

if-matchͷ����ʱ��Ҫƥ��etag�жϻ��棺

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

precondition_failed ���� 412 Ԥ�������if_none_match_exists����if-none-match����ͷ��

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

precondition_is_head_get����etagƥ��ɹ��������if_modified_since_exists����if-modified-sinceͷ�����޸�ʱ��С��ָ����ֵʱ��ֱ�ӷ���not_modified����304�������ǰʱ�䳬����ֵ��ֱ�ӵ���method�������á�

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

not_modified����etag��expires��������304��Ӧ��

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

if_unmodified_since_exists����if-unmodified-since����ͷ��

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

if-unmodified-since ͷ������޸ģ�ֱ�ӷ��� 412 Ԥ����ʧ�ܣ�û���޸ľͼ�� if-none-match ͷ������������ܵĴ���

����������ֵ�ѭ����⣬��ΪҪ���ǵ����е�����ͷ��������β�ݹ飬���Կ������е��ҡ����⻹�и���©������Դ������ʱ�Ĵ���

```
is_put_to_missing_resource(Req, State=#state{method= <<"PUT">>}) ->
	moved_permanently(Req, State, fun is_conflict/2);
is_put_to_missing_resource(Req, State) ->
	previously_existed(Req, State).
```

moved_permanently���ڴ���PUT����ͨ������location��Ӧͷ��ʵ���ض��򡣻��ߵ���accept_resource������Դ��

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

process_content_type������Դ���ͣ�������Ӧ�ĺ���������������content_types_accepted���ص�MIME���ͺʹ������������õĵط�����������������method������Ҳ�Ǻ���Ҫ�ķ�֧��

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

����������󶼻��ߵ�method��������������

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

�򵥽���һ��set_resp_body_etag������GET����ʱ�����ᴦ���ض������Ͳ����ظ��ͻ�����set_resp_body��ص�content_types_provided�ķ������ݡ�

upgrade������response���ս᣺

```
respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).

terminate(Req, #state{handler=Handler, handler_state=HandlerState}) ->
	Result = cowboy_handler:terminate(normal, Req, HandlerState, Handler),
	{ok, Req, Result}.
```
