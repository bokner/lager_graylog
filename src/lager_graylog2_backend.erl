%% @author bokner
%% @doc @todo Add description to lager_graylog2_rest.

-module(lager_graylog2_backend).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_node_inputs/3, get_node_inputs/4]).

-define(UDP_GELF_INPUT, <<"org.graylog2.inputs.gelf.udp.GELFUDPInput">>).
-define(UDP_GELF_TYPE, udp_gelf).
-define(DEFAULT_GRAYLOG2_PING_INTERVAL, 5000).
-define(DEFAULT_GRAYLOG2_TIMEOUT, 5000).

-record(state, {current_node :: list(), input_finder :: fun(), backend :: atom(), backend_state :: any()}).

-type state() :: #state{}.

%% @private
-spec init(term()) -> {ok, state()} | {error, term()}.
init(Args) ->
	InputType = proplists:get_value(graylog2_input_type, Args, ?UDP_GELF_TYPE),
	User = proplists:get_value(graylog2_user, Args),
	Password = proplists:get_value(graylog2_password, Args),
	NodeUrls = proplists:get_value(graylog2_nodes, Args, []),
	PingInterval = proplists:get_value(graylog2_ping_interval, Args, ?DEFAULT_GRAYLOG2_PING_INTERVAL),
	InputFinder = fun(CurrentNodeUrl) ->
						erlang:send_after(PingInterval, self(), ping_graylog2), %% Schedule next input health check
						case find_input(NodeUrls, User, Password, InputType) of
						{Url, _InputParams} when Url == CurrentNodeUrl ->
								void; %% The working input is found on the same node as previously, leave as is
						{Url, InputParams} ->
							{Url, InputParams ++ Args};
						_ ->
							void			
						end
					end,

	set_backend(#state{backend = backend(InputType), input_finder = InputFinder}).
						
set_backend(#state{backend = Backend, current_node = CurrentNodeUrl, input_finder = InputFinder} = State) ->
	case InputFinder(CurrentNodeUrl) of
		void ->
			{ok, State};
		{Url, BackendParams} ->
    case Backend:init(BackendParams) of
        ignore ->
            ignore;
        {stop, Reason} ->
            {stop, Reason};
        {ok, BackendState} ->
            {ok, State#state{current_node = Url, backend_state = BackendState}};
        {ok, BackendState, Timeout} ->
            {ok, State#state{current_node = Url, backend_state = BackendState}, Timeout}
	end
    end.    
  
%% @private
-spec handle_call(term(), state()) -> {ok, term(), state()}.
handle_call(Request, #state{backend = Backend, backend_state = BackendState} = State) ->
    {ok, Reply, NewBackendState} = Backend:handle_call(Request, BackendState),
    {ok, Reply, State#state{backend_state = NewBackendState}}.

%% @private
-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Request, #state{backend = Backend, backend_state = BackendState} = State) ->
	{ok, NewBackendState} = Backend:handle_event(Request, BackendState),
    {ok, State#state{backend_state = NewBackendState}}.

%% @private
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(ping_graylog2, State) ->
	set_backend(State);

handle_info(Request, #state{backend = Backend, backend_state = BackendState} = State) ->
    {ok, NewBackendState} = Backend:handle_info(Request, BackendState),
    {ok, State#state{backend_state = NewBackendState}}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(Reason, #state{backend = Backend, backend_state = BackendState}) ->
	ok = Backend:terminate(Reason, BackendState).

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
get_node_inputs(Url, User, Password) ->
  case httpc:request(get, 
					 {Url ++ "/system/inputs", 
					  [{"Authorization", 
						"Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)}]}, 
						[{timeout, ?DEFAULT_GRAYLOG2_TIMEOUT}], [])
	of {ok, {{_Version, 200, _Phrase} = _StatusLine, _Headers, Body}} ->
		 {ok, proplists:get_value(<<"inputs">>,
								  element(1, jiffy:decode(Body)))};
	   {ok, StatusLine} ->
		 {error, StatusLine};
	   {error, Error} ->
		 {error, Error}
  end.

get_node_inputs(Url, User, Password, Type) ->
  case get_node_inputs(Url, User, Password) of
	{ok, Inputs} ->
  InputType = input_type(Type),
  {ok, lists:foldl(fun(I, Acc) -> 
				J2 = proplists:get_value(<<"message_input">>, element(1, I)), 
				case proplists:get_value(<<"type">>, element(1, J2)) of T when T == InputType -> 
					[element(1, proplists:get_value(<<"attributes">>, element(1, J2))) | Acc]; 
					_ -> Acc 
				end 
			end, [], Inputs)};
	{error, Error} ->
		{error, Error}
	end.



find_input([], _User, _Password, _InputType) ->
	none;

find_input([Url | Rest], User, Password, InputType) ->
	case get_node_inputs(Url, User, Password, InputType) of
		{error, _Error} ->
			find_input(Rest, User, Password, InputType);
		{ok, []} ->
			find_input(Rest, User, Password, InputType);
		{ok, [InputArgs | _]} ->
			{Url, make_input_args(Url, InputType, InputArgs)}
	end.

input_type(?UDP_GELF_TYPE) ->
	?UDP_GELF_INPUT;

input_type(_Other) ->
	undefined.

backend(?UDP_GELF_TYPE) ->
	lager_udp_backend;

backend(_Other) ->
	undefined_lager_backend.

make_input_args(Url, ?UDP_GELF_TYPE, InputArgs) ->
  {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(Url),
	[{host, Host}, {port, proplists:get_value(<<"port">>, InputArgs)}];

make_input_args(_Url, _Type, _InputArgs) ->
	[].		 

		 

