% Copyright 2012 Tensor Wrench LLC.  All rights reserved.
% https://github.com/TensorWrench/lager_extras

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
%     * Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above
% copyright notice, this list of conditions and the following disclaimer
% in the documentation and/or other materials provided with the
% distribution.
%     * Neither the name of TensorWrench,LLC nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(lager_gelf_formatter).

%%
%% Include files
%%
-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%
%% Exported Functions
%%
-export([format/2]).


-define(DEFAULT_SHORT_MESSAGE_SIZE, 32*1024).

%% @doc
%% Outputs the message and all of it's metadata in GELF format.
%% Config is a prop list that contains:
%%  {host, Name}  % name of the host to report.  default: "erlang"
%%  {facility, Name} % facility name used to report. default: "erlang"
%%  {short_message_size, N::integer()} % first N bytes of the message will be the short message. default: 80
%%  {utf8,boolean()} % whether to use utf8 or not in the JSON.  default: true 
-spec format(lager_msg:lager_msg(),list()) -> iolist().
format(Msg,[]) ->
	format(Msg,[date, " ", time," [",severity,"] ",pid, " ", message, "\n"]);
format(Message,Config) ->
	Encoder=lager_extras_mochijson2:encoder([{utf8,proplists:get_value(utf8,Config,true)}]),
	Encoder(convert(Message,Config)).

convert(Msg, Config) ->
	ShortMessageSize=proplists:get_value(short_message_size,Config, ?DEFAULT_SHORT_MESSAGE_SIZE),
	LongMessage=iolist_to_binary(lager_msg:message(Msg)),
	%% {Mega,Sec,Micro} = os:timestamp(),
	%% Timestamp=Mega*1000000 + Sec*1000 + Micro/1000,
	Timestamp = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ),
	case size(LongMessage) =< ShortMessageSize of 
		true -> ShortMessage=LongMessage;
		_ -> <<ShortMessage:ShortMessageSize/binary,_/binary>> = LongMessage
	end,
	Metadata = lager_msg:metadata(Msg),
	{struct,[{version,<<"1.0">>},
			 {level, log_level(lager_msg:severity_as_int(Msg))},
			 {short_message,ShortMessage},
			 {long_message,LongMessage},
			 {timestamp,Timestamp},
			 {line, proplists:get_value(line, Metadata, -1)},
			 {file, proplists:get_value(module,Metadata,unknown)},
			 {host,proplists:get_value(host,Config,node())},
			 {facility,proplists:get_value(facility,Config,erlang)}
			| [ {make_key(K),make_printable(V)} ||  {K,V} <- Metadata]]}.

make_printable(A) when is_atom(A) orelse is_binary(A) orelse is_number(A) -> A;
make_printable(P) when is_pid(P) -> iolist_to_binary(pid_to_list(P));
make_printable(Other) -> iolist_to_binary(io_lib:format("~p",[Other])).

make_key(id) -> <<"_user_id">>;
make_key(K) when is_atom(K) -> list_to_binary(["_" | atom_to_list(K)]).

%% GELF/syslog Log levels
log_level(?EMERGENCY) -> 0;
log_level(?ALERT) -> 1;
log_level(?CRITICAL) -> 2;
log_level(?ERROR) -> 3;
log_level(?WARNING) -> 4;
log_level(?NOTICE) -> 5;
log_level(?INFO) -> 6;
log_level(?DEBUG) -> 7;
log_level(_Other) -> -1.

-ifdef(TEST).
to_property([K,V]) ->
	{binary_to_atom(K, utf8),V}.

basic_test_() ->
	[{"Just a plain message.",
	  	fun() -> 
	  		<<${,$",Bin/binary>>=iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
										  message="Message",
										  severity_as_int=lager_util:level_to_num(error),
										  metadata=[]},
									   [])),
 	        Properties=[ to_property(re:split(P,"\"?:\"?",[{return,binary}])) || P <- re:split(Bin, "\"?,\"?|\"}", [{return, list},trim])],
			?assertMatch(<<"1.0">>,proplists:get_value(version,Properties)),
			?assertMatch(<<"3">>,proplists:get_value(level,Properties)),
			?assertMatch(<<"Message">>,proplists:get_value(short_message,Properties)),
			?assertMatch(<<"Message">>,proplists:get_value(long_message,Properties))
		end
	  },
	 {"Just a plain with standard metadata.",
	  	fun() -> 
	  		<<${,$",Bin/binary>>= iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
															  message="Message",
															  severity_as_int=lager_util:level_to_num(error),
															  metadata=[{module,?MODULE},{function,my_function},{line,999},{pid,pid_to_list(self())}]},[])),
 	        Properties=[ to_property(re:split(P,"\"?:\"?",[{return,binary}])) || P <- re:split(Bin, "\"?,\"?|\"}", [{return, list},trim])],
			?assertMatch(<<"lager_gelf_formatter">>,proplists:get_value(file,Properties)),
			?assertMatch(<<"999">>,proplists:get_value(line,Properties)),
			?assertMatch(<<"my_function">>,proplists:get_value('_function',Properties)),
			?assertMatch(<<"\\\"<", _/binary>>,proplists:get_value('_pid',Properties))
		end
	  }	 
	 ].


-endif.

