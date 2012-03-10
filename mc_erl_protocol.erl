-module(mc_erl_protocol).

-export([decode_packet/1, encode_packet/1]).

-define(enchantable, [
	16#103, %Flint and steel
	16#105, %Bow
	16#15A, %Fishing rod
	16#167, %Shears
	%TOOLS
	%sword, shovel, pickaxe, axe, hoe
	16#10C, 16#10D, 16#10E, 16#10F, 16#122, %WOOD
	16#110, 16#111, 16#112, 16#113, 16#123, %STONE
	16#10B, 16#100, 16#101, 16#102, 16#124, %IRON
	16#114, 16#115, 16#116, 16#117, 16#125, %DIAMOND
	16#11B, 16#11C, 16#11D, 16#11E, 16#126, %GOLD
	%ARMOUR
	%helmet, chestplate, leggings, boots
	16#12A, 16#12B, 16#12C, 16#12D, %LEATHER
	16#12E, 16#12F, 16#130, 16#131, %CHAIN
	16#132, 16#133, 16#134, 16#135, %IRON
	16#136, 16#137, 16#138, 16#139, %DIAMOND
	16#13A, 16#13B, 16#13C, 16#13D]). %GOLD

% decoding
decode_packet(Socket) ->
	Recv = gen_tcp:recv(Socket, 1, 10000),
	case Recv of
		{ok, <<Id>>} ->
			{Id, Name, TypeParamList} = mc_erl_packets:get_by_id(Id),
			ParamList = decode_param_list(Socket, TypeParamList, []),
			{ok, {Name, ParamList}};
		{error, Reason} ->
			{error, Reason}
	end.

decode_param_list(_Socket, [], Output) ->
	lists:reverse(Output);

decode_param_list(Socket, [TypeParam|TypeParamList], Output) ->
	decode_param_list(Socket, TypeParamList,
		[case TypeParam of
			bool -> read_bool(Socket);
			byte -> read_byte(Socket);
			ubyte -> read_ubyte(Socket);
			short -> read_short(Socket);
			int -> read_int(Socket);
			long -> read_long(Socket);
			float -> read_float(Socket);
			double -> read_double(Socket);
			string -> read_string(Socket);
			metadata -> read_metadata(Socket);
			slots -> read_slots(Socket);
			slot -> read_slot(Socket);
			chunk_data -> read_chunk_data(Socket);
			multi_block_change_data -> read_multi_block_change_data(Socket);
			X -> {error, unknown_datatype, X}
		end|Output]).

read_bool(Socket) ->
	{ok, <<N:8>>} = gen_tcp:recv(Socket, 1),
	N =:= 1.

read_byte(Socket) ->
	{ok, <<N:8/signed>>} = gen_tcp:recv(Socket, 1),
	N.

read_ubyte(Socket) ->
	{ok, <<N:8/unsigned>>} = gen_tcp:recv(Socket, 1),
	N.

read_short(Socket) ->
	{ok, <<N:16/signed>>} = gen_tcp:recv(Socket, 2),
	N.

read_int(Socket) ->
	{ok, <<N:32/signed>>} = gen_tcp:recv(Socket, 4),
	N.

read_long(Socket) ->
	{ok, <<N:64/signed>>} = gen_tcp:recv(Socket, 8),
	N.

read_float(Socket) ->
	{ok, <<N:32/float>>} = gen_tcp:recv(Socket, 4),
	N.

read_double(Socket) ->
	{ok, <<N:64/float>>} = gen_tcp:recv(Socket, 8),
	N.

read_string(Socket) ->
	case read_short(Socket) of
		0 -> [];
		Length ->
			BinLength = Length * 2,
			{ok, Bin} = gen_tcp:recv(Socket, BinLength),
			decode_ucs_2(Bin, [])
	end.

decode_ucs_2(<<>>, Output) ->
	lists:reverse(Output);
decode_ucs_2(Bin, Output) ->
	{No, Rest} = split_binary(Bin, 2),
	<<N:16>> = No,
	decode_ucs_2(Rest, [N|Output]).

read_metadata(Socket) ->
	read_metadata(Socket, []).

read_metadata(Socket, Output) ->
	X = read_ubyte(Socket),
	case X of
		127 -> lists:reverse(Output);
		X ->
			<<Type:3, Key:5>> = <<X>>,
			O = {Key, case Type of
				0 -> {byte, read_byte(Socket)};
				1 -> {short, read_short(Socket)};
				2 -> {int, read_int(Socket)};
				3 -> {float, read_float(Socket)};
				4 -> {string, read_string(Socket)};
				5 -> {short_byte_short, [read_short(Socket), read_byte(Socket),
				                         read_short(Socket)]};
				6 -> {int_int_int, [read_int(Socket), read_int(Socket),
				                    read_int(Socket)]};
				N -> {error, unknown_metadata_type, N}
			end},
			read_metadata(Socket, [O|Output])
	end.

%% enchantment information is not parsed
read_slot(Socket) ->
	case read_short(Socket) of
		-1 -> empty;
		ItemId ->
			ItemCount = read_byte(Socket),
			Metadata = read_short(Socket),
			case lists:member(ItemId, ?enchantable) of
				true ->
					case read_short(Socket) of
						-1 ->
							{ItemId, ItemCount, Metadata, []};
						BinLength ->
							io:format("length: ~p~n", [BinLength]),
							{ok, Bin} = gen_tcp:recv(Socket, BinLength),
							{ItemId, ItemCount, Metadata, {raw, Bin}}
					end;
				false -> 
					{ItemId, ItemCount, Metadata}
			end
	end.

read_slots(Socket) ->
	Count = read_short(Socket),
	read_slots(Socket, [], Count).

read_slots(_Socket, Output, 0) ->
	lists:reverse(Output);

%% enchantment information is not parsed
read_slots(Socket, Output, RemainingSlots) ->
	read_slots(Socket, [read_slot(Socket)|Output], RemainingSlots-1).

%% chunks are unparsed
read_chunk_data(Socket) ->
	Length = read_int(Socket),
	_ = read_int(Socket),
	io:format("[~w] got chunk with compressed data length=~p~n", [?MODULE, Length]),
	{ok, Bin} = gen_tcp:recv(Socket, Length),
	{raw, Bin}.

read_multi_block_change_data(Socket) ->
	ArraySize = read_short(Socket),
	{ok, Bin} = gen_tcp:recv(Socket, 4*ArraySize),
	{raw, ArraySize, Bin}.

% encoding
encode_packet({Name, ParamList}) ->
	{Id, Name, TypeParamList} = mc_erl_packets:get_by_name(Name),
	case length(TypeParamList) of
		0 ->
			<<Id>>;
		_N ->
			Bin = encode_param_list(ParamList, TypeParamList),
			list_to_binary([Id, Bin])
	end.

encode_param_list(ParamList, TypeParamList) ->
	encode_param_list(ParamList, TypeParamList, []).

encode_param_list([], [], Output) ->
	list_to_binary(lists:reverse(Output));
encode_param_list([P|ParamList], [T|TypeParamList], Output) ->
	O = case T of
		bool -> encode_bool(P);
		byte -> encode_byte(P);
		ubyte -> encode_ubyte(P);
		short -> encode_short(P);
		int -> encode_int(P);
		long -> encode_long(P);
		float -> encode_float(P);
		double -> encode_double(P);
		string -> encode_string(P);
		metadata -> encode_metadata(P);
		slot -> encode_slot(P);
		slots -> encode_slots(P);
		chunk_data -> encode_chunk_data(P);
		multi_block_change_data -> encode_multi_block_change_data(P);
		X -> {error, unknown_datatype, X}
	end,
	encode_param_list(ParamList, TypeParamList, [O|Output]).

encode_bool(N) ->
	 if
	 	N =:= true -> <<1>>;
	 	true -> <<0>>
	 end.

encode_byte(N) ->
	<<N:8/signed>>.

encode_ubyte(N) ->
	<<N:8/unsigned>>.

encode_short(N) ->
	<<N:16/signed>>.

encode_int(N) ->
	<<N:32/signed>>.

encode_long(N) ->
	<<N:64/signed>>.

encode_float(N) ->
	<<N:32/float>>.

encode_double(N) ->
	<<N:64/float>>.

encode_string(String) ->
	encode_string(String, [], length(String)).

encode_string([], Output, Length) ->
	list_to_binary([encode_short(Length), lists:reverse(Output)]);
encode_string([C|Rest], Output, Length) ->
	O = <<C:16>>,
	encode_string(Rest, [O|Output], Length).

encode_metadata(P) ->
	encode_metadata(P, []).

encode_metadata([], Output) ->
	list_to_binary(lists:reverse([127|Output]));

encode_metadata([P|Rest], Output) ->
	{Key, {Type, Data}} = P,
	TypeBin = case Type of
		byte -> 0;
		short -> 1;
		int -> 2;
		float -> 3;
		string -> 4;
		short_byte_short -> 5;
		int_int_int -> 6
	end,
	Comp = <<TypeBin:3, Key:5>>,
	DataBin = case Type of
		byte -> encode_byte(Data);
		short -> encode_short(Data);
		int -> encode_int(Data);
		'float' -> encode_float(Data);
		string -> encode_string(Data);
		short_byte_short ->
			[D1, D2, D3] = Data,
			[encode_short(D1), encode_byte(D2), encode_short(D3)];
		int_int_int	->
			[encode_int(D) || D <- Data]
	end,
	encode_metadata(Rest, [ [Comp, DataBin] | Output ]).

encode_slot(P) ->
	case P of
		empty -> encode_short(-1);
		{ItemId, Count, Metadata} ->
			[encode_short(ItemId), encode_byte(Count),
			 encode_short(Metadata)];
		{ItemId, Count, Metadata, []} ->
			[encode_short(ItemId), encode_byte(Count),
			 encode_short(Metadata), encode_short(-1)];
		{ItemId, Count, Metadata, {raw, Bin}} ->
			[encode_short(ItemId), encode_byte(Count),
			 encode_short(Metadata), encode_short(byte_size(Bin)), Bin]
	end.

encode_slots(P) ->
	Length = length(P),
	encode_slots(P, [encode_short(Length)]).

encode_slots([], Output) ->
	lists:reverse(Output);

encode_slots([Slot|Rest], Output) ->
	encode_slots(Rest, [encode_slot(Slot)|Output]).
			
encode_chunk_data({raw, Bin}) ->
	[encode_int(byte_size(Bin)), encode_int(0), Bin].

encode_multi_block_change_data({raw, ArraySize, Bin}) ->
	[encode_short(ArraySize), Bin].
