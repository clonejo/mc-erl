-module(mc_erl_protocol).

-export([decode_packet/1, encode_packet/1]).

-include("records.hrl").

-define(timeout, 10000).

% Protocol weirdness handling
to_absint(Value) when is_float(Value) orelse is_integer(Value) ->
	trunc(Value*32).

from_absint(Value) when is_integer(Value) ->
	Value/32.


% ======================================================================
% decoding
% ======================================================================

decode_packet(Socket) ->
	Recv = gen_tcp:recv(Socket, 1),
	case Recv of
		{ok, <<Id>>} ->
			%io:format("[~p] received packet id: ~p~n", [?MODULE, Id]),
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
		[read_value(Socket, TypeParam)|Output]).

read_value(Socket, Type) ->
	case Type of
			bool -> read_bool(Socket);
			byte -> read_byte(Socket);
			ubyte -> read_ubyte(Socket);
			abs_byte -> from_absint(read_byte(Socket));
			short -> read_short(Socket);
			ushort -> read_ushort(Socket);
			int -> read_int(Socket);
			abs_int -> from_absint(read_int(Socket));
			long -> read_long(Socket);
			float -> read_float(Socket);
			double -> read_double(Socket);
			string -> read_string(Socket);
			metadata -> read_metadata(Socket);
			slots -> read_slots(Socket);
			slot -> read_slot(Socket);
			chunk_data -> read_chunk_data(Socket);
			multi_block_change_data -> read_multi_block_change_data(Socket);
			coordinate_offsets -> read_coordinate_offsets(Socket);
			projectile_data -> read_projectile_data(Socket);
			{array, CountType, PayloadType} -> read_array(Socket, CountType, PayloadType);
			_ ->
				io:format("[~w] unknown datatype: ~p~n", [?MODULE, Type]),
				{error, unknown_datatype, Type}
	end.

read_bool(Socket) ->
	{ok, <<N:8>>} = gen_tcp:recv(Socket, 1, ?timeout),
	N =:= 1.

read_byte(Socket) ->
	{ok, <<N:8/signed>>} = gen_tcp:recv(Socket, 1, ?timeout),
	N.

read_ubyte(Socket) ->
	{ok, <<N:8/unsigned>>} = gen_tcp:recv(Socket, 1, ?timeout),
	N.

read_short(Socket) ->
	{ok, <<N:16/signed>>} = gen_tcp:recv(Socket, 2, ?timeout),
	N.

read_ushort(Socket) ->
	{ok, <<N:16/unsigned>>} = gen_tcp:recv(Socket, 2, ?timeout),
	N.

read_int(Socket) ->
	{ok, <<N:32/signed>>} = gen_tcp:recv(Socket, 4, ?timeout),
	N.

read_long(Socket) ->
	{ok, <<N:64/signed>>} = gen_tcp:recv(Socket, 8, ?timeout),
	N.

read_float(Socket) ->
	{ok, <<N:32/float>>} = gen_tcp:recv(Socket, 4, ?timeout),
	N.

read_double(Socket) ->
	{ok, <<N:64/float>>} = gen_tcp:recv(Socket, 8, ?timeout),
	N.

read_bit_set(Socket, Bytes) ->
	{ok, Bin} = gen_tcp:recv(Socket, Bytes, ?timeout),
	parse_bit_set(binary_to_list(Bin), []).

parse_bit_set([], Output) ->
	List = lists:flatten(Output),
	L2 = [round(math:pow(2,N)) || N <- List],
	lists:filter(fun(N) -> not (N =:= 0) end, L2);
parse_bit_set([B|Bytes], Output) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1>> = <<B>>,
	parse_bit_set(Bytes, [[B7, B6, B5, B4, B3, B2, B1, B0]|Output]).

read_string(Socket) ->
	case read_short(Socket) of
		0 -> [];
		Length ->
			BinLength = Length * 2,
			{ok, Bin} = gen_tcp:recv(Socket, BinLength, ?timeout),
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

read_slot(Socket) ->
	case read_short(Socket) of
		-1 -> empty;
		ItemId ->
			ItemCount = read_byte(Socket),
			Metadata = read_short(Socket),
			case read_short(Socket) of
				-1 ->
					{ItemId, ItemCount, Metadata, []};
				BinLength ->
					{ok, BinEnchantments} = gen_tcp:recv(Socket, BinLength, ?timeout),
					{ItemId, ItemCount, Metadata, read_enchantments(BinEnchantments)}
			end
	end.


read_enchantments(BinEnchantments) ->
	EnchNbtBin = zlib:gunzip(BinEnchantments),
	EnchData = nbt:decode(EnchNbtBin),
	
	% I am really sorry for the following, but that NBT library has this crazy format.
	% Note, however, that variable Enchantments becomes bound after this line, populated
	% by a list of [{<<"id">>, {short, Eid}}, {<<"lvl">>, {short, Lvl}}]. 
	{<<"tag">>,{compound,[{<<"ench">>,{compound, Enchantments}}]}} = EnchData,
	
	% This cryptic fun makes enchantments readable. get_enchantment_by_id return
	% a tuple, structured as {ench_name, [applicable_item_classes], max_lvl}.
	% Validation is performed in server implementation.
	lists:map(fun(X) ->
			[{<<"id">>, {short, Eid}}, {<<"lvl">>, {short, Lvl}}] = X,
			{_EnchId, EnchName, _AppliedTo, _MaxLvl} = mc_erl_packets:get_enchantment_by_id(Eid),
			{EnchName, Lvl} end, 
		Enchantments).
	

read_slots(Socket) ->
	Count = read_short(Socket),
	read_slots(Socket, [], Count).

read_slots(_Socket, Output, 0) ->
	lists:reverse(Output);

read_slots(Socket, Output, RemainingSlots) ->
	read_slots(Socket, [read_slot(Socket)|Output], RemainingSlots-1).

read_chunk_data(Socket) ->
	FullColumn = read_bool(Socket),
	ContainedChunks = read_bit_set(Socket, 2),
	ChunksCount = lists:sum(ContainedChunks),
	_AddChunks = read_bit_set(Socket, 2),
	Length = read_int(Socket),
	{ok, Bin} = gen_tcp:recv(Socket, Length, ?timeout),
	Uncompressed = zlib:uncompress(Bin),
	{TypeBin, Rest1} = split_binary(Uncompressed, 4096*ChunksCount),
	{MetadataBin, Rest2} = split_binary(Rest1, 2048*ChunksCount),
	{BlockLightBin, Rest3} = split_binary(Rest2, 2048*ChunksCount),
	{SkyLightBin, Rest4} = split_binary(Rest3, 2048*ChunksCount),
	
	BiomeBin = case FullColumn of
		true -> element(1, split_binary(Rest4, 256));
		false -> undefined
	end,
	
	Types = split_chunks(TypeBin, 4096),
	Metadata = split_chunks(MetadataBin, 2048),
	BlockLight = split_chunks(BlockLightBin, 2048),
	SkyLight = split_chunks(SkyLightBin, 2048),
	Chunks = [{N, #chunk_data{types=T, metadata=M, block_light=BL, sky_light=SL}} ||
	          N <- ContainedChunks, T <- Types, M <- Metadata, BL <- BlockLight,
	          SL <- SkyLight],
	{parsed, #chunk_column_data{full_column=FullColumn, chunks=Chunks,
	                            biome=BiomeBin}}.

split_chunks(Bin, ChunkSize) ->
	split_chunks(Bin, ChunkSize, []).

split_chunks(<<>>, _ChunkSize, Output) ->
	lists:reverse(Output);
split_chunks(Bin, ChunkSize, Output) ->
	{Chunk, Rest} = split_binary(Bin, ChunkSize),
	split_chunks(Rest, ChunkSize, [Chunk|Output]).


read_multi_block_change_data(Socket) ->
	RecordCount = read_short(Socket),
	_ = read_int(Socket),
	{multi_block_change_data, RecordCount, read_multi_block_change_datasets(Socket, RecordCount)}.

read_multi_block_change_datasets(Socket, RecordCount) -> read_multi_block_change_datasets(Socket, RecordCount, []).

read_multi_block_change_datasets(_Socket, 0, DeltaBlocks) -> lists:reverse(DeltaBlocks);
read_multi_block_change_datasets(Socket, RecordCount, DeltaBlocks) ->
	{ok, Raw} = gen_tcp:recv(Socket, 4, ?timeout),
	<<DX:4/unsigned, DZ:4/unsigned, DY:8/unsigned, BlockID:12/unsigned, Metadata:4/unsigned>> = Raw,
	read_multi_block_change_datasets(Socket, RecordCount - 1, [{DX, DZ, DY, BlockID, Metadata}|DeltaBlocks]).

read_projectile_data(Socket) ->
	case read_int(Socket) of
		0 -> {projectile, none};
		Owner ->
			SpeedX = read_short(Socket),
			SpeedY = read_short(Socket),
			SpeedZ = read_short(Socket),
			{projectile, {Owner, SpeedX, SpeedY, SpeedZ}}
	end.

%% coordinates are also unparsed
read_coordinate_offsets(Socket) ->
	OffsetsNum = read_int(Socket),
	{ok, Bin} = gen_tcp:recv(Socket, 3*OffsetsNum, ?timeout),
	{raw, OffsetsNum, Bin}.

read_array(Socket, CountType, PayloadType) ->
    Num = read_value(Socket, CountType),
    {entity_ids, [ read_value(Socket, PayloadType) || _ <- lists:seq(1, Num) ]}.

% ======================================================================
% encoding
% ======================================================================

encode_packet({Name, ParamList}) ->
	{Id, Name, TypeParamList} = mc_erl_packets:get_by_name(Name),
	%io:format("[~p] sending packet id: ~p~n", [?MODULE, Id]),
	case length(TypeParamList) of
		0 ->
			<<Id>>;
		_N ->
			case encode_param_list(ParamList, TypeParamList) of
				Bin when is_binary(Bin) ->
					R = list_to_binary([Id, Bin]),
					%case Name of
					%	named_entity_spawn ->
					%		io:format("[~s] packet ~p, params:~n~p, binary:~n", [?MODULE, Name, ParamList]),
					%		print_hex(R);
					%	_ -> ok
					%end,
					R;
				{error, not_enough_arguments, OutputSoFar} ->
					io:format("[~s] Error encoding ~p: not enough arguments!~nOutput so far: ~p~n", [?MODULE, Name, OutputSoFar]),
					{error, not_enough_arguments}
			end
	end.

encode_param_list(ParamList, TypeParamList) ->
	encode_param_list(ParamList, TypeParamList, []).

encode_param_list([], [], Output) ->
	list_to_binary(lists:reverse(Output));
encode_param_list([], _TypeList, Output) ->
	{error, not_enough_arguments, Output};
encode_param_list([P|ParamList], [T|TypeParamList], Output) ->
	O = encode_value(T, P),
	encode_param_list(ParamList, TypeParamList, [O|Output]).

encode_value(Type, P) ->
	case Type of
		bool -> encode_bool(P);
		byte -> encode_byte(P);
		ubyte -> encode_ubyte(P);
		abs_byte -> encode_byte(to_absint(P));
		short -> encode_short(P);
		ushort -> encode_ushort(P);
		int -> encode_int(P);
		abs_int -> encode_int(to_absint(P));
		long -> encode_long(P);
		float -> encode_float(P);
		double -> encode_double(P);
		string -> encode_string(P);
		metadata -> encode_metadata(P);
		slot -> encode_slot(P);
		slots -> encode_slots(P);
		chunk_data -> encode_chunk_data(P);
		multi_block_change_data -> encode_multi_block_change_data(P);
		coordinate_offsets -> encode_coordinate_offsets(P);
		projectile_data -> encode_projectile_data(P);
		{array, CountType, PayloadType} -> encode_array(P, CountType, PayloadType);
		X -> {error, unknown_datatype, X}
	end.

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

encode_ushort(N) ->
	<<N:16/unsigned>>.

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

encode_bit_set(BitsSet, Length) ->
	N = set_bits(BitsSet),
	BitLength = Length*8,
	<<N:BitLength/unsigned>>.

set_bits(Bits) -> set_bits(Bits, 0).
set_bits([], Result) -> Result;
set_bits([Bit|Rest], Result) -> set_bits(Rest, Result bor (1 bsl Bit)).

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
		{ItemId, Count, Metadata, EnchList} ->
			NbtEncoded = encode_enchantments(EnchList),
			[encode_short(ItemId), encode_byte(Count),
			 encode_short(Metadata), encode_short(byte_size(NbtEncoded)), NbtEncoded]
	end.


encode_enchantments(EnchList) ->
	EnchNbtList = lists:map(fun(X) ->
			{EnchName, Lvl} = X,
			{Eid, _EName, _AppliedTo, _MaxLvl} = mc_erl_packets:get_enchantment_by_name(EnchName),
			[{<<"id">>, {short, Eid}}, {<<"lvl">>, {short, Lvl}}] end,
		EnchList),
	
	EnchNbt = {<<"tag">>,{compound,[{<<"ench">>,{compound, EnchNbtList}}]}},
	zlib:gzip(nbt:encode(EnchNbt)).
	
		
	
encode_slots(P) ->
	Length = length(P),
	encode_slots(P, [encode_short(Length)]).

encode_slots([], Output) ->
	lists:reverse(Output);

encode_slots([Slot|Rest], Output) ->
	encode_slots(Rest, [encode_slot(Slot)|Output]).

encode_chunk_data(unload) ->
	[encode_bool(false), encode_short(0), encode_short(0), encode_int(0)];
			
encode_chunk_data({raw, Bin}) ->
	[encode_int(byte_size(Bin)), encode_int(0), Bin];
	
encode_chunk_data({uncompressed, Uncompressed}) ->
	Bin = zlib:compress(Uncompressed),
	[encode_int(byte_size(Bin)), encode_int(0), Bin];
	
encode_chunk_data({parsed, Column=#chunk_column_data{}}) ->
	FullColumn = Column#chunk_column_data.full_column,
	ContainedChunks = lists:map(fun(X) -> element(1, X) end, Column#chunk_column_data.chunks),
	AddChunks = lists:map(fun(X) -> element(1, X) end, Column#chunk_column_data.add_data),
	Chunks = Column#chunk_column_data.chunks,
	Types = lists:map(fun(X) -> element(2, element(2, X)) end, Chunks),
	Metadata = lists:map(fun(X) -> element(3, element(2, X)) end, Chunks),
	BlockLight = lists:map(fun(X) -> element(4, element(2, X)) end, Chunks),
	SkyLight = lists:map(fun(X) -> element(5, element(2, X)) end, Chunks),
	BiomeData = Column#chunk_column_data.biome,
	BinData = list_to_binary([Types, Metadata, BlockLight, SkyLight, BiomeData]),
	CompressedData = zlib:compress(BinData),
	
	[encode_bool(FullColumn), encode_bit_set(ContainedChunks, 2), encode_bit_set(AddChunks, 2),
		encode_int(byte_size(CompressedData)), CompressedData].

encode_multi_block_change_datasets(BlockDelta) -> encode_multi_block_change_datasets(BlockDelta, []).

encode_multi_block_change_datasets([], Delta) -> lists:reverse(Delta);
encode_multi_block_change_datasets([{DX, DZ, DY, BlockID, Metadata}|Rest], Delta) -> 
	encode_multi_block_change_datasets(Rest, [<<DX:4, DZ:4, DY:8, BlockID:12, Metadata:4>>|Delta]).

encode_multi_block_change_data({multi_block_change_data, RecordsNum, BlockDelta}) ->
	Delta = list_to_binary(encode_multi_block_change_datasets(BlockDelta)),
	[encode_short(RecordsNum), encode_int(byte_size(Delta)), Delta].

encode_coordinate_offsets({raw, OffsetsNum, Bin}) ->
	[encode_int(OffsetsNum), Bin].

encode_projectile_data({projectile, none}) ->
	encode_int(0);
encode_projectile_data({projectile, {Owner, SpeedX, SpeedY, SpeedZ}}) ->
	[encode_int(Owner), encode_short(SpeedX), encode_short(SpeedY), encode_short(SpeedZ)].

encode_array(PayloadList, CountType, PayloadType) when is_list(PayloadList) ->
    list_to_binary([encode_value(CountType, length(PayloadList)), [ encode_value(PayloadType, Id) || Id <- PayloadList ]]).


% ======================================================================
% helper stuff
% ======================================================================

print_hex(<<>>) -> ok;
print_hex(Bin) ->
	{Head, Rest} = binsplit(Bin, 16),
	Bytes = binary_to_list(Head),
	[ io:format("~2.16b ", [Byte]) || Byte <- Bytes ],
	[ io:format("   ") || _ <- lists:seq(1, 16-length(Bytes)) ],
	[ if B >= 16#21, B =< 16#7e; B >= 16#41, B =< 16#7a -> io:format("~c", [B]); true -> io:format(".") end || B <- Bytes ],
	io:format("~n"),
	print_hex(Rest).

binsplit(Bin, Bytes) ->
	if
		size(Bin) =< Bytes -> {Bin, <<>>};
		true -> split_binary(Bin, Bytes)
	end.



