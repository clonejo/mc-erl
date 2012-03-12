-module(mc_erl_protocol).

-export([decode_packet/1, encode_packet/1]).

-include("records.hrl").

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

% ======================================================================
% decoding
% ======================================================================

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
			ushort -> read_ushort(Socket);
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
			coordinate_offsets -> read_coordinate_offsets(Socket);
			projectile_data -> read_projectile_data(Socket);
			_ ->
				io:format("[~w] unknown datatype: ~p~n", [?MODULE, TypeParam]),
				{error, unknown_datatype, TypeParam}
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

read_ushort(Socket) ->
	{ok, <<N:16/unsigned>>} = gen_tcp:recv(Socket, 2),
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

read_bit_set(Socket, Bytes) ->
	{ok, Bin} = gen_tcp:recv(Socket, Bytes),
	parse_bit_set(binary_to_list(Bin), []).

parse_bit_set([], Output) -> lists:flatten(Output);
parse_bit_set([B|Bytes], Output) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1>> = <<B>>,
	parse_bit_set(Bytes, [[B7, B6, B5, B4, B3, B2, B1, B0]|Output]).

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
							{ok, BinEnchantments} = gen_tcp:recv(Socket, BinLength),
							{ItemId, ItemCount, Metadata, read_enchantments(BinEnchantments)}
					end;
				false -> 
					{ItemId, ItemCount, Metadata}
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

%% enchantment information is not parsed
read_slots(Socket, Output, RemainingSlots) ->
	read_slots(Socket, [read_slot(Socket)|Output], RemainingSlots-1).

%% chunks are unparsed
read_chunk_data(Socket) ->
	FullColumn = read_bool(Socket),
	ContainedChunks = read_bit_set(Socket, 2),
	ChunksCount = lists:sum(ContainedChunks),
	_ = read_bit_set(Socket, 2),
	Length = read_int(Socket),
	_ = read_int(Socket),
	{ok, Bin} = gen_tcp:recv(Socket, Length),
	Uncompressed = zlib:uncompress(Bin),
	{TypeBin, Rest1} = split_binary(Uncompressed, 4096*ChunksCount),
	{MetadataBin, Rest2} = split_binary(Rest1, 2048*ChunksCount),
	{BlockLightBin, Rest3} = split_binary(Rest2, 2048*ChunksCount),
	{SkyLightBin, Rest4} = split_binary(Rest3, 2048*ChunksCount),
	{BiomeBin, <<>>} = split_binary(Rest4, 256),
	Types = refer_chunks(ContainedChunks, TypeBin, 4096),
	Metadata = refer_chunks(ContainedChunks, MetadataBin, 2048),
	BlockLight = refer_chunks(ContainedChunks, BlockLightBin, 2048),
	SkyLight = refer_chunks(ContainedChunks, SkyLightBin, 2048),
	{parsed, #chunk_column_data{full_column=FullColumn, types=Types,
	                            metadata=Metadata, block_light=BlockLight,
	                            sky_light=SkyLight, biome=BiomeBin}}.

refer_chunks(ContainedChunks, Bin, ChunkSize) ->
	refer_chunks(ContainedChunks, Bin, ChunkSize, 0, []).

refer_chunks([], <<>>, _ChunkSize, 16, Output) -> lists:reverse(Output);
refer_chunks([0|ContainedChunks], Bin, ChunkSize, ZChunk, Output) ->
	refer_chunks(ContainedChunks, Bin, ChunkSize, ZChunk+1, Output);
refer_chunks([1|ContainedChunks], Bin, ChunkSize, ZChunk, Output) ->
	{Chunk, Rest} = split_binary(Bin, ChunkSize),
	refer_chunks(ContainedChunks, Rest, ChunkSize, ZChunk+1,
	             [{ZChunk, Chunk}|Output]).


read_multi_block_change_data(Socket) ->
	RecordCount = read_short(Socket),
	_ = read_int(Socket),
	{multi_block_change_data, RecordCount, read_multi_block_change_datasets(Socket, RecordCount)}.

read_multi_block_change_datasets(Socket, RecordCount) -> read_multi_block_change_datasets(Socket, RecordCount, []).

read_multi_block_change_datasets(_Socket, 0, DeltaBlocks) -> lists:reverse(DeltaBlocks);
read_multi_block_change_datasets(Socket, RecordCount, DeltaBlocks) ->
	{ok, Raw} = gen_tcp:recv(Socket, 4),
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

%% coordinates are unparsed also
read_coordinate_offsets(Socket) ->
	OffsetsNum = read_int(Socket),
	{ok, Bin} = gen_tcp:recv(Socket, 3*OffsetsNum),
	{raw, OffsetsNum, Bin}.

% ======================================================================
% encoding
% ======================================================================

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
		ushort -> encode_ushort(P);
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
		coordinate_offsets -> encode_coordinate_offsets(P);
		projectile_data -> encode_projectile_data(P);
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
			
encode_chunk_data({raw, Bin}) ->
	[encode_int(byte_size(Bin)), encode_int(0), Bin];
encode_chunk_data({uncompressed, Uncompressed}) ->
	Bin = zlib:compress(Uncompressed),
	[encode_int(byte_size(Bin)), encode_int(0), Bin].
	
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



