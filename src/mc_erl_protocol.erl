%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_protocol).

-export([decode_packet/1, encode_packet/1, decrypt/4, encrypt/4, print_hex/1]).

-include("records.hrl").

-define(timeout, 10000).

% Protocol weirdness handling
to_absint(Value) when is_float(Value) orelse is_integer(Value) ->
	trunc(Value*32).

from_absint(Value) when is_integer(Value) ->
	Value/32.

% ======================================================================
% cryptography
% ======================================================================

decrypt(Socket, Key, IVec, BytesCount) ->
	decrypt(Socket, Key, IVec, BytesCount, []).

decrypt(_Socket, _Key, IVec, 0, Return) ->
	{list_to_binary(lists:reverse(Return)), IVec};
decrypt(Socket, Key, IVec, BytesCount, Return) ->
	case gen_tcp:recv(Socket, 1, ?timeout) of
		{error, closed} ->
			throw(connection_closed);
		{ok, <<Bin>>=B} ->
			%io:format("received:~n"),
			%mc_erl_protocol:print_hex(Bin),
			Cipher = <<Bin, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
			Text = crypto:aes_cfb_128_decrypt(Key, IVec, Cipher),

			%io:format("decrypted:~n"),
			%mc_erl_protocol:print_hex(Text),

			<<Ret:1/binary, _/binary>> = Text,
			decrypt(Socket, Key, gen_ivec(IVec, B), BytesCount-1, [Ret|Return])
	end.

encrypt(Socket, Key, IVec, Text) when is_binary(Text) ->
	encrypt(Socket, Key, IVec, binary_to_list(Text), []).

encrypt(_, _, IVec, [], Return) ->
	{lists:reverse(Return), IVec};
encrypt(Socket, Key, IVec, [Text|Rest], Return) ->
	Cipher = crypto:aes_cfb_128_encrypt(Key, IVec, <<Text, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),
	<<Ret:1/binary, _/binary>> = Cipher,
	encrypt(Socket, Key, gen_ivec(IVec, Ret), Rest, [Ret|Return]).


gen_ivec(OldIVec, Data) when byte_size(Data) =:= 1 ->
	%L = size(Data),
	<<_:1/binary, IVPart/binary>> = OldIVec,
	%io:format("IVPart=~p~n", [IVPart]),
	list_to_binary([IVPart, Data]).


% ======================================================================
% decoding
% ======================================================================

decode_packet(Reader) ->
	Recv = get_bytes(Reader, 1),
	case Recv of
		{ok, <<Id>>} ->
			%io:format("[~p] received packet id: ~p~n", [?MODULE, Id]),
			{Id, Name, TypeParamList} = mc_erl_packets:get_by_id(Id),
			ParamList = decode_param_list(Reader, TypeParamList, []),
			{ok, {Name, ParamList}};
		{error, Reason} ->
			{error, Reason}
	end.

decode_param_list(_Reader, [], Output) ->
	lists:reverse(Output);

decode_param_list(Reader, [TypeParam|TypeParamList], Output) ->
	decode_param_list(Reader, TypeParamList,
		[read_value(Reader, TypeParam)|Output]).

read_value(Reader, Type) ->
	case Type of
			bool -> read_bool(Reader);
			byte -> read_byte(Reader);
			ubyte -> read_ubyte(Reader);
			abs_byte -> from_absint(read_byte(Reader));
			short -> read_short(Reader);
			ushort -> read_ushort(Reader);
			int -> read_int(Reader);
			abs_int -> from_absint(read_int(Reader));
			long -> read_long(Reader);
			float -> read_float(Reader);
			double -> read_double(Reader);
			string -> read_string(Reader);
			metadata -> read_metadata(Reader);
			slots -> read_slots(Reader);
			slot -> read_slot(Reader);
			chunk_data -> read_chunk_data(Reader);
			multi_block_change_data -> read_multi_block_change_data(Reader);
			coordinate_offsets -> read_coordinate_offsets(Reader);
			projectile_data -> read_projectile_data(Reader);
			{array, CountType, PayloadType} -> read_array(Reader, CountType, PayloadType);
			_ ->
				io:format("[~w] unknown datatype: ~p~n", [?MODULE, Type]),
				{error, unknown_datatype, Type}
	end.

read_bool(Reader) ->
	{ok, <<N:8>>} = get_bytes(Reader, 1),
	N =:= 1.

read_byte(Reader) ->
	{ok, <<N:8/signed>>} = get_bytes(Reader, 1),
	N.

read_ubyte(Reader) ->
	{ok, <<N:8/unsigned>>} = get_bytes(Reader, 1),
	N.

read_short(Reader) ->
	{ok, <<N:16/signed>>} = get_bytes(Reader, 2),
	N.

read_ushort(Reader) ->
	{ok, <<N:16/unsigned>>} = get_bytes(Reader, 2),
	N.

read_int(Reader) ->
	{ok, <<N:32/signed>>} = get_bytes(Reader, 4),
	N.

read_long(Reader) ->
	{ok, <<N:64/signed>>} = get_bytes(Reader, 8),
	N.

read_float(Reader) ->
	{ok, <<N:32/float>>} = get_bytes(Reader, 4),
	N.

read_double(Reader) ->
	{ok, <<N:64/float>>} = get_bytes(Reader, 8),
	N.

read_bit_set(Reader, Bytes) ->
	{ok, Bin} = get_bytes(Reader, Bytes),
	parse_bit_set(binary_to_list(Bin), []).

parse_bit_set([], Output) ->
	List = lists:flatten(Output),
	L2 = [round(math:pow(2,N)) || N <- List],
	lists:filter(fun(N) -> not (N =:= 0) end, L2);
parse_bit_set([B|Bytes], Output) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1>> = <<B>>,
	parse_bit_set(Bytes, [[B7, B6, B5, B4, B3, B2, B1, B0]|Output]).

read_string(Reader) ->
	case read_short(Reader) of
		0 -> [];
		Length ->
			BinLength = Length * 2,
			{ok, Bin} = get_bytes(Reader, BinLength),
			decode_ucs_2(Bin, [])
	end.

decode_ucs_2(<<>>, Output) ->
	lists:reverse(Output);
decode_ucs_2(Bin, Output) ->
	{No, Rest} = split_binary(Bin, 2),
	<<N:16>> = No,
	decode_ucs_2(Rest, [N|Output]).

read_metadata(Reader) ->
	read_metadata(Reader, []).

read_metadata(Reader, Output) ->
	X = read_ubyte(Reader),
	case X of
		127 -> lists:reverse(Output);
		X ->
			<<Type:3, Key:5>> = <<X>>,
			O = {Key, case Type of
				0 -> {byte, read_byte(Reader)};
				1 -> {short, read_short(Reader)};
				2 -> {int, read_int(Reader)};
				3 -> {float, read_float(Reader)};
				4 -> {string, read_string(Reader)};
				5 -> {short_byte_short, [read_short(Reader), read_byte(Reader),
				                         read_short(Reader)]};
				6 -> {int_int_int, [read_int(Reader), read_int(Reader),
				                    read_int(Reader)]};
				N -> {error, unknown_metadata_type, N}
			end},
			read_metadata(Reader, [O|Output])
	end.

read_slot(Reader) ->
	case read_short(Reader) of
		-1 -> empty;
		ItemId ->
			ItemCount = read_byte(Reader),
			Metadata = read_short(Reader),
			case read_short(Reader) of
				-1 ->
					#slot{id=ItemId, count=ItemCount, metadata=Metadata};
				BinLength ->
					{ok, BinEnchantments} = get_bytes(Reader, BinLength),
					#slot{id=ItemId, count=ItemCount, metadata=Metadata, enchantments=read_enchantments(BinEnchantments)}
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
	

read_slots(Reader) ->
	Count = read_short(Reader),
	read_slots(Reader, [], Count).

read_slots(_Reader, Output, 0) ->
	lists:reverse(Output);

read_slots(Reader, Output, RemainingSlots) ->
	read_slots(Reader, [read_slot(Reader)|Output], RemainingSlots-1).

read_chunk_data(Reader) ->
	FullColumn = read_bool(Reader),
	ContainedChunks = read_bit_set(Reader, 2),
	ChunksCount = lists:sum(ContainedChunks),
	_AddChunks = read_bit_set(Reader, 2),
	Length = read_int(Reader),
	{ok, Bin} = get_bytes(Reader, Length),
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


read_multi_block_change_data(Reader) ->
	RecordCount = read_short(Reader),
	_ = read_int(Reader),
	{multi_block_change_data, RecordCount, read_multi_block_change_datasets(Reader, RecordCount)}.

read_multi_block_change_datasets(Reader, RecordCount) -> read_multi_block_change_datasets(Reader, RecordCount, []).

read_multi_block_change_datasets(_Reader, 0, DeltaBlocks) -> lists:reverse(DeltaBlocks);
read_multi_block_change_datasets(Reader, RecordCount, DeltaBlocks) ->
	{ok, Raw} = get_bytes(Reader, 4),
	<<DX:4/unsigned, DZ:4/unsigned, DY:8/unsigned, BlockID:12/unsigned, Metadata:4/unsigned>> = Raw,
	read_multi_block_change_datasets(Reader, RecordCount - 1, [{DX, DZ, DY, BlockID, Metadata}|DeltaBlocks]).

read_projectile_data(Reader) ->
	case read_int(Reader) of
		0 -> {projectile, none};
		Owner ->
			SpeedX = read_short(Reader),
			SpeedY = read_short(Reader),
			SpeedZ = read_short(Reader),
			{projectile, {Owner, SpeedX, SpeedY, SpeedZ}}
	end.

%% coordinates are also unparsed
read_coordinate_offsets(Reader) ->
	OffsetsNum = read_int(Reader),
	{ok, Bin} = get_bytes(Reader, 3*OffsetsNum),
	{raw, OffsetsNum, Bin}.

read_array(Reader, CountType, binary) ->
	Num = read_value(Reader, CountType),
	{ok, Bytes} = get_bytes(Reader, Num),
	Bytes;
read_array(Reader, CountType, PayloadType) ->
    Num = read_value(Reader, CountType),
    [ read_value(Reader, PayloadType) || _ <- lists:seq(1, Num) ].

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
					%	disconnect ->
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

encode_byte(B) when byte_size(B) =:= 1 ->
	B;
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

encode_slot(empty) -> encode_short(-1);
encode_slot(#slot{}=S) ->
	case S of
		#slot{enchantments=[]} ->
			[encode_short(S#slot.id), encode_byte(S#slot.count),
			 encode_short(S#slot.metadata), encode_short(-1)];
		#slot{} ->
			NbtEncoded = encode_enchantments(S#slot.enchantments),
			[encode_short(S#slot.id), encode_byte(S#slot.count),
			 encode_short(S#slot.metadata), encode_short(byte_size(NbtEncoded)), NbtEncoded]
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
	[encode_bool(true), encode_short(0), encode_short(0), encode_int(0)];
			
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

encode_array(PayloadList, CountType, binary) when is_binary(PayloadList) ->
	list_to_binary([encode_value(CountType, byte_size(PayloadList)), PayloadList]);
encode_array(PayloadList, CountType, PayloadType) when is_list(PayloadList) ->
    list_to_binary([encode_value(CountType, length(PayloadList)), [ encode_value(PayloadType, Id) || Id <- PayloadList ]]).


% ======================================================================
% helper stuff
% ======================================================================

% read from decrypting process
get_bytes(Reader, N) when is_pid(Reader) ->
	Reader ! {get_bytes, N},
	receive
		{bytes, Bin} -> {ok, Bin}
	end;

% read directly from socket
get_bytes(Socket, N) ->
	gen_tcp:recv(Socket, N, ?timeout).

print_hex(<<>>) -> ok;
print_hex(Bin) when byte_size(Bin) < 16 ->
	Bytes = binary_to_list(Bin),
	[ io:format("~2.16b ", [Byte]) || Byte <- Bytes ],
	[ io:format("   ") || _ <- lists:seq(1, 16-length(Bytes)) ],
	[ if B >= 16#21, B =< 16#7e; B >= 16#41, B =< 16#7a -> io:format("~c", [B]); true -> io:format(".") end || B <- Bytes ],
	io:format("~n");
print_hex(Bin) ->
	<<Head:16/binary, Rest/binary>> = Bin,
	Bytes = binary_to_list(Head),
	[ io:format("~2.16b ", [Byte]) || Byte <- Bytes ],
	[ if B >= 16#21, B =< 16#7e; B >= 16#41, B =< 16#7a -> io:format("~c", [B]); true -> io:format(".") end || B <- Bytes ],
	io:format("~n"),
	print_hex(Rest).
