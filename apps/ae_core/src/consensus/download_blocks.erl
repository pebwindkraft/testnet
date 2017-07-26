-module(download_blocks).
-export([sync_all/2, sync/2, do_send_blocks/2]).

%% TODO: introduce ranking mechanism. It may be based on initial top request.
%%       The quickest and the longest chains are promoted
%%       There is no point of quering thousands of peers and updating our chain at once from all of them

sync_all([], _) -> ok;
sync_all([Peer|T], Height) ->
    spawn(fun() ->
                  sync(Peer, Height)
          end),
    sync_all(T, Height).

sync(Peer, MyHeight) ->
    %lower their ranking
    %peers:update_score(Peer, peers:initial_score()),
    io:fwrite("top of sync\n"),
    %S = erlang:timestamp(),
    TopResp = talker:talk({top}, Peer),
    case TopResp of
        {error, failed_connect} -> 
            io:fwrite("failed connect"),
            ok;
        {ok, TopBlock, Height}  ->
            %io:fwrite("got topblock\n"),
            {ok, DBB} = application:get_env(ae_core, download_blocks_batch),
	    HH = MyHeight + DBB,
	    if
                HH < Height ->
		    %io:fwrite("HH < Height\n"),
                    {ok, Sizecap} = application:get_env(ae_core, download_blocks_sizecap),
                    {ok, Blocks} = talker:talk({block_sizecap, HH, Sizecap}, Peer),
                    trade_blocks(Peer, Blocks);
                true ->
                    trade_blocks(Peer, [TopBlock]),
                    get_txs(Peer)
            end,
	    trade_peers(Peer);
            X -> io:fwrite(X)
    end.

trade_blocks(Peer, [PrevBlock|PBT] = Blocks) ->
    %io:fwrite("trade blocks"),
    %"nextBlock" is from earlier in the chain than prevblock. we are walking backwards
    case block:height(PrevBlock) of 
        0 ->  %if PrevBlock is block 0 (genesis) we stop
            send_blocks(Peer, block:hash(block:top()), block:hash(block:read_int(0)), [], 0),
            block_absorber:enqueue(PBT);
        _ ->
            %PrevHash = block:hash(PrevBlock), we can't get that becouse to hash we need headers, and to have header we have to have the header of previous block
            %{ok, PowBlock} = talker:talk({block, Height}, Peer),
            NextHash = block:prev_hash(PrevBlock),
            M = block:read(NextHash),%check if it is in our memory already.
            case M of
	        empty -> 
                    {ok, Sizecap} = application:get_env(ae_core, download_blocks_sizecap),
                    {ok, NextBatch} = talker:talk({block_sizecap, NextHash, Sizecap}, Peer),
	    	    %Heighest first, lowest last. We need to reverse
		    trade_blocks(Peer, lists:append(lists:reverse(NextBatch), Blocks));
	        _ ->
                    LastCommonHash = last_known_block_hash(NextHash, Blocks),
                    %We send blocks before absorbing to make sure we don't send any downloaded blocks
                    send_blocks(Peer, block:hash(block:top()), LastCommonHash, [], block:height(M)),
                    NewBlocks = remove_known_blocks(Blocks),
	            block_absorber:enqueue(NewBlocks)
            end
    end.

remove_known_blocks([]) ->
    [];
remove_known_blocks([PrevBlock | PBT]) ->
    PrevHash = block:hash(PrevBlock),
    M = block:read(PrevHash),
    case M of 
        empty ->
            [PrevBlock | PBT];
        _ ->
            remove_known_blocks(PBT)
    end.

last_known_block_hash(Hash,[]) ->
    Hash;
last_known_block_hash(Hash,[First | Other]) ->
    HashFirst = block:hash(First),
    M = block:read(HashFirst),
    case M of
        empty ->
            Hash;
        _ ->
            last_known_block_hash(HashFirst,Other)
    end.

send_blocks(Peer, T, T, L, _) -> 
    send_blocks_external(Peer, L);
send_blocks(Peer, 0, _, L, _) ->
    send_blocks_external(Peer, L);
send_blocks(Peer, TopHash, CommonHash, L, CommonHeight) ->
    BlockPlus = block:read(TopHash),
    PrevHash = block:prev_hash(BlockPlus),
    case block:height(BlockPlus) of
        CommonHeight -> %if we realize, we are on diffrent fork then the peer
            BlockCommon = block:read(CommonHash),
            NewCommon = block:prev_hash(BlockCommon),
            send_blocks(Peer, PrevHash, NewCommon, [BlockPlus|L], block:height(BlockCommon)-1); %we send one more block till we find one common with our main chain and the fork
        _ ->
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|L], CommonHeight)
    end.

send_blocks_external(Peer, Blocks) ->
    lager:debug("send_blocks_external: ~p" ,[packer:pack({sending_blocks, Blocks})]),
    spawn(?MODULE, do_send_blocks, [Peer, Blocks]).

do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
    remote_peer({give_block, Block}, Peer),
    timer:sleep(20),
    do_send_blocks(Peer, T).

get_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).

trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
