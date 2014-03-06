% @copyright 2014 Feiko Nanninga

-module(mc_erl_inventory).

-export([get_slot/2, send_inventory/2, inventory_add/3, inventory_add/5,
         items_equal/2, inventory_add_to_stack/4, update_slot/4]).

-include("records.hrl").

send_inventory(Writer, Inv) ->
    mc_erl_player_core:write(Writer, {window_items, [0, array:to_list(Inv)]}).

%% action = reduce | {replace, Slot} | empty
update_slot(Writer, Inv, SlotNo, Action) ->
    NewSlot = case Action of
                  empty ->
                      empty;
                  reduce ->
                      case array:get(SlotNo, Inv) of
                          empty -> empty;
                          #slot{count=1} -> empty;
                          #slot{} = S -> S#slot{count=S#slot.count-1}
                      end;
                  {replace, Slot} -> Slot
              end,
    NewInv = array:set(SlotNo, NewSlot, Inv),
    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
    NewInv.

get_slot(Inventory, SlotNo) ->
    array:get(SlotNo, Inventory).

items_equal(empty, empty) -> true;
items_equal(_, empty) -> false;
items_equal(empty, _) -> false;
items_equal(Slot1, Slot2) when is_record(Slot1, slot), is_record(Slot2, slot) ->
    Slot1#slot.id =:= Slot2#slot.id
    andalso Slot1#slot.metadata =:= Slot2#slot.metadata
    andalso Slot1#slot.enchantments =:= Slot2#slot.enchantments.

inventory_add(Writer, Inv, #slot{}=Slot) ->
    inventory_add(Writer, Inv, 9, 44, Slot).

inventory_add(_Writer, Inventory, _SlotNo, _EndSlot, empty) ->
    {Inventory, empty};
inventory_add(Writer, Inventory, SlotNo, EndSlot, Rest) when EndSlot =:= SlotNo-1 ->
    inventory_add_to_free_slot(Writer, Inventory, 9, 44, Rest);
inventory_add(Writer, Inventory, SlotNo, EndSlot,
              #slot{id=BlockId, count=Count, metadata=Metadata,
                    enchantments=Enchantments}=Slot) ->
    MaxStack = (mc_erl_blocks:block_info(BlockId))#block_type.maxstack,
    case array:get(SlotNo, Inventory) of
        #slot{id=BlockId, count=OldCount, metadata=Metadata,
              enchantments=Enchantments} ->
            if
                OldCount >= MaxStack ->
                    inventory_add(Writer, Inventory, SlotNo+1, EndSlot, Slot);
                OldCount+Count > MaxStack ->
                    NewSlot = Slot#slot{count=MaxStack},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    RestCount = OldCount+Count - MaxStack,
                    inventory_add(Writer, NewInv, SlotNo+1, EndSlot,
                                  Slot#slot{count=RestCount});
                true ->
                    NewSlot = Slot#slot{count=OldCount+Count},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    {NewInv, empty}
            end;
        empty ->
            if
                Count > MaxStack ->
                    NewSlot = Slot#slot{count=MaxStack},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    RestCount = Count - MaxStack,
                    inventory_add(Writer, NewInv, SlotNo+1, EndSlot,
                                  Slot#slot{count=RestCount});
                true ->
                    NewSlot = Slot#slot{count=Count},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    {NewInv, empty}
            end;
        _ ->
            inventory_add(Writer, Inventory, SlotNo+1, EndSlot, Slot)
    end.

inventory_add_to_free_slot(_Writer, Inventory, SlotNo, EndSlot, Rest)
  when EndSlot =:= SlotNo-1 ->
    {Inventory, Rest};
inventory_add_to_free_slot(Writer, Inventory, SlotNo, EndSlot,
                           #slot{id=BlockId, count=Count}=Slot) ->
    MaxStack = (mc_erl_blocks:block_info(BlockId))#block_type.maxstack,
    case array:get(SlotNo, Inventory) of
        empty ->
            if
                Count > MaxStack ->
                    NewSlot = Slot#slot{count=MaxStack},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    RestCount = Count - MaxStack,
                    inventory_add_to_free_slot(Writer, NewInv, SlotNo+1, EndSlot,
                                               Slot#slot{count=RestCount});
                true ->
                    NewInv = array:set(SlotNo, Slot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, Slot]}),
                    {NewInv, empty}
            end;
        _ ->
            inventory_add_to_free_slot(Writer, Inventory, SlotNo+1, EndSlot, Slot)
    end.

inventory_add_to_stack(Writer, Inventory, SlotNo, #slot{id=BlockId, count=Count}=Slot) ->
    MaxStack = (mc_erl_blocks:block_info(BlockId))#block_type.maxstack,
    case array:get(SlotNo, Inventory) of
        #slot{count=OldCount} ->
            if
                OldCount >= MaxStack ->
                    {Inventory, Slot};
                OldCount+Count > MaxStack ->
                    NewSlot = Slot#slot{count=MaxStack},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    RestCount = OldCount+Count - MaxStack,
                    {NewInv, Slot#slot{count=RestCount}};
                true ->
                    NewSlot = Slot#slot{count=OldCount+Count},
                    NewInv = array:set(SlotNo, NewSlot, Inventory),
                    mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, NewSlot]}),
                    {NewInv, empty}
            end;
        empty ->
            NewInv = array:set(SlotNo, Slot, Inventory),
            mc_erl_player_core:write(Writer, {set_slot, [0, SlotNo, Slot]}),
            {NewInv, empty}
    end.
