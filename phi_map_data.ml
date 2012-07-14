type position = {px : int; py : int}

type view_position = {x : int; y : int}

type absolute_direction = North | East | West | South

type relative_direction = Forth | Right | Left | Back

type direction = Absolute_direction of absolute_direction | Relative_direction of relative_direction

type mapchip = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall | Door_lock | Pcircle_lock

type flooritem = Normal

type view = {chip : mapchip; item : flooritem option}
