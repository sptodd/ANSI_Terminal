-- -----------------------------------------------------------------------------
-- Calculations to move the cursor position around the screen
-- 
-- Copyright (C) 2024 Steve Todd
--
-- Redistribution and use in source and binary forms, with or without 
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, 
--    this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation 
--    and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” 
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
-- POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.video_types_pkg.all;

package screen_location_pkg is
generic(
    horiz_chars    : integer;
    vert_chars     : integer
);

    constant VERT_CHAR_MAX    : integer := vert_chars  - 1;
    constant HORIZ_CHAR_MAX   : integer := horiz_chars - 1;
    constant CHARS_PER_SCREEN : integer := horiz_chars * vert_chars;

    -- Horizontal and vertical size limits
    subtype horiz_size_t  is integer range 0 to HORIZ_CHAR_MAX;
    subtype vert_size_t   is integer range 0 to VERT_CHAR_MAX;
    subtype screen_size_t is integer range 0 to CHARS_PER_SCREEN - 1;

    type screen_location_t is record
        Horizontal : horiz_size_t;
        Vertical   : vert_size_t;
        Position   : screen_size_t;
    end record screen_location_t;

    pure function get_position_sol(loc : screen_location_t) return screen_size_t;
    pure function get_position_eol(loc : screen_location_t) return screen_size_t;
    pure function get_position_eos return screen_size_t;
    pure function get_position_eos_wr return screen_size_t;

    procedure move_next( variable loc : inout screen_location_t; scroll : out std_logic);
    procedure move_to(   variable loc : inout screen_location_t; constant hp   : in horiz_size_t; constant vp : in vert_size_t);
    procedure move_up(   variable loc : inout screen_location_t; constant rows : in vert_size_t);
    procedure move_down( variable loc : inout screen_location_t; constant rows : in vert_size_t);
    procedure move_left( variable loc : inout screen_location_t; constant cols : in horiz_size_t);
    procedure move_right(variable loc : inout screen_location_t; constant cols : in horiz_size_t);
    procedure move_sol(  variable loc : inout screen_location_t);
    procedure backspace( variable loc : inout screen_location_t);

end package screen_location_pkg;

package body screen_location_pkg is

    pure function get_position_sol(loc : screen_location_t) return screen_size_t is
    begin
        return horiz_chars * loc.Vertical;
    end;

    pure function get_position_eol(loc : screen_location_t) return screen_size_t is
    begin
        return horiz_chars * loc.Vertical + horiz_chars - 1;
    end;

    pure function get_position_eos return screen_size_t is
    begin
        return CHARS_PER_SCREEN - 1;
    end;

    pure function get_position_eos_wr return screen_size_t is
    begin
        return CHARS_PER_SCREEN - 81;
    end;

    procedure move_next(variable loc : inout screen_location_t; variable scroll : out std_logic) is
    begin
        if loc.Horizontal < HORIZ_CHAR_MAX then
            loc.Horizontal := loc.Horizontal + 1;
            loc.Position   := loc.Position + 1;
            scroll := '0';

        else
            loc.Horizontal := 0;
            if loc.Vertical < VERT_CHAR_MAX then
                loc.Vertical := loc.Vertical + 1;
                loc.Position := loc.Position + 1;
                scroll := '0';

            else
                loc.Position := loc.Vertical * horiz_chars;
                scroll := '1';

            end if;
        end if;
    end procedure move_next;

    procedure move_to(variable loc : inout screen_location_t; constant hp : in horiz_size_t; constant vp : in vert_size_t) is
    begin
        loc.Horizontal := hp when loc.Horizontal <= HORIZ_CHAR_MAX else HORIZ_CHAR_MAX;
        loc.Vertical   := vp when vp < VERT_CHAR_MAX else VERT_CHAR_MAX;
        loc.Position   := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

    procedure move_up(variable loc : inout screen_location_t; constant rows : in vert_size_t) is
        variable row : vert_size_t;
    begin
        row := 1 when rows = 0 else rows;
        loc.Vertical := 0 when loc.Vertical < row else loc.Vertical - row;
        loc.Position := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

    procedure move_down(variable loc : inout screen_location_t; constant rows : in vert_size_t) is
        variable row : vert_size_t;
    begin
        row := 1 when rows = 0 else rows;
        loc.Vertical := VERT_CHAR_MAX when loc.Vertical + row > VERT_CHAR_MAX else loc.Vertical + row;
        loc.Position := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

    procedure move_left(variable loc : inout screen_location_t; constant cols : in horiz_size_t) is
        variable col : horiz_size_t;
    begin
        col := 1 when cols = 0 else cols;
        loc.Horizontal := 0 when loc.Horizontal < col else loc.Horizontal - col;
        loc.Position   := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

    procedure move_right(variable loc : inout screen_location_t; constant cols : in horiz_size_t) is
        variable col : horiz_size_t;
    begin
        col := 1 when cols = 0 else cols;
        loc.Horizontal := HORIZ_CHAR_MAX when loc.Horizontal + col > HORIZ_CHAR_MAX else loc.Horizontal + col;
        loc.Position   := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

    procedure move_sol(variable loc : inout screen_location_t) is
    begin
        loc.Horizontal := 0;
        loc.Position   := loc.Vertical * horiz_chars;
    end;

    procedure backspace(variable loc : inout screen_location_t) is
    begin
        if loc.Horizontal > 0 then
            loc.Horizontal := loc.Horizontal - 1;
        else
            if loc.Vertical > 0 then
                loc.Vertical   := loc.Vertical - 1;
                loc.Horizontal := HORIZ_CHAR_MAX;
            end if;
        end if;

        loc.Position := loc.Vertical * horiz_chars + loc.Horizontal;
    end procedure;

end package body screen_location_pkg;
