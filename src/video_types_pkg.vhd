-- -----------------------------------------------------------------------------
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
use ieee.numeric_std.all;
 
package video_types_pkg is

    -- byte
    subtype byte is std_logic_vector(7 downto 0);

    -- Word
    subtype word is std_logic_vector(15 downto 0);

    -- Word
    subtype word18 is std_logic_vector(17 downto 0);

    -- ten bits for attribute
    subtype attr_t is std_logic_vector(9 downto 0);

    -- RGB byte
    type rgb_t is
        record
            red     : byte;
            green   : byte;
            blue    : byte;
        end record;

    subtype horiz_val_t       is integer range 0 to 4095;
    subtype vert_val_t        is integer range 0 to 2047;

    subtype horiz_char_size_t is integer range 0 to 255;
    subtype vert_char_size_t  is integer range 0 to 63;

    subtype pix_t             is integer range 0 to 2047;
    subtype pix_short_t       is integer range 0 to 255;

    type frame_parts_t is
        record
            pixels    : pix_t;
            front_p   : pix_short_t;
            sync      : pix_short_t;
            back_p    : pix_short_t;
            sync_pol  : std_logic;
        end record;

    type active_parts_t is
        record
            start_vis : pix_short_t;
            end_vis   : pix_t;
        end record;

    type delay_parts_t is
        record
            v_sync   : std_logic;
            h_sync   : std_logic;
            visible  : std_logic;
            active   : std_logic;
            row      : std_logic_vector(9 downto 0);
            col      : std_logic_vector(10 downto 0);
            buff_add : std_logic_vector(10 downto 0);
        end record;

    type attr_parts_t is
        record
            fore_col : std_logic_vector(3 downto 0);
            back_col : std_logic_vector(3 downto 0);
            blink    : std_logic;
            underln  : std_logic;
        end record;

    type delay_parts_vector_t is array (natural range <>) of delay_parts_t;
    type attrib_delay_vector_t is array (natural range <>) of attr_parts_t;

    type display_char_t is
        record
            char : byte;
            attr : byte;
        end record;

end package video_types_pkg;
