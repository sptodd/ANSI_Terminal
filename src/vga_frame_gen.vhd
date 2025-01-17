-- -----------------------------------------------------------------------------
-- Generate VGA frame timings
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
--use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.video_types_pkg.all;

entity vga_frame_gen is
    port (
        I_reset_n     : in  std_logic;
        I_pixel_clock : in  std_logic;

        I_horiz       : in  frame_parts_t;
        I_vert        : in  frame_parts_t;

        I_h_active    : in  active_parts_t;
        I_v_active    : in  active_parts_t;

        O_v_blank     : out std_logic;
        O_v_sync      : out std_logic;

        O_h_blank     : out std_logic;
        O_h_sync      : out std_logic;

        O_h_pos       : out std_logic_vector(10 downto 0);
        O_v_pos       : out std_logic_vector(9 downto 0);

        O_address     : out std_logic_vector(10 downto 0);

        O_visible     : out std_logic;
        O_active      : out std_logic
    );
end entity;

architecture rtl of vga_frame_gen is
    type video_states_t is (pixels, front_p, sync, back_p);

    signal s_h_state     :  video_states_t;
    signal s_v_state     :  video_states_t;
    signal s_h_active    :  boolean;
    signal s_v_active    :  boolean;

    signal s_eol         :  boolean  := false; 

    signal s_v_pix_end   : pix_t;
    signal s_v_front_end : pix_t;
    signal s_v_sync_end  : pix_t;
    signal s_v_back_end  : pix_t;

    signal s_h_pix_end   : pix_t;
    signal s_h_front_end : pix_t;
    signal s_h_sync_end  : pix_t;
    signal s_h_back_end  : pix_t;

    signal s_row         : std_logic_vector(5 downto 0);
    signal s_col         : std_logic_vector(6 downto 0);

begin
    v_parts_change: process(I_vert)
    begin
        s_v_pix_end   <= I_vert.pixels;
        s_v_front_end <= I_vert.front_p + I_vert.pixels;
        s_v_sync_end  <= I_vert.sync + I_vert.front_p + I_vert.pixels;
        s_v_back_end  <= I_vert.back_p + I_vert.sync + I_vert.front_p + I_vert.pixels;
    end process;

    h_parts_change: process(I_horiz)
    begin
        s_h_pix_end   <= I_horiz.pixels;
        s_h_front_end <= I_horiz.front_p + I_horiz.pixels;
        s_h_sync_end  <= I_horiz.sync + I_horiz.front_p + I_horiz.pixels;
        s_h_back_end  <= I_horiz.back_p + I_horiz.sync + I_horiz.front_p + I_horiz.pixels;
    end process;

    -- Generate sync signals 
    set_sync: process(I_pixel_clock, I_horiz, s_h_state, I_vert, s_v_state)
        variable v_vsync : std_logic;
        variable v_hsync : std_logic;
    begin
        if rising_edge(I_pixel_clock) then
            v_hsync  := '1' when s_h_state = sync else '0';
            O_h_sync <= v_hsync xor I_horiz.sync_pol; -- Apply polarity to sync

            v_vsync  := '1' when s_v_state = sync else '0';
            O_v_sync <= v_vsync xor I_vert.sync_pol; -- Apply polarity to sync
        end if;
    end process;

    -- Generate blanking signals 
    set_blanking: process(I_pixel_clock, s_h_state, s_v_state)
        variable v_vblank : boolean;
        variable v_hblank : boolean;
    begin
        if rising_edge(I_pixel_clock) then
            v_hblank  := true when s_h_state = sync or s_h_state = back_p else false;
            v_vblank  := true when s_v_state = sync or s_v_state = back_p else false;
            O_visible <= '0' when v_hblank or v_vblank else '1';
            O_h_blank <= '1' when v_hblank else '0';
            O_v_blank <= '1' when v_vblank else '0';
        end if;
    end process;

    -- Generate active signal (active display portion of visible display)
    set_active: process(s_h_active, s_v_active)
    begin
        O_active <= '1' when s_h_active and s_v_active else '0';
    end process;

    calc_addr: process(s_row, s_col, s_h_active, s_v_active)
        variable v_addr : std_logic_vector(10 downto 0);
    begin
        v_addr    := std_logic_vector(shift_left(unsigned("00000" & s_row), 6) + shift_left(unsigned("00000" & s_row), 4) + unsigned(s_col));
        O_address <= v_addr when s_h_active and s_v_active else "11111010000";
    end process;

    -- Generate horizontal states
    h_counter: process(I_pixel_clock, I_h_active, I_reset_n)
        variable v_counter : pix_t;
        variable v_vis_col : pix_t;
        variable v_active  : boolean;
        variable v_h_cell  : std_logic_vector(10 downto 0);
    begin
        if I_reset_n = '0' then
            v_counter  := 0;
            v_vis_col  := 0;
            s_h_state  <= pixels;
            s_h_active <= false;
            s_eol      <= false;

        elsif rising_edge(I_pixel_clock) then
            s_eol      <= false;
            v_counter  := v_counter + 1;

            v_active   := false when v_counter < I_h_active.start_vis or v_counter >= I_h_active.end_vis else true;
            s_h_active <= v_active;

            v_vis_col  := v_vis_col + 1 when s_h_active else 0;
            v_h_cell   := std_logic_vector(to_unsigned(v_vis_col, 11));
            O_h_pos    <= v_h_cell;
            s_col      <= v_h_cell(9 downto 3);

            case s_h_state is
                when pixels =>
                    if v_counter = s_h_pix_end then
                        s_h_state <= front_p;
                    end if;

                when front_p =>
                    if v_counter = s_h_front_end then
                        s_h_state <= sync;
                    end if;

                when sync =>
                    if v_counter = s_h_sync_end then
                        s_h_state <= back_p;
                    end if;

                when back_p =>
                    if v_counter = s_h_back_end then
                        s_h_state  <= pixels;
                        v_counter  := 0;
                        s_h_active <= true when I_h_active.start_vis = 0 else false;
                        s_eol      <= true;
                    end if;

            end case;

        end if;

    end process;

    vert_count: process(I_pixel_clock, s_eol, I_v_active, I_reset_n)
        variable v_counter : pix_t;
        variable v_vis_row : pix_t;
        variable v_cell    : std_logic_vector(9 downto 0);
        variable v_active  : boolean;
    begin
        if I_reset_n = '0' then
            v_counter  := 0;
            v_vis_row  := 0;
            s_v_state  <= pixels;
            s_v_active <= false;

        elsif rising_edge(I_pixel_clock) and s_eol then
            
            v_active   := false when (v_counter < I_v_active.start_vis or v_counter >= I_v_active.end_vis) else true;
            s_v_active <= v_active;

            v_vis_row  := v_vis_row + 1 when v_active else 0;
            v_cell     := std_logic_vector(to_unsigned(v_vis_row, 10));
            O_v_pos    <= v_cell;
            s_row      <= v_cell(9 downto 4);

            v_counter  := v_counter + 1;

            case s_v_state is
                when pixels =>
                    if v_counter = s_v_pix_end then
                        s_v_state <= front_p;
                    end if;

                when front_p =>
                    if v_counter = s_v_front_end then
                        s_v_state <= sync;
                    end if;

                when sync =>
                    if v_counter = s_v_sync_end then
                        s_v_state <= back_p;
                    end if;

                when back_p =>
                    if v_counter = s_v_back_end then
                        s_v_state  <= pixels;
                        v_counter  := 0;
                        s_v_active <= true when I_v_active.start_vis = 0 else false;
                    end if;

            end case;
            
        end if;

    end process;

end architecture;