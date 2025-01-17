-- -----------------------------------------------------------------------------
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
use ieee.numeric_std.all;

use work.video_types_pkg.all;

entity vga_text_display is
port(
    I_pix_clk   : in  std_logic;
    I_pix_clk_n : in  std_logic;
    I_locked    : in  std_logic;
    I_address   : in  std_logic_vector(10 downto 0);
    I_data      : in  word18;
    I_write_en  : in  std_logic;
    I_cursor_pos: in  integer range 0 to 2047;

    O_data      : out word18;
    O_v_sync    : out std_logic;   
    O_h_sync    : out std_logic;
    O_vis       : out std_logic;
    O_red       : out byte;
    O_green     : out byte;
    O_blue      : out byte
);
end entity;

architecture rtl of vga_text_display is

    -- VGA colour index to RGB colour
    function idx_to_rgb(idx : std_logic_vector(3 downto 0) := (others => '0')) return rgb_t is
        variable res : rgb_t;
    begin
        case idx is 
            when "0000" => -- Black
                res := (red => "00000000", green => "00000000", blue => "00000000");
            when "0001" => -- Dark blue
                res := (red => "00000000", green => "00000000", blue => "11000000");
            when "0010" => -- Dark green
                res := (red => "00000000", green => "11000000", blue => "00000000");
            when "0011" => -- Dark cyan
                res := (red => "00000000", green => "11000000", blue => "11000000");
            when "0100" => -- Dark red
                res := (red => "11000000", green => "00000000", blue => "00000000");
            when "0101" => -- Dark magenta
                res := (red => "11000000", green => "00000000", blue => "11000000");
            when "0110" => -- Dark yellow
                res := (red => "11000000", green => "11000000", blue => "00000000");
            when "0111" => -- White
                res := (red => "11000000", green => "11000000", blue => "11000000");
            when "1000" => -- Grey
                res := (red => "00010101", green => "00010101", blue => "00010101");
            when "1001" => -- Light blue
                res := (red => "00010101", green => "00010101", blue => "11111111");
            when "1010" => -- Light green
                res := (red => "00010101", green => "11111111", blue => "00010101");
            when "1011" => -- Light cyan
                res := (red => "00010101", green => "11111111", blue => "11111111");
            when "1100" => -- Light red
                res := (red => "11111111", green => "00010101", blue => "00010101");
            when "1101" => -- Light magenta
                res := (red => "11111111", green => "00010101", blue => "11111111");
            when "1110" => -- Light yellow
                res := (red => "11111111", green => "11111111", blue => "00010101");
            when "1111" => -- Bright white
                res := (red => "11111111", green => "11111111", blue => "11111111");
        end case;

        return res;
    end function;

    component vga_frame_buf
        port (
            douta  : out std_logic_vector(17 downto 0);
            doutb  : out std_logic_vector(17 downto 0);
            clka   : in std_logic;
            ocea   : in std_logic;
            cea    : in std_logic;
            reseta : in std_logic;
            wrea   : in std_logic;
            clkb   : in std_logic;
            oceb   : in std_logic;
            ceb    : in std_logic;
            resetb : in std_logic;
            wreb   : in std_logic;
            ada    : in std_logic_vector(10 downto 0);
            dina   : in std_logic_vector(17 downto 0);
            adb    : in std_logic_vector(10 downto 0);
            dinb   : in std_logic_vector(17 downto 0)
        );
    end component;

    component vga_sample_buf
        port (
            douta  : out std_logic_vector(17 downto 0);
            doutb  : out std_logic_vector(17 downto 0);
            clka   : in std_logic;
            ocea   : in std_logic;
            cea    : in std_logic;
            reseta : in std_logic;
            wrea   : in std_logic;
            clkb   : in std_logic;
            oceb   : in std_logic;
            ceb    : in std_logic;
            resetb : in std_logic;
            wreb   : in std_logic;
            ada    : in std_logic_vector(10 downto 0);
            dina   : in std_logic_vector(17 downto 0);
            adb    : in std_logic_vector(10 downto 0);
            dinb   : in std_logic_vector(17 downto 0)
        );
    end component;

    signal s_font_dta   : std_logic_vector(7 downto 0);
    signal s_raster_dta : std_logic_vector(7 downto 0);

    signal s_disp_bit   : std_logic;

    signal cell_data    : std_logic_vector(17 downto 0);

    signal dvi_delay    : delay_parts_vector_t(3 downto 0);

    signal current_attr : attr_parts_t;

    signal s_v_sync     : std_logic;
    signal s_h_sync     : std_logic;
    signal s_vis        : std_logic;
    signal s_rgb        : rgb_t; 

    signal s_v_blank    : std_logic;
    signal s_last_v_bl  : std_logic;
    signal s_blink_cnt  : integer range 0 to 59 := 0;
    signal s_blink_inv  : std_logic := '0';

begin

    frame_gen: entity work.vga_frame_gen(rtl)
        port map (
            I_reset_n     => I_locked,
            I_pixel_clock => I_pix_clk,

            I_horiz       => (pixels => 640, front_p => 16, sync => 96, back_p => 48, sync_pol  => '1'),
            I_vert        => (pixels => 480, front_p => 10, sync => 2,  back_p => 33, sync_pol  => '1'),

            I_h_active    => (start_vis => 9,  end_vis => 648),
            I_v_active    => (start_vis => 40, end_vis => 439),

            O_v_blank     => s_v_blank,
            O_v_sync      => dvi_delay(0).v_sync,

            O_h_blank     => open,
            O_h_sync      => dvi_delay(0).h_sync,

            O_h_pos       => dvi_delay(0).col,
            O_v_pos       => dvi_delay(0).row,

            O_address     => dvi_delay(0).buff_add,

            O_visible     => dvi_delay(0).visible,
            O_active      => dvi_delay(0).active
        );

    frame_buf: vga_sample_buf
        port map (
            douta  => O_data,
            doutb  => cell_data,
            clka   => I_pix_clk,
            ocea   => '1',
            cea    => '1',
            reseta => '0',
            wrea   => I_write_en,
            clkb   => I_pix_clk,
            oceb   => '1',
            ceb    => '1',
            resetb => '0',
            wreb   => '0',
            ada    => I_address,
            dina   => I_data,
            adb    => dvi_delay(0).buff_add,
            dinb   => (others => '0')
        );

    font_rom: entity work.font_lookup(rtl)
        port map (
            I_reset_n     => I_locked,
            I_pixel_clock => I_pix_clk_n,

            I_char        => cell_data(7 downto 0),
            I_line        => dvi_delay(1).row(3 downto 0),

            O_raster      => s_font_dta
        );

    blink_count: process(I_pix_clk, s_v_blank)
    begin
        if rising_edge(I_pix_clk) then
            if s_last_v_bl = '0' and s_v_blank = '1' then
                s_blink_cnt <= 0 when s_blink_cnt = 59 else s_blink_cnt + 1;
                s_blink_inv <= '1' when s_blink_cnt > 29 else '0';
            end if;
            s_last_v_bl <= s_v_blank;
        end if;
    end process;

    dvi_delay_shift: process(I_pix_clk)
    begin
        if rising_edge(I_pix_clk) then
            dvi_delay(3 downto 1) <= dvi_delay(2 downto 0);
        end if;
    end process;

    char_lookup: process(I_pix_clk)
    begin
        if rising_edge(I_pix_clk) then
            if dvi_delay(2).col(2 downto 0) = "000" then
                s_raster_dta          <= s_font_dta(6 downto 0) & '0';
                s_disp_bit            <= s_font_dta(7);
                current_attr.fore_col <= cell_data(11 downto 8);
                current_attr.back_col <= cell_data(15 downto 12);
                current_attr.blink    <= cell_data(16);
                current_attr.underln  <= cell_data(17);                
            else
                s_disp_bit   <= s_raster_dta(7);
                s_raster_dta <= s_raster_dta(6 downto 0) & '0';
            end if;
        end if;
    end process;

    display_pixel: process(I_pix_clk, s_disp_bit, current_attr)
        variable v_col : std_logic_vector(3 downto 0);
        variable v_bit : std_logic;
        variable v_addr: integer range 0 to 2047;
    begin
        if rising_edge(I_pix_clk) then
            -- Copy the sync and visible states to output
            s_v_sync <= dvi_delay(3).v_sync;
            s_h_sync <= dvi_delay(3).h_sync;
            s_vis    <= dvi_delay(3).visible;
            v_addr   := to_integer(unsigned(dvi_delay(3).buff_add));

            -- Modify the pixel state based on the underline and blinking attribute flags
            -- Underline?
            if current_attr.underln = '1' and dvi_delay(3).row(3 downto 0) = "1101" then
                -- Underline sets all pixels for row 13 to on
                v_bit := '1';
            else
                -- Is this a foreground or background pixel?
                v_bit := s_disp_bit;
            end if;

            -- Handle blinking (via atribute or cursor)
            if s_blink_inv = '0' then
                -- Cursor blinks on first half of count
                if v_addr = I_cursor_pos then
                    v_bit := not v_bit;
                end if;
            elsif current_attr.blink = '1' then
                -- Characters with Blink attribute blinks on second half
                v_bit := not v_bit;
            end if;

            -- get the colour index (foreground or background) for this pixel
            v_col := current_attr.fore_col when v_bit = '1' else current_attr.back_col;

            -- convert colour index to RGB and send to output
            s_rgb <= idx_to_rgb(v_col);
        end if;
    end process;

    -- Clock out the completed pixel data to the DVI port
    dvi_signal: process(I_pix_clk, s_rgb, s_v_sync, s_h_sync, s_vis)
    begin
        if rising_edge(I_pix_clk) then
            O_v_sync <= s_v_sync;
            O_h_sync <= s_h_sync;
            O_vis    <= s_vis;
            O_red    <= s_rgb.red;
            O_green  <= s_rgb.green;
            O_blue   <= s_rgb.blue;
        end if;
    end process;

end architecture;