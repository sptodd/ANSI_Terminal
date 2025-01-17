-- -----------------------------------------------------------------------------
-- Sample top level entity to exercise the ANSI decoder
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

entity vga_text_top is
port(
    I_sys_clk       : in     std_logic;
    O_v_tmds_clk_p  : out    std_logic;
    O_v_tmds_clk_n  : out    std_logic;
    O_v_tmds_data_p : out    std_logic_vector(2 downto 0);
    O_v_tmds_data_n : out    std_logic_vector(2 downto 0);
    O_leds          : out    std_logic_vector(3 downto 0)
);
end entity;

architecture rtl of vga_text_top is

    component vga_dvi_tx
        port (
            I_rst_n       : in  std_logic;
            I_serial_clk  : in  std_logic;
            I_rgb_clk     : in  std_logic;
            I_rgb_vs      : in  std_logic;
            I_rgb_hs      : in  std_logic;
            I_rgb_de      : in  std_logic;
            I_rgb_r       : in  std_logic_vector(7 downto 0);
            I_rgb_g       : in  std_logic_vector(7 downto 0);
            I_rgb_b       : in  std_logic_vector(7 downto 0);
            O_tmds_clk_p  : out std_logic;
            O_tmds_clk_n  : out std_logic;
            O_tmds_data_p : out std_logic_vector(2 downto 0);
            O_tmds_data_n : out std_logic_vector(2 downto 0)
        );
    end component;

    component vga_fast_clock_rpll
        port (
            clkout: out std_logic;
            lock  : out std_logic;
            clkin : in  std_logic
        );
    end component;

    component vga_pix_clkdiv
        port (
            clkout: out std_logic;
            hclkin: in  std_logic;
            resetn: in  std_logic
        );
    end component;

    signal s_pix_clk    : std_logic;
    signal s_pix_clk_n  : std_logic;
    signal s_fast_clk   : std_logic;
    signal s_locked     : std_logic;

    signal s_red        : byte;
    signal s_green      : byte;
    signal s_blue       : byte;

    signal s_h_sync     : std_logic;
    signal s_v_sync     : std_logic;
    signal s_visible    : std_logic;

    signal s_cursor_pos : integer range 0 to 2047 := 0;

    signal s_wren       : std_logic;

    signal s_leds       : std_logic_vector(3 downto 0) := "1111";

    signal s_sys_count  : std_logic_vector(23 downto 0);
    signal s_pix_count  : std_logic_vector(23 downto 0);

    signal s_address    : std_logic_vector(10 downto 0);

    signal test_data      : string(1 to 255);
    signal test_index     : integer range 0 to 2047     := 1;
    signal delay_count    : integer range 0 to 60000000 := 0;

    signal s_char_rdy   : std_logic;
    signal s_term_rdy   : std_logic;
    signal s_char       : character;

    signal s_data_in    : word18;
    signal s_data_out   : word18;

    function padded_string(s: string; n: positive) return string is
        variable ps: string(1 to n) := (others => character'val(0));
    begin
        if s'length >= n then
            ps := s(1 to n); --- truncate the source string
        else
            ps(1 to s'length) := s;
            ps(s'length+1 to n) := (others => character'val(0));
        end if;
        return ps;
    end;

begin
    -- Set up working entities and connect them together
    fast_clock: vga_fast_clock_rpll
        port map (
            clkout => s_fast_clk,
            lock   => s_locked,
            clkin  => I_sys_clk
        );

    pix_clock: vga_pix_clkdiv
        port map (
            clkout => s_pix_clk,
            hclkin => s_fast_clk,
            resetn => s_locked
        );

    pix_clock_n: process(s_pix_clk)
    begin
        s_pix_clk_n <= not s_pix_clk;
    end process;

    dvi_tx: vga_dvi_tx
        port map (
            I_rst_n       => s_locked,
            I_serial_clk  => s_fast_clk,
            I_rgb_clk     => s_pix_clk,
            I_rgb_vs      => s_v_sync,
            I_rgb_hs      => s_h_sync,
            I_rgb_de      => s_visible,
            I_rgb_r       => s_red,
            I_rgb_g       => s_green,
            I_rgb_b       => s_blue,
            O_tmds_clk_p  => O_v_tmds_clk_p,
            O_tmds_clk_n  => O_v_tmds_clk_n,
            O_tmds_data_p => O_v_tmds_data_p,
            O_tmds_data_n => O_v_tmds_data_n
        );

    text_display: entity work.vga_text_display
        port map (
            I_pix_clk    => s_pix_clk,
            I_pix_clk_n  => s_pix_clk_n,
            I_locked     => s_locked,
            I_address    => s_address,
            I_data       => s_data_out,
            I_write_en   => s_wren,
            I_cursor_pos => s_cursor_pos,

            O_data       => s_data_in,
            O_v_sync     => s_v_sync,   
            O_h_sync     => s_h_sync,
            O_vis        => s_visible,
            O_red        => s_red,
            O_green      => s_green,
            O_blue       => s_blue
        );

    terminal_decode: entity work.ANSI_terminal_decode
        port map (
            I_clock      => s_pix_clk,
            I_reset_n    => s_locked,
            I_data_in    => s_data_in,
            I_char_rdy   => s_char_rdy,
            I_char       => s_char,
            I_sync       => s_v_sync,
            O_term_rdy   => s_term_rdy,
            O_data_out   => s_data_out,
            O_address    => s_address,
            O_wren       => s_wren,
            O_cursor_pos => s_cursor_pos
        );

    -- Simulate characters comming in from an external source
    send_chars: process(s_pix_clk)
        variable ch    : character;
        variable count : integer := -1;
        variable first : std_logic := '1';
    begin
        if rising_edge(s_pix_clk) then
            if count = -1 then
                test_data <= padded_string("Hello World" & character'val(13) & character'val(10) & character'val(0), test_data'length);
                count := 1;
            end if;
            s_char_rdy <= '0';
            if delay_count > 3000000 and s_term_rdy = '1' then
                s_leds(3)   <= '1';
                delay_count <= 0;
                ch := test_data(test_index);
                if ch = character'val(0) then
                    test_index <= 1;
                    count := count + 1;
                    if count = 11 then
                        count := 0;
                        if first then
                            test_data  <= padded_string(character'val(12) & 
                                                        character'val(27) & "[41;96m" & character'val(9) & "T1" & 
                                                        character'val(27) & "[2E" & character'val(9) & "T2" & 
                                                        character'val(27) & "[41G" & character'val(9) & "T3" & 
                                                        character'val(27) & "[1;1H" & character'val(0), test_data'length);
                            first      := '0';
                        else
                            test_data <= padded_string(character'val(27) & "[H" & character'val(0), test_data'length);
                        end if;
                    else
                        test_data <= padded_string("Hello World" & character'val(13) & character'val(10) & character'val(0), test_data'length);
                    end if;
                else
                    s_leds(2)  <= not s_leds(2);
                    s_char_rdy <= '1';
                    s_char     <= ch;
                    test_index <= test_index + 1;
                end if;
            else
                delay_count <= delay_count + 1;
            end if;
        end if;
    end process;

    -- Set the status leds
    display_leds: process(s_leds)
    begin
        O_leds <= s_leds;
    end process;

    -- Blink one of the ststus LEDs based on the system clock to show that we are alive
    blinker: process(I_sys_clk)
    begin
        if rising_edge(I_sys_clk) then
            s_sys_count <= std_logic_vector(unsigned(s_sys_count) + 1);
            s_leds(0)   <= s_sys_count(23);
        end if;
    end process;

    -- Blink another LED based on the VGA pixel clock
    fast_blinker: process(s_pix_clk)
    begin
        if rising_edge(s_pix_clk) then
            s_pix_count <= std_logic_vector(unsigned(s_pix_count) + 1);
            s_leds(1)   <= s_pix_count(23);
        end if;
    end process;

end architecture;