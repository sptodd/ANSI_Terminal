-- -----------------------------------------------------------------------------
-- Font lookup - Lookup the raster data for a given character and row from the
--               raw font ROM data
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
use ieee.std_logic_arith.all;

use work.video_types_pkg.all;

entity font_lookup is
    port (
        I_reset_n     : in  std_logic;
        I_pixel_clock : in  std_logic;

        I_char        : in  std_logic_vector(7 downto 0);
        I_line        : in  std_logic_vector(3 downto 0);

        O_raster      : out std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of font_lookup is

    component VGA_FONT_pROM
        port (
            dout: out std_logic_vector(7 downto 0);
            clk: in std_logic;
            oce: in std_logic;
            ce: in std_logic;
            reset: in std_logic;
            ad: in std_logic_vector(11 downto 0)
        );
    end component;

    signal addr   : std_logic_vector(11 downto 0);
    signal raster : std_logic_vector(7 downto 0);
    signal reset  : std_logic;

begin
    rom: VGA_FONT_pROM
    port map (
        dout  => O_raster,
        clk   => I_pixel_clock,
        oce   => '1',
        ce    => '1',
        reset => reset,
        ad    => addr
    );

    setup: process(I_pixel_clock, I_reset_n, I_char, I_line)
    begin
        reset <= not I_reset_n;

        if I_reset_n = '0' then
            addr <= (others => '0');
        elsif rising_edge(I_pixel_clock) then
            addr(11 downto 4) <= I_char;
            addr(3 downto 0)  <= I_line;
        end if;
    end process;

end architecture;