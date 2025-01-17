library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.video_types_pkg.all;

entity display_counter is
    port (
        I_reset_n     : in  std_logic;
        I_pixel_clock : in  std_logic;

        I_left        : in  pix_short_t;
        I_horizontal  : in  pix_t;
        I_top         : in  pix_short_t;
        I_vert        : in  pix_t;

        I_v_blank     : in  std_logic;
        I_h_blank     : in  std_logic;

        O_h_count     : out pix_t;
        O_v_count     : out pix_t
    );
end entity;

architecture rtl of display_counter is
    signal h_count_pos : pix_t;
    signal v_count_pos : pix_t;

begin
    h_count: process(I_pixel_clock, I_reset_n)
        variable h_counter : integer range 0 to 2047;
    begin
        if rising_edge(I_pixel_clock) then
            if I_reset_n = '0' then
                h_counter   := 0;
                h_count_pos <= 0;
            else
                -- reset the horizontal counter if we are in a blanking period, else increment it 
                if I_v_blank = '1' or I_h_blank = '1' then
                    h_counter   := 0;
                    h_count_pos <= 0;
                else
                    h_counter := h_counter + 1;
                    -- increment the actual counter if we are within its defined range
                    if h_counter > I_left and h_count_pos < I_horizontal - 1 then
                        h_count_pos <= h_count_pos + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    v_count: process(I_pixel_clock, I_reset_n)
        variable v_counter    : pix_t;
        variable last_h_blank : std_logic := '0';
    begin
        if rising_edge(I_pixel_clock) then
            if I_reset_n = '0' then
                v_counter   := 0;
                v_count_pos <= 0;
            else
                -- reset the vertical counter if we are in a vblank period, else increment it 
                if I_v_blank = '1' then
                    v_counter   := 0;
                    v_count_pos <= 0;
                elsif last_h_blank /= I_h_blank then
                    last_h_blank := I_h_blank;
                    if I_h_blank = '1' then
                        v_counter := v_counter + 1;
                        if v_counter > I_top and v_count_pos < I_vert - 1 then
                            v_count_pos <= v_count_pos + 1;
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process;

    h_out: process(h_count_pos)
    begin
        O_h_count <= h_count_pos;
    end process;

    v_out: process(v_count_pos)
    begin
        O_v_count <= v_count_pos;
    end process;

end architecture;
