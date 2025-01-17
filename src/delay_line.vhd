library ieee;
use ieee.std_logic_1164.all;

entity delay_line is
generic(
    data_width : integer := 8;
    depth      : integer := 40);
port(
    I_clock    : in  std_logic;
    I_data     : in  std_logic_vector(data_width-1 downto 0);
    O_data     : out std_logic_vector(data_width-1 downto 0));
end entity;

architecture rtl of delay_line is

    type std_logic_aoa is array (natural range <>) of std_logic_vector;
    signal dline : std_logic_aoa(0 to depth-1)(data_width-1 downto 0);    

begin
    shift: process(I_clock)
        variable i : integer range 0 to 31;
    begin
        if rising_edge(I_clock) then
            shift_reqd: if depth > 1 generate
            begin
                for i in depth - 1 downto 1 generate
                    dline(i - 1) <= dline(i);
                end generate;
                dline(depth - 1) <= I_data;
            end generate shift_reqd;

            no_shift_reqd: if depth = 1 generate
                dline(0) <= I_data;
            end generate no_shift_reqd;
    `   end if;

    end process;

    output: process(I_clock)
    begin
        if rising_edge(I_clock) then
             O_data <= dline(0);
        end if;
    end process;

end architecture;
