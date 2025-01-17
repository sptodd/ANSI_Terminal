--Copyright (C)2014-2024 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--Tool Version: V1.9.10.03 (64-bit)
--Part Number: GW2A-LV18PG256C8/I7
--Device: GW2A-18
--Device Version: C
--Created Time: Fri Nov  8 11:09:02 2024

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component vga_dvi_tx
	port (
		I_rst_n: in std_logic;
		I_serial_clk: in std_logic;
		I_rgb_clk: in std_logic;
		I_rgb_vs: in std_logic;
		I_rgb_hs: in std_logic;
		I_rgb_de: in std_logic;
		I_rgb_r: in std_logic_vector(7 downto 0);
		I_rgb_g: in std_logic_vector(7 downto 0);
		I_rgb_b: in std_logic_vector(7 downto 0);
		O_tmds_clk_p: out std_logic;
		O_tmds_clk_n: out std_logic;
		O_tmds_data_p: out std_logic_vector(2 downto 0);
		O_tmds_data_n: out std_logic_vector(2 downto 0)
	);
end component;

your_instance_name: vga_dvi_tx
	port map (
		I_rst_n => I_rst_n,
		I_serial_clk => I_serial_clk,
		I_rgb_clk => I_rgb_clk,
		I_rgb_vs => I_rgb_vs,
		I_rgb_hs => I_rgb_hs,
		I_rgb_de => I_rgb_de,
		I_rgb_r => I_rgb_r,
		I_rgb_g => I_rgb_g,
		I_rgb_b => I_rgb_b,
		O_tmds_clk_p => O_tmds_clk_p,
		O_tmds_clk_n => O_tmds_clk_n,
		O_tmds_data_p => O_tmds_data_p,
		O_tmds_data_n => O_tmds_data_n
	);

----------Copy end-------------------
