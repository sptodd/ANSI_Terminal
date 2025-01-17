--Copyright (C)2014-2024 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--Tool Version: V1.9.10.03 (64-bit)
--Part Number: GW2A-LV18PG256C8/I7
--Device: GW2A-18
--Device Version: C
--Created Time: Thu Nov 14 10:51:27 2024

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component vga_frame_buf
    port (
        douta: out std_logic_vector(17 downto 0);
        doutb: out std_logic_vector(17 downto 0);
        clka: in std_logic;
        ocea: in std_logic;
        cea: in std_logic;
        reseta: in std_logic;
        wrea: in std_logic;
        clkb: in std_logic;
        oceb: in std_logic;
        ceb: in std_logic;
        resetb: in std_logic;
        wreb: in std_logic;
        ada: in std_logic_vector(10 downto 0);
        dina: in std_logic_vector(17 downto 0);
        adb: in std_logic_vector(10 downto 0);
        dinb: in std_logic_vector(17 downto 0)
    );
end component;

your_instance_name: vga_frame_buf
    port map (
        douta => douta,
        doutb => doutb,
        clka => clka,
        ocea => ocea,
        cea => cea,
        reseta => reseta,
        wrea => wrea,
        clkb => clkb,
        oceb => oceb,
        ceb => ceb,
        resetb => resetb,
        wreb => wreb,
        ada => ada,
        dina => dina,
        adb => adb,
        dinb => dinb
    );

----------Copy end-------------------
