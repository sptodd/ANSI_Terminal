//Copyright (C)2014-2024 GOWIN Semiconductor Corporation.
//All rights reserved.
//File Title: Timing Constraints file
//Tool Version: V1.9.10.03 (64-bit) 
//Created Time: 2024-11-22 10:40:04
create_clock -name sys_clk -period 37.037 -waveform {0 18.518} [get_ports {I_sys_clk}]
create_clock -name fast_clk -period 7.937 -waveform {0 3.969} [get_nets {s_fast_clk}]
create_clock -name pix_clk -period 39.683 -waveform {0 19.841} [get_nets {s_pix_clk}]

