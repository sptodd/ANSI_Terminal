-- -----------------------------------------------------------------------------
-- ANSI Terminal decoder
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

package location_pkg is new work.screen_location_pkg 
    generic map (horiz_chars  => 80, vert_chars  => 25);

use work.location_pkg.all;
use work.video_types_pkg.all;

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
    
entity ANSI_terminal_decode is
    port (
        I_clock     : in  std_logic;
        I_reset_n   : in  std_logic;
        I_data_in   : in  word18;
        I_char_rdy  : in  std_logic;
        I_char      : in  character;
        I_sync      : in  std_logic;
        O_term_rdy  : out std_logic := '1';
        O_data_out  : out word18;
        O_address   : out std_logic_vector(10 downto 0);
        O_wren      : out std_logic;
        O_cursor_pos: out integer range 0 to 2047
    );
end ANSI_terminal_decode;

architecture rtl of ANSI_terminal_decode is

    constant DEFAULT_ATT      : attr_parts_t := (fore_col => "1111", back_col => "0000", blink => '0', underln => '0');
    constant ANSI_DEFAULT_ATT : attr_parts_t := (fore_col => "0111", back_col => "0000", blink => '0', underln => '0');

    constant HORIZ_CHAR_MAX   : integer := 79;
    constant VERT_CHAR_MAX    : integer := 24;
    constant CHARS_PER_SCREEN : integer := 2000;

    pure function DisplayData(char : character; attr : attr_parts_t; inverse : std_logic) return word18 is
    begin
        if inverse = '1' then
            return attr.underln & attr.blink & attr.fore_col & attr.back_col & std_logic_vector(to_unsigned(character'pos(char), 8));
        else
            return attr.underln & attr.blink & attr.back_col & attr.fore_col & std_logic_vector(to_unsigned(character'pos(char), 8));
        end if;
    end function;

    pure function chr(ch : byte) return character is
    begin
        return character'val(to_integer(unsigned(ch)));
    end;

    pure function chr_code(ch : pix_short_t) return character is
    begin
        return character'val(ch);
    end;

    pure function val(ch : character) return pix_short_t is
        variable ch_code : pix_short_t;
    begin
        ch_code   := character'pos(ch);
        return ch_code - character'pos('0');
    end;

    signal s_attr        : attr_parts_t := ANSI_DEFAULT_ATT; -- iBGR(back) iBGR(text)

    signal s_saved_h     : pix_short_t;
    signal s_saved_v     : pix_short_t;

    signal s_term_rdy    : std_logic := '1';

    signal s_cursorPos   : integer range 0 to 2047;
    signal s_cursor_vis  : std_logic := '1';

    signal s_rd_addr     : integer range 0 to CHARS_PER_SCREEN;
    signal s_wr_addr     : integer range 0 to CHARS_PER_SCREEN;
    signal s_end_addr    : integer range 0 to CHARS_PER_SCREEN;

    signal s_write_data  : word18;

    type   dispState_t is ( idle, setScroll, checkCommand, csiMode, csiParameters, applyAttr, fillMem, 
                            insertLine, deleteLine, moveUp, moveDown, decExtensions, scrollUp, scrollDown );
    type   memState_t is  ( memIdle, memCopy, memWrite, memCopyRev, memWriteRev );

    signal s_dispState   : dispState_t := idle;
    signal s_memState    : memState_t  := memIdle;
    signal s_mem_copy    : std_logic := '0';

    type   params_t is array(0 to 3) of integer range 0 to 255;

    signal s_params      : params_t;

    signal s_paramRdPos  : integer range 0 to 3;
    signal s_paramWrPos  : integer range 0 to 3;

    signal s_is_inverse  : std_logic := '0';
    signal s_is_bold     : std_logic := ANSI_DEFAULT_ATT.fore_col(3);

    signal s_inactive_cnt : integer range 0 to 59 := 0;
    signal s_prev_sync    : std_logic;
    signal s_reset_cnt    : std_logic;

begin

    cursor_act_cnt: process(I_clock, I_sync, s_reset_cnt)
    begin
        if rising_edge(I_clock) then
            if s_reset_cnt = '1' then
                s_inactive_cnt <= 59;
            elsif s_prev_sync /= I_sync and I_sync = '1' then
                if s_inactive_cnt > 0 then
                    s_inactive_cnt <= s_inactive_cnt - 1;
                end if;
            end if;
            s_prev_sync <= I_sync;
        end if;
    end process;

    -- Decode input stream and update the frame buffer based on results
    process_char: process(I_clock, I_reset_n, s_term_rdy, I_char_rdy)
        variable current_ch  : character;
        variable attrVal     : integer range 0 to 255;
        variable cursor      : screen_location_t;
        variable start_move  : screen_location_t;
        variable dest_move   : screen_location_t;
        variable end_move    : screen_location_t;
        variable start_fill  : screen_location_t;
        variable end_fill    : screen_location_t;
        variable scrollReq   : std_logic;
        variable v_posn      : integer range 0 to CHARS_PER_SCREEN;
        variable v_print     : std_logic;

    begin
    
        if I_reset_n = '0' then
            s_attr <= ANSI_DEFAULT_ATT;
            O_wren <= '0';

        -- Only run this section if memory operations are not in progress
        elsif rising_edge(I_clock) then

            case s_memState is

                when memIdle =>

                    O_wren <= '0';

                    if s_term_rdy = '0' then

                        s_reset_cnt <= '0';

                        case s_dispState is

                            when setScroll =>

                                -- Scroll the screen up one line
                                s_params(0) <= 1;
                                s_dispState <= scrollUp;

                            when fillMem =>

                                -- do this for the defined range
                                s_rd_addr    <= start_fill.Position;
                                s_wr_addr    <= start_fill.Position;
                                s_end_addr   <= end_fill.Position;

                                -- Set the character we want to write as space
                                s_write_data <= DisplayData(' ', s_attr, s_is_inverse);

                                -- Trigger this data to be written for the whole range
                                s_memState   <= memWrite;
                                s_dispState  <= idle;

                            when insertLine =>

                                -- prep to clear the cursor line
                                move_to(start_fill, 0, cursor.Vertical );
                                move_to(end_fill, HORIZ_CHAR_MAX, cursor.Vertical );

                                if cursor.Vertical = VERT_CHAR_MAX then                                    
                                    s_dispState <= fillMem;

                                else
                                    -- Move lines below down (reverse direction move)
                                    move_to(start_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX - 1 );
                                    move_to(end_move, 0, cursor.Vertical - 1 );
                                    move_to(dest_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                    s_dispState <= moveDown;

                                end if;

                            when deleteLine =>

                                -- after the scroll clear the last line
                                move_to(start_fill, 0, VERT_CHAR_MAX );
                                move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                if cursor.Vertical = VERT_CHAR_MAX then

                                    -- no move required, just clear the last line
                                    s_dispState <= fillMem;

                                else
                                    -- Move the lines below upwards
                                    move_to(start_move, 0, cursor.Vertical + 1 );
                                    move_to(end_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX );
                                    move_to(dest_move, 0, cursor.Vertical );

                                    -- set up and execute the move and fill 
                                    s_dispState <= moveUp;

                                end if;

                            when moveUp =>

                                -- Set up copy data
                                s_rd_addr   <= start_move.Position;
                                s_wr_addr   <= dest_move.Position;
                                s_end_addr  <= end_move.Position;

                                -- trigger the copy operation
                                s_memState  <= memCopy;

                                -- after the move, execute a fill to blank the required region 
                                s_dispState <= fillMem;

                            when moveDown =>

                                -- Set up copy data
                                s_rd_addr   <= start_move.Position;
                                s_wr_addr   <= dest_move.Position;
                                s_end_addr  <= end_move.Position;

                                -- trigger the copy operation
                                s_memState  <= memCopyRev;

                                -- after the move, execute a fill to blank the required region 
                                s_dispState <= fillMem;

                            when scrollUp =>

                                if s_params(0) >= VERT_CHAR_MAX then
                                    -- blank whole screen
                                    move_to(start_fill, 0, 0);
                                    move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                    s_dispState <= fillMem;

                                else
                                    -- Set up scroll
                                    move_to(start_move, 0, s_params(0) );
                                    move_to(end_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX );
                                    move_to(dest_move, 0, 0 );

                                    -- Set up blanking final line(s)
                                    move_to(start_fill, 0, VERT_CHAR_MAX - s_params(0) + 1);
                                    move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                    s_dispState <= moveUp;
                                end if;

                            when scrollDown =>
                                if s_params(0) >= VERT_CHAR_MAX then
                                    -- blank whole screen
                                    move_to(start_fill, 0, 0);
                                    move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                    s_dispState <= fillMem;

                                else
                                    -- Set up scroll
                                    move_to(start_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX - s_params(0));
                                    move_to(end_move, 0, s_params(0) );
                                    move_to(dest_move, HORIZ_CHAR_MAX, VERT_CHAR_MAX );

                                    -- Set up blanking starting line(s)
                                    move_to(start_fill, 0, 0);
                                    move_to(end_fill, HORIZ_CHAR_MAX, s_params(0) - 1 );

                                    s_dispState <= moveDown;
                                end if;

                            when applyAttr =>

                                attrVal := s_params(s_paramRdPos);

                                case attrVal is

                                    when 0 => -- Reset colours to default
                                        s_is_inverse <= '0';
                                        s_is_bold    <= ANSI_DEFAULT_ATT.fore_col(3);
                                        s_attr       <= ANSI_DEFAULT_ATT;

                                    when 1 => -- Set Bold flag
                                        s_is_bold          <= '1';
                                        s_attr.fore_col(3) <= '1';

                                    when 22 => -- Clear Bold flag
                                        s_is_bold          <= '0';
                                        s_attr.fore_col(3) <= '0';

                                    when 4 => -- Set underline flag
                                        s_attr.underln <= '1';

                                    when 24 => -- Clear underline flag
                                        s_attr.underln <= '0';

                                    when 5 => -- Set blinking flag
                                        s_attr.blink   <= '1';

                                    when 25 => -- Clear blinking flag
                                        s_attr.blink   <= '0';

                                    when 7 => -- Set Inverse flag
                                        s_is_inverse   <= '1';

                                    when 27 => -- Clear Inverse flag
                                        s_is_inverse   <= '0';

                                    when 30 to 37 => -- Foreground (with current Bold state)
                                        s_attr.fore_col <= s_is_bold & std_logic_vector(to_unsigned(attrVal - 30, 3));

                                    when 40 to 47 => -- Background (with current Bold state)
                                        s_attr.back_col <= s_is_bold & std_logic_vector(to_unsigned(attrVal - 40, 3));

                                    when 90 to 97 => -- Bold foreground
                                        s_attr.fore_col <= '1' & std_logic_vector(to_unsigned(attrVal - 90, 3));

                                    when 100 to 107 => -- Bold background
                                        s_attr.back_col <= '1' & std_logic_vector(to_unsigned(attrVal - 100, 3));

                                end case;

                                if s_paramRdPos = s_paramWrPos then
                                    s_dispState  <= idle;
                                    s_term_rdy   <= '1';

                                else
                                    s_paramRdPos <= s_paramRdPos + 1;

                                end if;

                        end case;

                    elsif I_char_rdy = '1' then

                        s_reset_cnt    <= '1';
                        current_ch     := I_char;
                        v_print        := '0';

                        case s_dispState is

                            when idle =>

                                case current_ch is

                                    when chr_code(7) => -- BEEP
                                        -- do nothing

                                    when chr_code(8) | chr_code(127) => -- Backspace or Delete

                                        backspace(cursor);
                                        s_cursorPos <= cursor.Position;

                                        move_to(start_fill, cursor.Horizontal, cursor.Vertical );
                                        move_to(end_fill, cursor.Horizontal, cursor.Vertical );

                                        s_dispState <= fillMem;
                                        s_term_rdy  <= '0';

                                    when chr_code(9) => -- TAB

                                        s_term_rdy <= '0';
                                        s_rd_addr  <= cursor.Position;
                                        s_wr_addr  <= cursor.Position;

                                        if cursor.Horizontal < HORIZ_CHAR_MAX - 7 then

                                            v_posn      := 8 - (cursor.Horizontal mod 8);
                                            move_to(cursor, cursor.Horizontal + v_posn, cursor.Vertical);
                                            s_end_addr  <= cursor.Position - 1;
                                            s_cursorPos <= cursor.Position;

                                            if s_end_addr < s_rd_addr then
                                                s_term_rdy <= '1';
                                            else
                                                s_write_data <= DisplayData(' ', s_attr, s_is_inverse);
                                                s_memState   <= memWrite;
                                            end if;

                                        else
                                            if cursor.Vertical < VERT_CHAR_MAX then

                                                move_to(cursor, 0, cursor.Vertical + 1);
                                                s_cursorPos <= cursor.Position;
                                                s_end_addr  <= cursor.Position - 1;

                                                if s_end_addr < s_rd_addr then
                                                    s_term_rdy <= '1';
                                                else
                                                    s_write_data <= DisplayData(' ', s_attr, s_is_inverse);
                                                    s_memState   <= memWrite;
                                                end if;

                                            else

                                                if cursor.Position /= CHARS_PER_SCREEN - 1 then
                                                    s_end_addr   <= CHARS_PER_SCREEN - 1;
                                                    s_write_data <= DisplayData(' ', s_attr, s_is_inverse);
                                                    s_memState   <= memWrite;
                                                end if;

                                                move_to(cursor, 0, VERT_CHAR_MAX);
                                                s_cursorPos <= cursor.Position;

                                                s_params(0) <= 1;
                                                s_dispState <= scrollUp;

                                            end if;
                                        end if;

                                    when chr_code(10) => -- Line Feed

                                        if cursor.Vertical < VERT_CHAR_MAX then

                                            move_down(cursor, 1);
                                            s_cursorPos <= cursor.Position;

                                        else

                                            s_params(0) <= 1;
                                            s_dispState <= scrollUp;
                                            s_term_rdy  <= '0';

                                        end if;

                                    when chr_code(12) => -- Form Feed
                                        -- home the cursor
                                        move_to(cursor, 0, 0);
                                        s_cursorPos <= cursor.Position;

                                        -- Fill the whole screen
                                        move_to(start_fill, 0, 0 );
                                        move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX);

                                        s_dispState <= fillMem;
                                        s_term_rdy  <= '0';

                                    when chr_code(13) => -- Carriage Return
                                        move_sol(cursor);
                                        s_cursorPos <= cursor.Position;

                                    when chr_code(27) => -- ESC - start of command?
                                        s_dispState <= checkCommand;

                                    when others => -- assume that the character is to be displayed
                                        v_print     := '1';

                                end case;

                            when checkCommand =>

                                case current_ch is
                                    when '[' =>
                                        -- Yes, this is a command, work out what it is
                                        s_paramRdPos <= 0;
                                        s_paramWrPos <= 0;
                                        s_dispState  <= csiMode;

                                    when others =>
                                        -- Not a command, so just output the char
                                        v_print     := '1';

                                end case;

                            when csiMode =>

                                case current_ch is
                                    when 'A' => -- ESC[A - Cursor up
                                        move_up(cursor, 1);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'B' => -- ESC[B - Cursor down
                                        move_down(cursor, 1);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'C' => -- ESC[C - Cursor forward
                                        move_right(cursor, 1);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'D' => -- ESC[D - Cursor backward
                                        move_left(cursor, 1);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'E' => -- ESC[E - Cursor next line. Move to start of next line
                                        move_to(cursor, 0, cursor.Vertical + 1);
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'F' => -- ESC[F - Cursor previous line. Move to start of previous
                                        move_to(cursor, 0, cursor.Vertical - 1);
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'G' => -- ESC[G - Cursor Horizontal Absolute
                                        move_to(cursor, 0, cursor.Vertical);
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'H' => -- ESC[H - move to home
                                        move_to(cursor, 0, 0);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'K' => -- ESC[K - erase EOL
                                        move_to(start_fill, cursor.Horizontal, cursor.Vertical);
                                        move_to(end_fill, HORIZ_CHAR_MAX, cursor.Vertical);
                                        s_dispState <= fillMem;
                                        s_term_rdy  <= '0';

                                    when 's' => -- ESC[s - save cursor pos
                                        s_saved_h   <= cursor.Horizontal;
                                        s_saved_v   <= cursor.Vertical; 
                                        s_dispState <= idle;

                                    when 'u' => -- ESC[u - restore cursor pos
                                        move_to(cursor, s_saved_h, s_saved_v);
                                        s_cursorPos <= cursor.Position;
                                        s_dispState <= idle;

                                    when 'J' => -- ESC[J - clear from cursor to end of screen
                                        move_to(start_fill, cursor.Horizontal, cursor.Vertical);
                                        move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX);
                                        s_dispState <= fillMem;
                                        s_term_rdy  <= '0';

                                    when 'L' => -- ESC[L - insert line
                                        s_term_rdy  <= '0';
                                        s_dispState <= insertLine;

                                    when 'M' => -- ESC[M - delete line
                                        s_term_rdy  <= '0';
                                        s_dispState <= deleteLine;

                                    when 'S' => -- ESC[S Scroll up one line 
                                        s_term_rdy  <= '0';
                                        s_params(0) <= 1;
                                        s_dispState <= scrollUp;

                                    when 'T' => -- ESC[T Scroll down one line 
                                        s_term_rdy  <= '0';
                                        s_params(0) <= 1;
                                        s_dispState <= scrollDown;
                                
                                    when '0' to '9' => -- numeric parameter
                                        s_params(s_paramWrPos) <= character'pos(current_ch) - character'pos('0');
                                        s_dispState            <= csiParameters;

                                    when ';' => -- blank numeric parameter (default 0)
                                        s_paramWrPos <= 1;
                                        s_dispState  <= csiParameters;

                                    when '?' => -- DEC extensions (used for cursor on and off)
                                        s_dispState <= decExtensions;

                                    when others => -- Anything not recognised should be printed
                                        v_print     := '1';

                                end case;

                            when csiParameters =>

                                case current_ch is
                                    when 'A' => -- ESC[{param1}A - Cursor up
                                        move_up(cursor, s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'B' => -- ESC[{param1}B - Cursor down
                                        move_down(cursor, s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'C' => -- ESC[{param1}C - Cursor forward
                                        move_right(cursor, s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'D' => -- ESC[{param1}D - Cursor backward
                                        move_left(cursor, s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'E' => -- ESC[{param1}E - Cursor next line. Move to start of line N lines down
                                        move_to(cursor, 0, cursor.Vertical + s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'F' => -- ESC[{param1}F - Cursor previous line. Move to start of line N lines up
                                        move_to(cursor, 0, cursor.Vertical - s_params(0));
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'G' => -- ESC[{param1}G - Cursor Horizontal Absolute. Move the cursor to column N
                                        move_to(cursor, s_params(0) - 1, cursor.Vertical);
                                        s_cursorPos  <= cursor.Position;
                                        s_dispState  <= idle;

                                    when 'H' => -- ESC[{param1};{param2}H - Move to
                                        if s_paramWrPos = 1 then
                                            move_to(cursor, s_params(1) - 1, s_params(0) - 1);
                                            s_cursorPos  <= cursor.Position;
                                        end if;
                                        s_dispState <= idle;

                                    when 'J' =>
                                        case s_params(0) is
                                            when 0 => -- ESC[0J - clear from cursor to end of screen
                                                move_to(start_fill, cursor.Horizontal, cursor.Vertical);
                                                move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX);

                                                s_dispState <= fillMem;
                                                s_term_rdy  <= '0';

                                            when 1 => -- ESC[1J - clear from start of screen to cursor
                                                if cursor.Horizontal /= 0 and cursor.Vertical /= 0 then
                                                    move_to(start_fill, 0, 0 );
                                                    move_to(end_fill, cursor.Horizontal, cursor.Vertical);
                                                    backspace(end_fill );
                                                    s_dispState <= fillMem;
                                                    s_term_rdy  <= '0';
                                                end if;

                                            when 2 => -- ESC[2J - clear whole screen
                                                move_to(start_fill, 0, 0 );
                                                move_to(end_fill, HORIZ_CHAR_MAX, VERT_CHAR_MAX);
                                                s_dispState <= fillMem;
                                                s_term_rdy  <= '0';

                                            when others =>
                                                v_print := '1';

                                        end case;

                                    when 'K' =>
                                        case s_params(0) is
                                            when 0 => -- ESC[0K - clear from cursor to end of line
                                                move_to(start_fill, cursor.Horizontal, cursor.Vertical);
                                                move_to(end_fill, HORIZ_CHAR_MAX, cursor.Vertical);
                                                s_dispState <= fillMem;
                                                s_term_rdy  <= '0';

                                            when 1 => -- ESC[1K - clear from start of line to cursor
                                                if cursor.Horizontal > 0 then
                                                    move_to(start_fill, 0, cursor.Vertical);
                                                    move_to(end_fill, cursor.Horizontal - 1, cursor.Vertical);
                                                    s_dispState <= fillMem;
                                                    s_term_rdy  <= '0';
                                                end if;

                                            when 2 => -- ESC[2K - clear line
                                                move_to(start_fill, 0, cursor.Vertical);
                                                move_to(end_fill, HORIZ_CHAR_MAX, cursor.Vertical);
                                                s_dispState <= fillMem;
                                                s_term_rdy  <= '0';

                                            when others =>
                                                v_print := '1';

                                        end case;

                                    when 'S' => -- ESC[{param1}S Scroll up n lines 

                                        s_term_rdy  <= '0';
                                        s_params(0) <= 1 when s_params(0) = 0 else s_params(0);
                                        s_dispState <= scrollUp;
                                        
                                    when 'T' => -- ESC[{param1}T Scroll down n lines 

                                        s_term_rdy  <= '0';
                                        s_params(0) <= 1 when s_params(0) = 0 else s_params(0);
                                        s_dispState <= scrollDown;

                                    when '?' =>
                                        s_dispState <= decExtensions;

                                    when 'm' => -- ESC[{param1}m or ESC[{param1};{param2}m- set graphics rendition                                
                                        s_dispState <= applyAttr;
                                        s_term_rdy  <= '0';

                                    when ';' => -- param separator char
                                        v_posn           := s_paramWrPos + 1;
                                        s_paramWrPos     <= v_posn;
                                        s_params(v_posn) <= 0;

                                    when '0' to '9' =>
                                        s_params(s_paramWrPos) <= s_params(s_paramWrPos) * 10 + val(current_ch);

                                    when others =>
                                        v_print     := '1';

                                end case;

                             when decExtensions =>

                                case current_ch is
                                    when '0' to '9' =>
                                        s_params(s_paramWrPos) <= s_params(s_paramWrPos) * 10 + val(current_ch);

                                    when 'l' => -- ESC[?xxl
                                        if s_params(s_paramRdPos) = 25 then -- disable cursor
                                            s_cursor_vis <= '0';
                                        end if;
                                        s_dispState <= idle;

                                    when 'h' => -- ESC[?xxh
                                        if s_params(s_paramRdPos) = 25 then -- enable cursor
                                            s_cursor_vis <= '1';
                                        end if;
                                        s_dispState <= idle;

                                    when others =>
                                        v_print     := '1';

                                end case;

                        end case;

                        if v_print = '1' then
                            -- Cause the visible character to be written at the cursor position

                            s_write_data <= DisplayData(current_ch, s_attr, s_is_inverse);
                            O_address    <= std_logic_vector(to_unsigned(cursor.Position, 11));
                            O_wren       <= '1';

                            -- Attempt to move the cursor the next position on the screen, trigger a scroll if past the end
                            move_next(cursor, scrollReq);
                            s_cursorPos  <= cursor.Position;

                            if scrollReq then
                                s_term_rdy  <= '0';
                                s_dispState <= setScroll;
                            else
                                s_dispState <= idle;
                            end if;

                        end if;
                    else
                        s_reset_cnt <= '0';

                    end if;

                when memCopy =>
                    s_mem_copy   <= '1';
                    O_address    <= std_logic_vector(to_unsigned(s_rd_addr, 11));
                    O_wren       <= '0';

                    s_memState   <= memWrite;

                when memWrite =>
                    O_address    <= std_logic_vector(to_unsigned(s_wr_addr, 11));
                    O_wren       <= '1';

                    if s_rd_addr = s_end_addr then
                        s_term_rdy <= '1' when s_dispState = idle else '0';
                        s_mem_copy <= '0';
                        s_memState <= memIdle;
                    else
                        s_rd_addr  <= s_rd_addr + 1;
                        s_wr_addr  <= s_wr_addr + 1;
                        s_memState <= memCopy when s_mem_copy else memWrite;
                    end if;

                when memCopyRev =>
                    s_mem_copy <= '1';
                    O_address  <= std_logic_vector(to_unsigned(s_rd_addr, 11));
                    O_wren     <= '0';
                    s_memState <= memWriteRev;

                when memWriteRev =>
                    O_address  <= std_logic_vector(to_unsigned(s_wr_addr, 11));
                    O_wren     <= '1';

                    if s_wr_addr = s_end_addr then
                        s_term_rdy <= '1' when s_dispState = idle else '0';
                        s_mem_copy <= '0';
                        s_memState <= memIdle;
                    else
                        s_rd_addr  <= s_rd_addr - 1;
                        s_wr_addr  <= s_wr_addr - 1;
                        s_memState <= memCopyRev;
                    end if;

            end case;

        end if;      
 
    end process;

    mem_copy: process(I_clock, I_data_in, s_mem_copy, s_write_data)
    begin
        O_data_out <= I_data_in when s_mem_copy = '1' else s_write_data;
    end process;

    sync_cursor: process(s_cursorPos, s_cursor_vis, s_inactive_cnt)
    begin
        O_cursor_pos <= s_cursorPos when s_cursor_vis = '1' and s_inactive_cnt = 0 else 2002;
    end process;

    set_term_rdy: process(s_term_rdy)
    begin
        O_term_rdy <= s_term_rdy;
    end process;

end architecture rtl;