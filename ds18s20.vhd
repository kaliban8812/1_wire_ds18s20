-- Filename     : ds18s20.vhd
-- Author       : Vladimir Lavrov
-- Date         : 16.04.2020
-- Annotation   : controller for ds18s20
-- Version      : 1.0
-- Mod.Data     : 28.05.2020
-- Note         : https://static.chipdip.ru/lib/073/DOC000073557.pdf
--                refs #1392
-- |DS18S20
-- |bit 15|bit 14|bit 13|bit 12|bit 11|bit 10|bit 9 |bit 8 |bit 7 |bit 6 |bit 5 |bit 4 |bit 3 |bit 2 |bit 1 |bit 0 |
-- |  S   |  S   |  S   |  S   |  S   |  S   |  S   |  S   | 2^6  | 2^5  | 2^4  | 2^3  | 2^2  | 2^1  | 2^0  | 2^-1 |
-- |DS18B20
-- |bit 15|bit 14|bit 13|bit 12|bit 11|bit 10|bit 9 |bit 8 |bit 7 |bit 6 |bit 5 |bit 4 |bit 3 |bit 2 |bit 1 |bit 0 |
-- |  S   |  S   |  S   |  S   |  S   |  2^6 |  2^5 |  2^4 | 2^3  | 2^2  | 2^1  | 2^0  | 2^-1 | 2^-2 | 2^-3 | 2^-4 |
------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ds18s20 is
    generic (
        TEMP_SENS_READ : integer := 9
    );
    port (
        clk        : in std_logic;
        rst        : in std_logic;

        -- rdy_stb    : out std_logic;
        data_wire  : inout std_logic;
        data       : out std_logic_vector (12 downto 0);
        error_bit  : out std_logic;

        test_name : in std_logic_vector(9 downto 0); -- 9 - run, 8 - r/w_n, 7-0 - addr;
        test_data : inout std_logic_vector(64 downto 0)
    );
end entity ds18s20;

architecture rtl of ds18s20 is
    -- clk  
    constant CLK_CONST : integer := 49;                -- 50MHz/(CLK_CONST+1)= 1 MHz (1 us) 
    signal clk_stb     : std_logic;                    -- one strob
    signal clk_cnt     : integer range 0 to CLK_CONST; -- counter for clk divider
    signal clk_en      : std_logic;
    -- mgr
    type mgr_fsm_statetype is (mgr_idle, mgr_reset, mgr_rom, mgr_funct, mgr_wait, mgr_recieve); -- fsm
    signal mgr_fsm          : mgr_fsm_statetype := mgr_idle;                                    -- fsm
    signal mgr_rdy_stb      : std_logic;
    signal mgr_stb_flag     : std_logic;      -- flag for stb
    signal mgr_status       : std_logic;      -- from act to mgr if '1' - good, '0' - error;
    signal mgr_sel_funct    : std_logic;      -- selector for change the funct command
    -- time constants (time without minus one)
    constant RESET_PULL     : integer := 479; -- 480 us min 
    constant RESET_WAIT_MIN : integer := 14;  -- 15 to 60 us
    constant RESET_WAIT_MAX : integer := 59;  -- 60 us
    constant RESET_PRE      : integer := 59;  -- 60 to 240 us
    constant WRITE_SLOT     : integer := 62;  -- 61 us min + 2 us recovery
    constant WRITE_PULL_0   : integer := 60;  -- 61 -- 60 us to 120 us
    constant WRITE_PULL_1   : integer := 1;   -- 2 us -- 1 to 15 us
    constant READ_PULL      : integer := 1;   -- 2 us -- 1 to inf us
    constant READ_SLOT      : integer := 61;  -- 60 us min
    constant READ_SAMPLE    : integer := 10;  -- 14 us sample
    -- 750000/CLK(1) = 750000 == WAIT_LRG_CNT * WAIT_SML_CNT * WAIT_BIT_CNT
    constant WAIT_LRG_CNT   : integer := 794; -- 
    constant WAIT_SML_CNT   : integer := 62;  -- max act_cnt_sml 
    constant WAIT_BIT_CNT   : integer := 15;  -- max act_cnt_bit
    -- action
    constant BIT_COMMAND    : integer := 7;
    constant BIT_DATA       : integer := 15;
    type act_fsm_statetype is (act_idle, act_reset, act_write, act_read, act_wait); -- fsm
    signal act_fsm     : act_fsm_statetype := act_idle;                             -- fsm
    signal act_run_stb : std_logic;
    type act_code_statetype is (act_code_reset, act_code_write, act_code_read, act_code_wait);
    signal act_code          : act_code_statetype := act_code_reset;
    signal act_command       : std_logic_vector(BIT_COMMAND downto 0);
    signal act_data_buf      : std_logic_vector(BIT_DATA downto 0);
    signal act_cnt_lrg       : integer range 0 to (RESET_PULL + RESET_PULL + 1); -- 959
    signal act_cnt_sml       : integer range 0 to WRITE_SLOT;                    -- 62
    signal act_cnt_bit       : integer range 0 to BIT_DATA;                      -- 16
    signal act_rst_flag      : std_logic;                                        -- dop signal for searching errors in the reset state
    signal act_rst_det       : std_logic;                                        -- dop signal for searching errors in the reset state
    signal act_write_pull    : integer range 0 to WRITE_PULL_0;                  -- choose write 0 or 1
    -- commands
    constant SKIP_ROM        : std_logic_vector(act_command'range) := 8x"CC";
    constant CONVERT_T       : std_logic_vector(act_command'range) := 8x"44";
    constant READ_SCRATCHPAD : std_logic_vector(act_command'range) := 8x"BE";
    -- status
    -- constant ERROR_RESET     : std_logic_vector(data'range)        := 13b"1_1101_1010_1000"; -- -600
    -- constant ERROR_WAIT      : std_logic_vector(data'range)        := 13b"1_1101_0100_0100"; -- -700
    -- constant ERROR_TEMP      : std_logic_vector(data'range)        := 13b"1_1100_1110_0000"; -- -800
    constant MULT_KOEFF      : signed(3 downto 0)                  := 4b"0101";
    signal act_sign_data     : signed(8 downto 0);
    constant INIT_VAL        : std_logic_vector(data'range)          := 13b"0_0000_1111_1010"; -- +25
    constant MAX_TEMP        : std_logic_vector(act_sign_data'range) := 9b"0_1010_1010"; -- ds18s20 = 9b"0_1010_1010" ; ds18b20 = 16x"7D0"
    constant MIN_TEMP        : std_logic_vector(act_sign_data'range) := 9b"1_1001_0010"; -- ds18s20 = 16x"F92" ; ds18b20 = 16x"C90"
    -- multiply

begin
    ACT_PROC : process (clk, rst)
    begin
        if rst = '1' then
            mgr_rdy_stb    <= '0';
            mgr_status     <= '0';
            data_wire      <= 'Z';
            clk_en         <= '0';
            act_fsm        <= act_idle;
            act_cnt_lrg    <= 0;
            act_cnt_sml    <= 0;
            act_cnt_bit    <= 0;
            act_data_buf   <= (others => '0');
            act_rst_flag   <= '0';
            act_rst_det    <= '0';
            act_write_pull <= 1;
        elsif rising_edge(clk) then
            case act_fsm is
                when act_idle =>
                    if act_run_stb = '1' then
                        if act_code = act_code_reset then
                            act_fsm <= act_reset;
                        elsif act_code = act_code_write then
                            act_fsm <= act_write;
                        elsif act_code = act_code_read then
                            act_fsm <= act_read;
                        else
                            act_fsm <= act_wait;
                        end if;
                        clk_en    <= '1';
                        data_wire <= '0';
                    else
                        mgr_rdy_stb <= '0';
                        clk_en      <= '0';
                        mgr_status  <= '0';
                        data_wire   <= 'Z';
                    end if;
                    act_data_buf <= (others => '0');

                when act_reset          =>
                    if clk_stb = '1' then
                        if act_cnt_lrg = (RESET_PULL + RESET_PULL + 1) then
                            mgr_rdy_stb <= '1';
                            act_fsm     <= act_idle;
                            act_cnt_lrg <= 0;
                            act_rst_det <= '0';
                        else
                            act_cnt_lrg <= act_cnt_lrg + 1;
                            if act_cnt_lrg >= (RESET_PULL + RESET_WAIT_MIN + 1) then
                                if (data_wire = '0') and (act_rst_det = '0') then
                                    act_rst_flag <= '1';
                                    act_rst_det  <= '1';
                                end if;
                            elsif act_cnt_lrg >= RESET_PULL then
                                data_wire <= 'Z';
                            else
                                data_wire <= '0';
                            end if;

                            if act_rst_flag = '1' then
                                if act_cnt_sml = RESET_PRE then
                                    mgr_status   <= '1';
                                    act_cnt_sml  <= 0;
                                    act_rst_flag <= '0';
                                else
                                    if data_wire = '0' then
                                        act_cnt_sml <= act_cnt_sml + 1;
                                    else
                                        act_rst_flag <= '0';
                                    end if;
                                end if;
                            end if;
                        end if;
                    end if;

                when act_write =>
                    if clk_stb = '1' then
                        if act_cnt_sml = WRITE_SLOT then
                            if act_cnt_bit = BIT_COMMAND then
                                data_wire   <= 'Z';
                                act_cnt_bit <= 0;
                                act_fsm     <= act_idle;
                                mgr_rdy_stb <= '1';
                            else
                                data_wire   <= '0';
                                act_cnt_bit <= act_cnt_bit + 1;
                            end if;
                            act_cnt_sml <= 0;
                        elsif act_cnt_sml >= act_write_pull then
                            data_wire   <= 'Z';
                            act_cnt_sml <= act_cnt_sml + 1;
                        else
                            data_wire   <= '0';
                            act_cnt_sml <= act_cnt_sml + 1;
                        end if;

                        if act_command(act_cnt_bit) = '1' then
                            act_write_pull <= WRITE_PULL_1;
                        else
                            act_write_pull <= WRITE_PULL_0;
                        end if;
                    end if;

                when act_read =>
                    if clk_stb = '1' then
                        if act_cnt_sml = READ_SLOT then
                            if act_cnt_bit = BIT_DATA then
                                act_fsm     <= act_idle;
                                act_cnt_bit <= 0;
                                mgr_rdy_stb <= '1';
                                -- act_data_buf <= act_data_buf and 16x"FFFF"; -- debug
                            else
                                act_cnt_bit <= act_cnt_bit + 1;
                                data_wire   <= '0';
                            end if;
                            act_cnt_sml <= 0;
                        else
                            if act_cnt_sml = READ_SAMPLE then
                                act_data_buf(act_cnt_bit) <= data_wire;
                            elsif act_cnt_sml >= READ_PULL then
                                data_wire <= 'Z';
                            else
                                data_wire <= '0';
                            end if;
                            act_cnt_sml <= act_cnt_sml + 1;
                        end if;
                    end if;

                when act_wait =>
                    if clk_stb = '1' then
                        if act_cnt_lrg = WAIT_LRG_CNT then
                            act_cnt_lrg <= 0;
                            act_fsm     <= act_idle;
                            mgr_rdy_stb <= '1';
                        else
                            if act_cnt_bit = WAIT_BIT_CNT then
                                act_cnt_bit <= 0;
                                act_cnt_lrg <= act_cnt_lrg + 1;
                            else
                                if act_cnt_sml = WAIT_SML_CNT then
                                    act_cnt_sml <= 0;
                                    if mgr_status = '1' then
                                        act_fsm     <= act_idle;
                                        mgr_rdy_stb <= '1';
                                        act_cnt_lrg <= 0;
                                        act_cnt_bit <= 0;
                                        data_wire   <= 'Z';
                                    else
                                        act_cnt_bit <= act_cnt_bit + 1;
                                        data_wire   <= '0';
                                    end if;
                                else
                                    if act_cnt_sml = READ_SAMPLE then
                                        if data_wire = '0' then
                                            mgr_status <= '1';
                                        end if;
                                    elsif act_cnt_sml >= READ_PULL then
                                        data_wire <= 'Z';
                                    else
                                        data_wire <= '0';
                                    end if;
                                    act_cnt_sml <= act_cnt_sml + 1;
                                end if;
                            end if;
                        end if;
                    end if;

                when others =>
                    act_fsm <= act_idle;
            end case;
        end if;
    end process ACT_PROC;

    MGR_PROC : process (clk, rst)
    begin
        if rst = '1' then
            mgr_fsm       <= mgr_idle;
            act_run_stb   <= '0';
            data          <= INIT_VAL;
            mgr_stb_flag  <= '0';
            act_command   <= (others => '0');
            mgr_sel_funct <= '0';
            error_bit     <= '0';
            -- rdy_stb       <= '0';
        elsif rising_edge(clk) then
            case mgr_fsm is
                when mgr_idle =>
                    mgr_fsm       <= mgr_reset;
                    -- rdy_stb       <= '0';
                    mgr_stb_flag  <= '0';
                    act_run_stb   <= '0';
                    act_command   <= (others => '0');
                    mgr_sel_funct <= '0';
                    error_bit     <= '0';

                when mgr_reset =>
                    if mgr_stb_flag = '0' then
                        mgr_stb_flag <= '1';
                        act_run_stb  <= '1';
                        act_code     <= act_code_reset;
                    else
                        act_run_stb <= '0';
                        if mgr_rdy_stb = '1' then
                            mgr_stb_flag <= '0';
                            if mgr_status = '1' then
                                mgr_fsm <= mgr_rom;
                            else
                                error_bit <= '1';
                                -- data    <= ERROR_RESET;
                                -- rdy_stb <= '1';
                                mgr_fsm   <= mgr_idle;
                            end if;
                        end if;
                    end if;

                when mgr_rom =>
                    if mgr_stb_flag = '0' then
                        mgr_stb_flag <= '1';
                        act_run_stb  <= '1';
                        act_code     <= act_code_write;
                        act_command  <= SKIP_ROM;
                    else
                        act_run_stb <= '0';
                        if mgr_rdy_stb = '1' then
                            mgr_stb_flag <= '0';
                            mgr_fsm      <= mgr_funct;
                        end if;
                    end if;

                when mgr_funct =>
                    if mgr_stb_flag = '0' then
                        mgr_stb_flag <= '1';
                        act_run_stb  <= '1';
                        act_code     <= act_code_write;
                        if mgr_sel_funct = '0' then
                            act_command <= CONVERT_T;
                        else
                            act_command <= READ_SCRATCHPAD;
                        end if;
                    else
                        act_run_stb <= '0';
                        if mgr_rdy_stb = '1' then
                            mgr_stb_flag <= '0';
                            if mgr_sel_funct = '0' then
                                mgr_fsm       <= mgr_wait;
                                mgr_sel_funct <= '1';
                            else
                                mgr_fsm       <= mgr_recieve;
                                mgr_sel_funct <= '0';
                            end if;
                        end if;
                    end if;

                when mgr_wait =>
                    if mgr_stb_flag = '0' then
                        mgr_stb_flag <= '1';
                        act_run_stb  <= '1';
                        act_code     <= act_code_wait;
                    else
                        act_run_stb <= '0';
                        if mgr_rdy_stb = '1' then
                            mgr_stb_flag <= '0';
                            if mgr_status = '1' then
                                mgr_fsm <= mgr_reset;
                            else
                                error_bit <= '1';
                                -- data    <= ERROR_WAIT;
                                -- rdy_stb <= '1';
                                mgr_fsm   <= mgr_idle;
                            end if;
                        end if;
                    end if;

                when mgr_recieve =>
                    if mgr_stb_flag = '0' then
                        mgr_stb_flag <= '1';
                        act_run_stb  <= '1';
                        act_code     <= act_code_read;
                    else
                        act_run_stb <= '0';
                        if mgr_rdy_stb = '1' then
                            mgr_stb_flag <= '0';
                            mgr_fsm      <= mgr_idle;
                            -- rdy_stb      <= '1';
                            if ((act_data_buf(8 downto 0) < MIN_TEMP) and (act_data_buf(8 downto 0) > MAX_TEMP)) then
                                -- data <= ERROR_TEMP;
                                error_bit <= '1';
                            else
                                act_sign_data <= signed(act_data_buf(8 downto 0)); -- ds18s20 = 8 downto 0; ds18b20 = 11 downto 3;
                                data          <= std_logic_vector (act_sign_data * MULT_KOEFF);
                            end if;
                        end if;
                    end if;

                when others =>
                    mgr_fsm <= mgr_idle;
            end case;
        end if;
    end process MGR_PROC;

    CLK_PROC : process (clk, rst)
    begin
        if rst = '1' then
            clk_cnt <= 0;
            clk_stb <= '0';
        elsif rising_edge(clk) then
            if clk_en = '1' then
                if clk_cnt = CLK_CONST then
                    clk_cnt <= 0;
                    clk_stb <= '1';
                else
                    clk_cnt <= clk_cnt + 1;
                    clk_stb <= '0';
                end if;
            else
                clk_cnt <= 0;
                clk_stb <= '0';
            end if;
        end if;
    end process CLK_PROC;

    TEST_PROC : process (clk)
    begin
        if rising_edge(clk) then
            if test_name(9) = '1' then
                if test_name(7 downto 0) = std_logic_vector(to_unsigned(TEMP_SENS_READ, 8)) then
                    if test_name(8) = '0' then
                        test_data(12 downto 0) <= data(12 downto 0);
                    end if;
                end if;
                test_data(64) <= '0';
            else
                test_data <= (others => 'Z');
            end if;
        end if;
    end process;

end architecture rtl;