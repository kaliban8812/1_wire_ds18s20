library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_TEXTIO.all;
use STD.TEXTIO.all;
use ieee.math_real.all;

entity tb_ds18s20 is
end entity tb_ds18s20;

architecture tb of tb_ds18s20 is

	component ds18s20
		port (
			clk       : in std_logic;
			rst       : in std_logic;
			run_stb   : in std_logic;
			rdy_stb   : out std_logic;
			data      : out std_logic_vector (15 downto 0);
			data_wire : inout std_logic
		);
	end component ds18s20;

	constant CLK_PER         : time      := 20 ns;
	constant CLK_HLF         : time      := CLK_PER/2;
	constant TIME_1_US       : time      := 1 us;

	signal clk               : std_logic := '0';
	signal rst               : std_logic;
	signal run_stb           : std_logic;
	signal rdy_stb           : std_logic;
	signal data              : std_logic_vector (15 downto 0);
	signal data_wire         : std_logic;
	-- time constant
	constant RESET_TIME_LOW  : time      := 480 us - CLK_HLF; -- 480 us
	constant RESET_TIME_WAIT : time      := 20 us;            -- 15 to 60 us;
	constant RESET_TIME_PULL : time      := 60 us;            -- 60 - 240 us;
	constant RECIEVE_INIT    : time      := 1 us - CLK_HLF;
	constant RECIEVE_SAMPLE  : time      := 15 us - CLK_HLF; -- 15 - 30 us;
	constant CONVERITNG      : time      := 50 ms;
	constant WRITE_SAMPLE    : time      := 1 us;
	constant WRITE_WAIT      : time      := 45 us;

	signal rst_dtct          : boolean   := false;
	signal ura               : std_logic := '0';

	type sens_fsm_statetype is (
		sens_idle,         -- recieve reset pulse
		sens_rst_response, -- sending the respose to fpga
		sens_rec_ini,      -- recieve 1 us initialization pulse               (1/4)
		sens_rec_samp,     -- put value to data signal, go to conv or sending (2/4)
		sens_rec_rec,      -- wait pull-up signal 'H'                         (3/4)
		sens_rec_compare,  -- compare recieved data                           (4/4)
		sens_converting,    -- recieve 1 us initialization pulse
		sens_send_rdy,     -- converting is over, sending '0'
		sens_write_ini,    -- write temperature to frpg ini by 1 us (1/3)
		sens_write_pull,   -- pull or now wire                      (2/3)
		sens_write_bit);   -- count data bit                        (3/3)

	signal sens_fsm          : sens_fsm_statetype := sens_idle;

	constant BIT_COMMAND     : integer            := 7;
	constant BIT_DATA        : integer            := 15;
	signal sens_command      : std_logic_vector(BIT_COMMAND downto 0);
	signal sens_cnt          : integer range 0 to BIT_DATA;
	signal sens_num_comm     : integer range 0 to 3;

	constant SKIP_ROM        : std_logic_vector(sens_command'range) := 8x"CC";
	constant CONVERT_T       : std_logic_vector(sens_command'range) := 8x"44";
	constant READ_SCRATCHPAD : std_logic_vector(sens_command'range) := 8x"BE";

	constant ERROR_RESET     : std_logic_vector(data'range)         := 16x"FFF0";
	constant ERROR_WAIT      : std_logic_vector(data'range)         := 16x"FF00";
	constant ERROR_TEMP      : std_logic_vector(data'range)         := 16x"F000";
	constant INIT_VAL        : std_logic_vector(data'range)         := 16x"00FF";

	signal sens_temp         : std_logic_vector(data'range)         := 16x"00FF";
	constant TEMP_MAX        : std_logic_vector(data'range)         := 16x"00AA";
	constant TEMP_MIN        : std_logic_vector(data'range)         := 16x"FF92";
	constant TEMP_TYP        : std_logic_vector(data'range)         := 16x"0032";
	constant TEMP_TABU       : std_logic_vector(data'range)         := 16x"0F0F";

	signal sens_reset_wait   : time;
	constant RESET_WAIT_TYP  : time := 35 us;
	constant RESET_WAIT_MIN  : time := 15 us;
	constant RESET_WAIT_MAX  : time := 60 us;
	constant RESET_WAIT_ERR  : time := 480 us;

	signal sens_reset_pull   : time;
	constant RESET_PULL_TYP  : time := 150 us;
	constant RESET_PULL_MIN  : time := 60 us;
	constant RESET_PULL_MAX  : time := 240 us;
	constant RESET_PULL_ERR  : time := 50 us;

	signal sens_read_sample  : time;
	constant READ_SAMPLE_TYP : time := 14 us; -- 15 us minus 1 us ini
	constant READ_SAMPLE_MIN : time := 29 us; -- 30 us minus 1 us ini
	constant READ_SAMPLE_MAX : time := 59 us; -- 60 us minus 1 us ini

	signal sens_convert      : time;
	constant CONVERTING_TYP  : time := 10 ms;
	constant CONVERTING_ERR  : time := 760 ms;

	signal sens_total_dur    : time;
	constant TOTAL_DUR_NORM  : time    := 50 ms;
	constant TOTAL_DUR_CONV  : time    := 780 ms;
	constant TOTAL_DUR_RES   : time    := 5 ms;

	-- type mode_statetype is (
	-- 	mode_time_typ,  -- use the typ time settings 0
	-- 	mode_time_min,  -- use the min time settings 1
	-- 	mode_time_max,  -- use the max time settings 2
	-- 	mode_t_min,     -- sending min temperature   3
	-- 	mode_t_max,     -- sending max temperature   4 
	-- 	mode_t_tabu,    -- sending tabu temperature  5 
	-- 	mode_reset_err, -- do not answer to reset pulse 6 
	-- 	mode_conv_err); -- do not answer within converting process 7
	-- signal mode_fsm : mode_statetype := mode_time_typ;
	signal mode_fsm          : integer := 0;
	signal sens_end_cycle    : boolean := false;

begin

	clk       <= not clk after CLK_PER / 2;
	data_wire <= 'H';

	TESTS_PROC : process
	begin
		if sens_end_cycle then
			case mode_fsm is

				when 0 => -- mode_time_typ =>
					report "**********************";
					report "[MODE] => TIME TYPICAL";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_time_min;

				when 1 => -- mode_time_min =>
					report "**********************";
					report "[MODE] => TIME MINIMAL";
					sens_reset_wait  <= RESET_WAIT_MIN;
					sens_reset_pull  <= RESET_PULL_MIN;
					sens_read_sample <= READ_SAMPLE_MIN;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_time_max;

				when 2 => -- mode_time_max =>
					report "**********************";
					report "[MODE] => TIME MAXIMAL";
					sens_reset_wait  <= RESET_WAIT_MAX;
					sens_reset_pull  <= RESET_PULL_MAX;
					sens_read_sample <= READ_SAMPLE_MAX;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_t_min;

				when 3 => -- mode_t_min =>
					report "**********************";
					report "[MODE] => TEMP MINIMAL";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_MIN;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_t_max;

				when 4 => -- mode_t_max =>
					report "**********************";
					report "[MODE] => TEMP MAXIMAL";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_MAX;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_t_tabu;

				when 5 => -- mode_t_tabu =>
					report "**********************";
					report "[MODE] => TEMP PROHIBITED";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TABU;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_reset_err;

				when 6 => -- mode_reset_err =>
					report "**********************";
					report "[MODE] => RESET WAIT ERROR";
					sens_reset_wait  <= RESET_WAIT_ERR;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_NORM;
					-- mode_fsm         <= mode_conv_err;

				when 7 => -- mode_reset_pull_err
					report "**********************";
					report "[MODE] => RESET PULL ERROR";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_ERR;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_TYP;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_NORM;

				when 8 => -- mode_conv_err =>
					report "**********************";
					report "[MODE] => CONVERTING ERROR";
					sens_reset_wait  <= RESET_WAIT_TYP;
					sens_reset_pull  <= RESET_PULL_TYP;
					sens_read_sample <= READ_SAMPLE_TYP;
					sens_convert     <= CONVERTING_ERR;
					sens_temp        <= TEMP_TYP;
					sens_total_dur   <= TOTAL_DUR_CONV;

				when 9 =>
					report "[INFO] : End simulation" severity failure;

				when others =>
					report "**********************";
					report "SIMULATION ERROR";
			end case;
		end if;

		wait for CLK_HLF;
	end process TESTS_PROC;

	RST_RUN_PROC : process
	begin
		sens_end_cycle <= true;
		wait for CLK_HLF;
		sens_end_cycle <= false;

		rst            <= '1';
		wait for TIME_1_US;
		rst <= '0';
		wait for TIME_1_US;

		run_stb <= '1';
		wait for CLK_PER;
		run_stb <= '0';
		wait for sens_total_dur;

		mode_fsm <= mode_fsm + 1;

	end process RST_RUN_PROC;

	SENSOR_PROC : process
	begin
		data_wire <= 'Z';

		case sens_fsm is

			when sens_idle =>
					if data_wire'stable(RESET_TIME_LOW) and (data_wire'delayed(RESET_TIME_LOW) = '0') then
						sens_fsm <= sens_rst_response;
						report "[INFO] : SENS RESET -> SUCCESS";
					end if;

			when sens_rst_response =>
				wait for sens_reset_wait;
				data_wire <= '0';
				wait for sens_reset_pull;
				data_wire <= 'Z';
				if (sens_reset_wait = RESET_WAIT_ERR) or (sens_reset_pull = RESET_PULL_ERR) then
					sens_fsm <= sens_idle;
				else
					sens_fsm <= sens_rec_ini;
				end if;

			when sens_rec_ini =>
				if data_wire'stable(RECIEVE_INIT) and (data_wire'delayed(RECIEVE_INIT) = '0') then
					wait for sens_read_sample;
					sens_fsm <= sens_rec_samp;
				end if;

			when sens_rec_samp =>
				if sens_cnt = BIT_COMMAND then
					sens_cnt <= 0;
					sens_fsm <= sens_rec_compare;
				else
					sens_cnt <= sens_cnt + 1;
					sens_fsm <= sens_rec_rec;
				end if;
				sens_command(sens_cnt) <= data_wire;

			when sens_rec_compare =>
				case sens_num_comm is
					when 0 =>
						if sens_command = SKIP_ROM then
							report "[INFO] : SKIP_ROM -> SUCCESS";
						else
							report "[INFO] : SKIP_ROM -> ERROR";
						end if;
						sens_num_comm <= sens_num_comm + 1;
						sens_fsm      <= sens_rec_rec;
						sens_command  <= (others => '0');
					when 1                   =>
						if sens_command = CONVERT_T then
							report "[INFO] : CONVERT_T -> SUCCESS";
						else
							report "[INFO] : CONVERT_T -> ERROR";
						end if;
						sens_num_comm <= sens_num_comm + 1;
						wait for sens_convert;
						sens_fsm     <= sens_converting;
						sens_command <= (others => '0');
					when 2                  =>
						if sens_command = SKIP_ROM then
							report "[INFO] : SKIP_ROM -> SUCCESS";
						else
							report "[INFO] : SKIP_ROM -> ERROR";
						end if;
						sens_num_comm <= sens_num_comm + 1;
						sens_fsm      <= sens_rec_rec;
						sens_command  <= (others => '0');
					when 3                   =>
						if sens_command = READ_SCRATCHPAD then
							report "[INFO] : READ_SCRATCHPAD -> SUCCESS";
						else
							report "[INFO] : READ_SCRATCHPAD -> ERROR";
						end if;
						sens_num_comm <= 0;
						sens_fsm      <= sens_write_ini;
						sens_command  <= (others => '0');
					when others              =>
						report "[INFO] : case_state -> ERROR";
						sens_command <= (others => '0');
						wait;
				end case;

			when sens_rec_rec =>
				if data_wire = 'H' then
					sens_fsm <= sens_rec_ini;
				end if;

			when sens_converting =>
				if data_wire'stable(RECIEVE_INIT) and (data_wire'delayed(RECIEVE_INIT) = '0') then
					sens_fsm <= sens_send_rdy;
				end if;

			when sens_send_rdy =>
				wait for WRITE_SAMPLE;
				data_wire <= '0';
				wait for WRITE_WAIT;
				data_wire <= 'Z';
				sens_fsm  <= sens_idle;

			when sens_write_ini =>
				if data_wire'stable(RECIEVE_INIT) and (data_wire'delayed(RECIEVE_INIT) = '0')then
					sens_fsm <= sens_write_pull;
				end if;

			when sens_write_pull =>
				if sens_temp(sens_cnt) = '0' then
					wait for WRITE_SAMPLE;
					data_wire <= '0';
					wait for WRITE_WAIT;
					data_wire <= 'Z';
					sens_fsm  <= sens_write_bit;
				else
					data_wire <= 'Z';
					wait for WRITE_WAIT;
					sens_fsm <= sens_write_bit;
				end if;

			when sens_write_bit =>
				if sens_cnt = BIT_DATA then
					sens_fsm <= sens_idle;
					sens_cnt <= 0;
				else
					sens_fsm <= sens_write_ini;
					sens_cnt <= sens_cnt + 1;
				end if;

			when others =>
				sens_fsm <= sens_idle;

		end case;

		wait for CLK_HLF;

	end process SENSOR_PROC;

	DATA_COLLECT_PROC : process (clk)
	begin
		if rising_edge (clk) then
			if rdy_stb = '1' then
				if data = ERROR_RESET then
					report "[INFO] : ERROR_RESET -> ERROR";
				elsif data = ERROR_WAIT then
					report "[INFO] : ERROR_WAIT -> ERROR";
				elsif data = ERROR_TEMP then
					report "[INFO] : ERROR_TEMP -> ERROR";
				elsif data = INIT_VAL then
					report "[INFO] : INIT_VAL -> ERROR";
				elsif data = sens_temp then
					report "[INFO] : TEMP_TEST -> SUCCESS";
				else
					report "[INFO] : OTHERS -> ERROR";
				end if;
			end if;
		end if;
	end process DATA_COLLECT_PROC;

	dut : ds18s20
	port map
	(
		clk       => clk,
		rst       => rst,
		run_stb   => run_stb,
		rdy_stb   => rdy_stb,
		data      => data,
		data_wire => data_wire
	);

end architecture tb;