LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL; -- addition of std_logic_vector

LIBRARY work;
USE work.bebichiken.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY sdram_test IS
    GENERIC (
        num_ports : INTEGER := 1

    );

    PORT (
        clk_25mhz : IN STD_LOGIC;

        led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        btn : IN STD_LOGIC_VECTOR(6 DOWNTO 0);

        sdram_a : OUT STD_LOGIC_VECTOR(12 DOWNTO 0);
        sdram_ba : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
        sdram_d : INOUT STD_LOGIC_VECTOR(15 DOWNTO 0);
        sdram_cke : OUT STD_LOGIC;
        sdram_csn : OUT STD_LOGIC;
        sdram_rasn : OUT STD_LOGIC;
        sdram_casn : OUT STD_LOGIC;
        sdram_wen : OUT STD_LOGIC;
        sdram_dqm : OUT STD_LOGIC_VECTOR(1 DOWNTO 0)
    );
END sdram_test;

ARCHITECTURE Behavioral OF sdram_test IS

    COMPONENT sdram_cache IS

        GENERIC (
            base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"D0000000";
            clk_freq : NATURAL := 25;
            CAS_LATENCY : NATURAL := 2; -- 2=below 133MHz, 3=above 133MHz

            -- timing values (in nanoseconds)
            --
            -- These values can be adjusted to match the exact timing of your SDRAM
            -- chip (refer to the datasheet).
            T_DESL : real := 100000.0; -- startup delay
            T_MRD : real := 12.0; -- mode register cycle time
            T_RC : real := 60.0; -- row cycle time
            T_RCD : real := 18.0; -- RAS to CAS delay
            T_RP : real := 18.0; -- precharge to activate delay
            T_WR : real := 12.0; -- write recovery time
            T_REFI : real := 7800.0; -- average refresh interval

            num_ports : INTEGER := 1

        );
        PORT (
            reset : IN STD_LOGIC;
            clk : IN STD_LOGIC;

            sdram_a : OUT STD_LOGIC_VECTOR(12 DOWNTO 0);
            sdram_ba : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
            sdram_dq : INOUT STD_LOGIC_VECTOR(15 DOWNTO 0);
            sdram_cke : OUT STD_LOGIC;
            sdram_cs_n : OUT STD_LOGIC;
            sdram_ras_n : OUT STD_LOGIC;
            sdram_cas_n : OUT STD_LOGIC;
            sdram_we_n : OUT STD_LOGIC;
            sdram_dqml : OUT STD_LOGIC;
            sdram_dqmh : OUT STD_LOGIC;

            mem_clk : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
            mem_we : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
            mem_re : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
            mem_addr : IN word_array_t(num_ports - 1 DOWNTO 0);
            mem_width : IN width_array_t(num_ports - 1 DOWNTO 0);
            mem_wdata : IN word_array_t(num_ports - 1 DOWNTO 0);
            mem_rdata : OUT word_array_t(num_ports - 1 DOWNTO 0);
            mem_rdy : OUT STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
            mem_wack : OUT STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0)
        );
    END COMPONENT;
    SIGNAL rst, state, n_state, reset_address, inc_address, inc_errors : STD_LOGIC;
    SIGNAL mem_clk, mem_we, mem_re, mem_rdy, mem_wack : STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
    SIGNAL mem_addr, mem_wdata, mem_rdata : word_array_t(num_ports - 1 DOWNTO 0);
    SIGNAL mem_width : width_array_t(num_ports - 1 DOWNTO 0);
    SIGNAL errors : STD_LOGIC_VECTOR(7 DOWNTO 0);
BEGIN

    mem_clk <= (OTHERS => clk_25mhz);
    mem_width <= (OTHERS => "10");
    rst <= NOT btn(0);
    sim : sdram_cache

    GENERIC MAP(
        num_ports => num_ports
    )

    PORT MAP(
        reset => rst, clk => clk_25mhz,
        mem_addr => mem_addr, mem_wdata => mem_wdata,
        mem_rdata => mem_rdata,
        mem_we => mem_we, mem_re => mem_re,
        mem_width => mem_width,
        mem_rdy => mem_rdy, mem_wack => mem_wack,
        mem_clk => mem_clk,

        sdram_a => sdram_a,
        sdram_ba => sdram_ba,
        sdram_dq => sdram_d,
        sdram_cke => sdram_cke,
        sdram_cs_n => sdram_csn,
        sdram_ras_n => sdram_rasn,
        sdram_cas_n => sdram_casn,
        sdram_we_n => sdram_wen,
        sdram_dqml => sdram_dqm(0),
        sdram_dqmh => sdram_dqm(1)

    );
    PROCESS (state, mem_wack, mem_rdy, mem_rdata, mem_addr)
    BEGIN
        n_state <= state;
        reset_address <= '0';
        inc_address <= '0';
        mem_we <= (OTHERS => '0');
        mem_re <= (OTHERS => '0');
        mem_wdata <= (OTHERS => (OTHERS => '0'));
        inc_errors <= '0';

        IF state = '0' THEN
            mem_we(0) <= '1';

            mem_wdata(0) <= mem_addr(0);

            IF (mem_wack(0) = '1') THEN
                IF (mem_addr(0) = X"D1FFFFFC") THEN
                    reset_address <= '1';
                    n_state <= '1';
                ELSE
                    inc_address <= mem_wack(0);
                END IF;
            END IF;
        ELSE
            mem_re(0) <= '1';

            IF (mem_rdy(0) = '1') THEN
                inc_address <= mem_rdy(0);
                IF (mem_rdata /= mem_addr) THEN
                    inc_errors <= '1';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (rst, clk_25mhz)
    BEGIN
        IF rst = '1' THEN
            mem_addr <= (OTHERS => X"D0000000");
            state <= '0';
            errors <= (OTHERS => '0');
        ELSIF rising_edge(clk_25mhz) THEN
            state <= n_state;
            IF reset_address = '1' THEN
                mem_addr(0) <= X"D0000000";
            ELSIF inc_address = '1' THEN
                mem_addr(0) <= mem_addr(0) + X"00000004";
            END IF;

            IF inc_errors = '1' THEN
                IF errors /= X"FF" THEN
                    errors <= errors + X"01";
                END IF;
            END IF;
        END IF;
    END PROCESS;

    led(3 DOWNTO 0) <= errors(3 DOWNTO 0);
    led(6) <= rst;
    led(7) <= state;

END Behavioral;