
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- SubModule LD
-- Created   02/17/2016 9:41:04 PM
--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;
Use IEEE.Std_Logic_unsigned.all;
Use IEEE.numeric_std.all;

entity FPGA is port
   (
     RS_TX : out  STD_LOGIC;
     RS_RX : in STD_LOGIC;
     CLK_I    : in  STD_LOGIC;
     RESET    : in  STD_LOGIC;
     BTN_IN  : in  STD_LOGIC_VECTOR(4 downto 0);
     SW_IN  : in  STD_LOGIC_VECTOR(7 downto 0);
     LED_OUT : out STD_LOGIC_VECTOR(7 downto 0)
   );
end FPGA;

--------------------------------------------------------------------------------

architecture Behavioural of FPGA is

begin

serial: entity work.SERIAL_CALCULATOR
  generic map(
    F                     => 20000000,       -- czestotliwosc zegata w [Hz]
    BAUD_RATE             => 9600,           -- predkosc nadawania w [bodach]
    NUM_BITS              => 8,              -- liczba bitow slowa danych (5-8)
    PARITY_BITS           => 0,              -- liczba bitow parzystosci (0-1)
    STOP_BITS             => 1,              -- liczba bitow stopu (1-2)
    MAX_DIGITS            => 10             -- liczba cyfr dziesietnych
  )
  port map(
    R                    => RESET,     -- sygnal resetowania
    C                    => CLK_I,          -- zegar taktujacy
    RX                   => RS_RX,          -- odebrany sygnal szeregowy
    TX                   => RS_TX          -- odebrany sygnal szeregowy
  );

end Behavioural;

