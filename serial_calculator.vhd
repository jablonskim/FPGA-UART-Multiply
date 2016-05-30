library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_CALCULATOR is

    generic (
        F               :natural := 20_000_000;         -- czestotliwosc zegata w [Hz]
        BAUD_RATE       :natural := 9600;               -- predkosc nadawania w [bodach]
        NUM_BITS        :natural := 8;                  -- liczba bitow slowa danych (5-8)
        PARITY_BITS     :natural := 1;                  -- liczba bitow parzystosci (0-1)
        STOP_BITS       :natural := 2;                  -- liczba bitow stopu (1-2)
        MAX_DIGITS      :natural := 3                   -- liczba cyfr dziesietnych
    );
  
    port (
        R           :in  std_logic;                     -- sygnal resetowania
        C           :in  std_logic;                     -- zegar taktujacy
        RX          :in  std_logic;                     -- odbierany sygnal szeregowy
        TX          :out std_logic                      -- wysylany sygnal szeregowy
    );
    
end SERIAL_CALCULATOR;

architecture behavioural of SERIAL_CALCULATOR is

    signal      rx_byte         :std_logic_vector(NUM_BITS - 1 downto 0);                                   -- odebrane slowo danych
    signal      rx_ready        :std_logic;                                                                 -- flaga potwierdzenia odbioru
    signal      rx_error        :std_logic;                                                                 -- flaga wykrycia bledu w odbiorze
        
    signal      tx_byte         :std_logic_vector(NUM_BITS - 1 downto 0);                                   -- wysylane slowo danych
    signal      tx_send         :std_logic;                                                                 -- flaga zadania nadawania
    signal      tx_sending      :std_logic;                                                                 -- flaga potwierdzenia nadawania
        
    type        INSTRUCTION is (ARGUMENT1, ARGUMENT2, CALCULATING, START);                                  -- lista instrukcji pracy interpretera
    signal      state :INSTRUCTION;                                                                         -- rejestr maszyny stanow interpretera
        
    subtype     DIGIT   is natural range 0 to 9;                                                            -- typ cyfry dziesietnej
    type        NUMBER  is array(natural range <>) of DIGIT;                                                -- typ liczby dziesietnej zlozonej z cyfr
        
    type        SIGN    is (PLUS, MINUS);                                                                   -- typ znaku plus/minus
    
    signal      arg1        :NUMBER(MAX_DIGITS - 1 downto 0);                                               -- argument 1
    signal      arg2        :NUMBER(MAX_DIGITS - 1 downto 0);                                               -- argument 2
    
    signal      result      :NUMBER(2 * MAX_DIGITS - 1 downto 0);                                           -- wynik dzialania kalkulatora
    signal      result_sign :SIGN;                                                                          -- znak wyniku
    signal      num_digits  :natural range 0 to MAX_DIGITS;                                                 -- licznik cyfr argumentu

    type        CALCULATION_STATE is (COMPUTING, REVERSING, SENDING, SENDING_SIGN, WAITING, WAITING_SIGN);  -- lista instrukcji wyznaczania wyniku
    signal      calculator_state    :CALCULATION_STATE;                                                     -- rejestr maszyny stanow wyznaczania wyniku
    signal      carry_flag          :natural range 0 to 1;                                                  -- wartosc przeniesienia dodawania/odejmowania
    signal      is_prev_digit       :std_logic;                                                             -- flaga oznaczająca koniec wprowadzania znaków argumentu

    constant    ZERO_BYTE           :std_logic_vector(NUM_BITS - 1 downto 0) := (others => '0');            -- slowo z ustawiona wartoscia 0
  
begin

    srx: entity work.SERIAL_RX(behavioural)                     -- instancja odbirnika szeregowego 'SERIAL_RX'
        generic map(                                            -- mapowanie parametrow biezacych
            F               => F,                               -- czestotliwosc zegara w [Hz]
            BAUD_RATE       => BAUD_RATE,                       -- predkosc nadawania w [bodach]
            NUM_BITS        => NUM_BITS,                        -- liczba bitow slowa danych (5-8)
            PARITY_BITS     => PARITY_BITS,                     -- liczba bitow parzystosci (0-1)
            STOP_BITS       => STOP_BITS                        -- liczba bitow stopu (1-2)
        )
        port map(                                               -- mapowanie sygnalow do portow
            R               => R,                               -- sygnal resetowania
            C               => C,                               -- zegar taktujacy
            RX              => RX,                              -- odebrany sygnal szeregowy
            BYTE            => rx_byte,                         -- odebrane slowo danych
            READY           => rx_ready,                        -- flaga potwierdzenia odbioru
            ERROR           => rx_error                         -- flaga wykrycia bledu w odbiorze
        );

    stx: entity work.SERIAL_TX(behavioural)                     -- instancja nadajnika szeregowego 'SERIAL_TX'
        generic map(                                            -- mapowanie parametrow biezacych
            F               => F,                               -- czestotliwosc zegara w [Hz]
            BAUD_RATE       => BAUD_RATE,                       -- predkosc nadawania w [bodach]
            NUM_BITS        => NUM_BITS,                        -- liczba bitow slowa danych (5-8)
            PARITY_BITS     => PARITY_BITS,                     -- liczba bitow parzystosci (0-1)
            STOP_BITS       => STOP_BITS                        -- liczba bitow stopu (1-2)
        )
        port map(                                               -- mapowanie sygnalow do portow
            R               => R,                               -- sygnal resetowania
            C               => C,                               -- zegar taktujacy
            TX              => TX,                              -- nadawany sygnal szeregowy
            BYTE            => tx_byte,                         -- nadawane slowo danych
            SEND            => tx_send,                         -- flaga zadania nadawania
            SENDING         => tx_sending                       -- flaga potwierdzenia nadawania
        );

    process (R, C) is
    
        function char_code(c :character) return std_logic_vector is                 -- konwersja kodu znaku do rozmiaru slowa
        begin
            return(ZERO_BYTE + character'pos(c));                                   -- wyznaczenia i zwrocenie wartosci slowa
        end function;

        function calculate_digit_value(a :std_logic_vector) return natural is       -- konwersja kodu slowa zawierajacego cyfre na warosc
        begin
            if (a >= char_code('0') and a <= char_code('9')) then                   -- zbadanie czy kod slowa jest cyfra
                return(CONV_INTEGER(a) - character'pos('0'));                       -- wyznaczenia i zwrocenie wartosci cyfry
            else                                                                    -- slowo nie jest cyfra
                return(10);                                                         -- zwrocenie flagi bledu jako wartosci 10
            end if;
        end function;
        
        function reverse_sign(s :SIGN) return SIGN is                       -- odwracanie znaku
        begin
            if (s = PLUS) then
                return MINUS;
            else
                return PLUS;
            end if;
        end function;

        constant RECV_ERROR         :std_logic_vector := char_code('!');            -- slowo z kodem przypisanym do bledu odbioru
        constant INSTRUCTION_ERROR  :std_logic_vector := char_code('?');            -- slowo z kodem przypisanym do bledu instrukcji
        
        --variable digit_sum      :integer range -10 to 19;
        --variable tmp_result     :NUMBER(MAX_DIGITS - 1 downto 0);
        --variable sign           :OPERATOR;
        --variable tmp_num_args   :natural range 0 to MAX_ARGS;

    begin

        if (R = '0') then                                                           -- asynchroniczna inicjalizacja rejestrow

            tx_byte         <= (others => '0');
            tx_send         <= '0';
            state           <= ARGUMENT1;
            arg1            <= (others => 0);
            arg2            <= (others => 0);
            result          <= (others => 0);
            result_sign     <= PLUS;
            is_prev_digit   <= '0';
            num_digits      <= 0;
            carry_flag      <= 0;

        elsif (rising_edge(C)) then                                                 -- synchroniczna praca kalkulatora

            tx_send <= '0';                                                         -- defaultowe ustawienie flagi zadania nadawania

            if (rx_error = '1') then                                                -- obsluga bledu odbioru zgloszonego przez 'SERIAL_RX'
                tx_byte <= RECV_ERROR;                                              -- ustawienie slowa nadawania na RECV_ERROR
                tx_send <= '1';                                                     -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
            elsif (rx_ready = '1') then                                             -- obsluga potwierdzenia odbioru slowa przez 'SERIAL_RX'
                tx_byte <= rx_byte;                                                 -- ustawienie slowa nadawania na slowo odebrane (echo)
                tx_send <= '1';                                                     -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
                
                if (rx_byte = char_code(LF) or rx_byte = char_code(CR) or state = START) then               -- zbadanie zadania inicjalizacji
                    state           <= ARGUMENT1;                                                           -- poczatkowy stan pracy interpretera
                    arg1            <= (others => 0);
                    arg2            <= (others => 0);
                    result          <= (others => 0);                                                       -- wyzerowanie sumy argumentow
                    result_sign     <= PLUS;
                    is_prev_digit   <= '0';
                    num_digits      <= 0;                                                                   -- wyzerowanie licznika cyfr
                else                                                                                        -- interpretacja odebranego slowa
            
                    case state is                                                                           -- badanie aktualnego stanu maszyny interpretera

                        when ARGUMENT1 =>
                        
                            if (rx_byte = char_code('-') or rx_byte = char_code('+')) then
                                if (is_prev_digit = '1') then
                                    tx_byte <= INSTRUCTION_ERROR;
                                    state   <= START;
                                elsif (rx_byte = char_code('-')) then
                                    result_sign <= reverse_sign(result_sign);
                                end if;
                            elsif (rx_byte = char_code('*')) then
                                is_prev_digit <= '0';
                                num_digits <= 0;
                                state <= ARGUMENT2;
                            elsif (calculate_digit_value(rx_byte) /= 10) then
                                arg1(0) <= calculate_digit_value(rx_byte);
                                arg1(arg1'left downto 1) <= arg1(arg1'left - 1 downto 0);
                                
                                if (num_digits /= MAX_DIGITS) then
                                    num_digits <= num_digits + 1;
                                else
                                    tx_byte <= INSTRUCTION_ERROR;
                                    state   <= START;
                                end if;
                                
                                is_prev_digit <= '1';
                            else
                                tx_byte <= INSTRUCTION_ERROR;
                                state   <= START;
                            end if;
                            
                        when ARGUMENT2 =>
                        
                            if (rx_byte = char_code('-') or rx_byte = char_code('+')) then
                                if (is_prev_digit = '1') then
                                    tx_byte <= INSTRUCTION_ERROR;
                                    state   <= START;
                                elsif (rx_byte = char_code('-')) then
                                    result_sign <= reverse_sign(result_sign);
                                end if;
                            elsif (rx_byte = char_code('=')) then
                                is_prev_digit <= '0';
                                num_digits <= 0;
                                state <= CALCULATING;
                            elsif (calculate_digit_value(rx_byte) /= 10) then
                                arg2(0) <= calculate_digit_value(rx_byte);
                                arg2(arg1'left downto 1) <= arg2(arg2'left - 1 downto 0);
                                
                                if (num_digits /= MAX_DIGITS) then
                                    num_digits <= num_digits + 1;
                                else
                                    tx_byte <= INSTRUCTION_ERROR;
                                    state   <= START;
                                end if;
                                
                                is_prev_digit <= '1';
                            else
                                tx_byte <= INSTRUCTION_ERROR;
                                state   <= START;
                            end if;
                        
                        when others => null;

                    end case;
                end if;
            end if;

            if (state /= CALCULATING) then                                                              -- oczekiwanie na stan CALCULATING interpretera
                carry_flag          <= 0;                                                               -- wyzerowanie wartosci przeniesienia
                calculator_state    <= COMPUTING;                                                       -- ustawienie poczatkowe stanu COMPUTING
            else                                                                                        -- osiagnieto stan CALCULATING
                case calculator_state is
                        
                    when SENDING_SIGN =>                                                                -- wysylanie znaku jesli minus
                    
                        if (result_sign = MINUS) then
                            tx_byte             <= ZERO_BYTE + character'pos('-');
                            tx_send             <= '1';
                            calculator_state    <= WAITING;
                        else
                            calculator_state    <= SENDING;
                        end if;

                    when SENDING =>
                    
                        if (num_digits /= MAX_DIGITS * 2) then                                          -- badanie czy pozostaly cyfry do wyslania
                            num_digits <= num_digits + 1;                                               -- zwiekszenie o 1 liczby wyslanych cyfr
                            
                            if (carry_flag = 1 or result(MAX_DIGITS - 1) /= 0 or num_digits = MAX_DIGITS - 1) then   -- badanie czy nlezy wyslac cyfre 
                                tx_byte             <= ZERO_BYTE + character'pos('0') + result(MAX_DIGITS - 1);      -- wyznaczenie i ustawienie kodu wysylanej cyfry
                                tx_send             <= '1';                                             -- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
                                carry_flag          <= 1;                                               -- ustawienie flagi przeniesienia jako znacznika wysylania
                                calculator_state    <= WAITING;                                         -- przejscie do stanu WAITING
                            end if;
                        else                                                                            -- wykonano wyslanie wszystkich cyfr
                            calculator_state    <= COMPUTING;                                           -- przejscie do stanu COMPUTING
                            state               <= START;                                               -- przejscie do stanu START interpretera
                        end if;
                        
                        result(result'left downto 1) <= result(result'left - 1 downto 0);

                    when WAITING =>
                    
                        if (tx_send = '0' and tx_sending = '0') then                                    -- badanie czy 'SERIAL_TX' nie jest aktywny
                            calculator_state <= SENDING;                                                -- przejscie do stanu SENDING
                        end if;
                        
                    when WAITING_SIGN =>
                    
                        if (tx_send = '0' and tx_sending = '0') then                                    -- badanie czy 'SERIAL_TX' nie jest aktywny
                            calculator_state <= SENDING_SIGN;                                           -- przejscie do stanu SENDING_SIGN
                        end if;
                        
                    when others => null;

                end case;
                
            end if;
 
        end if;

    end process;
   
end behavioural;

