IRC chat
=======

**Kompilácia:**
1. spusti erlang vm

	./src > erl
	1> make:all\.
	
2. spusti príkaz ./src/start.cmd. Výstupom by v základnej konzolby my mal vyzerať nasledovne:

	1> ok   
	2> ok   
	3> ok   
	4> ok   
	5> ok   
	6> ok   
	7> ok   
	8> ok   
	9> ok   
	10> ok   
	11> ok   
	12> PID   
	13> PID   
	14> PID  
	15> PID   
	16> PID   
	17> PID  
	18> PID   
	19> PID      
	20> PID   
	21> PID   
	22>   
	
**Notes:**
* Niekedy môže nastať pri spustený chyba. Dôvodom môže byť, že spúšťanie nodov je príliš rýchle a niekedy erlang pri spusení virtuálnej mašiny zlihá. 
* Alternatívne môže nastať chyba pri spúšťaní niektorého komponentu. Zdrojom problému je, že erlang nestihol odsynchronizovať globálny menný priestor. Je to nedostatok spúšťacieho skriptu. Snaha riešiť prostredníctom timer:sleep\.
* Pre pridanie nového klienta je nutné spustiť novú pomenovanú evm a pingnúť jeden z existujúcich uzlov.

**Príkazy:**   
* Vytvorenie klienta:   

	\> client:start_link\. 				% spustenie klienta - jeden node jeden klient   
	\> client:login(LoginName).			% prihlásenie do systému   
	\> client:j2g(GroupName).			% pripojenie sa do skupiny   
	\> client:s2g(Message, GroupName).	% poslanie správy do skupiny   
	\> client:s2u(Message, UserName).	% poslanie správy inému klientovi   

* Spustenie komponent:   

	\> login_server:start\.				% spustí login server (1N:1C)   
	\> lookup:start\.					% spustí lookup server (1N:1C)   
	\> internal_lookup:start\.			% spustí internal lookup server (1N:1C)   
	\> accesspointmanager:start_link\.	% spustí APM (1N:1C); predpokladom je    spustený lookup & login server   
	\> chatroommanager:start_link\.		% spustí CHRM (1N:1C); predpokladom je    spustený internal lookup   
	
* ostatné príkazy:   

	\> net_adm:ping(Node)				% pripojí sa do siete prostr   edníctvom uzla Node. Kladná odpoveď = pong.