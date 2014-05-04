IRC chat
=======

**Kompilácia:**
1. spusti erlang vm

	./src > erl
	1> make:all().
	
2. spusti príkaz ./src/start.cmd. Výstupom by v základnej konzolby my mal vyzerať nasledovne:
	
	(initiator@erlang)1> ok   
	(initiator@erlang)2> ok   
	(initiator@erlang)3> ok   
	(initiator@erlang)4> ok   
	(initiator@erlang)5> ok   
	(initiator@erlang)6> ok   
	(initiator@erlang)7> ok   
	(initiator@erlang)8> ok   
	(initiator@erlang)9> ok   
	(initiator@erlang)10> ok   
	(initiator@erlang)11> ok   
	(initiator@erlang)12> PID   
	(initiator@erlang)13> PID
	(initiator@erlang)14> PID  
	(initiator@erlang)15> PID   
	(initiator@erlang)16> PID   
	(initiator@erlang)17> PID  
	(initiator@erlang)18> PID   
	(initiator@erlang)19> PID      
	(initiator@erlang)20> PID   
	(initiator@erlang)21> PID   
	(initiator@erlang)22>   
	
**Notes:**
* Niekedy môže nastať pri spustený chyba. Dôvodom môže byť, že spúšťanie nodov je príliš rýchle a niekedy erlang pri spusení virtuálnej mašiny zlihá. 
* Alternatívne môže nastať chyba pri spúšťaní niektorého komponentu. Zdrojom problému je, že erlang nestihol odsynchronizovať globálny menný priestor. Je to nedostatok spúšťacieho skriptu. Snaha riešiť prostredníctom timer:sleep().
* Pre pridanie nového klienta je nutné spustiť novú pomenovanú evm a pingnúť jeden z existujúcich uzlov.

**Príkazy:**   
* Vytvorenie klienta:   
	
	> client:start_link(). 				% spustenie klienta - jeden node jeden klient
	> client:login(LoginName).			% prihlásenie do systému
	> client:j2g(GroupName).			% pripojenie sa do skupiny
	> client:s2g(Message, GroupName).	% poslanie správy do skupiny
	> client:s2u(Message, UserName).	% poslanie správy inému klientovi

* Spustenie komponent:   
	
	> login_server:start().				% spustí login server (1N:1C)
	> lookup:start().					% spustí lookup server (1N:1C)
	> internal_lookup:start().			% spustí internal lookup server (1N:1C)
	> accesspointmanager:start_link().	% spustí APM (1N:1C); predpokladom je spustený lookup & login server
	> chatroommanager:start_link().		% spustí CHRM (1N:1C); predpokladom je spustený internal lookup
	
  ostatné príkazy:
	> net_adm:ping(Node)				% pripojí sa do siete prostredníctvom uzla Node. Kladná odpoveď = pong.