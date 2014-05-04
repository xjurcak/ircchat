IRC chat
=======

kompilácia:
1. spusti erlang vm

	./src > erl
	1> make:all().
	
2. spusti príkaz ./src/start.cmd. Výstupom by v základnej konzolby my mal vyzerať nasledovne:

	(initiator@MiroslavV-PC)1> ok
	(initiator@MiroslavV-PC)2> ok
	(initiator@MiroslavV-PC)3> ok
	(initiator@MiroslavV-PC)4> ok
	(initiator@MiroslavV-PC)5> ok
	(initiator@MiroslavV-PC)6> ok
	(initiator@MiroslavV-PC)7> ok
	(initiator@MiroslavV-PC)8> ok
	(initiator@MiroslavV-PC)9> ok
	(initiator@MiroslavV-PC)10> ok
	(initiator@MiroslavV-PC)11> ok
	(initiator@MiroslavV-PC)12> <6886.114.0>
	(initiator@MiroslavV-PC)13> <6887.112.0>
	(initiator@MiroslavV-PC)14> <6888.116.0>
	(initiator@MiroslavV-PC)15> <6889.143.0>
	(initiator@MiroslavV-PC)16> <6890.116.0>
	(initiator@MiroslavV-PC)17> <6891.142.0>
	(initiator@MiroslavV-PC)18> <6892.96.0>
	(initiator@MiroslavV-PC)19> <6893.96.0>
	(initiator@MiroslavV-PC)20> <6894.100.0>
	(initiator@MiroslavV-PC)21> <6895.102.0>
	(initiator@MiroslavV-PC)22>
	
Notes:
* Niekedy môže nastať pri spustený chyba. Dôvodom môže byť, že spúšťanie nodov je príliš rýchle a niekedy erlang pri spusení virtálnej mašiny zlihá. 
* Alternatívne môže nastať chyba pri spúšťaní niektorého komponentu. Zdrojom problému, že erlang nestihol odsynchronizovať globálny menný priestor. Je to nedostatok spúšťacieho skriptu.
* Pre pridanie nového klienta je nutné spustíť novú pomenovanú evm a pingnúť jeden z existujúcich uzlov.

Príkazy:
 Vytvorenie klienta:
	
	> client:start_link(). 				% spustenie klienta - jeden node jeden klient
	> client:login(loginName).			% prihlásenie do systému
	> client:j2g(groupName).			% pripojenie sa do skupiny
	> client:s2g(Message, GroupName).	% poslanie správy do skupiny
	> client:s2u(Message, UserName).	% poslanie správy inému klientovi

 Spustenie komponent:
	
	> login_server:start().				% spustí login server (1N:1C)
	> lookup:start().					% spustí lookup server (1N:1C)
	> internal_lookup:start().			% spustí internal lookup server (1N:1C)
	> accesspointmanager:start_link().	% spustí APM (1N:1C); predpokladom je spustený lookup & login server
	> chatroommanager:start_link().		% spustí CHRM (1N:1C); predpokladom je spustený internal lookup
	
  ostatné príkazy:
	> net_adm:ping(Node)				% pripojí sa do siete prostredníctvom uzla Node. Kladná odpoveď = pong.