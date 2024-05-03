cl: gstlbl dms5_9qvs *add;
--Ordini CLienti General Testata
select *from oclgt00f;

/*viste logiche (00L)
 oclgt00l anno-numero ordine chiave combinata
 
 oclgt01l cod cliente-anno-numero ordine chiave combinata
 */
 
 --num ordini degli ultimi 2 anni per clienti
 
 
 --num ordini degli ultimi 2 anni per nazione
 
 --cfnzne cfcdcf ctnror ctaaor ctcclc   count(*)as num_ordini
 ;
 select cfnzne ,
sum(case when ctaaor=year(current date) then 1 else 0 end) as num_ac, 
sum(case when ctaaor=year(current date-1 year) then 1 else 0 end) as "num_ac-1",
sum(case when ctaaor=year(current date-2 year) then 1 else 0 end) as "num_ac-2",
sum(case when ctaaor=year(current date-3 year) then 1 else 0 end) as "num_ac-3"
 from  oclgt00f join angcf00f on cfcdcf=ctcclc
 where ctaaor>=year(current date- 3 year)
 group by cfnzne
 order by cfnzne;
 
select * from sysibm/sysdummy1;

select *
 from  oclgt00f 
 where ctaaor=year(current date);
