cl: gstlbl DMP2_1MTS1 *add;

select ctrvso from oclgt00f order by ctcclc;


select atctof, atccof from ofrat00f where ctdvso<>'' order by ctcclc;

select atcdfr, atevor, atdtrc, atdtpc from ofrat00f  order by atcdfr;

select *
                from   qflag00f 
                where  qfprfs='OFRAT00F'  order by qfprfs;
                
                select *
                from   qflag00f 
                where  qfprfs='OCLGT00F'  order by qfprfs;

select *
                from   tbbse00f
                where  tbprfs='COF' or tbprfs='COC';
                
                
 select adnrrg, adcdar, adnot1, addscr, adqtor, adprzo, addtrc, addtpc from ofrad00f where 1=1 and adaaor=2019 and adnror=1275 order by fetch first 10000 rows only for read only;

select * from oclgt00f;
select * from oclgd00f;
select * from ofrat00f;
select * from ofrad00f;

select* from tbbse00f;


select * from gstgas00v;
select * from grtang00f;
select * from dbqang00f order by dbqtblnme;



