cl: gstlbl dms2_1acdy *add;

select cfnmgl,  cfrgs1, cfrgs2, cfindr, cfcap,
                         cflclt, cfprvn, cfnzne, cfpiva
                   from angcf00f;
                   
select cfcdcf, cfnmgl, cfwww,  cfrgs1, cfrgs2, cfindr, cfcap,
                         cflclt, cfprvn, cfnzne, cfpiva
                   from angcf00f
                   where cfwww<>'';
                                      

                   
select 1 
from   angcf00f 
where  cfnmgl =  'xxxxxx'
fetch first 1 rows only;

select flcdcf, flcdfl, flindr, flcap, fllclt, flprvn, flnzne, flntlf 
                      from angfl00f 
                      order by flcdcf;
                      
select flcdfl, flindr, flcap, fllclt, flprvn, flnzne, flntlf from angfl00f where 1=1 and flcdcf=8 order by fetch first 10000 rows only for read only;


select * from (
select a.*, concat(cfcdcf, concat('-', cfnmgl)) as cliente
from angcf00f as a);



select ctcclc,ctaaor, ctnror, ctdtor, ctrfrm, ctdtrc, ctevor
from oclgt00f
order by ctcclc,ctaaor,ctnror;

select *
                from   oclgt00f
                where  ctcclc=2;
                
select *
                from   qflag00f 
                where  qfprfs='OCLGT00F' and qfidnt='CTEVOR' and qfflag=2
                order by qfprfs;          
