cl: gstlbl Gsb120 *add;

select * from GSTASS00F;      --assenze individuali 149 ------------------------------------------
select * from GSTASS00F where gsaidn=38;

update GSTASS00F set gsagrtidn=29 where gsaidn=38;
select * from gstgas00f order by gasgsaidn; ------------------------------------------------------

select * from gstgas00v;          -- giustificazione retribuzione --------------------------------
delete from gstass00f where gsaidn=0;

select * from gstgas00v where gsarsuidn=2; 

select * from grtang00f; --------------------------------------------------------------------------

select * from dbqang00f tpo where tpo.dbqtblnme='GRTANG00F' 
                       and tpo.dbqclnnme='GRTTPO';

select * from MSGANG00F where msgtxt like '%data inizio%';

--delete from GSTASS00F where gsaidn=43;

select * from cdrazn00f;

select GASUSRIMM, GASDTEIMM,                      
GASHHMIMM, GASUSRAGG, GASDTEAGG, 
GASHHMAGG, GASIDN, GASGSAIDN,    
GASAZNIDN, GASRSUIDN, GASDTEASS, 
GASHHMMIA, GASHHMMTA, GASMMASS,  
GASTPOASS, GSAUSRIMM, GSADTEIMM, 
GSAHHMIMM, GSAUSRAGG, GSADTEAGG, 
GSAHHMAGG, GSAIDN, GSAAZNIDN,    
GSARSUIDN, GSAGRTIDN, GSADTE,    
GSASTT, GSAFRQZ, GSADTIASS,      
GSADTTASS, GSASTTAUT, GSAIDNRSG, 
GSADTAAUT, GSAMTV, GSASTTAUR,    
GSAIDNRSR, GSADTAAUR                       

from gstgas00f                            
  join gstass00f on gsaaznidn=gasaznidn     
                and gsarsuidn=gasrsuidn
                and gsaidn=gasgsaidn;

select *
                    from gstgas00v 
                    join grtang00f 
                    on gsagrtidn=grtidn
                    join dbqang00f tpo on tpo.dbqtblnme='GRTANG00F' 
                       and tpo.dbqclnnme='GRTTPO'
                       and tpo.dbqvle=grttpo 
                    where gasgsaidn=40
                    fetch first 1 rows only;
                    
                    


select count(distinct gsagrtidn) 
from gstgas00v   
where gsaaznidn=1                    
and gasdteass>20230501
and gsarsuidn=23                     
and gsasttaur='5';

select * from grtang00f where grttpo='3' or grttpo='4' ;                  
                           
update GSTASS00F set gsasttaut=1, gsasttaur=1 where gsarsuidn=23 and gsaaznidn=1 and (gsaidn=40 or gsaidn=39);

update gstgas00f set gasmmass=60 where gasidn=148;

select  gsaidn,                                                          -- id 
        tpo.dbqdsc,                                                      -- descrizione(ferie, permessi, altre voci)
        ' ' as padding,                                                  -- vuoto. Deve essere compilato con la durata dell'assenza da codice
        min(gasdteass) as dteinz,                                        -- data inizio
        max(gasdteass)as dtefin,                                         -- data fine
        gsasttaut,                                                       -- stato autorizzazione
        sttaut.dbqdsc,                                                   -- descrizione autorizzazione
        coalesce(sttaur.dbqdsc, '')as sttautrpe,                         -- descrizione autorizzazione rep/ente
        sum(case when gastpoass='1' then 1 else 0 end)as ggass,         -- giorni di assenza
        sum(case when gastpoass='0' then gasmmass else 0 end) as mmass  -- minuti di assenza 
                                                                           
        from gstgas00v                                        
        join grtang00f on gsagrtidn=grtidn                    
        join dbqang00f tpo on tpo.dbqtblnme='GRTANG00F'     
                       and tpo.dbqclnnme='GRTTPO'            --tipo giustificazione retribuzione
                       and tpo.dbqvle=grttpo                   
        join dbqang00f sttaut on sttaut.dbqtblnme='GSTASS00F'      
                       and sttaut.dbqclnnme='GSASTTAUT'      --stato autorizzazione generale
                       and sttaut.dbqvle=gsasttaut             
   left join dbqang00f sttaur on sttaur.dbqtblnme='GSTASS00F'      
                       and sttaur.dbqclnnme='GSASTTAUR'      --stato autorizzazione responsabile rep/ente
                       and sttaur.dbqvle=gsasttaur             
                                                                   
        where gsaaznidn=1                                   --id azienda
                       and gsarsuidn=23                      --id risorsa
                       and gasdteass> 20150410               --data assenza
                       and gsasttaur='1'                     --stato autorizzazione(1-da autorizzare, 4-rifiutato, 5-autorizzato 9-non richiesto)     
                       and GrtGstPrm = '1'                   --visibilità getione richieste permesso (0-no 1-si)     
        group by gsaidn, tpo.dbqdsc,              
                  gsasttaut, gsasttaur, sttaut.dbqdsc,    
                  sttaur.dbqdsc                    
        order by dteinz                                  
        fetch first 10000 rows only               
        for read only ;
        
        
        
select grtidn, grtdsc, count(*)
    from GRTANG00F join GSTASS00F on gsagrtidn=grtidn where grtgstass=1 group by grtdsc,grtidn order by grtdsc ;

SELECT grtidn,
       grtdsc,
       grttpo
    FROM tblaut00f
         JOIN grtang00f
             ON grtidn = taukeyidn
    WHERE 1 = 1
          AND tauaznidn = 1
          AND tautblnme = 'GRTANG00F'
          AND grtnat = '1'
          AND grtgstprm = '1'
    ORDER BY grttpo,
             grtdsc
    FETCH FIRST 10000 ROWS ONLY FOR READ ONLY;
    
    
    
select tpo.dbqdsc, grtcod, grtdsc,grtidn
                    from grtang00f
                    join dbqang00f tpo on tpo.dbqtblnme='GRTANG00F'
                       and tpo.dbqclnnme='GRTTPO'
                       and tpo.dbqvle=grttpo
                    order by grtidn;
                    
                    
                    
select  coalesce(max(cnpdtacmp), 0)
                  from precns00f
                 where cnpstt='1';
                 
select  coalesce(max(cdrdtenmr), 0)
                  from cdrazn00f
                 where cdraznidn='1';
                 
           
                 
                 select gasdteass, gashhmmia, gashhmmta 
                        from gstgas00v 
                       where gasaznidn=1 
                         and gasgsaidn=39 
                         and gsafrqz='1 ' 
                    order by gasdteass, gashhmmia 
                      fetch first 2 rows only 
                      for read only;

select min(gasidn),Count(*)
                  from gstgas00f
                  where gasgsaidn=55
                    and gasrsuidn=23; 
                    
select grtcod, grtdsc, grttpo, GRTGSTPRM, GRTNAT  from GRTANG00F 
where  grtgstass = '1';

select * from rsuang00f where rsucGn like 'ZANETT%';

select count(distinct gsaidn)
                from gstgas00v join grtang00f on grtidn=gsagrtidn
                where gsaaznidn=1
                  and gasdteass>=int(current date)
                  and gsarsuidn=23
                  and GrtGstPrm = '1'
                  and gsasttaur='1';

select  coalesce(max(cnpdtacmp), 0)
                  from precns00f
                 where cnpaznidn=1
                   and cnpstt='1';
