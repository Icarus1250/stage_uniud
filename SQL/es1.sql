--commento riga

/*
 commento con tante riche
 */
 
-- Aggiungo elenco librerie al mio sistema
-- librerie contengono file indici programmi e altro
cl: gstlbl dms5_9qvs *add;
select * from angcf00f where cfwww<>'';

select cfcdcf, cfrgs1,cfidit, concat(cfcdcf, concat('-', cfnmgl)) as nome
from angcf00f
where cfidit<>'';

-- clienti con e senza email
select sum(case when cfidit='' then 1 else 0 end) as senza_email, sum(case when cfidit<>'' then 1 else 0 end) as con_email 
from angcf00f;

--sposto i risultati in riga
select categoria, count(*)from
(
select case when cfidit='' then 'senza_email' else 'con_email' end as categoria
from angcf00f
)as tabella -- obbligatorio dare nome alla tabella. ina query è una tabella utilizzabile da altri codici sql
group by categoria;

-- clienti italiani(iso=it), contare quante diverse provincie ci sono in anagrafica clienti

select count(provincie) as n_provincie
from(
    select cfprvn as provincie
    from angcf00f 
    where cfciso='IT'
    group by cfprvn
    )as tabella
;

--oppure

select count(distinct (cfprvn)) as n_provincie
    from angcf00f 
    where cfciso='IT';
    
    SELECT coalesce(CFDTAG, 0) AS CFDTAG_FLD,count(*) AS Conta , concat(cfcdcf, concat('-', cfnmgl)) as nome 
    from angcf00f 
    GROUP BY coalesce(CFDTAG, 0) 
    ORDER BY coalesce(CFDTAG, 0) 
    FOR READ ONLY ;
    
select cfcdcf, cfnmgl,  cfrgs1, cfrgs2, cfindr, cfcap,
                         cflclt, cfprvn, cfnzne, cfpiva, cfwww
                   from angcf00f
                   order by cfnmgl
                  
