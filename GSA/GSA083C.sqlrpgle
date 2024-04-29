     H*
     H* Delta System S.r.l. a Socio Unico - Via Palladio, 7 31020 San Fior TV
     H*_____________________________________________________________________________________________
     H* xx.xx.xxxx ---- UTN
     H*_____________________________________________________________________________________________
     H COPYRIGHT('Delta System S.r.l. a Socio Unico +
     H            - Via Palladio, 7 31020 San Fior (TV)')
     H DECEDIT('0,')       DATEDIT(*DMY.)       EXPROPTS(*RESDECPOS)
     H CVTOPT(*VARCHAR)

     H BNDDIR('FRMBND':'TLSBND':'ENGBND':'PLFBND')

     H/If defined(CRTBNDRPG)
     H    DFTACTGRP(*NO) ACTGRP(*caller)
     H/Endif
      *_____________________________________________________________________________________________
     Fgsa083v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut
     D/COPY QSBR,IncDfnGsa
     D/COPY QSBR,IncDfnGrt
     D/COPY QSBR,IncDfnDbq

      * Parametri dinamici (Qpr*)
     DQprIdnRsu        s              9s 0
     DQprIdnGrt        s              9s 0
     DQprIdnGsa        s              9s 0

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z

     DQvgHf0Prd        s             50a   varying
     DQvgHf0Dte1       s             50a   varying
     DQvgHf0Dte2       s             50a   varying

     DQvgDteCmp        s             11s 0
     DQvgDteUcl        s             11s 0
     DQvgFlgRcr        s              2a
      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(gsa083v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)
     DQdgGsa           ds                  likeds(QdtGsa)
     DQdgGas           ds                  likeds(QdtGas)
     DQdgGrt           ds                  likeds(QdtGrt)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,IncPrtGrt
     D/COPY QSBR,IncPrtGsa
     D/COPY QSBR,IncPrtDbq
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dgsa083c          PI
     D QprStr                              like(QvtPrmStf)
      *=============================================================================================

       // Main
       monitor;

          // Load
          if InitFrm() = true;
             lodfrm();
          endif;

          dou (%error);

             // Intercetta tasti uscita
             if g(QvgFrm:'hpract') = 'rtn' or g(QvgFrm:'hpract') = 'end';
                leave;
             endif;

             FrmSend('');
             CntFrm();

             // Intercetta tasti uscita
             if g(QvgFrm:'hpract') = 'rtn' or g(QvgFrm:'hpract') = 'end';
                leave;
             endif;

          enddo;

          // Valorizza Parametri Output
          //  flag ricarica
         QprStr = ap(QprStr:'qprflgupd':QvgFlgRcr:'');

       on-error;
          // error trapping
          jobtrperr(QvgFrm);
       endmon;

       // fine
       *inlr = *on;

      *=============================================================================================
      **********************************************************************************************
      * InitFrm: Init form
     PInitFrm          B
     DInitFrm          PI             1a
      *=============================================================================================

       // Memorizza momento
       QvgTmeStp = %timestamp();

       clear QdgFrm;

       // Rileva profilo di navigazione da container
       QdgPnv = FrmGetPnv(QvgFrm:QdgRtc);

       // Rileva Parametri Input
       QprIdnRsu = gpn(QprStr:'qpridnrsu');
       QprIdnGrt = gpn(QprStr:'qpridngrt');
       QprIdnGsa = gpn(QprStr:'qpridngsa');

       // Forza abbandono del programma ed evita caricamento dati
       //if QvgQualcosa = 'errore';
       //   ...
       //   s(QvgFrm:'hpract':'rtn');
       //   return false;
       //endif;

       RtvTag();

       //data chiusura consuntivo
       exec sql select  coalesce(max(cnpdtacmp), 0)
                  into :QvgDteCmp
                  from precns00f
                 where cnpaznidn=:QdgPnv.idnazn
                   and cnpstt='1';

       //ultima data calendario
       exec sql select  coalesce(max(cdrdtenmr), 0)
                  into :QvgDteUcl
                  from cdrazn00f
                 where cdraznidn=:QdgPnv.idnazn;

       return true;

      *=============================================================================================
     PInitFrm          E
      **********************************************************************************************
      **********************************************************************************************
      * RtvTag: Rileva tag
     PRtvTag           B
     DRtvTag           PI

      *=============================================================================================

       //ottieni riferimento alla riga di hf0dtiass
       QvgHf0Prd = GetPrn(QvgFrm:'hf0dtiass':'xtag="tr"');

       //ottieni riferimento alla riga di hf0dteass
       QvgHf0Dte1 = GetPrn(QvgFrm:'hf0dteass1':'xtag="tr"');

       //ottieni riferimento alla riga di hf0dteass1
       QvgHf0Dte2 = GetPrn(QvgFrm:'hf0dteass2':'xtag="tr"');

      *=============================================================================================
     PRtvTag           E
      **********************************************************************************************
      **********************************************************************************************
      * LodFrm: Load form
     PLodFrm           B
     DLodFrm           PI
      *=============================================================================================

       // Controllo allocaggi
       if QprIdnGsa<>0;
         QvgLck=joblck('record':'GSTASS00F':%char(QprIdnGsa):QsdNmPrgr:'lock');
         if QvgLck = 'false';
           s(QvgFrm:'hpract':'rtn');
           return;
         endif;
       endif;

       // Imposta controllo
       s(QvgFrm:'hpract':'cnt');

       // Load hf0
       lodhf0();

      *=============================================================================================
     PLodFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * CntFrm: Control form
     PCntFrm           B
     DCntFrm           PI
      *=============================================================================================

       // Intercetta tasti uscita
       if g(QvgFrm:'hhdendbtn') = '*on' or g(QvgFrm:'hhdrtnbtn') = '*on';
          FrmEnd();
          return;
       endif;

       // Controllo hf0
       cnthf0();

       // Salva
       if QdgFrm.HF0FOOTSAV='*on' and
          isErr(QvgFrm:'hf0')=*off ;
          QvgFlgRcr='si';
          FrmGo();
          return;
       endif;

      *=============================================================================================
     PCntFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf0:
     PLodhf0           B
     DLodhf0           PI
     D
      * Strutture dati locali
     DQdlAdsRcd        ds                  qualified inz
     D dteass                              like(QdtGas.dteass     )
     D hhmmia                              like(QdtGas.hhmmia     )
     D hhmmta                              like(QdtGas.hhmmta     )

      * Variabili locali
     D QvlStrSql       s          10000a                                        Form
     D QvlCount        s              9s 0                                      Count
      *=============================================================================================

       // Inz. var.
       QdgFrm.HF0DTE= %dec(%date());
       QdgFrm.hf0dteass1=0;
       QdgFrm.hf0hhmmia1=0;
       QdgFrm.hf0hhmmta1=0;
       QdgFrm.hf0dteass2=0;
       QdgFrm.hf0hhmmia2=0;
       QdgFrm.hf0hhmmta2=0;

       // Lettura Giustificativo Assenze Individuali
       if QprIdnGsa<>0;
         QdgGsa=GsaGet(QprIdnGsa);
         QdgFrm.HF0DTE=QdgGsa.DTE;
         QdgFrm.HF0MTV=QdgGsa.MTV;
         QdgFrm.HF0DTIASS=QdgGsa.DTIASS;
         QdgFrm.HF0DTTASS=QdgGsa.DTTASS;

         // Costruisci interrogazione per ore di assenza
         QvlStrSql = 'select gasdteass, gashhmmia, gashhmmta +
                        from gstgas00v +
                       where gasaznidn=' +%char(QdgPnv.idnazn)+ ' +
                         and gasgsaidn=' +%char(QprIdnGsa)+ ' +
                         and gsafrqz=''1 '' +
                    order by gasdteass, gashhmmia +
                      fetch first 2 rows only +
                      for read only +
                      ';

         // Esegui interrogazione database
         exec sql prepare ads_prp from :QvlStrSql;
         exec sql declare ads_crs cursor for ads_prp;
         exec sql open ads_crs;

         // Inz conteggio e first record
         QvlCount = 0;

         // Ciclo di caricamento
         dou (%error);

            exec sql fetch next from ads_crs
                     into :QdlAdsRcd;

            if sqlstt <>'00000';
               leave;
            endif;

            QvlCount += 1;

            // Val. var.
            if QvlCount=1;
               QdgFrm.hf0dteass1=QdlAdsRcd.dteass;
               QdgFrm.hf0hhmmia1=QdlAdsRcd.hhmmia;
               QdgFrm.hf0hhmmta1=QdlAdsRcd.hhmmta;
            elseif QvlCount=2;
               QdgFrm.hf0dteass2=QdlAdsRcd.dteass;
               QdgFrm.hf0hhmmia2=QdlAdsRcd.hhmmia;
               QdgFrm.hf0hhmmta2=QdlAdsRcd.hhmmta;
            endif;
         enddo;
         exec sql close ads_crs;
       endif;

       // Decodifica Giustificativi Retribuzione
       if QprIdnGsa<>0;
         QdgGrt=GrtGet(QdgGsa.grtidn:'');
       else;
         QdgGrt=GrtGet(QprIdnGrt:'');
       endif;
       QdgFrm.HF0COD=QdgGrt.COD;
       QdgFrm.HF0DSC=QdgGrt.DSC;

       if QprIdnGsa>0;
         QdgGsa=GsaGet(QprIdnGsa);
         if QdgGsa.STTAUT='5';

           //se autorizzati,imposta campi finestra in read only
           setatr(QvgFrm:'hf0mtv':'readonly':'readonly');
           setatr(QvgFrm:'hf0dtiass':'readonly':'readonly');
           setatr(QvgFrm:'hf0dttass':'readonly':'readonly');
           setatr(QvgFrm:'hf0dteass1':'readonly':'readonly');
           setatr(QvgFrm:'hf0hhmmia1':'readonly':'readonly');
           setatr(QvgFrm:'hf0hhmmta1':'readonly':'readonly');
           setatr(QvgFrm:'hf0dteass2':'readonly':'readonly');
           setatr(QvgFrm:'hf0hhmmia2':'readonly':'readonly');
           setatr(QvgFrm:'hf0hhmmta2':'readonly':'readonly');

           //se seconda riga vuota viene nascosta
           if QdgFrm.hf0dteass2=0;
             addatr(QvgFrm:QvgHf0Dte2:'class':'hidden');
           endif;

           addatr(QvgFrm:'hf0footrmv':'class':'hidden');
           addatr(QvgFrm:'hf0footsav':'class':'hidden');
         endif;
       else;
           //se nuovo nascondi pulsante "rimuovi"
           addatr(QvgFrm:'hf0footrmv':'class':'hidden');
       endif;


       // se ferie nasconde la selezione delle ore
       if QdgGrt.Tpo='3';
         addatr(QvgFrm:QvgHf0Dte1:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dte2:'class':'hidden');

       // se permesso nasconde la selezione del periodo
       else;
         addatr(QvgFrm:QvgHf0Prd :'class':'hidden');

       endif;

      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================

      // Elimina
       if QdgFrm.HF0FOOTRMV ='*on';
         QdgRtc=GsaDlt(QprIdnGsa:'msg');
          if QdgRtc.exc='true';
            QvgFlgRcr='si';
            FrmEnd();
            return;
          endif;
       endif;

       // ferie
       if QdgGrt.Tpo='3';

         // data inizio
           //obbligatoria
           if QdgFrm.HF0DTIASS = 0 ;
              seterr(QvgFrm:'hf0dtiass':'GEAGEN000');
              return;
           endif;

         // data fine
           // obbligatoria
           if QdgFrm.HF0DTTASS = 0 ;
              seterr(QvgFrm:'hf0dttass':'GEAGEN000');
              return;
           endif;

       //  data supera data chiusura consuntivo
           if QdgFrm.HF0DTTASS < QvgDteCmp;
              seterr(QvgFrm:'hf0dttass':'':'supera data chiusura consuntivo');
              return;
           endif;

           // supera ultimo giorno del calendario
           if QdgFrm.HF0DTTASS >= QvgDteUcl;
              seterr(QvgFrm:'hf0dttass':'GEAGSA009');
              return;
           endif;

           // minore di data inizio
           if QdgFrm.HF0DTTASS < QdgFrm.HF0DTIASS;
              seterr(QvgFrm:'hf0dttass':'GEAESR002');
              seterr(QvgFrm:'hf0dtiass':'GEAESR002');
              return;
           endif;

       endif;

       // Permessi
       if QdgGrt.Tpo='4';

         //Motivo obbligatorio
         if QdgFrm.HF0MTV = '';
            seterr(QvgFrm:'hf0mtv':'GEAGEN000');
            return;
         endif;

         // data obbligatoria

         if QdgFrm.hf0dteass1 = 0 ;
            seterr(QvgFrm:'hf0dteass1':'GEAGEN000');
            return;
         endif;

         //data supera data chiusura consuntivo
         if QdgFrm.hf0dteass1 < QvgDteCmp;
            seterr(QvgFrm:'hf0dteass1':'':'supera data chiusura consuntivo');
            return;
         endif;

         //data supera data chiusura consuntivo
         if QdgFrm.hf0dteass2 < QvgDteCmp and QdgFrm.hf0dteass2<>0;
            seterr(QvgFrm:'hf0dteass2':'':'supera data chiusura consuntivo');
            return;
         endif;

         //data supera ultimo giorno del calendario
         if QdgFrm.hf0dteass1 >= QvgDteUcl;
            seterr(QvgFrm:'hf0dteass1':'GEAGSA009');
            return;
         endif;

         //data presente ma ora assente
         if QdgFrm.hf0dteass2<>0 and QdgFrm.hf0hhmmia2=0;
            seterr(QvgFrm:'hf0hhmmia2':'GEAGEN000');
            return;
         endif;

         //data presente ma ora assente
         if QdgFrm.hf0dteass2<>0 and QdgFrm.hf0hhmmta2=0;
            seterr(QvgFrm:'hf0hhmmta2':'GEAGEN000');
            return;
         endif;

         //ora presente ma data assente
         if QdgFrm.hf0dteass2=0 and
            (QdgFrm.hf0hhmmia2<>0 or QdgFrm.hf0hhmmta2<>0);
            QdgFrm.HF0DTEASS2=QdgFrm.HF0DTEASS1;
            //seterr(QvgFrm:'hf0dteass2':'':'Data inserita automaticamente');
            return;
         endif;

         // ora inizio obbligatoria
         if QdgFrm.hf0hhmmia1=0;
            seterr(QvgFrm:'hf0hhmmia1':'GEAGEN000');
            return;
         endif;

         // ora inizio 1 supera o è uguale a ora fine 1
         if QdgFrm.hf0hhmmia1>=QdgFrm.hf0hhmmta1;
            seterr(QvgFrm:'hf0hhmmia1':'GEATME001');
            seterr(QvgFrm:'hf0hhmmta1':'GEATME001');
            return;
         endif;

         // ora inizio 2 supera o è uguale a ora fine 2
         if QdgFrm.hf0hhmmia2<>0 and QdgFrm.hf0hhmmia2>=QdgFrm.hf0hhmmta2;
            seterr(QvgFrm:'hf0hhmmia2':'GEATME001');
            seterr(QvgFrm:'hf0hhmmta2':'GEATME001');
            return;
         endif;
       endif;

      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * FrmGo:
     PFrmGo            B
     DFrmGo            PI
      *=============================================================================================
      // Aggiornamento giorni assenza
       if QprIdnGsa = 0;
         clear QdgGsa;
         QdgGsa.aznidn=QdgPnv.idnazn;
         QdgGsa.rsuidn=QprIdnRsu;
         QdgGsa.grtidn=QprIdnGrt;
         QdgGsa.sttaut='1';
       else;
         QdgGsa=GsaGet(QprIdnGsa);
       endif;

       QdgGsa.mtv=Qdgfrm.hf0mtv;
       QdgGsa.dte=Qdgfrm.hf0dte;
       QdgGsa.stt='0';
       QdgGsa.Arc='0';

       // Frequenza
       if Qdgfrm.hf0dtiass>0;
          QdgGsa.frqz='0';
          QdgGsa.DtiAss=Qdgfrm.hf0dtiass;
          QdgGsa.DttAss=Qdgfrm.hf0dttass;
       else;
          QdgGsa.frqz='1';
          QdgGsa.DtiAss=0;
          QdgGsa.DttAss=0;
       endif;

       QdgGsa=GsaSet(QdgGsa:QdgRtc);

       // Aggiornamento ore assenza
       // elimina eventuali record già caricati
       exec sql delete gstgas00f
                 where gasgsaidn=:QprIdnGsa;

       if QdgGsa.frqz='0';
          lodcnt();
       else;
          loddsc();
       endif;

       FrmEnd();

      *=============================================================================================
     PFrmGo            E
      **********************************************************************************************
      **********************************************************************************************
      * FrmEnd:
     PFrmEnd           B
     DFrmEnd           PI
      *=============================================================================================

       // Libera allocaggi
       if QprIdnGsa<>0;
         if QvgLck =  'true';
            joblck('record':'GSTASS00F':%char(QprIdnGsa):QsdNmPrgr:'unlock');
         endif;
       endif;

       // Fine lavoro
       s(QvgFrm:'hpract':'rtn');

      *=============================================================================================
     PFrmEnd           E
      **********************************************************************************************
      **********************************************************************************************
      * LodCnt : Carica Giornate Assenza (continuative)
     PLodCnt           B
     DLodCnt           PI

      * Variabili locali
     D QvlStrSql       s          10000a   varying                              Form
     D QvlCount        s              9s 0                                      Count
     D QvlDteNmr       s                   like(QdtGas.dteass) inz

      *=============================================================================================

       // Costruisci interrogazione
       QvlStrSql = 'select cdrdtenmr +
                    from cdrazn00f +
                    where cdraznidn=' +%char(QdgPnv.idnazn);

       if QdgFrm.hf0dttass>0;
          QvlStrSql += ' +
                     and cdrdtenmr>= +
                         ' +%char(QdgFrm.hf0dtiass)+ ' +
                     and cdrdtenmr<= +
                         ' +%char(QdgFrm.hf0dttass);
       else;
          QvlStrSql += ' +
                     and cdrdtenmr= +
                         ' +%char(QdgFrm.hf0dtiass);
       endif;

       QvlStrSql += ' +
                    order by cdrdtenmr +
                    fetch first 100000 rows only +
                    for read only +
                    ';

       // Esegui interrogazione database
       exec sql prepare gsa_prp from :QvlStrSql;
       exec sql declare gsa_crs cursor for gsa_prp;
       exec sql open gsa_crs;

       // Inz conteggio e first record
       QvlCount = 0;

       // Ciclo di caricamento
       dou (%error);

          exec sql fetch next from gsa_crs
                   into :QvlDteNmr;

          if sqlstt <>'00000';
             leave;
          endif;

          clear QdgGas;

          QdgGas.aznidn=QdgPnv.idnazn;
          QdgGas.rsuidn=QdgGsa.RSUIDN;
          QdgGas.gsaidn=QdgGsa.idn;
          QdgGas.dteass=QvlDteNmr;
          QdgGas.hhmmia=0;
          QdgGas.hhmmta=0;
          QdgGas=GasSet(QdgGas:QdgRtc);

          QvlCount= QvlCount+1;

       enddo;

       // Chiudi cursore
       exec sql close gsa_crs;

      *=============================================================================================
     PLodCnt           E
      **********************************************************************************************
      **********************************************************************************************
      * LodDsc : Carica Giornate Assenza (discontiune)
     PLodDsc           B
     DLodDsc           PI
      *=============================================================================================

       // Prima giornata
       if QdgFrm.hf0dteass1>0;
          clear QdgGas;

          QdgGas.aznidn=QdgPnv.idnazn;
          QdgGas.rsuidn=QdgGsa.RSUIDN;
          QdgGas.gsaidn=QdgGsa.idn;
          QdgGas.dteass=QdgFrm.hf0dteass1;
          QdgGas.hhmmia=QdgFrm.hf0hhmmia1;
          QdgGas.hhmmta=QdgFrm.hf0hhmmta1;
          QdgGas=GasSet(QdgGas:QdgRtc);
       endif;

       // Seconda giornata
       if QdgFrm.hf0dteass2>0;
          clear QdgGas;

          QdgGas.aznidn=QdgPnv.idnazn;
          QdgGas.rsuidn=QdgGsa.RSUIDN;
          QdgGas.gsaidn=QdgGsa.idn;
          QdgGas.dteass=QdgFrm.hf0dteass2;
          QdgGas.hhmmia=QdgFrm.hf0hhmmia2;
          QdgGas.hhmmta=QdgFrm.hf0hhmmta2;
          QdgGas=GasSet(QdgGas:QdgRtc);
       endif;

      *=============================================================================================
     PLodDsc           E
      **********************************************************************************************
      **********************************************************************************************
     PExpExc           B
     DExpExc           PI            20a   RTNPARM
     D QprExpTag                     50a   value varying                        Exit point
     DQvlFrmPtr        s               *
      *=============================================================================================

       // Richiedi exit point attivati
       if QprExpTag <> 'lod_exp_list' and
          %scan(%trim(QprExpTag) + ';':QvgExpRgs) = 0;
          return '';
       endif;

       // Alimenta row (solo per i cicli di caricamento)

       QvlFrmPtr = %addr(QdgFrm);
       return ExpDpt(QsdNmPrgr:QvgFrm:QvlFrmPtr:QprExpTag:QvgExpRgs);

      *=============================================================================================
     PExpExc           E
      **********************************************************************************************
      /include qsbr,IncFncFrm
