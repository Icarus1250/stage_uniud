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
     Fgsa084v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

     D/COPY QSBR,IncDfnGsa
     D/COPY QSBR,IncDfnGrt

      * Parametri dinamici (Qpr*)
     DQprIdnRsu        s              9s 0
     DQprIdnGrt        s              9s 0
     DQprIdnGsa        s              9s 0

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgStep          s              4s 0                                      0-data inizio
                                                                                1-data termine se ferie, intervallo ore se permesso
                                                                                2-conferma dati e salvataggio

     DQvgHf0Dti        s             50a   varying
     DQvgHf0Slt1       s             50a   varying
     DQvgHf0Dtt        s             50a   varying
     DQvgHf0Slt2       s             50a   varying
     DQvgHf0Mia        s             50a   varying
     DQvgHf0Mta        s             50a   varying

     DQvgFlgRcr        s              2a


      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(gsa084v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)
     DQdgGrt           ds                  likeds(QdtGrt)
     DQdgGas           ds                  likeds(QdtGas)
     DQdgGsa           ds                  likeds(QdtGsa)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtGrt
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,IncPrtGsa
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dgsa084c          PI
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
     DQvlYear          s              4s 0
     DQvlSlt           s           5000a   varying
     Di                s              4s 0
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

       //popolamento select giorni
       QvlSlt='option::;';
       for i = 1 to 31;
         QvlSlt+=  'option:' + %char(i) +':' + %char(i) + ';';
       endfor;
       setatr(QvgFrm:'hf0sltdd1':'xsltlst':%trim(QvlSlt));
       setatr(QvgFrm:'hf0sltdd2':'xsltlst':%trim(QvlSlt));

       //popolamento select mesi
       QvlSlt='option::;';
       for i = 1 to 12;
         QvlSlt+=  'option:' + %char(i) +':' + %char(i) + ';';
       endfor;
       setatr(QvgFrm:'hf0sltmm1':'xsltlst':%trim(QvlSlt));
       setatr(QvgFrm:'hf0sltmm2':'xsltlst':%trim(QvlSlt));

       //popolamento select anni
       QvlSlt='option::;';
       QvlYear= %int(%subst(%char(%date(): *ISO): 1: 4));
       for i = 0 to 1;
         QvlSlt+=  'option:' + %char(QvlYear+i) +':' + %char(QvlYear+i) + ';';
       endfor;
       setatr(QvgFrm:'hf0sltyy1':'xsltlst':%trim(QvlSlt));
       setatr(QvgFrm:'hf0sltyy2':'xsltlst':%trim(QvlSlt));

       RtvTag();

       return true;

      *=============================================================================================
     PInitFrm          E
      **********************************************************************************************
      **********************************************************************************************
      * RtvTag: Rileva tag
     PRtvTag           B
     DRtvTag           PI

      *=============================================================================================

       //ottieni riferimento alle righe di hf0
       QvgHf0Dti = GetPrn(QvgFrm:'hf0dtiass':'xtag="tr"');
       QvgHf0Slt1 = GetPrn(QvgFrm:'hf0sltdd1':'xtag="tr"');
       QvgHf0Dtt = GetPrn(QvgFrm:'hf0dttass':'xtag="tr"');
       QvgHf0Slt2 = GetPrn(QvgFrm:'hf0sltdd2':'xtag="tr"');
       QvgHf0Mia = GetPrn(QvgFrm:'hf0hhmmia':'xtag="tr"');
       QvgHf0Mta = GetPrn(QvgFrm:'hf0hhmmta':'xtag="tr"');


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

       //inizializzo data
       QdgFrm.HF0DTE= %dec(%date());

       // Decodifica giustificativo assenza
       if QprIdnGsa<>0;
          QdgGsa=GsaGet(QprIdnGsa);
         QdgGrt=GrtGet(QdgGsa.grtidn:'');
       else;
         QdgGrt=GrtGet(QprIdnGrt:'');
       endif;
       QdgFrm.HF0DSC=QdgGrt.DSC;

       //se permesso
       if QdgGrt.TPO='4';
         s(QvgFrm:'hf0lbldti1':'Data');
         s(QvgFrm:'hf0lbldti2':'Data');
       endif;

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

       //AVANTI/SALVA
       if QdgFrm.HF0FOOTFWD ='*on' and isErr(QvgFrm:'hf0')=*off ;
         QvgStep=QvgStep+1;
         if QvgStep<2 and QprIdnGsa=0;
           Lodhf0();
         else;
           QvgFlgRcr='si';
           FrmGo();
         endif;
       endif;
      *=============================================================================================
     PCntFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf0:
     PLodhf0           B
     DLodhf0           PI
      *=============================================================================================
      // Inz. var.
       //0 - inserimento data inizio assenza
       if QprIdnGsa = 0;
         if QvgStep=0;
            LodStp0Ins();
         //1 - se ferie, inserimento data termine
         //    se permesso, inserimento intervallo ore
         elseif QvgStep=1;
            LodStp1Ins();
         //2 - riassunto informazioni e salvataggio
       //  elseif QvgStep=2;
       //     LodStp2Ins();
         endif;
       else;
         //mostra schermata modifica
         if QvgStep=0;
            LodStp0Mdf();
         endif;
       endif;

      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * LodStp0: Step 0 - inserimento data inizio assenza
     PLodStp0Ins       B
     DLodStp0Ins       PI
      *=============================================================================================
         addatr(QvgFrm:'hf0footbck':'class':'hidden');
         setatr(QvgFrm:'hf0footfwd':'xdsc':'AVANTI');
         rmvatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dti:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
         addatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mia:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mta:'class':'hidden');

      *=============================================================================================
     PLodStp0Ins       E
      **********************************************************************************************
      **********************************************************************************************
      * LodStp1: Step 1 - inserimento data termine/intervallo ore
     PLodStp1Ins       B
     DLodStp1Ins       PI
      *=============================================================================================
         rmvatr(QvgFrm:'hf0footbck':'class':'hidden');
         setatr(QvgFrm:'hf0footfwd':'xdsc':'SALVA');
         addatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
         rmvatr(QvgFrm:QvgHf0Dti:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
         addatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mia:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mta:'class':'hidden');
         //se ferie
         if QdgGrt.Tpo='3';
           rmvatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
         //se permesso
         else ;
           rmvatr(QvgFrm:QvgHf0Mia:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Mta:'class':'hidden');

           setatr(QvgFrm:'hf0hhmmia':'readonly':'');
           setatr(QvgFrm:'hf0hhmmta':'readonly':'');
         endif;
      *=============================================================================================
     PLodStp1Ins       E
      **********************************************************************************************
      **********************************************************************************************
      * LodStp1: Step 2
     PLodStp2Ins       B
     DLodStp2Ins       PI

      *=============================================================================================

         //se ferie
         if QdgGrt.Tpo='3';
           addatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
           addatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Dti:'class':'hidden');
           addatr(QvgFrm:QvgHf0Mia:'class':'hidden');
           addatr(QvgFrm:QvgHf0Mta:'class':'hidden');

         //se permesso
         else ;
           rmvatr(QvgFrm:QvgHf0Mia:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Mta:'class':'hidden');
           addatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Dti:'class':'hidden');
           addatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
           addatr(QvgFrm:QvgHf0Slt2:'class':'hidden');

           setatr(QvgFrm:'hf0hhmmia':'readonly':'readonly');
           setatr(QvgFrm:'hf0hhmmta':'readonly':'readonly');
         endif;
      *=============================================================================================
     PLodStp2Ins       E
      **********************************************************************************************
      **********************************************************************************************
      * LodStp0: Step 0 - inserimento data inizio assenza
     PLodStp0Mdf       B
     DLodStp0Mdf       PI
      *variabili locali
     D QvlStrSql       s          10000a
      *strutture dati locali
     DQdlAdsRcd        ds                  qualified inz
     D dteass                              like(QdtGas.dteass     )
     D hhmmia                              like(QdtGas.hhmmia     )
     D hhmmta                              like(QdtGas.hhmmta     )
      *=============================================================================================
         setatr(QvgFrm:'hf0footfwd':'xdsc':'SALVA');

         rmvatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dti:'class':'hidden');
         addatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
         addatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mia:'class':'hidden');
         addatr(QvgFrm:QvgHf0Mta:'class':'hidden');
         //se ferie
         if QdgGrt.Tpo='3';


           if QdgGsa.STTAUR='5';
             addatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
             rmvatr(QvgFrm:QvgHf0Dti:'class':'hidden');
             rmvatr(QvgFrm:QvgHf0Dtt:'class':'hidden');
             setatr(QvgFrm:'hf0dtiass':'readonly':'readonly');
             setatr(QvgFrm:'hf0dttass':'readonly':'readonly');

             addatr(QvgFrm:'hf0footfwd':'class':'hidden');
             addatr(QvgFrm:'hf0footbck':'class':'hidden');
           else;
             rmvatr(QvgFrm:QvgHf0Slt2:'class':'hidden');
           endif;
         //se permesso
         else ;

           rmvatr(QvgFrm:QvgHf0Mia:'class':'hidden');
           rmvatr(QvgFrm:QvgHf0Mta:'class':'hidden');

           if QdgGsa.STTAUR='5';
             addatr(QvgFrm:QvgHf0Slt1:'class':'hidden');
             rmvatr(QvgFrm:QvgHf0Dti:'class':'hidden');
             setatr(QvgFrm:'hf0hhmmia':'readonly':'readonly');
             setatr(QvgFrm:'hf0hhmmta':'readonly':'readonly');

             addatr(QvgFrm:'hf0footfwd':'class':'hidden');
             addatr(QvgFrm:'hf0footbck':'class':'hidden');
           endif;
         endif;


         QdgGsa=GsaGet(QprIdnGsa);
         QdgFrm.HF0DTE=QdgGsa.DTE;

         //assegnazione valori select inizio
         QdgFrm.HF0DTIASS=QdgGsa.DTIASS;
         if QdgGsa.DTIASS<>0;
           QdgFrm.hf0sltdd1=%int(%subst(%char(QdgGsa.DTIASS): 7: 2));
           QdgFrm.hf0sltmm1=%int(%subst(%char(QdgGsa.DTIASS): 5: 2));
           QdgFrm.hf0sltyy1=%int(%subst(%char(QdgGsa.DTIASS): 1: 4));

         endif;
         //assegnazione valori select termine
         QdgFrm.HF0DTTASS=QdgGsa.DTTASS;
         if QdgGsa.DTTASS<>0;
           QdgFrm.hf0sltdd2=%int(%subst(%char(QdgGsa.DTTASS): 7: 2));
           QdgFrm.hf0sltmm2=%int(%subst(%char(QdgGsa.DTTASS): 5: 2));
           QdgFrm.hf0sltyy2=%int(%subst(%char(QdgGsa.DTTASS): 1: 4));
         endif;


         // Costruisci interrogazione per ore di assenza
         QvlStrSql = 'select gasdteass, gashhmmia, gashhmmta +
                        from gstgas00v +
                       where gasaznidn=' +%char(QdgPnv.idnazn)+ ' +
                         and gasgsaidn=' +%char(QprIdnGsa)+ ' +
                         and gsafrqz=''1 '' +
                    order by gasdteass, gashhmmia +
                      fetch first 1 rows only +
                      for read only +
                      ';

         // Esegui interrogazione database
         exec sql prepare ads_prp from :QvlStrSql;
         exec sql declare ads_crs cursor for ads_prp;
         exec sql open ads_crs;
         dou (%error);
           exec sql fetch next from ads_crs
                       into :QdlAdsRcd;

           if sqlstt <>'00000';
               leave;
            endif;

           QdgFrm.hf0dtiass=QdlAdsRcd.dteass;
           if QdlAdsRcd.dteass<>0;
             QdgFrm.hf0sltdd1=%int(%subst(%char(QdgFrm.hf0dtiass): 7: 2));
             QdgFrm.hf0sltmm1=%int(%subst(%char(QdgFrm.hf0dtiass): 5: 2));
             QdgFrm.hf0sltyy1=%int(%subst(%char(QdgFrm.hf0dtiass): 1: 4));
           endif;

           QdgFrm.hf0hhmmia=QdlAdsRcd.hhmmia;
           QdgFrm.hf0hhmmta=QdlAdsRcd.hhmmta;
         enddo;
         exec sql close ads_crs;
         QdgFrm.HF0DSC=QdgGrt.DSC;


      *=============================================================================================
     PLodStp0Mdf       E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================
       //INDIETRO
       if QdgFrm.HF0FOOTBCK ='*on';
         QvgStep=QvgStep-1;
         if QvgStep>=0;
           Lodhf0();
         else;
           FrmEnd();
         endif;
       endif;

       // controllo requisiti

       //inserimento
       if QprIdnGsa=0;
         if QvgStep=0;

           //controllo data
           chkdte('hf0sltdd1':'hf0sltmm1':'hf0sltyy1');

           QdgFrm.hf0dtiass = QdgFrm.hf0sltyy1*10000+
                              QdgFrm.hf0sltmm1*100+
                              QdgFrm.hf0sltdd1;
         elseif QvgStep=1;
           //se ferie
           if QdgGrt.Tpo='3';

              //controllo data
              chkdte('hf0sltdd2':'hf0sltmm2':'hf0sltyy2');

              QdgFrm.hf0dttass = QdgFrm.hf0sltyy2*10000+
                              QdgFrm.hf0sltmm2*100+
                              QdgFrm.hf0sltdd2;

              //data inizio maggiore di data fine
              if QdgFrm.hf0dtiass>QdgFrm.hf0dttass;
                 seterr(QvgFrm:'hf0sltdd2':'GEAESR002');
                  return;
              endif;

           //se permesso
           else;
             //controllo ore
             ChkOra('hf0hhmmia':'hf0hhmmta');
           endif;
         endif;
       else;

         //modifica
         if QvgStep<>2;
           //controllo data
           chkdte('hf0sltdd1':'hf0sltmm1':'hf0sltyy1');

           QdgFrm.hf0dtiass = QdgFrm.hf0sltyy1*10000+
                              QdgFrm.hf0sltmm1*100+
                              QdgFrm.hf0sltdd1;
           //se ferie
           if QdgGrt.Tpo='3';
             //controllo data
             chkdte('hf0sltdd2':'hf0sltmm2':'hf0sltyy2');

             QdgFrm.hf0dttass = QdgFrm.hf0sltyy2*10000+
                                QdgFrm.hf0sltmm2*100+
                                QdgFrm.hf0sltdd2;

             //data inizio maggiore di data fine
             if QdgFrm.hf0dtiass>QdgFrm.hf0dttass;
                 seterr(QvgFrm:'hf0sltdd2':'GEAESR002');
                  return;
             endif;

           //se permesso
           else;
             //controllo ore
             ChkOra('hf0hhmmia':'hf0hhmmta');
           endif;
         endif;
       endif;






      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      *ChkDte: controlla correttezza delle date inserite da select
     PChkDte           B
     DChkDte           PI
     D QprDdTag                      50a   value varying
     D QprMmTag                      50a   value varying
     D QprYyTag                      50a   value varying
     DQvlmm30          S              4s 0    Dim(4)                            mesi da 30 giorni
     DQvldd            s              4s 0                                      val. select giorno
     DQvlmm            s              4s 0                                      val. select mese
     DQvlyy            s              4s 0                                      val. select anno
      *=============================================================================================

          Qvlmm30(1)=4;
          Qvlmm30(2)=6;
          Qvlmm30(3)=9;
          Qvlmm30(4)=11;

          Qvldd=%int(g(QvgFrm:QprDdTag));
          Qvlmm=%int(g(QvgFrm:QprMmTag));
          Qvlyy=%int(g(QvgFrm:QprYyTag));

         // giorno inizio obbligatorio
         if Qvldd=0;
            seterr(QvgFrm:QprDdTag:'GEAGEN000');
            return;
         endif;

         // mese inizio obbligatorio
         if Qvlmm=0;
            seterr(QvgFrm:QprMmTag:'GEAGEN000');
            return;
         endif;

         // anno inizio obbligatorio
         if Qvlyy=0;
            seterr(QvgFrm:QprYyTag:'GEAGEN000');
            return;
         endif;

         //giorno non contenuto nel mese
         if (Qvlmm in Qvlmm30 and Qvldd>30);
            seterr(QvgFrm:QprDdTag:'':'giorno non presente in questo mese');
            return;
         endif;

         //anno bisestile
         if %rem(Qvlyy:4)=0 and Qvlmm=2;
           if Qvldd>29;
             seterr(QvgFrm:QprDdTag:'':'giorno non presente in questo mese');
             return;
           elseif Qvldd>28;
             seterr(QvgFrm:QprDdTag:'':'giorno non presente in questo mese');
             return;
           endif;
         endif;


      *=============================================================================================
     PChkDte           E
      **********************************************************************************************
      **********************************************************************************************
      *ChkDte: controlla correttezza intervallo ore
     PChkOra           B
     DChkOra           PI
     D QprInzTag                     50a   value varying
     D QprFinTag                     50a   value varying
     DQvlinz           s              4s 0                                      val. ora inizio
     DQvlfin           s              4s 0                                      val. ora fine
      *=============================================================================================
       Qvlinz=%int(g(QvgFrm:QprInzTag));
       Qvlfin=%int(g(QvgFrm:QprFinTag));

       // ora inizio obbligatoria
        if Qvlinz=0;
           seterr(QvgFrm:QprInzTag:'GEAGEN000');
            return;
        endif;
        // ora fine obbligatoria
        if Qvlfin=0;
           seterr(QvgFrm:QprFinTag:'GEAGEN000');
           return;
        endif;

        //ora fine minore di ora inizio
        if Qvlfin<Qvlinz;
           seterr(QvgFrm:QprFinTag:'GEATME001');
           return;
        endif;


      *=============================================================================================
     PChkOra           E
      **********************************************************************************************
      **********************************************************************************************
      * FrmGo:
     PFrmGo            B
     DFrmGo            PI
      *=============================================================================================
       //Carica assenza individuale
        if QprIdnGsa = 0;
         clear QdgGsa;
         QdgGsa.aznidn=QdgPnv.idnazn;
         QdgGsa.rsuidn=QprIdnRsu;
         QdgGsa.grtidn=QprIdnGrt;
         QdgGsa.sttaut='1';
       else;
         QdgGsa=GsaGet(QprIdnGsa);
       endif;

       QdgGsa.mtv=Qdgfrm.hf0dsc;
       QdgGsa.dte=Qdgfrm.hf0dte;
       QdgGsa.stt='0';
       QdgGsa.Arc='0';

       // Frequenza
       if Qdgfrm.hf0dttass>0;
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

        // Orario

        clear QdgGas;

        QdgGas.aznidn=QdgPnv.idnazn;
        QdgGas.rsuidn=QdgGsa.RSUIDN;
        QdgGas.gsaidn=QdgGsa.idn;
        QdgGas.dteass=QdgFrm.hf0dtiass;
        QdgGas.hhmmia=QdgFrm.hf0hhmmia;
        QdgGas.hhmmta=QdgFrm.hf0hhmmta;
        QdgGas=GasSet(QdgGas:QdgRtc);

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
