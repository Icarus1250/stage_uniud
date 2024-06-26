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
     Fgsa080v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)
     DQprIdnRsu        s              9s 0

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgFlgRcr        s              2a                                        flag ricarica
     DQvgDteCmp        s              8s 0

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(gsa080v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dgsa080c          PI
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
          //QprStr = ap(QprStr:'qpresempio':QprValore:'');

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
       QprIdnRsu=gpn(QprStr:'qpridnrsu');

       // Forza abbandono del programma ed evita caricamento dati
       //if QvgQualcosa = 'errore';
       //   ...
       //   s(QvgFrm:'hpract':'rtn');
       //   return false;
       //endif;


       //data chiusura consuntivo
       exec sql select  coalesce(max(cnpdtacmp), 0)
                  into :QvgDteCmp
                  from precns00f
                 where cnpaznidn=:QdgPnv.idnazn
                   and cnpstt='1';

       return true;

      *=============================================================================================
     PInitFrm          E
      **********************************************************************************************
      **********************************************************************************************
      * LodFrm: Load form
     PLodFrm           B
     DLodFrm           PI
      *=============================================================================================

       // Controllo allocaggi
       //QvgLck = joblck('record':'file':'chiave':QsdNmPrgr:'lock');
       //if QvgLck = 'false';
       //   s(QvgFrm:'hpract':'rtn');
       //   return;
       //endif;

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
       if isChg(Qvgfrm:'hf0') = *on;
          cnthf0();
          if QvgFlgRcr='si' ;
            Lodhf0();
         endif;
       endif;

      *=============================================================================================
     PCntFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf0:
     PLodhf0           B
     DLodhf0           PI
     DQvlCount         s              6s 0
      *=============================================================================================
       //ricava numero assenze da autorizzare
       exec sql select count(distinct gsaidn)
                into :QvlCount
                from gstgas00v join grtang00f on grtidn=gsagrtidn
                where gsaaznidn=1
                  and gasdteass>=:QvgDteCmp
                  and gsarsuidn=:QprIdnRsu
                  and GrtGstPrm = '1'
                  and gsasttaur='1';

       setatr(QvgFrm:'hf0btndau':'xdsc':'Da autorizzare ('+%char(QvlCount)+')');

       if QvlCount=0;
         setatr(QvgFrm:'hf0btndau':'disabled':'disabled');
       endif;

       //ricava numero assenze autorizzate
       clear QvlCount;
       exec sql select count(distinct gsaidn)
                into :QvlCount
                from gstgas00v join grtang00f on grtidn=gsagrtidn
                where gsaaznidn=1
                  and gasdteass>:QvgDteCmp
                  and gsarsuidn=:QprIdnRsu
                  and GrtGstPrm = '1'
                  and gsasttaur='5';

       setatr(QvgFrm:'hf0btnaut':'xdsc':'Autorizzate ('+%char(QvlCount)+')');
       if QvlCount=0;
         setatr(QvgFrm:'hf0btnaut':'disabled':'disabled');
       endif;
      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
     DQvlIdnGrt        s              9s 0
      *=============================================================================================
        // Nuova
        if QdgFrm.HF0BTNNEW='*on';
            // selezione causale
            clear QvgPrmInp ;
            QvgFrmCnl='GSA082C';
            FrmCnl(QvgPrmInp);
            QvlIdnGrt= gpn(QvgPrmInp:'qpridngrt');  //valore ritornato da gsa082c

            // inserimento richiesta
            if QvlIdnGrt <> 0;
            clear QvgPrmInp ;
              QvgPrmInp = ap(QvgPrmInp:'qpridnrsu':%char(QprIdnRsu):' ');
              QvgPrmInp = ap(QvgPrmInp:'qpridngrt':%char(QvlIdnGrt):' ');
              QvgPrmInp = ap(QvgPrmInp:'qpridngsa':'0':' ');
              QvgFrmCnl='GSA084C';
              FrmCnl(QvgPrmInp);
              QvgFlgRcr=gp(QvgPrmInp:'qprflgupd');
            endif;
        endif;

        // Da autorizzare
        if QdgFrm.HF0BTNDAU='*on';
            clear QvgPrmInp ;
            QvgPrmInp = ap(QvgPrmInp:'qpridnrsu':%char(QprIdnRsu):' ');
            QvgPrmInp = ap(QvgPrmInp:'qprsttass':'1':' ');
            QvgFrmCnl='GSA081C';
            FrmCnl(QvgPrmInp);
            QvgFlgRcr=gp(QvgPrmInp:'qprflgupd');
          endif;

        // Autorizzate
        if QdgFrm.HF0BTNAUT='*on';
            clear QvgPrmInp ;
            QvgPrmInp = ap(QvgPrmInp:'qpridnrsu':%char(QprIdnRsu):' ');
            QvgPrmInp = ap(QvgPrmInp:'qprsttass':'5':' ');
            QvgFrmCnl='GSA081C';
            FrmCnl(QvgPrmInp);
            QvgFlgRcr=gp(QvgPrmInp:'qprflgupd');
          endif;
      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * FrmGo:
     PFrmGo            B
     DFrmGo            PI
      *=============================================================================================

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
       //if QvgLck =  'true';
       //   joblck('record':'file':'chiave':QsdNmPrgr:'unlock');
       //endif;

       // Fine lavoro
       s(QvgFrm:'hpract':'rtn');

      *=============================================================================================
     PFrmEnd           E
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
