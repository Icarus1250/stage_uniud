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
     Fst2014v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)
     DQprCodClfr       s              6s 0                                      fare var globali
     DQprCodFll        s              6s 0

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgFlgSav        s              2a   inz('no')
     D$$PRGM           s             10a   INZ('')
     DAUABDL           s              1a   INZ('S')
     DAUABUP           s              1a   INZ('S')
     DAUABWR           s              1a   INZ('S')
     DAUTELB           s              1a   INZ('I')
     DQvgCodClfr       s              6s 0                                      fare var globali
     DQvgCodFll        s              6s 0

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(st2014v   :form) qualified
     D
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)
     D$ANGFL         e ds                  extname(ANGFL00F)
     D$ANGCF         e ds                  extname(ANGCF00F)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dst2014c          PI
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
       QprCodClfr=gpn(QprStr:'qprcodclfr');
       QprCodFll=gpn(QprStr:'qprcodfll');

       QvgCodClfr=QprCodClfr;
       QvgCodFll=QprCodFll;
       //QprEsempio = gp(QprStr:'qpresempio');

       // Forza abbandono del programma ed evita caricamento dati
       //if QvgQualcosa = 'errore';
       //   ...
       //   s(QvgFrm:'hpract':'rtn');
       //   return false;
       //endif;

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
       $$PRGM = QsdNmMdlo;
       LKFLNM = 'ANGFL00F';
       LKKFLD = %editc(QprCodFll:'X');
       LKTALK = 'V';
       exsr ALKRCD;
          if LKRCDE = *on;
       //      QvlMsg='In uso';
       //      QvlAct=msgeng(QvgFrm:QvlMsg:''
       //            :'INVIO':'':''
       //            :'':'':'');
             s(QvgFrm:'hpract':'rtn');
             return;
          endif;

       // Imposta controllo
       s(QvgFrm:'hpract':'cnt');

       // Load hf0
       lodhf0();

     C/define  FUNCTION
     C/copy qsbr,alkrcd

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

       //controllo cnthf0
       cnthf0();


       // Aggiorna
       if QdgFrm.HF0FOOTSAV = '*on' and
          isErr(QvgFrm:'hf0')=*off ;
          QvgFlgSav = 'si';
          Frmgo();
          return;
       endif;

      *=============================================================================================
     PCntFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf0:
     PLodhf0           B
     DLodhf0           PI
      *=============================================================================================
      // lettura Cliente/Fornitore
        flcdfl = QvgCodFll;
        flcdcf = QvgCodClfr;

        if flcdfl=0;
          chread = 'R';
        else;
          chread = 'C';
        endif;
        exsr chnafl;

        cfcdcf = QvgCodClfr;
        chread = 'C';
        exsr chnacf;

        QvgCodFll=FLCDFL;

        // valorizza dati a video
        QdgFrm.hf0rgscln = CFRGS1;
        QdgFrm.hf0cdcf = QvgCodClfr;
        QdgFrm.hf0cdfl = FLCDFL;
        QdgFrm.hf0rgs1 = FLRGS1;
        QdgFrm.hf0rgs2 = FLRGS2;
        QdgFrm.hf0indr = FLINDR;
        QdgFrm.hf0cap  = FLCAP ;
        QdgFrm.hf0lclt = FLLCLT;
        QdgFrm.hf0prvn = FLPRVN;
        QdgFrm.hf0nzne = FLNZNE;
        QdgFrm.hf0ntlf = FLNTLF;

     C/define  FUNCTION
     C/copy qsbr,chnafl
     C/copy qsbr,chnacf

      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================
       //Premuto il tasto elimina
       if QdgFrm.HF0FOOTRMV ='*on';
          flcdcf = QvgCodClfr;
          flcdfl = QvgCodFll;
          upannl = 'S';
          uprcnf = 'S';
          exsr updafl;
          if uprcde=*off;
             QvgFlgSav = 'si';
             FrmEnd();
             return;
          endif;
       endif;

     C/define  FUNCTION
     C/copy qsbr,updafl
      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * FrmGo:
     PFrmGo            B
     DFrmGo            PI
      *=============================================================================================
       //variabili locali
       flcdcf = QvgCodClfr;
       flcdfl = QvgCodFll;
       chread = 'C';
       exsr chnafl;

       FLRGS1 = QdgFrm.hf0rgs1 ;
       FLRGS2 = QdgFrm.hf0rgs2 ;
       FLINDR = QdgFrm.hf0indr ;
       FLCAP = QdgFrm.hf0cap ;
       FLLCLT = QdgFrm.hf0lclt ;
       FLPRVN = QdgFrm.hf0prvn ;
       FLNZNE = QdgFrm.hf0nzne ;
       FLNTLF = QdgFrm.hf0ntlf ;


       upannl = 'N';  //richiesta cancellazione(s/n)
       uprcnf = 'N';  //no richiesta conferma(s/n)
       exsr updafl;

       FrmEnd();

     C/define  FUNCTION
     C/copy qsbr,chnafl
     C/copy qsbr,updafl
      *=============================================================================================
     PFrmGo            E
      **********************************************************************************************
      **********************************************************************************************
      * FrmEnd:
     PFrmEnd           B
     DFrmEnd           PI
      *=============================================================================================

       // Libera allocaggi (copiata ma non capita)
       $$PRGM = QsdNmMdlo;
       LKFLNM = 'ANGFL00F';  //nome file fisico
       LKKFLD = %editc(QprCodFll:'X');  //chiave
       LKTALK = 'A';         //tipo allocaggio(vedi libreria in fondo alla procedura)
       exsr ALKRCD;          //Effettuare la gestione dei vincoli per un
                             //record richiesto.

       // Fine lavoro
       s(QvgFrm:'hpract':'rtn');

     C/define  FUNCTION
     C/copy qsbr,alkrcd

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