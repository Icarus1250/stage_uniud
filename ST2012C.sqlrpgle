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
     Fst2012v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)
     DQprCodClfr       s              6s 0
      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgnmgl          s             15a
     DQvgFlgSav        s              2a   inz('no')
     D$$PRGM           s             10a   INZ('')
     DAUABDL           s              1a   INZ('S')
     DAUABUP           s              1a   INZ('S')
     DAUABWR           s              1a   INZ('S')
     DAUTELB           s              1a   INZ('I')

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(st2012v   :form) qualified
     D                                                              template
      * Strutture dati di controllo campi

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)
     D$ANGCF         e ds                  extname(ANGCF00F)
     D$TBBSE         e ds                  extname(TBBSE00F)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dst2012c          PI
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
          QprStr = ap(QprStr:'qprflgsav':QvgFlgSav:'');

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
       QprCodClfr = gpn(QprStr:'qprcodclfr');
       //QprEsempio = gp(QprStr:'qpresempio');

       return true;

      *=============================================================================================
     PInitFrm          E
      **********************************************************************************************
      **********************************************************************************************
      * LodFrm: Load form
     PLodFrm           B
     D QvlMsg          s            700a   varying                              Messaggio
     D QvlAct          s             10a   inz                                  Tasto premuto
     DLodFrm           PI

      *=============================================================================================

       // Controllo allocaggi      -???-
       $$PRGM = QsdNmMdlo;
       LKFLNM = 'ANGCF00F';
       LKKFLD = %editc(QprCodClfr:'X');
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

       cnthf0();

       // Verifica se richiesto il doppio invio  -???-
       //eval-corr QdgFrmCtrNew=Qdgfrm ;
       //if QdgFrmCtrNew <> QdgfrmCtrOld;
       //   QdgFrmCtrOld = QdgfrmCtrNew;
       //   return;
       //endif;

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
        cfcdcf = QprCodClfr;
        chread = 'C';
        exsr chnacf;

        // valorizza dati a video
        QdgFrm.hf0cdcf = QprCodClfr;
        QdgFrm.hf0nmgl = CFNMGL;
        QdgFrm.hf0rgs1 = CFRGS1;
        QdgFrm.hf0rgs2 = CFRGS2;
        QdgFrm.hf0indr = CFINDR;
        QdgFrm.hf0cap  = CFCAP ;
        QdgFrm.hf0lclt = CFLCLT;
        QdgFrm.hf0prvn = CFPRVN;
        QdgFrm.hf0nzne = CFNZNE;
        QdgFrm.hf0piva = CFPIVA;

        // Gestione attributi    -???-
        if QdgFrm.hf0nzne <> TBDSCR;
           addatr(QvgFrm:'hf0nzne':'class':'bkgevatt');
        endif;

     C/define  FUNCTION
     C/copy qsbr,chnacf
     C/copy qsbr,chntbs

      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI

     D QvlStrSql       s          10000a                                        Stringa sql
     D QvlMatch        s              1a   inz                                  Record trovato
     D QvlNmgl         s             15a
     D QvlCdcf         s              6s 0
      *=============================================================================================
       //Premuto il tasto elimina
       if QdgFrm.HF0FOOTRMV ='*on';
          cfcdcf = QdgFrm.Hf0cdcf;
          upannl = 'S';
          uprcnf = 'S';
          exsr updacf;
          if uprcde=*off;
             QvgFlgSav = 'si';
             FrmEnd();
             return;
          endif;
          //exec sql DELETE FROM angcf00f
          //               WHERE cfcdcf = :QprCodClfr;
       endif;

       // Nomignolo
       QvlNmgl=QdgFrm.hf0nmgl;
       QvlCdcf= QdgFrm.Hf0cdcf;
          // Obbligatorio
          if QvlNmgl = '';
             seterr(QvgFrm:'hf0nmgl':'GEAGEN000');
             return;
          endif;
           //Univoco
           // Controllo se presenti ordini
       exec sql select '1'
                into  :QvlMatch
                from   angcf00f
                where  cfnmgl = :QvlNmgl and cfcdcf<> :QvlCdcf
                fetch first 1 rows only;
          // Trovato

          if sqlstt= '00000';
             seterr(QvgFrm:'hf0nmgl':'':'Nomignolo già esistente');
             return;
          endif;



       // Ragione sociale: obbligatoria
       if QdgFrm.hf0rgs1 = '';
          seterr(QvgFrm:'hf0rgs1':'':'Ragione sociale 1 obbligatoria');
          Return;
       endif;

     C/define  FUNCTION
     C/copy qsbr,updacf

      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * FrmGo:
     PFrmGo            B
     DFrmGo            PI
      *=============================================================================================


       //variabili locali
       cfcdcf = QprCodClfr;
       chread = 'C';
       exsr chnacf;

       CFNMGL = QdgFrm.hf0nmgl ;
       CFRGS1 = QdgFrm.hf0rgs1 ;
       CFRGS2 = QdgFrm.hf0rgs2 ;
       CFINDR = QdgFrm.hf0indr ;
       CFCAP = QdgFrm.hf0cap ;
       CFLCLT = QdgFrm.hf0lclt ;
       CFPRVN = QdgFrm.hf0prvn ;
       CFNZNE = QdgFrm.hf0nzne ;
       CFPIVA = QdgFrm.hf0piva ;


       upannl = 'N';  //richiesta cancellazione(s/n)
       uprcnf = 'N';  //no richiesta conferma(s/n)
       exsr updacf;
       //exec sql UPDATE angcf00f
       //            SET cfnmgl=:Qvgnmgl, cfrgs1=:Qvgrgs1, cfrgs2=:Qvgrgs2,
       //                cfindr=:Qvgindr, cfcap=:Qvgcap, cflclt=:Qvglclt,
       //                cfprvn=:Qvgprvn, cfnzne=:Qvgnzne, cfpiva=:Qvgpiva
       //          WHERE cfcdcf = :QprCodClfr;

       FrmEnd();

     C/define  FUNCTION
     C/copy qsbr,chnacf
     C/copy qsbr,updacf

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
       LKFLNM = 'ANGCF00F';  //nome file fisico
       LKKFLD = %editc(QprCodClfr:'X');  //chiave
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
     DQprExpTag                      50a   value varying                        Exit point
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