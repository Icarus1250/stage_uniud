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
     Fst2016v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)
     DQprAaaOrd        s              4s 0                                      anno ordine
     DQprNmrOrd        s              6s 0

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(st2016v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)
     D$OFRAT         e ds                  extname(OFRAT00F)
     D$QFLAG         e ds                  extname(QFLAG00F)
     D$ANGCF         e ds                  extname(ANGCF00F)
     D$TBBSE         e ds                  extname(TBBSE00F)



     D* Grid h50
     DQdgh50Rcd        ds                  qualified inz
     D h50nrrg                             like(QdtFrmDef.h50nrrg       )
     D h50cdar                             like(QdtFrmDef.h50cdar       )
     D h50not1                             like(QdtFrmDef.h50not1       )
     D h50dscr                             like(QdtFrmDef.h50dscr       )
     D h50qtor                             like(QdtFrmDef.h50qtor       )
     D h50przo                             like(QdtFrmDef.h50przo       )
     D h50dtrc                             like(QdtFrmDef.h50dtrc       )
     D h50dtpc                             like(QdtFrmDef.h50dtpc       )

     DQdgh50           ds                  likeds(Qdgh50Rcd) dim(10000)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dst2016c          PI
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
       //QprEsempio = gp(QprStr:'qpresempio');
       QprAaaOrd = gpn(QprStr:'qpraaor');
       QprNmrOrd = gpn(QprStr:'qprnror');

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
       //QvgLck = joblck('record':'file':'chiave':QsdNmPrgr:'lock');
       //if QvgLck = 'false';
       //   s(QvgFrm:'hpract':'rtn');
       //   return;
       //endif;

       // Imposta controllo
       s(QvgFrm:'hpract':'cnt');

       // Load hf0
       lodhf0();

       // Load hf1
       lodhf1();

       // Load hf2
       lodhf2();

       // Load hg0
       lodhg0();

       // Write hg0
       wrthg0();

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
       endif;

       // Controllo hf1
       if isChg(Qvgfrm:'hf1') = *on;
          cnthf1();
       endif;

       // Controllo hf2
       if isChg(Qvgfrm:'hf2') = *on;
          cnthf2();
       endif;

       // Controllo hg0
       if isChg(QvgFrm:'hg0') = *on;
          cnthg0();
       endif;

       // Reload grid body
       if isErr(QvgFrm:'h50') = *off;

          // roll up / down / vertica scroll bar
          if FrmRll(QvgFrm:'h50') = *on;
             Wrthg0();
          endif;

          // reload for changed order or selection
          if isChg(QvgFrm:'h20') = *on;
             lodhg0();
             rstgrdpge(QvgFrm:'h50');
             wrthg0();
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
        //Ordine
        ataaor=QprAaaOrd;
        atnror=QprNmrOrd;
        exsr chnoat;

        QdgFrm.hf0dtor=ATDTOR;

        //traduzione codice tipo
        TBPRFS='TOF';
        TBCTB1=ATCTOF;
        exsr chntbs;
        QdgFrm.hf0ctof=TBDSCR ;

        //traduzione codice causale
        TBPRFS='COF';
        TBCTB1=ATCCOF;
        exsr chntbs;
        QdgFrm.hf0ccof=tbdscr ;

     C/define  FUNCTION
     C/copy qsbr,chnoat
     C/copy qsbr,chntbs
      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf1:
     PLodhf1           B
     DLodhf1           PI
      *=============================================================================================
       //Avanzamento
        ataaor=QprAaaOrd;
        atnror=QprNmrOrd;
        exsr chnoat;

        //traduzione codice stato
        QFPRFS='OFRAT00F';
        QFIDNT='ATSTOR';
        QFFLAG=ATSTOR;
        exsr chnqfl;
        QdgFrm.HF1STOR=QFDSCR;

        //traduzione codice evasione
        QFPRFS='OFRAT00F';
        QFIDNT='ATEVOR';
        QFFLAG=ATEVOR;
        exsr chnqfl;
        QdgFrm.HF1EVOR=QFDSCR;

        //mostra a video i dati
        QdgFrm.HF1DTRC=ATDTRC;
        QdgFrm.HF1NSRC=ATNSRC;
        QdgFrm.HF1DTPC=ATDTPC;
        QdgFrm.HF1NSPC=ATNSPC;




     C/define  FUNCTION
     C/copy qsbr,chnoat
     C/copy qsbr,chnqfl
      *=============================================================================================
     PLodhf1           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf2:
     PLodhf2           B
     DLodhf2           PI
      *=============================================================================================
      //Fornitore
        ataaor=QprAaaOrd;
        atnror=QprNmrOrd;
        exsr chnoat;

        CFCDCF=ATCDFR;
        exsr chnacf;

        s(QvgFrm:'hf2lblrgsc':CFRGS1+'('+%char(CFCDCF)+')');
        s(QvgFrm:'hf2lbllcl':CFLCLT+'('+CFPRVN+')');

     C/define  FUNCTION
     C/copy qsbr,chnoat
     C/copy qsbr,chnacf
      *=============================================================================================
     PLodhf2           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhg0:
     PLodhg0           B
     DLodhg0           PI

      * Variabili locali
     D QvlSqlStr       s                   like(QvtSqlStr)                      Form
     D QvlCount        s              9s 0                                      Count
     D QvlElm          s              9s 0                                      Max Elements grid
      *=============================================================================================

       // Rileva numero elementi massimo
       QvlElm = %elem(Qdgh50);

       // Costruisci interrogazione
       QvlSqlStr = 'select +
                           adnrrg, +
                           adcdar, +
                           adnot1, +
                           addscr, +
                           adqtor, +
                           adprzo, +
                           addtrc, +
                           addtpc +
                      from ofrad00f +
                    :where and adaaor='+ %char(QprAaaOrd) +' and adnror=+
                               ' + %char(QprNmrOrd) +' +
                    :order adaaor DESC +
                     fetch first :elem rows only +
                       for read only';

       // Valorizza numero massimo elementi
       QvlSqlStr = %scanrpl(':elem':%char(QvlElm):QvlSqlStr);
       // Build where condition
       QvlSqlStr = %scanrpl(':where':
                            BldWhr(QvgFrm:getatr(QvgFrm:'h20':'xwhrstr')):
                            QvlSqlStr);
       // Build order condition
       QvlSqlStr = %scanrpl(':order':
                            BldOrd(QvgFrm:getatr(QvgFrm:'h20':'xordstr')):
                            QvlSqlStr);

       // Esegui interrogazione database
       exec sql prepare hg0_prp from :QvlSqlStr;
       exec sql declare hg0_crs cursor for hg0_prp;
       exec sql open hg0_crs;
       SQLChkMsg(QvgFrm:SQLCODE:SQLSTT:QvlSqlStr);

       // Inz h50
       QdgFrm.h50RWNMR = 0;
       QdgFrm.h50RWNMRD= 0;
       clear Qdgh50Rcd;
       clear Qdgh50;

       // Inz conteggio e first record
       QvlCount = 0;

       // Ciclo di caricamento
       dou (%error);

          exec sql fetch next from hg0_crs
                   into :Qdgh50Rcd;

          if SQLSTT <> '00000' and
             SQLChkMsg(QvgFrm:SQLCODE:SQLSTT) = 'leave';
             leave;
          endif;

          // Conta row e verifica se raggiunto limite massimo abbandona
          QvlCount += 1;
          if QvlCount = QvlElm;
             leave;
          endif;

          // Carica record set
          Qdgh50(QvlCount) = Qdgh50Rcd;

       enddo;

       // Set max rows grid
       if QvlCount > QvlElm - 1;
          rmvAtr(QvgFrm:'h10rcs':'class':'hidden');
          s(QvgFrm:'h10maxrow':%char(QvlCount - 1));
       else;
          addAtr(QvgFrm:'h10rcs':'class':'hidden');
          s(QvgFrm:'h10maxrow':%char(QvlCount));
       endif;

       // Chiudi cursore
       exec sql close hg0_crs;

      *=============================================================================================
     PLodhg0           E
      **********************************************************************************************
      **********************************************************************************************
      * Wrthg0:
     PWrthg0           B
     DWrthg0           PI

      * Variabili locali
     D QvlCount        s              9s 0                                      Count
     D QvlRwNmr        s              9s 0                                      Count
     D QvlRowStr       s              9s 0                                      Row start
     D QvlRowEnd       s              9s 0                                      Row end
     D QvlMaxRow       s              9s 0                                      Max Row
      *=============================================================================================

       // Trova limiti paginazione
       QvlRowStr = GetAtrN(QvgFrm:'h50':'xrowstr');
       QvlRowEnd = GetAtrN(QvgFrm:'h50':'xrowend');
       QvlMaxRow = %dec(g(QvgFrm:'h10maxrow'):9:0);

       // Inz grid
       InzRows(QvgFrm:'h50');
       QvlRwNmr = 0;

       // Carica grid
       for QvlCount = QvlRowStr to QvlRowEnd;

          if QvlCount > %elem(Qdgh50) or QvlCount > QvlMaxRow;
             leave;
          endif;

          QvlRwNmr += 1;
          QdgFrm.h50RwNmr = QvlRwNmr;
          QdgFrm.h50RwNmrD = QvlCount;
          eval-corr QdgFrm = Qdgh50(QvlCount);

          // Scrive riga
          FrmWriteAdd('h50');

       endfor;

      *=============================================================================================
     PWrthg0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================

      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf1:
     PCnthf1           B
     DCnthf1           PI
      *=============================================================================================

      *=============================================================================================
     PCnthf1           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf2:
     PCnthf2           B
     DCnthf2           PI
      *=============================================================================================

      *=============================================================================================
     PCnthf2           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthg0:
     PCnthg0           B
     DCnthg0           PI

     D QvlPagRowNmr    s              9s 0                                      Page Row number
     D QvlCount        s              9s 0                                      Row start
      *=============================================================================================

       // Controllo button

       // Lettura change
       if isChg(Qvgfrm:'h50') <> *on;
          return;
       endif;

       QvlPagRowNmr = GetAtrN(QvgFrm:'h50':'xpagrownmr');
       for QvlCount = 1 to QvlPagRowNmr;

          if isChg(QvgFrm:'h50row':QvlCount:QvgMsgId) = *off;
             if QvgMsgId <> '';
                leave;
             endif;
             iter;
          endif;

          QdgFrm.h50RWNMR = QvlCount;

          if FrmRead('h50') = 'not found';
             leave;
          endif;

          // Aggiorna record set
          eval-corr Qdgh50(QdgFrm.h50rwnmrd) = QdgFrm;

          // Aggiorna grid
          QdgFrm.h50RWNMR = QvlCount;
          FrmWrite('h50');

       endfor;

      *=============================================================================================
     PCnthg0           E
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
       if QprExpTag = 'lodhg0_start';
          eval-corr QdgFrm = Qdgh50Rcd;
       endif;

       QvlFrmPtr = %addr(QdgFrm);
       return ExpDpt(QsdNmPrgr:QvgFrm:QvlFrmPtr:QprExpTag:QvgExpRgs);

      *=============================================================================================
     PExpExc           E
      **********************************************************************************************
      /include qsbr,IncFncFrm                            