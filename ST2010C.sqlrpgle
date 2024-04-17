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
     Fst2010v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgTxtWhr        s            100a   varying                              Txt. where

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(st2010v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)

     D* Grid h50
     DQdgh50Rcd        ds                  qualified inz
     D h50cdcf                             like(QdtFrmDef.h50cdcf       )
     D h50rgs1                             like(QdtFrmDef.h50rgs1       )
     D h50lclt                             like(QdtFrmDef.h50lclt       )
     D h50prvn                             like(QdtFrmDef.h50prvn       )
     D h50nzne                             like(QdtFrmDef.h50nzne       )
     D h50dtan                             like(QdtFrmDef.h50dtan       )

     DQdgh50           ds                  likeds(Qdgh50Rcd) dim(10000)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg
     D/COPY QSBR,RgtRgsDsf
     D/COPY QSBR,strexp

      * MAIN PROCEDURE
     Dst2010c          PI
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

       // Load hf0 Filtri
       lodhf0();

       // Load hg0
       lodhg0();
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
          // ricarica Clienti/fornitori
          lodhg0();
          rstgrdpge(QvgFrm:'h50');
          wrthg0();
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

      *=============================================================================================
     PLodhf0           E
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
                           cfcdcf, +
                           cfrgs1, +
                           cflclt, +
                           cfprvn, +
                           cfnzne, +
                           cfdtan +
                      from angcf00f +
                    :where ' + QvgTxtWhr+ ' +
                    :order cfcdcf +
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
     D QvlNmrQry       s              6s 0                                      Num. risultati query
     D Qvlcdcf         s              6s 0                                      Codice cliente forni
      *=============================================================================================
       // Inz. atr.
       addatr(QvgFrm:'h50btnfll':'class':'hidden');

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


          if QdgFrm.h50dtan = 20991231;
             QdgFrm.h50dtan = 0;
          endif;

          //Scrive riga
          FrmWriteAdd('h50');

          // disabilita selezione se data annullamento minore della data odierna
          if QdgFrm.h50dtan <> 0 and QdgFrm.h50dtan < %dec(%date());
             setatr(QvgFrm:'h50btnslz':'disabled':'disabled':QvlRwNmr);
             addatr(QvgFrm:'h50dtan':'style':'color:red':QvlRwNmr);
          else;
             setatr(QvgFrm:'h50btnslz':'disabled':' ':QvlRwNmr);
             rmvatr(QvgFrm:'h50dtan':'style':'color:red':QvlRwNmr);
          endif;


          Qvlcdcf = QdgFrm.h50cdcf;

          // segnala presenza e numero filiali
             // rileva numero filiali
             QvlNmrQry = 0;
             exec sql select count(*)
                into  :QvlNmrQry
                from   angfl00f
                where  flcdcf= :Qvlcdcf
                group by flcdcf
                fetch first 1 rows only;

             if QvlNmrQry>0;
                // mostra semaforo filiali
                rmvatr(QvgFrm:'h50btnfll':'class':'hidden':QvlRwNmr);
                // assegna numero a tooltip
                setatr(QvgFrm:'h50btnfll':'title':%char(QvlNmrQry):QvlRwNmr);
             endif;

           // ordini cliente
             QvlNmrQry = 0;
             exec sql select count(*)
                into  :QvlNmrQry
                from   oclgt00f
                where  ctcclc= :Qvlcdcf
                group by ctcclc
                fetch first 1 rows only;

             if QvlNmrQry<=0;
                addatr(QvgFrm:'h50btnocl':'class':'hidden':QvlRwNmr);
             endif;

            // ordini fornitore
             QvlNmrQry = 0;

             exec sql select count(*)
                into  :QvlNmrQry
                from   ofrat00f
                where  atcdfr= :Qvlcdcf
                group by atcdfr
                fetch first 1 rows only;

             if QvlNmrQry<=0;
                addatr(QvgFrm:'h50btnofr':'class':'hidden':QvlRwNmr);
             endif;
       endfor;

      *=============================================================================================
     PWrthg0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0:
     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================
        // seleziona tutti
       if QdgFrm.hf0btnall = '*on';
           QvgTxtWhr = ' ';
           s(QvgFrm:'h10tle':'Business partners');
       endif;

       // seleziona clienti
       if QdgFrm.hf0btncln = '*on';
           QvgTxtWhr = ' and cfccgc <> '' '' ';
           s(QvgFrm:'h10tle':'Clienti');
       endif;

       // seleziona fornitori
       if QdgFrm.hf0btnfrn = '*on';
           QvgTxtWhr = ' and cfccgf <> '' '' ';
           s(QvgFrm:'h10tle':'Fornitori');
       endif;

      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthg0:
     PCnthg0           B
     DCnthg0           PI

     D QvlPagRowNmr    s              9s 0                                      Page Row number
     D QvlCount        s              9s 0                                      Row start
     D QvlTblNme       s            256a   varying                              Nome tabella
      *=============================================================================================

       // Controllo button
       //esportazione in xls
       if QdgFrm.H10BTNXLS='*on';
         QvlTblNme = RgtRgsDsf(50:'ST2010V':'h50':'+
                                                   h50cdcf,+
                                                   h50rgs1,+
                                                   h50lclt,+
                                                   h50prvn,+
                                                   h50nzne,+
                                                   h50dtan');

          QvlCount = %dec(g(QvgFrm:'h10maxrow'):9:0);
          QdgRtc = strExp(%addr(QdgH50):%size(QdgH50):QvlCount:QvlTblNme);

          Clear Qvgprminp;
          Qvgprminp=ap(Qvgprminp:'qprtagname':'h50':'');
          Qvgprminp=ap(Qvgprminp:'qprstoragekey':QvlTblNme:'');
          Qvgprminp=ap(Qvgprminp:'qprstoragetype':'db':'');
          Qvgprminp=ap(Qvgprminp:'qprexporttype':'xlsx':'');
          FrmRemoteCall('':'ExportGrid':QvgPrmInp);

       endif;

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

          if QdgFrm.H50btnSlz = '*on';
             clear QvgPrmInp ;
             QvgPrmInp = ap(QvgPrmInp:'qprcodclfr':%char(QdgFrm.H50cdcf):' ');
             QvgFrmCnl='ST2011C';
             FrmCnl(QvgPrmInp);
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