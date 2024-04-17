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
     Fst2011v   CF   E             WORKSTN handler('FRMSRV(FRMHND)':QvgFrm)

      * Info
     D/COPY QSBR,IncInfPgm
     D/COPY QSBR,IncDfnGen
     D/COPY QSBR,IncDfnFrm
     D/COPY QSBR,IncDfnPut

      * Parametri dinamici (Qpr*)
     DQprCodClfr       s              6s 0                                      Cod. cln./frn.

      * Variabili globali
     DQvgFrm           s         500000a   varying
     DQvgTmeStp        s               z
     DQvgnmgl          s             15a
     DQvgrgs1          s             35a
     DQvgrgs2          s             35a
     DQvgindr          s             35a
     DQvgcap           s              5a
     DQvglclt          s             30a
     DQvgprvn          s              2a
     DQvgnzne          s             20a
     DQvgpiva          s             16a
     DQvgwww           s             50a
     DQvgFlgRcr        s              2a   inz
     DQvgCodClfr       s              6s 0                                      Cod. cln./frn.
     DQvgbtnocl        s             50a   varying                              Riga di hf2btnocl
     DQvgbtnofr        s             50a   varying                              Riga di hf2btnofr

      * Form buffer
     DQdgFrm           ds                  likerec(FORM:*all)
     DQdtFrmDef      e ds                  extname(st2011v   :form) qualified
     D                                                              template

      * Strutture dati globali
     DQdgRtc           ds                  likeds(QdtRtc)
     DQdgPnv           ds                  likeds(QdtPnv)

     D* Grid h50
     DQdgh50Rcd        ds                  qualified inz
     D h50cdfl                              like(QdtFrmDef.h50cdfl       )
     D h50rgs1                              like(QdtFrmDef.h50rgs1       )
     D h50indr                              like(QdtFrmDef.h50indr       )
     D h50lclt                              like(QdtFrmDef.h50lclt       )
     D h50prvn                              like(QdtFrmDef.h50prvn       )
     D h50ntlf                              like(QdtFrmDef.h50ntlf       )

     DQdgh50           ds                  likeds(Qdgh50Rcd) dim(10000)

     D* Grid h51
     DQdgh51Rcd        ds                  qualified inz
     D h51evor                             like(QdtFrmDef.h51evor       )
     D h51nror                             like(QdtFrmDef.h51nror       )
     D h51aaor                             like(QdtFrmDef.h51aaor       )
     D h51dtor                             like(QdtFrmDef.h51dtor       )
     D h51rfrm                             like(QdtFrmDef.h51rfrm       )
     D h51dtrc                             like(QdtFrmDef.h51dtrc       )

     DQdgh51           ds                  likeds(Qdgh51Rcd) dim(10000)

     D* Grid h52
     DQdgh52Rcd        ds                  qualified inz
     D h52ctof                             like(QdtFrmDef.h52ctof       )
     D h52ccof                             like(QdtFrmDef.h52ccof       )
     D h52evor                             like(QdtFrmDef.h52evor       )
     D h52nror                             like(QdtFrmDef.h52nror       )
     D h52aaor                             like(QdtFrmDef.h52aaor       )
     D h52dtrc                             like(QdtFrmDef.h52dtrc       )
     D h52dtpc                             like(QdtFrmDef.h52dtpc       )

     DQdgh52           ds                  likeds(Qdgh52Rcd) dim(10000)

      * Prototype

      * Function Prototype
     D/COPY QSBR,IncPrtFrm
     D/COPY QSBR,IncPrtJob
     D/COPY QSBR,SqlChkMsg

      * MAIN PROCEDURE
     Dst2011c          PI
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

      *InitFrm: Init form

     PInitFrm          B
     DInitFrm          PI             1a
      *=============================================================================================

       // Memorizza momento
       QvgTmeStp = %timestamp();

       clear QdgFrm;

       // Rileva profilo di navigazione da container
       QdgPnv = FrmGetPnv(QvgFrm:QdgRtc);

       // Rileva parametri in input
       QprCodClfr = gpn(QprStr:'qprcodclfr');

       // Assegna codice cliente/fornitore
       QvgCodClfr = QprCodClfr;

       // inz. atr
       addatr(QvgFrm:'hs0':'class':'hidden');
       addatr(QvgFrm:'hs1':'class':'hidden');
       addatr(QvgFrm:'hs2':'class':'hidden');

       //rilleva tag
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
       //ottieni riferimento alla riga di hf2btnocl
       Qvgbtnocl = GetPrn(QvgFrm:'hf2btnocl':'xtag="tr"');

       //ottieni riferimento alla cella di hf2btnocl
       //Qvgbtnocl = GetPrn(QvgFrm:'hf2codocl':'xtag="td"');

       //ottieni riferimento alla riga di hf2btnofr
       Qvgbtnofr = GetPrn(QvgFrm:'hf2btnofr':'xtag="tr"');

       //ottieni riferimento alla cella di hf2btnofr
       //Qvgbtnofr = GetPrn(QvgFrm:'hf2codofr':'xtag="td"');

      *=============================================================================================
     PRtvTag           E
      **********************************************************************************************
      **********************************************************************************************
      * LodFrm: Load form
     PLodFrm           B
     DLodFrm           PI
      *=============================================================================================


       // Imposta controllo
       s(QvgFrm:'hpract':'cnt');

       // Load hf0  Dati cliente/fornitore
       lodhf0();

       // Load hf2 Viste
       lodhf2();

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

       // Controllo hf0 Dati cliente/fornitore
       if isChg(Qvgfrm:'hf0') = *on;
          cnthf0();
          if QvgFlgRcr='si' ;
            Lodhf0();
          endif;
       endif;



       // Controllo hf2  Viste
       if isChg(Qvgfrm:'hf2') = *on;
          cnthf2();
       endif;

       // Controllo hg0 Filiali
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

       // Controllo hg1
       if isChg(QvgFrm:'hg1') = *on;
          cnthg1();
       endif;

       // Reload grid body
       if isErr(QvgFrm:'h51') = *off;

          // roll up / down / vertica scroll bar
          if FrmRll(QvgFrm:'h51') = *on;
             Wrthg1();
          endif;

          // reload for changed order or selection
          if isChg(QvgFrm:'h21') = *on;
             lodhg1();
             rstgrdpge(QvgFrm:'h51');
             wrthg1();
          endif;
       endif;

      // Controllo hg2
       if isChg(QvgFrm:'hg2') = *on;
          cnthg2();
       endif;

       // Reload grid body
       if isErr(QvgFrm:'h52') = *off;

          // roll up / down / vertica scroll bar
          if FrmRll(QvgFrm:'h52') = *on;
             Wrthg2();
          endif;

          // reload for changed order or selection
          if isChg(QvgFrm:'h22') = *on;
             lodhg2();
             rstgrdpge(QvgFrm:'h52');
             wrthg2();
          endif;
       endif;
      *=============================================================================================
     PCntFrm           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf0:  Dati cliente/fornitore
     PLodhf0           B
     DLodhf0           PI

      *=============================================================================================
        // decodifica cliente/fornitore


        exec sql select cfnmgl,  cfrgs1, cfrgs2, cfindr, cfcap,
                         cflclt, cfprvn, cfnzne, cfpiva, cfwww
                   into :Qvgnmgl,:Qvgrgs1, :Qvgrgs2, :Qvgindr, :Qvgcap,
                         :Qvglclt, :Qvgprvn, :Qvgnzne, :Qvgpiva, :Qvgwww
                   from angcf00f
                  where cfcdcf= :QvgCodClfr;

        QdgFrm.hf0cdcf = QvgCodClfr;
        QdgFrm.hf0nmgl = Qvgnmgl;
        QdgFrm.hf0rgs1 = Qvgrgs1;
        QdgFrm.hf0rgs2 = Qvgrgs2;
        QdgFrm.hf0indr = Qvgindr;
        QdgFrm.hf0cap = Qvgcap ;
        QdgFrm.hf0lclt = Qvglclt;
        QdgFrm.hf0prvn = Qvgprvn;
        QdgFrm.hf0nzne = Qvgnzne;
        QdgFrm.hf0piva = Qvgpiva;





      *=============================================================================================
     PLodhf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhf2:
     PLodhf2           B
     DLodhf2           PI

      * Variabili locali
     D QvlMatch        s              1a   inz                                  Record trovato
      *=============================================================================================

       // Controllo se presenti filiali
       exec sql select '1'
                into  :QvlMatch
                from   angfl00f
                where  flcdcf=:QprCodClfr
                fetch first 1 rows only;
        // Trovato
        if sqlstt= '00000';
           setatr(QvgFrm:'hf2btnfll':'disabled':' ');
        endif;
        if sqlstt= '02000';
           setatr(QvgFrm:'hf2btnfll':'disabled':'disabled');
        endif;

        // controllo se presenti ordini cliente
        clear QvlMatch;
        sqlstt='';
        exec sql select '1'
                into  :QvlMatch
                from   oclgt00f
                where  ctcclc=:QprCodClfr
                fetch first 1 rows only;
        // Trovato
        if sqlstt= '00000';
           rmvatr(QvgFrm:Qvgbtnocl:'class':'hidden');
        endif;
        if sqlstt= '02000';
           addatr(QvgFrm:Qvgbtnocl:'class':'hidden');
        endif;

        // controllo se presenti ordini fornitore
        clear QvlMatch;
        sqlstt='';
        exec sql select '1'
                into  :QvlMatch
                from   ofrat00f
                where  atcdfr=:QprCodClfr
                fetch first 1 rows only;
        // Trovato
        if sqlstt= '00000';
           rmvatr(QvgFrm:Qvgbtnofr:'class':'hidden');
        endif;
        if sqlstt= '02000';
           addatr(QvgFrm:Qvgbtnofr:'class':'hidden');
        endif;

        //controllo presenza della home page
        setatr(QvgFrm:'hf2link':'href':'');
        if Qvgwww<>'';                                              //RISOLTO
           setatr(QvgFrm:'hf2btnlnk':'disabled':'');                //tabella non ha record con cfww
           setatr(QvgFrm:'hf2link':'href':'http://'+Qvgwww);        //impostato caso di test in lodh
        else;
           setatr(QvgFrm:'hf2btnlnk':'disabled':'disabled');
        endif;


      *=============================================================================================
     PLodhf2           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhg0: Filiali
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
                           flcdfl, +
                           flrgs1, +
                           flindr, +
                           fllclt, +
                           flprvn, +
                           flntlf +
                      from angfl00f +
                    :where +
                       and flcdcf=' + %char(QvgCodClfr) + ' +
                    :order +
                           flcdfl +
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
      * Lodhg1:
     PLodhg1           B
     DLodhg1           PI

      * Variabili locali
     D QvlSqlStr       s                   like(QvtSqlStr)                      Form
     D QvlCount        s              9s 0                                      Count
     D QvlElm          s              9s 0                                      Max Elements grid
      *=============================================================================================

       // Rileva numero elementi massimo
       QvlElm = %elem(Qdgh51);

       // Costruisci interrogazione
       QvlSqlStr = 'select +
                           ctevor, +
                           ctnror, +
                           ctaaor, +
                           ctdtor, +
                           ctrfrm, +
                           ctdtrc +
                      from oclgt00f +
                    :where +
                           and ctcclc=' + %char(QvgCodClfr) + ' +
                    :order  ctaaor, ctnror +
                     fetch first :elem rows only +
                       for read only';

       // Valorizza numero massimo elementi
       QvlSqlStr = %scanrpl(':elem':%char(QvlElm):QvlSqlStr);
       // Build where condition
       QvlSqlStr = %scanrpl(':where':
                            BldWhr(QvgFrm:getatr(QvgFrm:'h21':'xwhrstr')):
                            QvlSqlStr);
       // Build order condition
       QvlSqlStr = %scanrpl(':order':
                            BldOrd(QvgFrm:getatr(QvgFrm:'h21':'xordstr')):
                            QvlSqlStr);

       // Esegui interrogazione database
       exec sql prepare hg1_prp from :QvlSqlStr;
       exec sql declare hg1_crs cursor for hg1_prp;
       exec sql open hg1_crs;
       SQLChkMsg(QvgFrm:SQLCODE:SQLSTT:QvlSqlStr);

       // Inz h51
       QdgFrm.h51RWNMR = 0;
       QdgFrm.h51RWNMRD= 0;
       clear Qdgh51Rcd;
       clear Qdgh51;

       // Inz conteggio e first record
       QvlCount = 0;

       // Ciclo di caricamento
       dou (%error);

          exec sql fetch next from hg1_crs
                   into :Qdgh51Rcd;

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
          Qdgh51(QvlCount) = Qdgh51Rcd;

       enddo;

       // Set max rows grid
       if QvlCount > QvlElm - 1;
          rmvAtr(QvgFrm:'h11rcs':'class':'hidden');
          s(QvgFrm:'h11maxrow':%char(QvlCount - 1));
       else;
          addAtr(QvgFrm:'h11rcs':'class':'hidden');
          s(QvgFrm:'h11maxrow':%char(QvlCount));
       endif;

       // Chiudi cursore
       exec sql close hg1_crs;

      *=============================================================================================
     PLodhg1           E
      **********************************************************************************************
      **********************************************************************************************
      * Lodhg2:
     PLodhg2           B
     DLodhg2           PI

      * Variabili locali
     D QvlSqlStr       s                   like(QvtSqlStr)                      Form
     D QvlCount        s              9s 0                                      Count
     D QvlElm          s              9s 0                                      Max Elements grid
      *=============================================================================================

       // Rileva numero elementi massimo
       QvlElm = %elem(Qdgh52);

       // Costruisci interrogazione
       QvlSqlStr = 'select +
                           atctof, +
                           atccof, +
                           atevor, +
                           atnror, +
                           ataaor, +
                           atdtrc, +
                           atdtpc +
                      from ofrat00f +
                    :where +
                       and atcdfr='+ %char(QvgCodClfr) +' +
                    :order ataaor desc, atnror desc +
                     fetch first :elem rows only +
                       for read only';

       // Valorizza numero massimo elementi
       QvlSqlStr = %scanrpl(':elem':%char(QvlElm):QvlSqlStr);
       // Build where condition
       QvlSqlStr = %scanrpl(':where':
                            BldWhr(QvgFrm:getatr(QvgFrm:'h22':'xwhrstr')):
                            QvlSqlStr);
       // Build order condition
       QvlSqlStr = %scanrpl(':order':
                            BldOrd(QvgFrm:getatr(QvgFrm:'h22':'xordstr')):
                            QvlSqlStr);

       // Esegui interrogazione database
       exec sql prepare hg2_prp from :QvlSqlStr;
       exec sql declare hg2_crs cursor for hg2_prp;
       exec sql open hg2_crs;
       SQLChkMsg(QvgFrm:SQLCODE:SQLSTT:QvlSqlStr);

       // Inz h52
       QdgFrm.h52RWNMR = 0;
       QdgFrm.h52RWNMRD= 0;
       clear Qdgh52Rcd;
       clear Qdgh52;

       // Inz conteggio e first record
       QvlCount = 0;

       // Ciclo di caricamento
       dou (%error);

          exec sql fetch next from hg2_crs
                   into :Qdgh52Rcd;

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
          Qdgh52(QvlCount) = Qdgh52Rcd;

       enddo;

       // Set max rows grid
       if QvlCount > QvlElm - 1;
          rmvAtr(QvgFrm:'h12rcs':'class':'hidden');
          s(QvgFrm:'h12maxrow':%char(QvlCount - 1));
       else;
          addAtr(QvgFrm:'h12rcs':'class':'hidden');
          s(QvgFrm:'h12maxrow':%char(QvlCount));
       endif;

       // Chiudi cursore
       exec sql close hg2_crs;

      *=============================================================================================
     PLodhg2           E
      **********************************************************************************************
      **********************************************************************************************
      * Wrthg0: Filiali
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
      * Wrthg1: ordini cliente
     PWrthg1           B
     DWrthg1           PI

      * Variabili locali
     D QvlCount        s              9s 0                                      Count
     D QvlRwNmr        s              9s 0                                      Count
     D QvlRowStr       s              9s 0                                      Row start
     D QvlRowEnd       s              9s 0                                      Row end
     D QvlMaxRow       s              9s 0                                      Max Row
     D QvlEvor         s              1a                                        Valore Evaso
     D QvlDsrc         s             35a                                        Descrizione flag
      *=============================================================================================

       // Trova limiti paginazione
       QvlRowStr = GetAtrN(QvgFrm:'h51':'xrowstr');
       QvlRowEnd = GetAtrN(QvgFrm:'h51':'xrowend');
       QvlMaxRow = %dec(g(QvgFrm:'h11maxrow'):9:0);

       // Inz grid
       InzRows(QvgFrm:'h51');
       QvlRwNmr = 0;

       // Carica grid
       for QvlCount = QvlRowStr to QvlRowEnd;

          if QvlCount > %elem(Qdgh51) or QvlCount > QvlMaxRow;
             leave;
          endif;

          QvlRwNmr += 1;
          QdgFrm.h51RwNmr = QvlRwNmr;
          QdgFrm.h51RwNmrD = QvlCount;
          eval-corr QdgFrm = Qdgh51(QvlCount);

          // Scrive riga
          FrmWriteAdd('h51');


          //applicazione colore evasione
          QvlEvor= Qdgh51(QvlCount).h51evor;

          if QvlEvor='0';
            setstyle(QvgFrm:'h51btnevor':'background-color':'red':QvlRwNmr);
          elseif QvlEvor='1';
            setstyle(QvgFrm:'h51btnevor':'background-color':'yellow':QvlRwNmr);
          else;
            setstyle(QvgFrm:'h51btnevor':'background-color':'green':QvlRwNmr);
          endif;

          //scrittura tooltip evasione
          exec sql select qfdscr
                into  :QvlDsrc
                from   qflag00f
                where  qfprfs='OCLGT00F' and qfidnt='CTEVOR' and qfflag=:QvlEvor
                fetch first 1 rows only;
          setatr(QvgFrm:'h51btnevor':'title':%char(QvlDsrc):QvlRwNmr);


       endfor;

      *=============================================================================================
     PWrthg1           E
      **********************************************************************************************
      **********************************************************************************************
      * Wrthg2:
     PWrthg2           B
     DWrthg2           PI

      * Variabili locali
     D QvlCount        s              9s 0                                      Count
     D QvlRwNmr        s              9s 0                                      Count
     D QvlRowStr       s              9s 0                                      Row start
     D QvlRowEnd       s              9s 0                                      Row end
     D QvlMaxRow       s              9s 0                                      Max Row
     D QvlEvor         s              1a                                        Valore Evaso
     D QvlCod          s              3a                                        Codice
     D QvlDsrc         s             35a                                        Descrizione
      *=============================================================================================

       // Trova limiti paginazione
       QvlRowStr = GetAtrN(QvgFrm:'h52':'xrowstr');
       QvlRowEnd = GetAtrN(QvgFrm:'h52':'xrowend');
       QvlMaxRow = %dec(g(QvgFrm:'h12maxrow'):9:0);

       // Inz grid
       InzRows(QvgFrm:'h52');
       QvlRwNmr = 0;

       // Carica grid
       for QvlCount = QvlRowStr to QvlRowEnd;

          if QvlCount > %elem(Qdgh52) or QvlCount > QvlMaxRow;
             leave;
          endif;

          QvlRwNmr += 1;
          QdgFrm.h52RwNmr = QvlRwNmr;
          QdgFrm.h52RwNmrD = QvlCount;
          eval-corr QdgFrm = Qdgh52(QvlCount);

          // Scrive riga
          FrmWriteAdd('h52');

          //tipo ordine fornitori

          QvlCod=Qdgh52(QvlCount).h52ctof;
          exec sql select tbdscr
                into  :QvlDsrc
                from   tbbse00f
                where  tbprfs='TOF' and tbctb1=:QvlCod
                fetch first 1 rows only;

          s(QvgFrm:'h52tof':QvlDsrc:QvlRwNmr);

          //causale ordine fornitori
          Clear QvlCod;
          Clear QvlDsrc;
          QvlCod=Qdgh52(QvlCount).h52ccof;
          exec sql select tbdscr
                into  :QvlDsrc
                from   tbbse00f
                where  tbprfs='COF' and tbctb1=:QvlCod
                fetch first 1 rows only;

          s(QvgFrm:'h52cof':QvlDsrc:QvlRwNmr);

          //applicazione colore evasione
          QvlEvor= Qdgh52(QvlCount).h52evor;

          if QvlEvor='0';
            setstyle(QvgFrm:'h52btnevor':'background-color':'red':QvlRwNmr);
          elseif QvlEvor='1';
            setstyle(QvgFrm:'h52btnevor':'background-color':'yellow':QvlRwNmr);
          else;
            setstyle(QvgFrm:'h52btnevor':'background-color':'green':QvlRwNmr);
          endif;

          //scrittura tooltip evasione

          Clear QvlDsrc;
          exec sql select qfdscr
                into  :QvlDsrc
                from   qflag00f
                where  qfprfs='OFRAT00F' and qfidnt='ATEVOR' and qfflag=:QvlEvor
                fetch first 1 rows only;

          setatr(QvgFrm:'h52btnevor':'title':%char(QvlDsrc):QvlRwNmr);

       endfor;

      *=============================================================================================
     PWrthg2           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf0: Dati cliente/fornitore

     PCnthf0           B
     DCnthf0           PI
      *=============================================================================================
      // richiama programma di manutenzione clienti/fornitori
       if QdgFrm.Hf0btnupd='*on';
          clear QvgPrmInp;
          QvgPrmInp = ap(QvgPrmInp:'qprcodclfr':%char(QdgFrm.hf0cdcf):' ');
          QvgFrmCnl='ST2012C';
          FrmCnl(QvgPrmInp);
          QvgFlgRcr=gp(QvgPrmInp:'qprflgsav');
       endif;

      *=============================================================================================
     PCnthf0           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthf2: Viste
     PCnthf2           B
     DCnthf2           PI
      *=============================================================================================
       // filiali
       if QdgFrm.Hf2btnFll='*on';
          rmvatr(QvgFrm:'hs0':'class':'hidden');
          addatr(QvgFrm:'hs1':'class':'hidden');
          addatr(QvgFrm:'hs2':'class':'hidden');
          Lodhg0();
          WrtHg0();
       endif;
       // ordini cliente
       if QdgFrm.Hf2btnocl='*on';
          addatr(QvgFrm:'hs0':'class':'hidden');
          rmvatr(QvgFrm:'hs1':'class':'hidden');
          addatr(QvgFrm:'hs2':'class':'hidden');
          Lodhg1();
          WrtHg1();
       endif;
       // ordini cliente
       if QdgFrm.Hf2btnofr='*on';
          addatr(QvgFrm:'hs0':'class':'hidden');
          addatr(QvgFrm:'hs1':'class':'hidden');
          rmvatr(QvgFrm:'hs2':'class':'hidden');
          Lodhg2();
          WrtHg2();
       endif;
      *=============================================================================================
     PCnthf2           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthg0: Filiali
     PCnthg0           B
     DCnthg0           PI

     D QvlPagRowNmr    s              9s 0                                      Page Row number
     D QvlCount        s              9s 0                                      Row start
      *=============================================================================================

       // Nuova filiale
       if QdgFrm.H10BTNADD='*on';
         clear QvgPrmInp ;
         QvgPrmInp = ap(QvgPrmInp:'qprcodfll':'0':' ');
         QvgPrmInp = ap(QvgPrmInp:'qprcodclfr':%char(QdgFrm.HF0CDCF):' ');
         QvgFrmCnl='ST2014C';
         FrmCnl(QvgPrmInp);
         QvgFlgRcr=gp(QvgPrmInp:'qprflgsav');
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

          // manutenzione filiale
          if QdgFrm.H50BTNSLZ='*on';
            clear QvgPrmInp ;
            QvgPrmInp = ap(QvgPrmInp:'qprcodfll':%char(QdgFrm.H50CDFL):' ');
            QvgPrmInp = ap(QvgPrmInp:'qprcodclfr':%char(QdgFrm.HF0CDCF):' ');
            QvgFrmCnl='ST2014C';
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
      * Cnthg1:
     PCnthg1           B
     DCnthg1           PI

     D QvlPagRowNmr    s              9s 0                                      Page Row number
     D QvlCount        s              9s 0                                      Row start
      *=============================================================================================

       // Controllo button

       // Lettura change
       if isChg(Qvgfrm:'h51') <> *on;
          return;
       endif;

       QvlPagRowNmr = GetAtrN(QvgFrm:'h51':'xpagrownmr');
       for QvlCount = 1 to QvlPagRowNmr;

          if isChg(QvgFrm:'h51row':QvlCount:QvgMsgId) = *off;
             if QvgMsgId <> '';
                leave;
             endif;
             iter;
          endif;

          QdgFrm.h51RWNMR = QvlCount;

          if FrmRead('h51') = 'not found';
             leave;
          endif;

        if QdgFrm.H51BTNSLZ = '*on';
             clear QvgPrmInp ;
             QvgPrmInp = ap(QvgPrmInp:'qpraaor':%char(QdgFrm.H51aaor):' ');
             QvgPrmInp = ap(QvgPrmInp:'qprnror':%char(QdgFrm.H51nror):' ');
             QvgFrmCnl='ST2015C';
             FrmCnl(QvgPrmInp);
          endif;

          // Aggiorna record set
          eval-corr Qdgh51(QdgFrm.h51rwnmrd) = QdgFrm;

          // Aggiorna grid
          QdgFrm.h51RWNMR = QvlCount;
          FrmWrite('h51');

       endfor;

      *=============================================================================================
     PCnthg1           E
      **********************************************************************************************
      **********************************************************************************************
      * Cnthg2:
     PCnthg2           B
     DCnthg2           PI

     D QvlPagRowNmr    s              9s 0                                      Page Row number
     D QvlCount        s              9s 0                                      Row start
      *=============================================================================================

       // Controllo button

       // Lettura change
       if isChg(Qvgfrm:'h52') <> *on;
          return;
       endif;


       QvlPagRowNmr = GetAtrN(QvgFrm:'h52':'xpagrownmr');
       for QvlCount = 1 to QvlPagRowNmr;

          if isChg(QvgFrm:'h52row':QvlCount:QvgMsgId) = *off;
             if QvgMsgId <> '';
                leave;
             endif;
             iter;
          endif;

          QdgFrm.h52RWNMR = QvlCount;

          if FrmRead('h52') = 'not found';
             leave;
          endif;
        if QdgFrm.H52BTNSLZ = '*on';
             clear QvgPrmInp ;
             QvgPrmInp = ap(QvgPrmInp:'qpraaor':%char(QdgFrm.H52aaor):' ');
             QvgPrmInp = ap(QvgPrmInp:'qprnror':%char(QdgFrm.H52nror):' ');
             QvgFrmCnl='ST2016C';
             FrmCnl(QvgPrmInp);
          endif;

          // Aggiorna record set
          eval-corr Qdgh52(QdgFrm.h52rwnmrd) = QdgFrm;

          // Aggiorna grid
          QdgFrm.h52RWNMR = QvlCount;
          FrmWrite('h52');

       endfor;

      *=============================================================================================
     PCnthg2           E
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
