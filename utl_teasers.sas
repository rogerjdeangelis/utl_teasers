===================================================================================================================================
TEASERS
===================================================================================================================================

  /* T000006 DELETE A MACRO (OR ANY CATALOG ENTRY) IF IT EXISTS

   %macro deleteme(dummy); roger %mend deleteme;

   %sysfunc(ifc(%sysfunc(cexist(work.sasmacr.deleteme.macro)),
        %nrstr(proc catalog cat=work.sasmacr;delete deleteme/et=macro;quit;)
       ,%nrstr(%put NoEntry;)));

   see T005480 DELETE A MACRO (OR ANY CATALOG ENTRY) IF IT EXISTS WITH BELLS AND WHISTLES


  /* T000007 VERY CLEVER METHOD TO SEQUENCE RECORDS WITHIN A BY GROUP - BY SASPROGRAMMER
     data xplus;
        retain visit (1);
        set x;
        by patient;
        visit=ifn(first.patient,1, visit+1);
     run;

  /* T000008 ONELINER TO CREATE LAB DATE TIME VARIABLE - STEPHEN HUNT
      labdatetime=dhms(labdate,max(0,hour(labtime)),max(0,minute(labtime)),0);

  /*  T002800 EASIEST WAY TO CREATE A CSV COMMA DELIMITER FILE OR SAS DATASET FROM CSV FILE */
    dm "dexport sashelp.class 'c:\temp\tmp.csv' replace";
    dm "dimport 'c:\temp\tmp.csv' class  replace";

  /*  T000004 DELETE A MACRO VARIABLE IF IT EXISTS
      %sysfunc(ifc(%symexist(pgm),%nrstr(%symdel pgm;),%nrstr(%put Does not exist;)));

  /* QUOTE A LIST OF NAMES */
  proc sql;select quote(strip(name)) into :quolst separated by ','  from sashelp.class(obs=5);quit;
     %put &quolst;  /* "Alfred","Alice","Barbara","Carol","Henry"  */

  /* T004925 PRINT A DATASET WITHOUT HEADINGS

    proc print data=sashelp.class split='*';
     attrib _all_ label="*";
    run;

  /* T004940 TWO IMPLEMENTATIONS OF THE LEAD FUNCTION IN SAS
   data class;
     set sashelp.class;
     lead=_n_+1;
     set sashelp.class(keep=name rename=name=leadname) point=lead;
     put name leadname;
   run;

   data class;
     merge  sashelp.class sashelp.class(firstobs=2
        rename=name=nextname keep=name);
   run;

  /* T000012 PRINT OB VERTICALLY */
  data; set sashelp.class(obs=1); put (_all_) (=/);run;
     Name=Alfred
     Sex=M
     Age=14
     Height=69
     Weight=112.5

  /* T000013 PUTLOG IN A PROC REPORT COMPUTE BLOCK SEENUM AND SEECHAR FUNCTIONS VERY USEFUL FOR DEBUGGING */
  proc report data=sashelp.class nowd list;
    columns _all_;
    compute weight;
       _n_ + 1;
       rcN = seenum(_n_,          'NOTE: OBS');
       rcN = seenum(weight.sum,'      WEIGHT');
       rcC = seechar(name,'             NAME');
       endcomp;
    run;quit;

  /* T000014 RARELY USED NAMED, LIST AND WRAP OPTIONS IN PROC REPORT - USEFUL TO GENERATE DEFINE STATEMENTS */
   /* IMPROVEMENTS BY TOM ABERNATHY */
   proc report data=sashelp.class nowd named list wrap;
     define _all_ / display;
   run;quit;
   /* YOU MAY WANT TO EXPERIMENT WITH COMBINATIONS OF THESE OPTIONS */
   proc report data=sashelp.class nowd named list wrap;
     columns _all_;
   run;quit;
   /* HERE IS WHAT IS IN THE LOG - WRAP MAY BE USEFUL FOR SEGMENTED REPORTS */
   PROC REPORT DATA=SASHELP.CLASS LS=171 PS=58  SPLIT="/" WRAP NAMED CENTER ;
   COLUMN  ( Name Sex Age Height Weight );
   DEFINE  Name / DISPLAY FORMAT= $8. WIDTH=8     SPACING=2   LEFT "Name" ;
   DEFINE  Sex / DISPLAY FORMAT= $1. WIDTH=1     SPACING=2   LEFT "Sex" ;
   DEFINE  Age / SUM FORMAT= BEST9. WIDTH=9     SPACING=2   RIGHT "Age" ;
   DEFINE  Height / SUM FORMAT= BEST9. WIDTH=9     SPACING=2   RIGHT "Height" ;
   DEFINE  Weight / SUM FORMAT= BEST9. WIDTH=9     SPACING=2   RIGHT "Weight" ;
   RUN;

   /* T000015 SELECT CLAUSES ON CASE STATEMENT */
   Proc Sql;
    /* Set Age to 1 if Age > then overall mean Age */
    select Name, Sex, Age,
       Case
         When Age > (Select Mean(Age) from sashelp.class) Then 1
         Else 0
       End as Age_Indicator
    from sashelp.class;
   quit;

  /* T000016 MACRO IN OPERATOR */
  /*9.2*/ %macro inop/minoperator; %let find=ROGER; %if &find in ROGER MARY GEORGE %then %put ROGER is there; %mend inop; %inop;
     ROGER is there
     options minoperator;%macro t(x=a)/minoperator;%if &x in (a b c) %then %put *&x*;%mend;%t;


  /*  T000022 DYNAMIC LIBNAME */
  libname winunx "%sysfunc(ifc(&sysscp=WIN,\\filesrv04,%str()))/dir1/misc/regusers";

  /*  T000011 DYNAMIC LIBNAME */
  /* MAGIC STRING _ GUARANTEED TO CLEAN UP ANY DISPLAY MANAGER FREEZE - MANUALLY TYPE INTO FUNCTION KEY*/
  ;;;;/*'*/ *);*};*/;/* " */;%mend;run;quit;%end;end;run;endcomp;%utl_fixopt;

  * put this in your autocall library;
  %macro utl_fixopt(dum);
    * fix frozen sas and restore to invocation;
    ods listing;
    proc printto;run;
    goptions reset=all;
    ods select _all_;
    proc optload data=sasuser.optsave;
    run;quit;
  %mend utl_fixopt;

  * put this in your autoexec;
  proc options out=sasuser.optsave;run;quit;

  /* T000018 WHY ISN'T THE CUBE ROOT OF A CUBE EQUAL TO THE SOURCE NUMBER */
  %macro utl_eqfuzz(var1, var2, fuzz=1e-12); . < abs(&var1 - &var2) < &fuzz %mend utl_eqfuzz;
  %macro utl_fuzz(var1, var2); (abs(fuzz(&var1) - fuzz(&var2))) = 0 %mend utl_fuzz;
  data;x=13.1;cube=x**3;cuberoot=cube**(1/3);if cuberoot=13.1 then put "EXPECTED RESULT EQUALITY";else put "UNEXPECTED";run;
  UNEXPECTED
  data;x=13.1;cube=x**3;cuberoot=cube**(1/3);if %utl_eqfuzz(cuberoot,13.1) then put "EXPECTED RESULT EQUALITY";else put "UNEXPECTED";run;
  EXPECTED RESULT EQUALITY

  /* T000019 NUMBER OF WORDS IN A STRING */
  data;NumWrd=countw("ROGER MARY JOSEPH");put NumWrd=;run; /* NUMWRD=3 */

  /* CREATING MANY EXCEL SHEETS IN UNIX */
  filename tagset http 'http://support.sas.com:80/rnd/base/topics/odsmarkup/excltags.tpl';
  %include tagset / nosource2;
  proc sort data=sashelp.class out=class;by sex;run;
  ods tagsets.excelxp file="/home/regusers/local/utl/xls/&sysuserid._t000060.xls" options(sheet_interval='bygroup');
     Proc print data=class label noobs;By sex;run;
  ods tagsets.excelxp close;

  /* COMPARE TWO FILES YOU SHOULD BE ABLE TO RUN THIS UNIX */
  x "/usr/bin/rm -f /home/regusers/local/utl/inp/t000390_tlgdif.txt";
  x "diff -s /home/regusers/local/utl/inp/t000390_tlgold.txt /home/regusers/local/utl/inp/t000390_tlgnew.txt > /home/regusers/local/utl/inp/t000390_tlgdif.txt";

  /* SAS 9.2 GETTING THE NAME OF THE INTERACTIVE PROGRAM AND PATH BATCH AND INTERACTIVE */
  /* ONLY WORKS IN SINGLE PLATFORM SAS EDITORS */
   * For file name of SAS program;      : %put %sysget(SAS_EXECFILENAME);
   * For path name where program located: %put %sysget(SAS_EXECFILEPATH);
   * Batch                              : %put %sysfunc(getOption(sysin));

/* T000030 NUMBER OF LOGICAL OBSERVATIONS(NO WHERE CLAUSE) IN A DATASET USE THIS FOR GEETING OBS*/
   %macro utl_nlobs(dsn);
     %let dsid=%sysfunc(open(&dsn));%sysfunc(attrn(&dsid,nlobs)) %let rc=%sysfunc(close(&dsid));
   %mend utl_lnobs;
   %put *** sashelp.class has %utl_nlobs(sashelp.class) logical observations ***;

/* T000040 LINING UP THE DECIMAL IN PROC REPORT (EVEN WORKS WITH CHAR VARS)
   style={cellwidth=32%  just=c pretext="\qj\tqdec\tx500 "} */

/* T000050 ACHME DATE AND TIME STRING  */
   %put %substr(%sysfunc(putn(%sysfunc(datetime()),datetime22.)),1,15);

/* T000060 USING THE EXCELXP TAGSET TO CREATE MANY EXCEL SHEETS ON UNIX ->/home/regusers/local/utl/xls/t000060.xls */
   filename tagset http 'http://support.sas.com:80/rnd/base/topics/odsmarkup/excltags.tpl';
   %include tagset / nosource2;
   proc sort data=sashelp.class out=class;by sex;run;
   ods tagsets.excelxp file="/home/regusers/local/utl/xls/&sysuserid._t000060.xls" options(sheet_interval='bygroup');
      Proc print data=class label noobs;By sex;run;
   ods tagsets.excelxp close;

/* T000070 NUMBER OF WORDS IN A MACRO STRING (OR DATASTEP VARIABLE) */
   %let String=This is the first day of the rest of your life;
   %put ** Number of words equal %sysfunc(countw(&String))  ***;
   data;NumWrd=countw("&String");put NumWrd=;run;

/* T000080 WHY NOT USE PERCENTS FOR CELLWIDTHS IN PROC REPORT (BUG IN SAS CANNOT SUM TO 100 EVEN WITH 0 CELL and BORDER OPTIONS ) */
   ods rtf file="/home/regusers/local/utl/rtf/&sysuserid._t000070.rtf" style=utl_rtflan100;
   ods escapechar='^';
   ods rtf prepage="^S={outputwidth=100% just=c font_size=11pt font_face=arial} {\tc Title line} ^R/RTF'\line' {(Safety Set)}";
   proc report data=sashelp.class(obs=15) missing split='#' nowd;
   cols name sex age height weight;
   define name   / display "\q\li180 {Patients Name}" style(column)={just=l cellwidth=30% pretext="\li180"};
   define sex    / display "\qc {Gender}    "  style(column)={just=c cellwidth=17%};
   define age    / display "\qc {Age#(years)}" style(column)={just=c cellwidth=17%};
   define height / display "\qc {Weight#(kg)}" style(column)={just=c cellwidth=17%};
   define weight / display "\qc {Height#(cm)}" style(column)={just=c cellwidth=17%};
   run;quit;
   ods rtf text="^S={outputwidth=100% just=r font_size=9pt} Page 1 of 1";
   ods rtf text="^S={outputwidth=100% just=l font_size=8pt font_style=italic}  {Footnote2}";
   ods rtf text="^S={outputwidth=100% just=l font_size=8pt font_style=italic}  {Footnote3}";
   ods rtf close;

/* T000090 QUOTE EACH WORD IN A MACRO OR VARIABLE STRING */
   %let String=This is the first day of the rest of your life;
   %put ** Quoted words = "%sysfunc(tranwrd(&String,%str( ),%str(" ")))"  *****;
   data;QuoStr=cats('"',tranwrd("&string",' ','","'),'"');put QuoStr=;run;
   proc sql noprint ; select distinct quote(name) into : names separated by ' ' from sashelp.class; quit;

/* T000100 ACHME FORMATTED CONFIDENCE INTERVAL */
   %macro utl_midlwrupr(Mid,Lwr,Upr,format=4.1) / des="create ACHME standard confidence interval";
      compbl(put(&Mid,&format -r)!!' ('!!compress(put(&Lwr,&format -l)!!',')!!' '!!put(&Upr,&format -r)!!')');
   %mend midlwrupr;
   data;Int=%Utl_MidLwrUpr(50,10,90);put Int=;run;
   data;Int=%Utl_MidLwrUpr(.50,.10,.90,format=percent5.1);put Int=;run;

/* T000118 DOES THE STRING CONTAIN 'MAY' or 'STEVE' (FASTER IF PATTERN IS COMPILED) */
   data x;x="ROGER MAY PENG"; output; x="GEORGE"; output; x="STEVE MAY"; output; run;
   data;set x;if prxmatch('/MAY|STEVE/',x)>0 then put x=;run;

/* T000120 ROTATED COLUMN HEADERS IN PROC REPORT
   http://support.sas.com/resources/papers/proceedings09/223-2009.pdf */

/* T000125 EASY WAY TO CREATE BIG N FOR HEADERS IN REPORTS */
   proc sql;select resolve(catx(' ','%let',trt,'=%str(',trt,'/line N =',put(count(pat),2. -l),');')) from utlinp.T000120_demog group by trt;quit;
   %put EPO=&EPO;           /* PLACEBO /line  N =  6 */
   %put PLACEBO=&PLACEBO;   /* EPO= /line N = 14 */

/* T000150  BINARY COPY BETTER THAN FTP BINARY WHEN COPYING FROM PC/UK UNIX/MAINFRAME */
    %macro BinCop(in=MAINFRAME.SAS.BIN,out=c:\pis\bne.xls) / des="Copy a binary file using sas";
    data _null_;
      infile "&in"  recfm=n;
      file   "&out" recfm=n;
      input byt $char1. @@;
      put   byt $char1. @@;
    run;
    %mend BinCop;
    %bincop;

/* T000170 EXACT NUMBER OF MONTHS BETWEEN TWO DATES */
   data dte;fdosedt=13897;studyday=706;eosdt=14213;
   months=floor((intck('month',fdosedt,fdosedt+studyday)- (day(fdosedt+studyday) < day(fdosedt))));put months=;
   months=floor((intck('month',fdosedt,eosdt)- (day(eosdt) < day(fdosedt))));put months=;run;

/* T000180 CREATING AN EMPTY SAS DATASET  * EACH METHOD HAS DIFF PROPERTIES                                */
   data shell; length x 8 name sex $16; call missing(of _all_); stop; run;  /* call missing elims warniing */
   data shell(drop=age); stop; set sashelp.class sashelp.shoes; AgeChar=put(age,z2.); run;
   proc sql; create table t1( id int, ic varchar(10), icd varchar(500), Idca varchar(500) ) ; quit;
   proc sql; create table new like sashelp.class; quit;

/* T000220 EXISTENTIAL SQL UPDATES
    FILLING IN MISSING SEX FOR MALES IN SASHELP.CLASS */
    data sexmis; set sashelp.class;if sex='M' then sex='';run;
    proc sql;update sexmis as mis
      set  sex=(select sex from sashelp.class as help where sexmis.name=help.name)
      where exists (select 1 from sashelp.class as help where mis.sex is null and sexmis.name=help.name)
    ;quit;

/* T000225 CONVERTING ALL EXCEL CELLS TO CHARACTER  */
    Latest VBA macros Addticks - forces all excel cells to be character
    before importing excel sheets to SAS this macro should be used

   /* this will stop when cell has TIUQ which is quit backwards */
   /* this is probably the best macro */
   Sub AddTicks()
    Dim LastPlace, Z As Variant, X As Variant
    LastPlace = ActiveCell.SpecialCells(xlLastCell).Address
    ActiveSheet.Range(Cells(1, 1), LastPlace).Select
    Z = Selection.Address   'Get the address
        For Each X In ActiveSheet.Range(Z)  'Do while
            If Len(X) > 0 Then      'Find cells with something
                X.FormulaR1C1 = Chr(39) & Mid(X, 1, Len(X))  '39 is code for tick
            Else
                X.FormulaR1C1 = ""  'If empty do not put tick
            End If
            If X = "TIUQ" Then
                Exit Sub
            End If
        Next
   End Sub

/* T000226 REMOVES TICKS FROM EXCEL CELLS  */
    Sub RemoveTicks()
    For Each currentcell In Selection
        If currentcell.HasFormula = False Then
            'Verifies that procedure does not change the
            'cell with the active formula so that it contains
            'only the value.
            currentcell.Formula = currentcell.Value
        End If
    Next
    End Sub

/* T000240 DOW LOOP PUT THE MEAN WEIGHT FOR MALES ON EACH MALE RECORD AND THE MEAN WEIGHT OF FEMALES ON EACH FEMALE RECORD */
   proc sort data=sashelp.class out=sex; by sex; run;
   data avgsex;
     do until (last.sex);
        set sex;
        by sex;
        wgt+weight;
        n+1;
     end;
     avgwght=wgt/n;
     do until(last.sex);
       set sex;
       by sex;
       output;
     end;
     n=0;
     wgt=0;
   run;
   proc print data=avgsex width

/* T000250 IS THE EMAIL ADDRESS VALID                                                                                    */
   %let regex='/^([a-zA-Z0-9_\+\-\.]+)@([a-zA-Z0-9_\+\-\.]+)\.([a-zA-Z]{2,5})$/';
   data;if prxmatch(&regex.,trim('regusers@^%&$-@@ACHME.com'))>0 then put 'email address is ok';else put "Not Ok";run;

/* T000310 CUT AND PASTE SYMBOLS FOR RTF AND GRAPHICS */
   %Let Lele=%Str(«);
   %Let Degr=%Str(°);
   %Let PlMi=%Str(±);
   %Let Sup1=%Str(¹);
   %Let Sup2=%Str(²);
   %Let Sup3=%Str(³);
   %Let Mico=%Str(µ);
   %Let Quar=%Str(¼);
   %Let Half=%Str(½);
   %Let Qua3=%Str(¾);

/* T000375  COMPARE TWO FILES YOU SHOULD BE ABLE TO RUN THIS <UNIX AND WINDOWS> */
  UNIX
  x "/usr/bin/rm -f /home/regusers/local/utl/inp/t000390_tlgdif.txt";
  x "diff -s /home/regusers/local/utl/inp/t000390_tlgold.txt /home/regusers/local/utl/inp/t000390_tlgnew.txt > /home/regusers/local/utl/inp/t000390_tlgdif.txt";

  WINDOWS
  %let cmd=%sysfunc(compbl('command/c fc "\\usal-home\regusers\local\utl\inp\t000390_tlgold.txt"
                                         "\\usal-home\regusers\local\utl\inp\t000390_tlgnew.txt" >
                                         "\\usal-home\regusers\local\utl\inp\t000390_tlgchg.txt"'));
  %put cmd=&cmd;
  x &cmd;
  data _null_;length cmd $350;cmd=&cmd;call system(cmd);run;

/* T000400 UNIX READING A ZIP FILE   */
   x "cd /home/regusers":
   x "setenv PWD /home/regusers";
   %put %sysget(PWD);
   x "gzip rdata.txt";
   filename pipes pipe "gunzip -cd rdata.txt.gz";
   data test;
     infile pipes;
     input;
     put _infile_;
   run;

/* T000430 CHECK TO DETERMINE IF A FILE IS EMPTY -> 1 if EMPTY */
   data _null_ ;
    infile "/home/regusers/local/utl/inp/t000430_empty.txt" end = empty ;
    call symputx("empty",put(empty,1.)); /* 1 if empty */
   run;
   %put %sysfunc(ifc(&empty,file is empty,file is not empty));;

/* T000460 WINDOWS CHECK THE DIRECTORY STUCTURE */
   filename pipetree pipe 'tree "\\usal-home/regusers/local/utl" /F /A' lrecl=5000;
   data _null_;
   infile pipetree truncover;
   input dirlist $char1000.;
   if index(dirlist,'.') =0;
   put dirlist;
   run;
   +---createdata
   |   |
   |   +---history
   |   +---validation
   |   |   |
   |   |   \---document
   |   |       |
   |   |       \---history
   |   \---output
   |

/* T000505 AUTOMATICALLY ALLOCATING A FILE BASED ON WHICH OPERATING SYSTEM IS ACTIVE
/*  GOOD IF YOU ARE USING NETWORK APPLIANCE
    libname winunx "%sysfunc(ifc(&sysscp=WIN,//filesrv04,%str()))/dir1/misc/regusers";

/* T000660 SAS 9.2 INSERT BLANKLINE EVERY 5 RECORDS - NEW IN PROC PRINT   */
   SAS 9.2 insert blankline every 5 records - new in proc print
   proc print data=sashelp.class
   blankline=5 ;
   run;

/* T000670 SAS 9.2 IGNORE CASE ON SORT     */
   proc sort data=maps.names out=french sortseq=linguistic (strength=primary) ;
   where Territory contains "France";
   by Territory Name ;
   run;

/* T000680 SAS 9.2 SETTING SAS DATA SETS BASED ON A VARIABLE IN ANOTHER SAS DATASET */
   data combined;
     set sashelp.prdsal:(keep=country product year actual) indsname=inputdsn ;
     length Source $ 32;
     Source=scan(inputdsn,2);
   run;

/* T000690 SAS 9.2 USING MULTI CHAACTER SEPARATORS */
   * string separataor
   data stringdelim;
   infile datalines dlmstr="{sep}" dlmsopt="I";
   input N1 N2 $ N3;
   datalines;
   123{sep}sep{sep}789
   0{Sep}ABCXYZ{SEP}456
   run;

/* T000700 SAS 9.2 NUMBER OF WORDS IN A STRING WORKS IN 9.1.3 */
   /* number of words in a string findw get the location of third word */
   data _null_;
     words=countw('the lazy brown fox jumped over the fence');
     put words=;
   run;

/* T000710 SAS 9.2 MACRO LANGUAGE IN OPERATOR */
   %macro Filter(ageparm) / minoperator;
      %if &ageparm in 11 12 13 14 15 16 %then %do;
           proc print data=sashelp.class;
             where age = &ageparm; title1 "Students of Age &ageparm";
           run;
   %end;
   %else %do;
       %put ERROR: No matching students.;
       %put ERROR- Valid ages are;
       %put ERROR- 11 12 13 14 15 16; %end;
   %mend Filter;

/* T000715 SAS 9.2 NESTING DEBUG OPTION ON DATASTEP   - EDUCATIONAL */

I discovered a new DATA statement argument new in V9.2 today.
The NESTING argument instructs SAS to print a note to the log
at the beginning and end of every DO/END and SELECT/END pairing.

This is jolly handy for debugging recalcitrant DATA steps with unbalanced nesting.
Usage:
 data newdata / nesting;
   do pat=1 to 3;
     do dte=13000 to 13005;
       val=uniform(-1);
     end;
   end;
 run;

/* T000716 9.2 and 9.1.3 SEARCH FOR A NUMERIC VALUE THAT IS EQUAL TO THE FIRST ARGUMENT AND RETURNS THE INDEX OF THE FIRST MATCHING VALUE */
data _null_;
  index1=whichn(17015,17015,17029,17043,17057,17071,17085,17099,17113);
  index2=whichn(17029,17015,17029,17043,17057,17071,17085,17099,17113);
  index3=whichn(17043,17015,17029,17043,17057,17071,17085,17099,17113);
  index4=whichn(17057,17015,17029,17043,17057,17071,17085,17099,17113);
  index5=whichn(17071,17015,17029,17043,17057,17071,17085,17099,17113);
  put (_all_) ( /=);
run;

/* T000810 CONDITIONAL LINE STATEMENT WORKS IN PROC REPORT WHEN YOU USE THE VARYING FORMAT CLEVER VERY USEFUL   */
   proc report data=sashelp.class nowd;
   columns sex name age;
   define sex / order width=3 spacing=0;
   define name / display;
   define age / display;
   compute after sex;
     msg="------------------------";
     if sex="M" then len=0;
     else len=24;
     line @1 msg $varying24. len;
   endcomp;
   run;

 /* T000820 OBSERVATION NUMBERS IN PROC REPORT   */
   proc report data=sashelp.class nowd;
   columns obs sex name age;
   define obs  / computed;
   define sex  / display;
   define name / display;
   define age  / display;
   compute obs;
     nobs+1;
     obs=nobs;
   endcomp;
   run;

/* T000875 COMPRESS OUT ALL TEXT BETWEEN 'L15' and  'SYSTEM'  */
   /* Minimal method but much slower if you have a lot of records */
   data _null_;
     length cleantxt $96;
     txt='ROGER DEANGELIS L15 NOVEMBER 21, 1963 The SAS SYSTEM 49 SPACKENKILL ROAD';
     rc0=prxparse('s/L15(.+?)SYSTEM//');
     call prxchange(rc0,-1,txt,cleantxt);
     put cleantxt=;
   run;

  * extract november;
  data _null_;
     length cleantxt $96;
     txt='ROGER DEANGELIS L15 <NOVEMBER> 21, 1963 The SAS SYSTEM 49 SPACKENKILL ROAD';
     re=prxparse('/<(.+?)>/');
     if prxmatch(re, txt) then result = prxposn(re, 1, txt);
     put result=;
   run;


   /* more efficient method */
   data _null_;
     retain compiled_pattern;
     length cleantxt $96;
     input txt & $100.;
     if _n_ =1 then do;
         compiled_pattern = prxparse('s/L15(.+?)SYSTEM//');
     end;
     call prxchange(compiled_pattern,-1,txt,cleantxt);
     put cleantxt;
cards4;
ROGER DEANGELIS L15 NOVEMBER 21, 1963 The SAS SYSTEM 49 SPACKENKILL ROAD
GREGG DEANGELIS L15 NOVEMBER 18, 1963 The SAS SYSTEM 49 PENSACOLA L ROAD
;;;;
run;

Hit #85 Extract or remove words between other words



   data _null_;
     length cleantxt $96;
     txt="My cow always gives the best milk";
     rc0=prxparse('s/(.*cow\s+)(.*)(\s+milk.*)/$2/');
     call prxchange(rc0,-1,txt,cleantxt);
     put cleantxt=;
   run;quit;

   CLEANTXT=always gives

   Compress out all text between and including 'always' and 'best';

   data _null_;
     length cleantxt $96;
     txt="My cow always gives the best milk";
     rc0= prxparse('s/always(.+?)best//');
     call prxchange(rc0,-1,txt,cleantxt);
     put cleantxt=;
   run;quit;

   CLEANTXT=My cow  milk


  data _null_;
     length cleantxt $96;
     txt='ROGER DEANGELIS L15 NOVEMBER 21, 1963 The SAS SYSTEM 49 SPACKENKILL ROAD';
     rc0=prxparse('s/L15(.+?)SYSTEM//');
     call prxchange(rc0,-1,txt,cleantxt);
     put cleantxt=;
   run;



/* T000910 HOW TO GET SQL TO DISPLAY NAMES INSTEAD OF LABELS    */
   options nolabel;
   proc sql;select * from sashelp.shoes;quit;
   options label; /* make sure you reset this messes up a lot of other procs */

/* T000930 MULTIPLE PROC PRINTS ON ONE PAGE - DUMB BUT EFFECTIVE METHOD LST OUTPUT ONLY   */
   options formdlim= ' ';
   title 'MALES 13+';
   proc print data=sashelp.class;
   where sex eq 'M' and age ge 13;
   run;
   title 'FEMALES 13+';
   proc print data=sashelp.class;
   where sex eq 'F' and age ge 13;
   run;

/* T000930 SAS 9.2 ERROR PROCESSING IN SAS WATCH FOR MAJOR ENHACEMENTS IN VERSION 9.2  */
   IN VERSION 9.2 DECODES ARE AVAILABLE IN SYSERRORTEXT AND SYSWARNING TEXT
   data NULL;
      set doesnotexist;
   run;
   %put Errortext= &syserrortext;
   %put Errorcode= &syserr;

   Errortext = File WORK.DOESNOTEXIST.DATA does not exist.
   Errorcode = 1012.

/* T00103X CREATING COMBINED ODS OUTPUT DATASETS WITH AND WITHOUT MATCH_ALL (MATCH_ALL SOMETIMES IS NOT NEEDED) */
   WITHOUT MATCH_ALL see T001030 for more options */
    ods output list=no_match_all;
    proc freq data=one;
       /* You must specify this as LIST because FREQ does not */
       /* create a table template for cross-tabs.             */
       tables x*y x*z / list sparse ;
    run;
    ods output close;

/* T001080 SIMPLE PROGRAM TO WRITE TO AN ODS DESTINATION FROM THE DATASTEP */
   ods rtf body="/home/regusers/local/utl/rtf/&sysuserid._t001080.rtf";
   data chkfmt;
    view_id=934;
    Bus_dest_class_id = 72;
    Dp_key = '10101010';
    Commodity_code = '1101';
    Geo_src_code = '1';
    Geo_dest_code = '0';
    Bus_src_code = '99';
    Bus_dest_code = '99';
    Macro_pool = 113;
    Macro_model = 990;
     file print ods;
     put _ods_;
   run;
   ods rtf close;

/* T001090 INVISIBLE FORMATTING CHARACTER(HIDDON DRAGON) ON ACHME LAPTOPS FUNCTION NUMLOCK ALT KII FOR INVISIBLE FORMATTING CHARACTER */

   LAPTOPS WITHOUT KEYPAD
   A single very useful invisible formating character is available on the standard ACHME laptops
   just do the following
         1. Hold down the blue function in the lower left of keyboard. Sometimes the key is labeled [FN}.
         2. Press and release the blue num lock key in the upper right of the keyboard
         3. Hold down alt key. Lower left on the keyboard
         4. Press kii.The cursor should move one space to the right. You have inserted the hiddon dragon.
   the hiddon dragon will at as a no breaking space in word, excel, sas formats and graphics.

   WITH KEYPAD
   1.  Hit the function key(blue fn key on my hp laptop) and numlock key
   2   Hold the alt key down and type kii
   3.  Turn the function key off.

   This will insert what looks like a blank.
   This is sometimes called the hiddon dragon and works well for pdf and rtf output.
   You can find moe info on www.lexjansen.com - search 'hiddon dragon'.

/* T001140  PIVOTING AND CREATING FREQUENCY COUNTS WITHOUT TRANSPOSE IN THREE LINES OF CODE  */
   * Create The Data do not change seeds ;
   Data  SexGov;
     Do Sex='Female','Male';
       Do Gov='FBI','CIA','IRS';
         Do Ids=10 to 25;
           Serial=Int(1E6*Uniform(5739));
           Pay=Int(50000*Uniform(57343));
           If Uniform(5643) < .5 Then Output;
         End;
       End;
     End;
   Run;
   /*---------------------------------------------*\
   | Output                                        |
   |   Sex      Gov    Ids       Pay               |
   |  Female    FBI     12    22831.13             |
   |  Female    FBI     14    16942.24             |
   |  Female    FBI     15     9239.63             |
   |  Female    FBI     20    27425.14             |
   |  Female    FBI     21     8424.24             |
   |  Female    FBI     22     9305.96             |
   |  Female    FBI     23    25826.22             |
   \*---------------------------------------------*/
   /* Put counts in Rectangular array */
   Ods Exclude All;
   Ods Output Observed=GovSexTbl(Rename=Label=Gov);
   Proc Corresp Data=SexGov Observed dim=1;
      Table Gov, Sex;
   Run;
   Ods Select All;
   proc print data=GovSexTbl;run;
   /*---------------------------------------------*\
   | Output                                        |
   | Gov      Female        Male         Sum       |
   | CIA           6          12          18       |
   | FBI           9           6          15       |
   | IRS           5           9          14       |
   | Sum          20          27          47       |
   \*---------------------------------------------*/

/* T001160 CONSTANTS AVAILABLE IN SAS   */
   CONSTANT  (c) machine or mathematical constant c, values of c are:
    E        natural base     LOGBIG     log w/respect to base of big
    PI       pi               LOGSMALL   log w/respect to base of small
    EULER    euler constant   SQRTBIG    square root of big
    EXACTINT exact integer    SQRTSMALL  square root of small
    BIG      largest dp num   MACEPS     machine precision constant
    SMALL    smallest dp num  LOGMACEPS  log w/respect to base of maceps
    [dp = double-precision]   SQRTMACEPS square root of maceps

/* T001170 "SOME FRENCH WOMEN GROW HAIRY ORANGES"  and "KISSING RIPE WATERMELONS"  GETTING THE ODER CORRECT */
   Here is sentence that will help you remember the order of sql clauses.
    SOME FRENCH WOMEN GROW HAIRY ORANGES
    Proc sql;
    Select   *
    From     Lab
    Where    Age ge 18
    Group    by pat
    Having   Min(Dte) ge '01JUL01'D
    Order    by Pat,  Dte
   ;quit

   KISSING RIPE WATERMELONS
   Data order(keep=name rename=name=newname where=(newname='John'));
     set sashelp.class;
   run;

/* T001180 TRUNCATION STRING OPERATIONS IN SQL LIKE '=:' IN DATSTEP */
   In addition to SAS operators, truncated string comparison operators are
   available in PROC SQL.
     EQT  equal to
     GTT  greater than
     LTT  less than
     GET  greater than or equal to
     LET  less than or equal to
     NET  not equal to
   These operators compare two strings after making the strings the same length
   by truncating the longer string to the same length as the shorter string.
   The truncation occurs for the comparison operation only and does not
   permanently affect the strings involved.

/* T002040 SAS 9.2 NEW MACRO FUNCTIONS */
   New Macro Statements
   The %ABORT statement stops the macro that is executing along with the current DATA step, SAS job, or SAS session.
   The %RETURN statement causes normal termination of the currently executing macro.
   The %COPY statement copies specified items from a SAS macro library.
   IN operator see above

   New Macro Functions
   The %SYMEXIST function returns an indication of the existence of a macro variable.
   The %SYMGLOBL function returns an indication as to whether a macro variable is global in scope.
   The %SYMLOCAL function returns an indication as to whether a macro variable is local in scope.

/* T002100 CREATE ERROR: AND WARNING LINES IN THE LOG THAT ARE RED AND GREEN RESPECTIVELY */
   data _null_;
      put 'ERROR: This is red';
      put 'WARNING: This is Green';
   run;

/* T002130 DEFENSIVE PROGRAMMING - LEAVE OFF THE OTHERWISE IN A SELECT CLAUSE TO FORCE AN ERROR */
   data sample;
      do grp=1 to 3;
        output;
      end;
   run;
   data err;
     set sample;
     select (grp);
        when (1) put grp=;
        when (2) put grp=;
     end;
   run;
   GRP=1
   GRP=2
   ERROR: Unsatisfied WHEN clause and no OTHERWISE clause at line 22687 column 6.

/* T002140 DYNAMIC STYLE ELEMENTS IN PROC REPORT - STYLE ELEMENTS FROM DATA */
   ods listing close;
   ods rtf file="/home/regusers/local/utl/rtf/&sysuserid._t002140.rtf";
   ods escapechar='^';
   proc report data=x nowindows missing;
     column text1 text2 text;
     define text1-text2 / noprint;
     define  text / computed;
     compute text / character length=200;
        text = '^S={font_face=courier font_weight=bold}' || text1
               || ' ^n^S={font_face=times font_style=italic}' || text2;
     endcomp;
   run;
   ods rtf close;
   ods listing;

/* T002170 NEW COMPRESS FUNCTION IN VERSION 9  */
  Selected list of COMPRESS modifiers (uppercase or lowercase)

  a      adds uppercase and lowercase letters
  d      adds numerals (digits)
  i      ignores case
  k      keeps listed characters instead of removing them
  s      adds space (blank, tabs, lf, cr) to the list
  p      adds punctuation

  data cmp;
    char  = "a c123xyz" ;
    phone = "(908) 777-1234";
    keepchr    =  compress("a c123xyz"    ,"0123456789"); put  'compress("a c123xyz"    ,"0123456789")    '  keepchr     ;
    dropnumspa =  compress("a c123xyz"    ,,"ds"       ); put  'compress("a c123xyz"    ,,"ds"       )    '  dropnumspa  ;
    keep12345  =  compress("a c123xyz","12345","k"     ); put  'compress("a c123xyz"char,"12345","k" )    '  keep12345   ;
    dropminpar =  compress("(908) 777-1234"," (-)"     ); put  'compress("(908) 777-1234"," (-)"     )    '  dropminpar  ;
    remnopun   =  compress("(908) 777-1234",,"ps"      ); put  'compress("(908) 777-1234",,"ps"      )    '  remnopun    ;
    keepnum    =  compress("(908) 777-1234",,"kd"      ); put  'compress("(908) 777-1234",,"kd"      )    '  keepnum     ;
  run;

  compress("a c123xyz"    ,"0123456789")    a cxyz
  compress("a c123xyz"    ,,"ds"       )    acxyz
  compress("a c123xyz"char,"12345","k" )    123
  compress("(908) 777-1234"," (-)"     )    9087771234
  compress("(908) 777-1234",,"ps"      )    9087771234
  compress("(908) 777-1234",,"kd"      )    9087771234

/* T002250 PRINTING SEQUENCE NUMBERS WITH PROC PRINT WHEN USING A WHERE CLAUSE FROM DATA _NULL_ */
   data classV / view=classV;
     set sashelp.class;
     where sex eq 'M';
   run;
   proc print data=classV;
   run;

/* T002260 GET THE START AND LENGTH OF IMBEDED STRING WITH CALL SCAN AND SUBSTITUTE X.... (REDACTING) */
   data _null_;
       length text $200;
       text='The Password is GO2SAS and it is current';
       call scan(text,4,start,len);
       put start= len=;
       substr(text,start,len)=repeat('X',len);
       put text=;
   run;
  start=17 end=6
  text=The Password is XXXXXX and it is current

/* T002290 EXTRACT THE FIRST STRING THAT BEGINS WITH 2009 AND ENDS WITH A SPACE */
   data _null_;
      rx = prxparse("/2009(.*?)\ /i");
      text="this study, protocol 20090919 is a pediatric study";
      call prxsubstr(rx, text, pos, len);
      protocol_number = substr(text, pos, len);
      put protocol_number=;
   run;

/* T002390 DOES A FOLDER EXIST MACRO AND DATASTEP */
   %let rc1 = %sysfunc(filename(fileref,/home/regusers/utl));
   %let xis = %sysfunc(fexist(&fileref));
   %put &xis;  /* 1 means it exists */
   data _null_;
       rc =filename('fileref','/home/regusers/utl');
       xis=fexist('fileref');
       put xis=;
   run;

/* T00245X NEW FUNCTION: CMISS HOLIDAY AND INDSNAME 9.2 FUNCTIONS */

   data _null_;
   x = 1; y = . ; z = 'abc';
   n=nmiss(x,y,z); *numeric args only;
   c=cmiss(x,y,z); *char or numeric;
   put n= c=;
   run;

   data _null_;
   Easter=holiday('Easter',2009);
   put Easter=weekdate.;
   run;

   Some other holidays: Columbus, Fathers, Labor, MLK, Memorial,
   Mothers, Thanksgiving, USPresidents, Veterans, Victoria
   Partial Log:
   Easter=Sunday, April 12, 2009

/* T00277X SINGLE LEVEL DATASETS WRITTEN TO A LIBRARY OTHER THAN WORK */
   libname user "c:\temp";
   data class;  /* class is in c:\temp */
      set sashelp.class;
   run;
   libname user clear; /* make work the default */

/* T004600 SCANNING BACKWARDS USING SCAN FUNCTION WITH NEGATIVE ARGUMENTS
data _null_;
 list = '1:2:3:4';
 last = scan(list,-1,':');
 put last=;
run;

/* T00528X MAKE CATS OPERATE BY CATS, CATT AND CATX
Most useful with call execute.

Suppose you have want to create the libname statement

%let sp='/**/ /**/';  /* this will insert a required space */

data _null_;
  lib='libname';
  ref='111';
  pth='/groundtruth/regusr/local/sd1';

  libcmd=cats(lib,&sp,'s',ref,&sp,'"',pth,'";');

  put libcmd=;
  call execute(libcmd);
run;

libcmd=libname s111 "/groundtruth/regusr/local/sd1";

NOTE: CALL EXECUTE generated line.
1     + libname s111 "/groundtruth/regusr/local/sd1";
NOTE: Library S111 does not exist.

SAS Forum: How to integrate SAS University Edition with R

Install R on your local power workstation(all post 2013 computers are power worstations
  when compared to SAS University limited and crippled environment)
  You do not need admin rights to install R.

Install WPS express (free version)
  WPS/Proc/R does not limit the size of SAS datasets created from R

  Program WPS/R to input/output SAS datasets to local shared folder (folder common to power workstation
  and the vm environment.


see
https://goo.gl/McAjSb
https://communities.sas.com/t5/SAS-Studio/How-to-integrate-SAS-University-Edition-with-R/td-p/261210


/* T00052X SAS Forum: Remove Observations based on a SAS table( I did not know that)

WORKING CODE

proc sql;
  delete from class
  where name in (select name from classm);

see
https://goo.gl/3oi4Hy
https://communities.sas.com/t5/SAS-Procedures/Remove-Observations-based-on-a-SAS-table/m-p/355532

RW9 profile
https://communities.sas.com/t5/user/viewprofilepage/user-id/45151

I want to remove males from SASHELP.CLASS

* Males;
data class;
  set sashelp.class;
run;quit;

* All students;
data classm;
 set sashelp.class(where=(sex='M'));
run;quit;

* What I did not know;

proc sql;
  delete from class
  where name in (select name from classm);
quit;

* I always did it this way;
proc sql;
  select *
  from class
  where name not in (se


  /* T000520 CHECKING MACRO ARGUMENTS CHANG CHUNG
  %macro checker(sysbin=,outdsn=);

   %put %sysfunc(ifc(%sysevalf(%superq(sysbin )=,boolean),**** Please Provide the sysinfo value    ****,));
   %put %sysfunc(ifc(%sysevalf(%superq(outdsn )=,boolean),**** Please Provide an output dataset    ****,));

    %let res= %eval
    (
        %sysfunc(ifc(%sysevalf(%superq(sysbin )=,boolean),1,0))
      + %sysfunc(ifc(%sysevalf(%superq(outdsn )=,boolean),1,0))
    );

     %if &res = 0 %then %do;
         %put do some work;
     %end;

  %mend checker;

  %checker;
  Log

===================================================================================================================================
END OF TEASERS
===================================================================================================================================

