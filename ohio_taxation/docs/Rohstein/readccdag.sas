*Program to read in CCD agency data;

*proc printto print="readccdag.lst" log="readccdag.log" new;
* run;
options ps=60 ls=120 nocenter macrogen nomprint noxwait xsync;
libname basedata "d:\data\ccd\agency";

%macro readag(yr);
 %if &yr.>80 %then %do;
  %let yrm1=%eval(&yr.-1);
  %end;
 %else %do;
  %if &yr.=00 %then %let yrm1=99;
  %if &yr.=01 %then %let yrm1=00;
  %if &yr.=02 %then %let yrm1=01;
  %if &yr.=03 %then %let yrm1=02;
  %if &yr.=04 %then %let yrm1=03;
  %if &yr.=05 %then %let yrm1=04;
  %if &yr.=06 %then %let yrm1=05;
  %end;
 *Unzip raw files;
 x "mkdir c:\temp\ccd";
 %if &yr.=87 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\pau&yrm1.data.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ccdsdu&yrm1..dat";
  %end;
 %if &yr.=88 | &yr.=89 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\pau&yrm1.data.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ccdsdu&yrm1..txt";
  %end;
 %if &yr.=90 | &yr.=91 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\pau&yrm1.data.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ccdsdu&yrm1..dat";
  %end;
 %if &yr.=92 | &yr.=93 | &yr.=94 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\pau&yrm1.data.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Agency&yrm1..dat";
  %end;
 %if &yr.=95 | &yr.=96 | &yr.=97 | &yr.=98 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\Ccdagn&yrm1..zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ccdagn&yrm1..dat";
  %end;
 %if &yr.=99 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag981c_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag981c.DAT";
  %end;
 %if &yr.=00 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag991b_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag991b.dat";
  %end;
 %if &yr.=01 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag001a_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag0001a.DAT";
  %end;
 %if &yr.=02 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag011a_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ag011a.dat";
  %end;
 %if &yr.=03 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag021a_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag021a.dat";
  %end;
 %if &yr.=04 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag031b_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag031b.txt";
  %end;
 %if &yr.=05 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag041c_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\ag041c.dat";
  %end;
 %if &yr.=06 %then %do;
  x "c:\progra~1\winzip\winzip32 -e -o d:\data\ccd\Raw\agency\ag051a_dat.zip
               c:\temp\ccd\";
  filename ccdraw "c:\temp\ccd\Ag051a.dat";
  %end;

 data basedata.ag&yrm1.&yr.;
  missing M N;
  %if &yr.=87 %then %do;
   infile ccdraw lrecl=233 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-51 city $ 77-94
         zip $ 97-101 cofips 122-124 ma 150-155 gslo $ 158-159 gshi $ 160-161
         nskls 187-191 enroll 224-231 ;
   drop enrollg;
   %end;
  %if &yr.=88 | &yr.=89 | &yr.=90 | &yr.=91 %then %do;
   infile ccdraw lrecl=221 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-51 city $ 77-94
         zip $ 97-101 cofips 122-124 ma 150-155 gslo $ 157-158 gshi $ 159-160
         nskls 161-165 nteach 166-171 enrollg 178-183 enroll 184-190 ;
   %end;
  %if &yr.=92 %then %do;
   infile ccdraw lrecl=227 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-51 city $ 82-99
         zip $ 102-106 cofips 125-129 ma 155-160 gslo $ 162-163 gshi $ 164-165
         nskls 166-170 nteach 171-176  enrollg 183-188 enroll 189-195;
   %end;
  %if &yr.=93 | &yr.=94 | &yr.=95 | &yr.=96 | &yr.=97 | &yr.=98 %then %do;
   infile ccdraw lrecl=1030 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-51 city $ 82-99
         zip $ 102-106 cofips 127-129 ma 155-160 gslo $ 163-164 gshi $ 165-166
         nskls 167-171 nteach 172-178 .1 enrollg 185-190 enroll 191-197;
   %end;
  %if &yr.=99 | &yr.=00 %then %do;
   infile ccdraw lrecl=722 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-81 city $ 193-222
         zip $ 225-229 cofips 240-242 ma 273-278 gslo $ 281-282 gshi $ 283-284
         nskls 285-289 nteach 290-296 .1 enrollg 304-310 enroll 311-317;
   label nskls="Agency # of schools"
         nteach="Agency # of FTE teachers";
   %end;
  %if &yr.=01 | &yr.=02 %then %do;
   infile ccdraw lrecl=725 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-81 city $ 193-222
         zip $ 225-229 cofips 240-242 ma 273-278 gslo $ 282-283 gshi $ 284-285
         nskls 286-290 nteach 291-297 .1 enrollg 305-311 enroll 312-318;
   label nskls="Agency # of schools"
         nteach="Agency # of FTE teachers";
   %end;
  %if &yr.=03 | &yr.=04 | &yr.=05 | &yr.=06 %then %do;
   infile ccdraw lrecl=729 missover;
   input leaid $ 1-7 stfips 1-2 stid $ 8-21 name $ 22-81 city $ 193-222
         zip $ 225-229 cofips 240-242 ma 276-280 gslo $ 285-286 gshi $ 287-288
         nskls 290-294 nteach 295-301 .1 enrollg 309-315 enroll 316-322;
   label nskls="Agency # of schools"
         nteach="Agency # of FTE teachers";
   %end;
  if stfips=12 & cofips=25 then cofips=86;
  label leaid="Agency ID code"
        stid="State ID for district"
        gslo="Agency low grade"
        gshi="Agency high grade"
        enrollg="Agency PK-12 enrollment"
        enroll="Agency total enrollment"
        ma="MA on CCD file--often wrong";
  run;

 proc means data=basedata.ag&yrm1.&yr.;
  title "Year = &yrm1.-&yr.";
  run;
%mend readag;

%readag(87);
%readag(88);
%readag(89);
%readag(90);
%readag(91);
%readag(92);
%readag(93);
%readag(94);
%readag(95);
%readag(96);
%readag(97);
%readag(98);
%readag(99);
%readag(00);
%readag(01);
%readag(02);
%readag(03);
%readag(04);
%readag(05);
%readag(06);

data basedata.agpanel;
 set basedata.ag8687 (in=fr87) basedata.ag8788 (in=fr88) basedata.ag8889 (in=fr89) basedata.ag8990 (in=fr90)
     basedata.ag9091 (in=fr91) basedata.ag9192 (in=fr92) basedata.ag9293 (in=fr93) basedata.ag9394 (in=fr94)
     basedata.ag9495 (in=fr95) basedata.ag9596 (in=fr96) basedata.ag9697 (in=fr97) basedata.ag9798 (in=fr98)
     basedata.ag9899 (in=fr99) basedata.ag9900 (in=fr00) basedata.ag0001 (in=fr01) basedata.ag0102 (in=fr02)
     basedata.ag0203 (in=fr03) basedata.ag0304 (in=fr04) basedata.ag0405 (in=fr05) basedata.ag0506 (in=fr06)
     ;
 length year 4;
 year=1987*(fr87=1)+1988*(fr88=1)+1989*(fr89=1)+1990*(fr90=1)+1991*(fr91=1)+1992*(fr92=1)+1993*(fr93=1)+1994*(fr94=1)
     +1995*(fr95=1)+1996*(fr96=1)+1997*(fr97=1)+1998*(fr98=1)+1999*(fr99=1)+2000*(fr00=1)+2001*(fr01=1)+2002*(fr02=1)
     +2003*(fr03=1)+2004*(fr04=1)+2005*(fr05=1)+2006*(fr06=1)
     ;
 run;
x "del /q /f c:\temp\ccd\*";
*proc printto;
* run;
