*Program to clean raw CCD LEA-Fiscal SAS files, label and standardize variables, etc.;

options ps=140 ls=120 nocenter macrogen nomprint;
libname rawsas "d:\data\ccd\leafinance\sasorig";
libname cleansas "d:\data\ccd\leafinance\cleaned";

%macro clean(year);
 %if &year.=1995 %then %do;
  %let origdat=sdf95c1d;
  %end;
 %if &year.=1996 %then %do;
  %let origdat=sdf96c1b;
  %end;
 %if &year.=1997 %then %do;
  %let origdat=sdf97d1a;
  %end;
 %if &year.=1998 %then %do;
  %let origdat=sdf98d1e;
  %end;
 %if &year.=1999 %then %do;
  %let origdat=sdf991c;
  %end;
 %if &year.=2000 %then %do;
  %let origdat=sdf001d;
  %end;
 %if &year.=2001 %then %do;
  %let origdat=sdf011d;
  %end;
 %if &year.=2002 %then %do;
  %let origdat=sdf021c;
  %end;
 %if &year.=2003 %then %do;
  %let origdat=sdf031b;
  %end;
 %if &year.=2004 %then %do;
  %let origdat=sdf041b;
  %end;
 %if &year.=2005 %then %do;
  %let origdat=sdf051b;
  %end;
 data clean&year. (rename=(yearnum=year
                           ccdmatch=ccdnf
                           ));
  set rawsas.&origdat.
        %if &year.>=2003 %then %do;
          (rename=(conum=fipsco csa=csatxt cbsa=cbsatxt
                     )) %end;
        %else %do; (rename=(cmsa=cmsatxt)) %end;
        ;
  keep leaid censusid stfips cofips name level yearnum ccdmatch gslo gshi v33
       totalrev tfedrev tstrev tlocrev
       t02 t06 t09 t15 t40 t99 d11 d23 a07 a08 a09 a11 a13 a15 a20 u22 u97
       totalexp tcurinst tcurssvc v40 v45 tcuroth v60 tnonelse tcapout
       f12 g15 k09 k10 k11
       c01 c04 c05 c06 c07 c08 c09 c10 c11 c12 c13 c35 c38 c39
       tcurelsc
       l12 m12 q11 i86
       z32 z33 v13 v15 v17 z34
       _19h _21f _31f _41f _61v _66v
       w01 w31 w61
       %if &year.>=1997 %then %do;
          v91 v92
          %end;
       %if &year.>=1998 %then %do;
          agchrt cenfile
          %end;
       %if &year.<2003 %then %do;  cmsa %end;
       %else %do; csa cbsa %end;
       ;
  yearnum=1900+year + (100*(year<70));
  ccdmatch=ccdnf+0;
  stfips=fipst+0;
  cofips=fipsco+0;
  level=schlev+0;
  label ccdmatch="CCD Agency Nonfiscal file match (1=match)"
        cofips="FIPS County code"
        stfips="FIPS State code"
        level ="School level code (1=el/2=sec/3=unif/4-6=other)";
  %if &year.<2003 %then %do;
    cmsa=cmsatxt+0;
    label cmsa="CMSA (2 digits)/MSA (4 digits) code";
    %end;
  %else %do;
    csa=csatxt+0;
    cbsa=cbsatxt+0;
    label csa="Consolidated statistical area"
          cbsa="Core-based statistical area";
    %end;
  run;
 proc sort data=clean&year.;
  by leaid censusid;
  run;
 data clean&year.;
  set clean&year.;
  by leaid censusid;
  dup_lea=(not(first.leaid & last.leaid));
  if not(first.leaid | first.censusid) | not(last.leaid | last.censusid) then dup_both=1;
  else dup_both=0;
  run;
 proc freq data=clean&year.;
  title "Counts of duplicate lea codes, failed matches, &year.";
  tables ccdnf*(dup_lea dup_both) /missing;
  run;
/*
 proc print data=clean&year. (where=(dup_lea=1));
  title "Duplicate Lea codes in &year. data";
  var leaid censusid name ccdnf dup_lea dup_both;
  run;
 proc print data=clean&year. (where=(ccdnf~=1 & dup_lea~=1));
  title "Flag indicates no match to Agency file, &year. data";
  var leaid censusid name ccdnf dup_lea dup_both;
  run;
*/
%mend clean;

%clean(1995);
%clean(1996);
%clean(1997);
%clean(1998);
%clean(1999);
%clean(2000);
%clean(2001);
%clean(2002);
%clean(2003);
%clean(2004);
%clean(2005);
proc contents;
 run;
proc means;
 run;

data panel;
 set clean1995 clean1996 clean1997 clean1998 clean1999
     clean2000 clean2001 clean2002 clean2003 clean2004 clean2005;
 run;
proc sort data=panel;
 by leaid censusid year;
 run;
data cleansas.leafinance_panel;
 set panel;
 by leaid censusid year;
 firstobs=(first.leaid=1 | first.censusid=1);
 lastobs=(last.leaid=1 | last.censusid=1);
 retain prevyear;
 if firstobs~=1 then skip=(year~=prevyear+1);
 prevyear=year;
 label firstobs="This is first obs from district"
       lastobs="This is last obs from district"
       skip="1:  Not first obs from district, but missing prev. year";
 drop prevyear;
/*
  retain prevname prevlev prevlo prevhi prevst;
  if firstobs=1 then do;
   prevname=name;
   prevlev=level;
   prevlo=gslo;
   prevhi=gshi;
   prevst=stfips;
   end;
  else do;
   diffname=(prevname~=name);
   difflev=(prevlev~=level);
   difflo=(prevlo~=gslo);
   diffhi=(prevhi~=gshi);
   diffst=(prevst~=stfips);
   if prevlo in ("PK", "KG") & gslo in ("PK", "KG") then difflo=0;
   end;
*/
 run;
proc means;
/*
proc print data=panel (where=(diffst=1));
 var difflo diffhi difflev diffname stfips prevst leaid censusid;
 run;
proc print data=panel (where=(difflo=1 | diffhi=1 | difflev=1));
 var difflo diffhi difflev diffname gslo prevlo gshi prevhi level prevlev year;
 run;
*/
