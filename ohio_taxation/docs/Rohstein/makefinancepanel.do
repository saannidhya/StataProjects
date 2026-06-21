capture log close
log using makefinancepanel.log, text replace

#delimit;
clear;
set mem 100m;

/*
if c(os)=="MacOSX" {;
  local ccddir "/Volumes/JESSEDATA/ccd/";
};
else {;
  local ccddir "d:/data/ccd";
  *use z:\ccd\leafinance\cleaned\leafinance_panel_ca.dta;
};
*/
local ccddir ".";
*use `ccddir'/leafinance/cleaned/leafinance_panel_ca.dta;
use `ccddir'/leafinance_panel_ca.dta;

keep if stfips==6;
 replace cofips=cofips-6000 if cofips>6000 & cofips<7000;

drop if leaid=="" | leaid=="N";

*Drop districts that arent elementary/secondary/unified;
 drop if level>3;

*Check that county is constant within LEA over time;
 sort leaid year;
 by leaid: gen firstco=cofips[1];
 count if cofips~=firstco;

*Assign each LEA to the modal county;
 by leaid: egen modeco=mode(cofips);
 *One district has 4 of each -- break the tie for the earlier county, as this dist is later absorbed;
  replace modeco=19 if leaid=="0600023";
 gen ismode=(cofips==modeco);
 by leaid: egen frmode=mean(ismode);
 replace cofips=modeco;
 rename frmode cofips_recode;
 drop modeco ismode;



gen newleaid=leaid;

replace newleaid="0636805" if leaid=="0600023";
replace newleaid="0600076" if leaid=="0600024";
replace newleaid="0600027" if leaid=="0600027";
replace newleaid="0600113" if leaid=="0600030";
replace newleaid="0600116" if leaid=="0600041";
replace newleaid="0636120" if leaid=="0600043";
replace newleaid="0691092" if leaid=="0600119";
replace newleaid="0600153" if leaid=="0601910";
replace newleaid="0600153" if leaid=="0601930";
replace newleaid="0600020" if leaid=="0602490";
replace newleaid="0600017" if leaid=="0602880";
replace newleaid="0636805" if leaid=="0603450";
replace newleaid="0614950" if leaid=="0604050";
replace newleaid="0691137" if leaid=="0604920";
replace newleaid="0614950" if leaid=="0606240";
replace newleaid="0600049" if leaid=="0607110";
replace newleaid="0691134" if leaid=="0607320";
replace newleaid="0614950" if leaid=="0607380";
replace newleaid="0600067" if leaid=="0607620";
replace newleaid="0600067" if leaid=="0607650";
*replace newleaid="0600023" if leaid=="0608280";
replace newleaid="0600049" if leaid=="0609180";
replace newleaid="0629540" if leaid=="0609666";
replace newleaid="0600039" if leaid=="0610920";
replace newleaid="0600047" if leaid=="0610990";
replace newleaid="0600065" if leaid=="0611160";
replace newleaid="0600065" if leaid=="0611190";
replace newleaid="0600033" if leaid=="0611370";
replace newleaid="0600033" if leaid=="0611410";
replace newleaid="0600026" if leaid=="0612240";
replace newleaid="0600027" if leaid=="0612540";
replace newleaid="0600027" if leaid=="0612570";
replace newleaid="0602250" if leaid=="0612660";
replace newleaid="0600052" if leaid=="0613040";
replace newleaid="0600052" if leaid=="0613060";
replace newleaid="0600035" if leaid=="0613590";
replace newleaid="0600037" if leaid=="0613660";
replace newleaid="0600037" if leaid=="0613680";
replace newleaid="0609030" if leaid=="0614640";
replace newleaid="0600046" if leaid=="0615540";
replace newleaid="0600046" if leaid=="0615570";
replace newleaid="0600051" if leaid=="0616140";
replace newleaid="0600051" if leaid=="0616170";
replace newleaid="0600018" if leaid=="0616710";
replace newleaid="0600044" if leaid=="0616770";
replace newleaid="0600044" if leaid=="0616800";
replace newleaid="0691134" if leaid=="0616890";
replace newleaid="0600011" if leaid=="0617010";
replace newleaid="0600060" if leaid=="0617910";
replace newleaid="0600060" if leaid=="0617940";
replace newleaid="0600018" if leaid=="0618090";
replace newleaid="0600011" if leaid=="0622530";
replace newleaid="0600014" if leaid=="0622780";
replace newleaid="0600015" if leaid=="0623070";
replace newleaid="0600025" if leaid=="0623580";
replace newleaid="0600038" if leaid=="0624120";
replace newleaid="0600022" if leaid=="0624510";
replace newleaid="0600019" if leaid=="0626520";
replace newleaid="0600029" if leaid=="0626550";
replace newleaid="0600036" if leaid=="0626700";
replace newleaid="0600116" if leaid=="0627540";
replace newleaid="0600062" if leaid=="0627960";
replace newleaid="0600062" if leaid=="0627990";
replace newleaid="0643370" if leaid=="0628020";
replace newleaid="0600045" if leaid=="0628890";
replace newleaid="0600045" if leaid=="0628920";
replace newleaid="0600033" if leaid=="0628980";
replace newleaid="0600031" if leaid=="0629310";
replace newleaid="0606390" if leaid=="0629730";
replace newleaid="0600048" if leaid=="0629970";
replace newleaid="0600048" if leaid=="0630000";
replace newleaid="0600069" if leaid=="0630060";
replace newleaid="0600048" if leaid=="0630300";
replace newleaid="0600020" if leaid=="0631080";
replace newleaid="0600064" if leaid=="0631410";
replace newleaid="0600064" if leaid=="0631440";
replace newleaid="0600032" if leaid=="0632100";
replace newleaid="0621450" if leaid=="0632490";
replace newleaid="0600061" if leaid=="0633000";
replace newleaid="0600040" if leaid=="0633030";
replace newleaid="0600040" if leaid=="0633060";
replace newleaid="0600013" if leaid=="0633300";
replace newleaid="0634425" if leaid=="0634420";
replace newleaid="0691134" if leaid=="0634530";
replace newleaid="0600012" if leaid=="0634610";
replace newleaid="0691136" if leaid=="0634660";
replace newleaid="0605580" if leaid=="0635640";
replace newleaid="0614040" if leaid=="0636060";
replace newleaid="0614950" if leaid=="0636540";
replace newleaid="0636805" if leaid=="0636720";
replace newleaid="0636805" if leaid=="0636770";
replace newleaid="0600001" if leaid=="0637080";
replace newleaid="0600032" if leaid=="0637410";
replace newleaid="0600116" if leaid=="0637740";
replace newleaid="0600021" if leaid=="0638490";
replace newleaid="0600028" if leaid=="0638940";
replace newleaid="0600047" if leaid=="0639480";
replace newleaid="0600047" if leaid=="0639510";
replace newleaid="0691134" if leaid=="0639540";
replace newleaid="0691134" if leaid=="0639570";
replace newleaid="0600158" if leaid=="0640050";
replace newleaid="0600158" if leaid=="0640080";
replace newleaid="0600016" if leaid=="0640440";
replace newleaid="0691135" if leaid=="0640620";
replace newleaid="0600069" if leaid=="0640770";
replace newleaid="0600042" if leaid=="0641370";
replace newleaid="0600012" if leaid=="0641490";
replace newleaid="0604080" if leaid=="0641670";
replace newleaid="0600063" if leaid=="0641700";
replace newleaid="0626280" if leaid=="0642390";
replace newleaid="0600034" if leaid=="0642870";
replace newleaid="0630660" if leaid=="0643350";
replace newleaid="0622110" if leaid=="0691053";
replace newleaid="0600085" if leaid=="0691054";
replace newleaid="0600088" if leaid=="0691056";
replace newleaid="0600150" if leaid=="0691057";
replace newleaid="0600090" if leaid=="0691060";
replace newleaid="0600092" if leaid=="0691061";
replace newleaid="0600093" if leaid=="0691064";
replace newleaid="0600096" if leaid=="0691065";
replace newleaid="0600097" if leaid=="0691066";
replace newleaid="0600098" if leaid=="0691067";
replace newleaid="0600099" if leaid=="0691068";
replace newleaid="0600100" if leaid=="0691069";
replace newleaid="0600101" if leaid=="0691070";
replace newleaid="0600102" if leaid=="0691071";
replace newleaid="0600073" if leaid=="0691072";
replace newleaid="0600074" if leaid=="0691073";
replace newleaid="0600075" if leaid=="0691074";
replace newleaid="0600077" if leaid=="0691075";
replace newleaid="0600078" if leaid=="0691076";
replace newleaid="0600079" if leaid=="0691077";
replace newleaid="0600103" if leaid=="0691079";
replace newleaid="0600105" if leaid=="0691081";
replace newleaid="0600106" if leaid=="0691082";
replace newleaid="0600108" if leaid=="0691084";
replace newleaid="0622710" if leaid=="0691085";
replace newleaid="0600110" if leaid=="0691086";
replace newleaid="0600111" if leaid=="0691087";
replace newleaid="0600112" if leaid=="0691088";
replace newleaid="0600117" if leaid=="0691089";
replace newleaid="0600118" if leaid=="0691091";
replace newleaid="0600081" if leaid=="0691094";
replace newleaid="0600120" if leaid=="0691095";
replace newleaid="0600080" if leaid=="0691096";
replace newleaid="0600121" if leaid=="0691097";
replace newleaid="0600122" if leaid=="0691098";
replace newleaid="0600123" if leaid=="0691099";
replace newleaid="0600124" if leaid=="0691100";
replace newleaid="0600082" if leaid=="0691101";
replace newleaid="0600083" if leaid=="0691103";
replace newleaid="0600094" if leaid=="0691104";
replace newleaid="0600095" if leaid=="0691105";
replace newleaid="0600130" if leaid=="0691107";
replace newleaid="0600131" if leaid=="0691108";
replace newleaid="0600132" if leaid=="0691109";
replace newleaid="0600138" if leaid=="0691110";
replace newleaid="0600139" if leaid=="0691113";
replace newleaid="0600140" if leaid=="0691114";
replace newleaid="0600137" if leaid=="0691116";
replace newleaid="0600143" if leaid=="0691117";
replace newleaid="0600125" if leaid=="0691118";
replace newleaid="0600157" if leaid=="0691121";
replace newleaid="0600128" if leaid=="0691123";
replace newleaid="0600129" if leaid=="0691124";
replace newleaid="0600145" if leaid=="0691126";
replace newleaid="0600146" if leaid=="0691127";
replace newleaid="0600147" if leaid=="0691128";
replace newleaid="0600148" if leaid=="0691129";
replace newleaid="0600149" if leaid=="0691130";
replace newleaid="0600141" if leaid=="0691131";
replace newleaid="0600142" if leaid=="0691132";
replace newleaid="0600133" if leaid=="0691133";

drop if newleaid=="";
duplicates report newleaid year;

tempfile newleaids;
save `newleaids';

*Make a crosswalk;
 keep leaid year newleaid;
 sort leaid year;
 save newleaids_xwalk.dta, replace;



*Prepare data from the CCD agency files for analysis with school bonds data;
 *use if stfips==6 & year>=1996 using `ccddir'\agency\agpanel.dta;
 use if stfips==6 & year>=1996 using `ccddir'\agpanel.dta;
 replace year=year-1;
 keep leaid enroll enrollg nskls nteach year;
 sort leaid year;
 *Merge to new leaids;
  merge leaid year using newleaids_xwalk, unique;
 tab _merge;
 keep if _merge==3;
 drop _merge;
 sort leaid year;
 tempfile ccdagdat;
 save `ccdagdat';

use `newleaids';
sort leaid year;
merge leaid year using `ccdagdat', unique;
drop _merge;

sort newleaid year;
foreach v of varlist v33 totalrev tfedrev tstrev c11 tlocrev t06 t09 t15 t40 t99  t02 d23 d11
                     a07 a08 a09 a11 a13 a20 a15 u22 u97 totalexp tcurinst tcurssvc v40 v45
                     tcuroth v60 tnonelse tcapout f12 k09 k10 k11 g15 tcurelsc l12 m12 q11
                     i86 z32 z33 v13 v15 v17 z34 _19h _21f _31f _41f _61v _66v c01 c12 c13
                     c35 w01 w31 w61 enroll enrollg nskls nteach {;
   replace `v'=. if `v'<0;
  };

collapse (sum) v33 totalrev tfedrev tstrev c11 tlocrev t06 t09 t15 t40 t99  t02 d23 d11
a07 a08 a09 a11 a13 a20 a15 u22 u97 totalexp tcurinst tcurssvc v40 v45 tcuroth v60 tnonelse tcapout
f12 k09 k10 k11 g15 tcurelsc l12 m12 q11 i86 z32 z33 v13 v15 v17 z34 _19h _21f _31f _41f _61v _66v
c01 c12 c13 c35 w01 w31 w61
enroll enrollg nskls nteach
(median) csa cbsa stfips cofips, by(newleaid year);

*note: i am losing many variables, only keep newleaid year + those which make sense to be added;
sort newleaid year;


label var newleaid "Consistent LEAID over time";


tempfile financepanel;
save `financepanel';
save financepanel, replace;

log close;
