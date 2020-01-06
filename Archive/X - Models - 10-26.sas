libname PSID 'C:/Users/bda13/Desktop/Sociology/Statistics/PSID';

Data DF;
	Set Psid.Psid;
run;

proc freq;
table ego_black2;
run;
*y~ego_black2 + ego_age+ego_age2 + ego_female + ego_black2 *(ego_age+ego_age2)+fam_region +year2+died  + (1 | fam_id_68) + (1| ind_id);

*ego_female ego_black2*ego_age ego_black2*ego_age2 fam_region year2 died  ;

proc sort data=DF ;
by ind_id fam_id_68;
run;

   proc glimmix data=DF;
      class fam_id_68 ind_id;
      model h_heart_attack(event="1") = ego_black2 ego_age ego_age2 ego_female ego_black2*ego_age ego_black2*ego_age2 fam_region year2 died/ dist=binary link=logit solution;
	  Random int/ subject=fam_id_68;
	  Random _residual_ / subject=ind_id(fam_id_68) type=cs;
  run;


   proc glimmix data=DF;
      class fam_id_68 ind_id;
      model h_activities(event=FIRST) = ego_black2 ego_age ego_age2 ego_female ego_black2*ego_age ego_black2*ego_age2 fam_region year2 died/ link=log dist=negbin solution;
	  Random int/ subject=fam_id_68;
	  Random _residual_ / subject=ind_id(fam_id_68) type=cs;
  run;
 


proc freq data=DF;
table h_activities;
run;
