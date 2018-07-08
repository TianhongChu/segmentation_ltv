libname imcdata '/projects/e30509/imc486';
libname hh '/home/tck9844/hwdata';

proc datasets lib=hh; quit; run;

* data read-in;
data ordprod;
set imcdata.ordprod; run;

proc print data=ordprod(obs=20); run;

data lookup;
set imcdata.lookup; run;
proc print data=lookup(obs=20); run;

*categorize minor_cat into 8 major categories;
data lookup1;
	set lookup;
if minor_cat = "" then delete;
if index(minor_cat, "HH") + index(minor_cat, "HG") 
	+ index(minor_cat,"BABY") + index(minor_cat, "PET") > 0
	then cat = "Household and Home" ;	
else if index(minor_cat, "HB") > 0
	then cat = "Health and Beauty";
else if index(minor_cat, "DAIRY") > 0 
	then cat = "Dairy";
else if index(minor_cat,"GROC SNACKS") + index(minor_cat, "GROC BEVERAGES") 
	+ index(minor_cat, "FROZEN JUICE/DRINK") + index(minor_cat,"FROZEN ICE CREAM/NOVELTIES/YOG") 
	+ index(minor_cat, "ALCOHOL")> 0 
	then cat = "Snacks,Beverages";	
else if index(minor_cat, "FROZEN") + index(minor_cat, "GROC BOXED") 
	+ index(minor_cat, "GROC INSTANT") + index(minor_cat, "GROC CANNED")
	+ index(minor_cat, "GROC SOUP CANNED") + index(minor_cat, "GROC REFRIG") > 0 
	then cat = "Frozen,Canned Food";
else if index(minor_cat, "FRESH MARKET DELI") > 0
	then cat = "Deli";
else if index(minor_cat, "GROC") > 0
	then cat = "Grocery";
else if index(minor_cat, "FRESH MARKET") > 0 
	then cat = "Fresh";
if cat = "" then delete;
run;

* join minor_cat to ordprod table;
proc sql;
create table ordprod_cat as
select ord_id, spec_cd, it_qy, it_pr_qy, tot_pr_qy, cat
from ordprod  as o left join lookup1 as l
on o.pod_id = l.pod_id; 
quit;

proc freq data=ordprod_cat;
table cat; run;

* join cnsm_id to ordprod table;
* order table - delete blank orders;
data order;
set imcdata.order; 
if it_dmnd_qy = . then delete;
if ord_seq_num = . then delete;
if sale_inct_qy = . then sale_inct_qy = 0;
run;

*717848/17241529 = 0.04 missing;
proc sql;
create table ordprod_cat1 as
select cnsm_id, spec_cd, it_qy, it_pr_qy, tot_pr_qy, cat 
from ordprod_cat as o left join order as i
on o.ord_id = i.ord_id;
quit;

* delete cnsm_id that has no match;
data ordprod_cat2;
set ordprod_cat1;
if cnsm_id = "" then delete; run;

** Start to roll-up;
* roll - up ordprod to customer level; 
proc sort data = ordprod_cat2 out = ordprod_cat_s;
	by cnsm_id; run;

data ordprod_cat_cust;
	set ordprod_cat_s;
	by cnsm_id;
	
	retain spec_cd_qty tot_qty 
	qty_hhhg qty_hb qty_dairy
	qty_snack_bev qty_frozen_canned qty_deli
	qty_groc qty_fresh
	spending_hhhg spending_hb spending_dairy
	spending_snack_bev spending_frozen_canned spending_deli
	spending_groc spending_fresh
	spending_tot
	;
	
	if first.cnsm_id then do;
	tot_qty = 0;
	spec_cd_qty = 0;
	qty_hhhg = 0;
	qty_hb = 0;
	qty_dairy = 0;
	qty_snack_bev = 0;
	qty_frozen_canned = 0;
	qty_deli = 0;
	qty_groc = 0;
	qty_fresh = 0;
	spending_hhhg = 0;
	spending_hb = 0;
	spending_dairy = 0;
	spending_snack_bev = 0;
	spending_frozen_canned = 0;
	spending_deli = 0;
	spending_groc = 0;
	spending_fresh = 0;
	spending_tot = 0;
	end;
	
	tot_qty = tot_qty + it_qy;
	spending_tot = spending_tot + tot_pr_qy;
	
	if spec_cd = "*" then do;
	spec_cd_qty = spec_cd_qty + it_qy;
	end;
		
	if cat = "Fresh" then do;
	qty_fresh = qty_fresh + it_qy;
	spending_fresh = spending_fresh + tot_pr_qy;
	end;
	
	else if cat = "Snacks,Beverages" then do;
	qty_snack_bev = qty_snack_bev + it_qy;
	spending_snack_bev = spending_snack_bev + tot_pr_qy;
	end;
	
	else if cat = "Frozen,Canned Food" then do;
	qty_frozen_canned = qty_frozen_canned + it_qy;
	spending_frozen_canned = spending_frozen_canned + tot_pr_qy;
	end;
	
	else if cat = "Dairy" then do;
	qty_dairy = qty_dairy + it_qy;
	spending_dairy = spending_dairy + tot_pr_qy;
	end;
	
	else if cat = "Grocery" then do;
	qty_groc = qty_groc + it_qy;
	spending_groc = spending_groc + tot_pr_qy;
	end;

	else if cat = "Household and Home" then do;
	qty_hhhg = qty_hhhg + it_qy;
	spending_hhhg = spending_hhhg + tot_pr_qy;
	end;
	
	else if cat = "Deli" then do;
	qty_deli = qty_deli + it_qy;
	spending_deli = spending_deli + tot_pr_qy;
    end;
    
	else if cat = "Health and Beauty" then do;
	qty_hb = qty_hb + it_qy;
	spending_hb = spending_hb + tot_pr_qy;
	end;
	
	if last.cnsm_id then do;
	if spending_tot = 0 then delete;
	if tot_qty = 0 then delete;
	qty_fresh_pct = qty_fresh/tot_qty;
	qty_snack_bev_pct = qty_snack_bev/tot_qty;
	qty_frozen_canned_pct = qty_frozen_canned/tot_qty;
	qty_dairy_pct = qty_dairy/tot_qty;
	qty_groc_pct = qty_groc/tot_qty;
	qty_hhhg_pct = qty_hhhg/tot_qty;
	qty_deli_pct = qty_deli/tot_qty;
	qty_hb_pct = qty_hb/tot_qty;
	spending_fresh_pct = spending_fresh/spending_tot;
	spending_snack_bev_pct = spending_snack_bev/spending_tot;
	spending_frozen_canned_pct = spending_frozen_canned/spending_tot;
	spending_dairy_pct =  spending_dairy/spending_tot;
	spending_groc_pct = spending_groc/spending_tot;
	spending_hhhg_pct = spending_hhhg/spending_tot;
	spending_deli_pct = spending_deli/spending_tot;
	spending_hb_pct = spending_hb/spending_tot;
	spec_cd_pct = spec_cd_qty/tot_qty;
    output; 
    end;
    
    keep cnsm_id 
    spec_cd_pct
    qty_fresh_pct
	qty_snack_bev_pct
	qty_frozen_canned_pct
	qty_dairy_pct
	qty_groc_pct
	qty_hhhg_pct
	qty_deli_pct
	qty_hb_pct
    spending_fresh_pct 
    spending_snack_bev_pct 
    spending_frozen_canned_pct
    spending_dairy_pct
    spending_groc_pct
    spending_hhhg_pct
    spending_deli_pct
    spending_hb_pct
    spending_tot
    tot_qty;
    
    label
    cnsm_id = "customer id"
    spec_cd_pct = "discount item quantity/total item quantity"
    qty_fresh_pct = "fresh quantity percent"
	qty_snack_bev_pct = "snack, beverage and alcohol quantity percent"
	qty_frozen_canned_pct = "frozen and canned quantity percent"
	qty_dairy_pct = "dairy quantity percent"
	qty_groc_pct = "grocery quantity percent"
	qty_hhhg_pct = "household and home quantity percent"
	qty_deli_pct = "deli quantity percent"
	qty_hb_pct = "health and beauty quantity percent"
    spending_fresh_pct = "fresh category percent spending"
    spending_snack_bev_pct = "snack, beverage and alcohol percent spending"
    spending_frozen_canned_pct = "frozen and canned percent spending"
    spending_dairy_pct = "dairy percent spending"
    spending_groc_pct = "grocery percent spending"
    spending_hhhg_pct = "household and home percent spending"
    spending_deli_pct = "deli percent spending"
    spending_hb_pct = "health and beauty percent spending"
    spending_tot = "total spending"
    tot_qty = "total_quantity";
run;
    
* order data - roll up to customer level;
proc sort data = order out = order_s;
	by cnsm_id; run;

*maximum date in the table;
proc means data=order_s;
var dlv_dt; run;

data order_cust;
	set order_s;
	by cnsm_id;

	retain frstdt lastdt freq tot_spending tot_incentive incentive_freq;
	
	if first.cnsm_id then do;
	frstdt = '31DEC2099'd;
	lastdt = '31DEC1850'd;
	freq = 0;
	tot_spending = 0;
	tot_incentive = 0;
	incentive_freq = 0;
	end;
	
	tot_spending = tot_spending + it_dmnd_qy;
	tot_incentive = tot_incentive + sale_inct_qy;
	if dlv_dt lt frstdt then frstdt = dlv_dt;
	if dlv_dt gt lastdt then lastdt = dlv_dt;
	if ord_seq_num gt freq then freq = ord_seq_num;
	if sale_inct_qy < 0 then incentive_freq = incentive_freq + 1;
	
	if last.cnsm_id then do;
	tenure = (19534 - frstdt)/365.25;
	recency = (19534 - lastdt)/365.25;
	avg_spending = tot_spending/freq;
	tot_incentive_pct = tot_incentive/tot_spending;
	incentive_freq_pct = incentive_freq/freq;
	output;
	end;
	
	keep 
	cnsm_id tenure recency freq avg_spending
	tot_incentive_pct incentive_freq_pct;
run;

*merge order_cust with ordprod_cust;
proc sql;
create table ordprod_cust as
select ord_id, spec_cd, it_qy, it_pr_qy, tot_pr_qy, cat
from order_cust as o left ordprod_cat_cust as l
on o.cnsm_id = l.cnsm_id; 
quit;

*factor analysis;
proc factor data = order_cust 
rotate=varimax reorder out=fact nfactors=7;
var tenure recency freq avg_spending tot_incentive_pct incentive_freq_pct;
run;

* clustering analysis;
/*Develop Cluster Seeds*/
proc fastclus data=fact1 maxclusters=75 maxiter=1 mean=seed;
var factor1-factor7;

proc sort data=seed;
		by descending _freq_; 





	

    
	
	
	
	
	
	
	
	
	
	
	
	

