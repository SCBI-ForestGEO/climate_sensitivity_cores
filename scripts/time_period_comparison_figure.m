
%script to make a plot for Helcoski et al.

clf; clear; close all;

% 1. SET UP
% define columns and rows in csv files
clim_var={'cld'; 'dtr'; 'pet';'PET-PRE'; 'pre' ;'tmn'; 'tmp'; 'tmx'; 'vap'; 'wet'; 'PDSI'};
months={'prev.apr'	'prev.may'	'prev.jun'	'prev.jul'	'prev.aug'	'prev.sep'	'prev.oct'	'prev.nov'	'prev.dec'	'curr.jan'	'curr.feb'	'curr.mar'	'curr.apr'	'curr.may'	'curr.jun'	'curr.jul'	'curr.aug'};

% create index variables that designate rows belonging to different variable groups
Energy_variables = [ NaN 1 1 1 NaN 1 1 1 NaN NaN NaN ]; % negative response expected during current gs
Water_variables = [ 1 NaN NaN NaN 1 NaN NaN NaN NaN 1 1 ];  % positive response expected during current gs
Temperature_variables = [ NaN NaN NaN NaN NaN 1 1 1 NaN NaN NaN ];  % positive response expected during current gs

% create index variables that designate columns belonging to different month groups
current_gs= [ NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN 1 1 1 1 ];
past_gs_early= [ NaN 1 1 1 NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ];
past_gs_late= [ NaN NaN NaN NaN 1 1 NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ];

% create matrices to store results
percent_neg_E_current_gs = NaN*ones(1,4);
percent_pos_W_current_gs = NaN*ones(1,4);
percent_expected_current_gs = NaN*ones(1,4);
percent_pos_E_past_egs = NaN*ones(1,4);
percent_neg_W_past_egs  = NaN*ones(1,4);
percent_expected_past_egs = NaN*ones(1,4);
%(not putting all here-- no need)

% 2. RETRIEVE & PROCESS DATA 
% cycle through time periods, pulling out and processing data 
for TP=1:4
    %change directory to results folder for time period
    if TP==1 %1901-2009
        n_sp=14; 
        cd '/Users/teixeirak/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/climate_sensitivity_cores/results/1901_2009/tables/monthly_correlation';       
    elseif TP==2 %1920-49
        n_sp=12;  
        cd '/Users/teixeirak/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/climate_sensitivity_cores/results/1920_1949/tables/monthly_correlation';
    elseif TP==3 %1950-79
        n_sp=14; 
        cd '/Users/teixeirak/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/climate_sensitivity_cores/results/1950_1979/tables/monthly_correlation';
    elseif TP==4 %1980-2009
        n_sp=14; 
        cd '/Users/teixeirak/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/climate_sensitivity_cores/results/1980_2009/tables/monthly_correlation';
    end
    
    %read in data from results files
    n_positive_corr = csvread('SUMMARY_n_positive_correlation.csv',1,1);
    mean_corr = csvread('SUMMARY_mean_correlation.csv',1,1);
    
    % calculate summary statistics of interest, store results:
    
    % percent responding positively to cool, moist conditions during current growing season
    n_pos_corr_current_gs_E=(n_positive_corr.*current_gs).*Energy_variables'; 
    per_neg_corr_current_gs_E=(n_sp - n_pos_corr_current_gs_E)/n_sp*100;
    percent_neg_E_current_gs (TP) = sum(nansum(per_neg_corr_current_gs_E))/sum(sum(1-isnan(per_neg_corr_current_gs_E)));
    n_pos_corr_current_gs_W=(n_positive_corr.*current_gs).*Water_variables'; 
    per_pos_corr_current_gs_W=(n_pos_corr_current_gs_W)/n_sp*100;
    percent_pos_W_current_gs (TP) = sum(nansum(per_pos_corr_current_gs_W))/sum(sum(1-isnan(per_pos_corr_current_gs_W)));
    percent_expected_current_gs (TP) = percent_neg_E_current_gs(TP)*(sum(sum(1-isnan(per_neg_corr_current_gs_E)))/40) +...
                                       percent_pos_W_current_gs(TP)*(sum(sum(1-isnan(per_pos_corr_current_gs_W)))/40);
    
    % percent responding negatively to cool, moist conditions during May-July of previous growing season
    n_pos_corr_past_egs_E=(n_positive_corr.*past_gs_early).*Energy_variables'; 
    per_pos_corr_past_egs_E=(n_pos_corr_past_egs_E)/n_sp*100;
    percent_pos_E_past_egs (TP) = sum(nansum(per_pos_corr_past_egs_E))/sum(sum(1-isnan(per_pos_corr_past_egs_E)));
    n_neg_corr_past_egs_W=(n_positive_corr.*past_gs_early).*Water_variables'; 
    per_neg_corr_past_egs_W=(n_sp-n_neg_corr_past_egs_W)/n_sp*100;
    percent_neg_W_past_egs (TP) = sum(nansum(per_neg_corr_past_egs_W))/sum(sum(1-isnan(per_neg_corr_past_egs_W)));
    percent_expected_past_egs (TP) = percent_pos_E_past_egs(TP)*(sum(sum(1-isnan(per_pos_corr_past_egs_E)))/30) +...
                                       percent_neg_W_past_egs(TP)*(sum(sum(1-isnan(per_neg_corr_past_egs_W)))/30);
                                   
    %average strength of May-Aug correlations
    mean_corr_mjja=(mean_corr.*current_gs);
    mean_corr_PET_mjja(TP)=nanmean(mean_corr_mjja(3,:));
    mean_corr_PETmPRE_mjja(TP)=nanmean(mean_corr_mjja(4,:));
    mean_corr_PRE_mjja(TP)=nanmean(mean_corr_mjja(5,:));
    mean_corr_WET_mjja(TP)=nanmean(mean_corr_mjja(10,:));
    mean_corr_TMAX_mjja(TP)=nanmean(mean_corr_mjja(8,:));
    
end

% 3. MAKE PLOTS
cd '/Users/teixeirak/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/climate_sensitivity_cores/results/figures_for_manuscript'

figure (1)

subplot(1,2,1)
x = categorical({'current MJJA' 'previous MJJ'});
y = [percent_expected_current_gs-50; 50-percent_expected_past_egs];
bar (x,y)
ylabel('% positive response to cool, moist conditions -50') 
legend({'1901-2009' '1920-1949' '1950-1979' '1980-2009'})

subplot(1,2,2)
x = categorical({'PET' 'WET' 'PET-PRE' 'T_{max}'});
y =[abs(mean_corr_PET_mjja); mean_corr_WET_mjja; abs(mean_corr_PETmPRE_mjja); abs(mean_corr_TMAX_mjja)];
bar (x,y)
ylabel('|mean growth correlation to MJJA climate|')
print('BarPlot','-dpng')

figure (4)
TP=categorical({'1901-2009' '1920-1949' '1950-1979' '1980-2009'});
bar (TP,mean_corr_PRE_mjja)
ylabel('mean correlation with PRE')
xlabel('Time period')