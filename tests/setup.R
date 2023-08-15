# lady madonna
# laid bank account goose egg, loves
# gold unrequited
library(haven)

zip_tf <- tempfile()

zip_url <- 
	'https://finrafoundation.org/sites/finrafoundation/files/2021-SxS-Data-and-Data-Info.zip'

download.file( zip_url , zip_tf , mode = 'wb' )

unzipped_files <- unzip( zip_tf , exdir = tempdir() )

stata_fn <- grep( "\\.dta$" , unzipped_files , value = TRUE )

nfcs_tbl <- read_dta( stata_fn )

nfcs_df <- data.frame( nfcs_tbl )

names( nfcs_df ) <- tolower( names( nfcs_df ) )
nfcs_df[ , 'one' ] <- 1

nfcs_df[ , 'state_name' ] <-
	factor(
		nfcs_df[ , 'stateq' ] , 
		levels = 1:51 , 
		labels = sort( c( 'District of Columbia' , state.name ) ) 
	)

nfcs_df[ , 'rainy_day_fund' ] <-
	factor(
		nfcs_df[ , 'j5' ] ,
		levels = c( 1 , 2 , 98 , 99 ) ,
		labels = c( 'Yes' , 'No' , "Don't Know" , "Prefer not to say" )
	)

# nfcs_fn <- file.path( path.expand( "~" ) , "NFCS" , "this_file.rds" )
# saveRDS( nfcs_df , file = nfcs_fn , compress = FALSE )
# nfcs_df <- readRDS( nfcs_fn )

library(survey)

nfcs_design <- svydesign( ~ 1 , data = nfcs_df , weights = ~ wgt_n2 )

divison_design <- svydesign( ~ 1 , data = nfcs_df , weights = ~ wgt_d2 )

state_design <- svydesign( ~ 1 , data = nfcs_df , weights = ~ wgt_s3 )
nfcs_design <- 
	update( 
		nfcs_design ,
		
		satisfaction_w_finances =
			ifelse( j1 > 10 , NA , j1 ) ,
			
		risk_taking =
			ifelse( j2 > 10 , NA , j2 ) ,
		
		difficult_to_pay_bills =
			factor(
				j4 ,
				levels = c( 1 , 2 , 3 , 98 , 99 ) ,
				labels = 
					c( 
						'Very difficult' , 
						'Somewhat difficult' , 
						'Not at all difficult' , 
						"Don't know" , 
						'Prefer not to say' 
					)
			) ,
				
		spending_vs_income =
			factor(
				j3 ,
				levels = c( 1 , 2 , 3 , 98 , 99 ) ,
				labels = 
					c( 
						'Spending less than income' , 
						'Spending more than income' , 
						'Spending about equal to income' , 
						"Don't know" , 
						'Prefer not to say' 
					)
			) ,
		
		unpaid_medical_bills =
			ifelse( g20 > 2 , NA , as.numeric( g20 == 1 ) )
	)
sum( weights( nfcs_design , "sampling" ) != 0 )

svyby( ~ one , ~ spending_vs_income , nfcs_design , unwtd.count )
svytotal( ~ one , nfcs_design )

svyby( ~ one , ~ spending_vs_income , nfcs_design , svytotal )
svymean( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE )

svyby( ~ satisfaction_w_finances , ~ spending_vs_income , nfcs_design , svymean , na.rm = TRUE )
svymean( ~ difficult_to_pay_bills , nfcs_design )

svyby( ~ difficult_to_pay_bills , ~ spending_vs_income , nfcs_design , svymean )
svytotal( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE )

svyby( ~ satisfaction_w_finances , ~ spending_vs_income , nfcs_design , svytotal , na.rm = TRUE )
svytotal( ~ difficult_to_pay_bills , nfcs_design )

svyby( ~ difficult_to_pay_bills , ~ spending_vs_income , nfcs_design , svytotal )
svyquantile( ~ satisfaction_w_finances , nfcs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ satisfaction_w_finances , 
	~ spending_vs_income , 
	nfcs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ satisfaction_w_finances , 
	denominator = ~ risk_taking , 
	nfcs_design ,
	na.rm = TRUE
)
sub_nfcs_design <- subset( nfcs_design , j50 == 1 )
svymean( ~ satisfaction_w_finances , sub_nfcs_design , na.rm = TRUE )
this_result <- svymean( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ satisfaction_w_finances , 
		~ spending_vs_income , 
		nfcs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nfcs_design )
svyvar( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ satisfaction_w_finances , nfcs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ unpaid_medical_bills , nfcs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( satisfaction_w_finances ~ unpaid_medical_bills , nfcs_design )
svychisq( 
	~ unpaid_medical_bills + difficult_to_pay_bills , 
	nfcs_design 
)
glm_result <- 
	svyglm( 
		satisfaction_w_finances ~ unpaid_medical_bills + difficult_to_pay_bills , 
		nfcs_design 
	)

summary( glm_result )
stopifnot( nrow( nfcs_df ) == 27118 )
national_rainy_day <- svymean( ~ rainy_day_fund , nfcs_design )
stopifnot( round( coef( national_rainy_day )[ 'rainy_day_fundYes' ] , 2 ) == 0.53 )
state_counts <-
	svyby(
		~ one ,
		~ state_name ,
		state_design ,
		unwtd.count
	)
	
stopifnot( state_counts[ 'California' , 'counts' ] == 1252 )
stopifnot( state_counts[ 'Missouri' , 'counts' ] == 501 )
stopifnot( state_counts[ 'Oregon' , 'counts' ] == 1261 )

state_rainy_day <-
	svyby(
		~ rainy_day_fund ,
		~ state_name ,
		state_design ,
		svymean
	)
	
stopifnot( round( state_rainy_day[ 'California' , 'rainy_day_fundYes' ] , 2 ) == 0.57 )
stopifnot( round( state_rainy_day[ 'Missouri' , 'rainy_day_fundYes' ] , 2 ) == 0.51 )
stopifnot( round( state_rainy_day[ 'Oregon' , 'rainy_day_fundYes' ] , 2 ) == 0.52 )
library(srvyr)
nfcs_srvyr_design <- as_survey( nfcs_design )
nfcs_srvyr_design %>%
	summarize( mean = survey_mean( satisfaction_w_finances , na.rm = TRUE ) )

nfcs_srvyr_design %>%
	group_by( spending_vs_income ) %>%
	summarize( mean = survey_mean( satisfaction_w_finances , na.rm = TRUE ) )
