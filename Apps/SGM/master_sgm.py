# Modules
import sys
import pandas as pd
import os
import json
import requests
from pymongo import MongoClient
sys.path.append('C:/Users/james/R_Projects/AFL-Fantasy/Apps/SGM')

# Read in data
uri = os.getenv("mongodb_connection_string")

client = MongoClient(uri)
db = client['Odds']
collection_1 = db['TAB-SGM']
collection_2 = db['Sportsbet-SGM']
collection_3 = db['BetRight-SGM']
collection_4 = db['Pointsbet-SGM']

sgm_lookup_data_tab = pd.DataFrame(list(collection_1.find()))
sportsbet_disposals_data = pd.DataFrame(list(collection_2.find()))
sgm_lookup_data_betright = pd.DataFrame(list(collection_3.find()))
sgm_lookup_data_pointsbet = pd.DataFrame(list(collection_4.find()))

# Convert float columns to integer where appropriate
sgm_lookup_data_tab['id'] = sgm_lookup_data_tab['id'].astype('Int64')

sgm_lookup_data_pointsbet['match_id'] = sgm_lookup_data_pointsbet['match_id'] .astype('Int64')
sgm_lookup_data_pointsbet['market_id'] = sgm_lookup_data_pointsbet['market_id'] .astype('Int64')
sgm_lookup_data_pointsbet['outcome_id'] = sgm_lookup_data_pointsbet['outcome_id'] .astype('Int64')

sgm_lookup_data_sportsbet['marketExternalId'] = sgm_lookup_data_sportsbet['marketExternalId'] .astype('Int64')
sgm_lookup_data_sportsbet['outcomeExternalId'] = sgm_lookup_data_sportsbet['outcomeExternalId'] .astype('Int64')
sgm_lookup_data_sportsbet['eventExternalId'] = sgm_lookup_data_sportsbet['eventExternalId'] .astype('Int64')
sgm_lookup_data_sportsbet['competitionExternalId'] = sgm_lookup_data_sportsbet['competitionExternalId'] .astype('Int64')
sgm_lookup_data_sportsbet['classExternalId'] = sgm_lookup_data_sportsbet['classExternalId'] .astype('Int64')

sgm_lookup_data_betright['event_id'] = sgm_lookup_data_betright['event_id'] .astype('Int64')
sgm_lookup_data_betright['outcome_id'] = sgm_lookup_data_betright['outcome_id'] .astype('Int64')
sgm_lookup_data_betright['fixed_market_id'] = sgm_lookup_data_betright['fixed_market_id'] .astype('Int64')
sgm_lookup_data_betright['points'] = sgm_lookup_data_betright['points'] .astype('Int64')

# import functions
import tab_sgm
import pointsbet_sgm
import sportsbet_sgm
import betright_sgm

# Create function to get the SGM data
def compare_sgm(player_names, number_of_disposals):
        
        # Create a list of the dataframes that exist, if one doesn't exist, it will be skipped
        df_list = []
        
        # Get the data from each bookmaker
        try:
                tab_data = tab_sgm.call_sgm_tab(sgm_lookup_data_tab, player_names, number_of_disposals)
                df_list.append(tab_data)
        except:
                pass
        try:
                pointsbet_data = pointsbet_sgm.call_sgm_pointsbet(sgm_lookup_data_pointsbet, player_names, number_of_disposals)
                df_list.append(pointsbet_data)
        except:
                pass
        
        try:
                sportsbet_data = sportsbet_sgm.call_sgm_sportsbet(sgm_lookup_data_sportsbet, player_names, number_of_disposals)
                df_list.append(sportsbet_data)
        except:
                pass
        
        try:
                betright_data = betright_sgm.call_sgm_betright(sgm_lookup_data_betright, player_names, number_of_disposals)
                df_list.append(betright_data)
        except:
                pass
          
        # Create dataframe with the data
        output_df = pd.concat(df_list, axis=0).sort_values(by=['Adjusted Price'], ascending=False).reset_index(drop=True)
        
        # Make numeric columns rounded to 2 decimal places
        output_df['Adjusted Price'] = output_df['Adjusted Price'].round(2)
        output_df['Unadjusted Price'] = output_df['Unadjusted Price'].round(2)
        output_df['Adjustment Factor'] = output_df['Adjustment Factor'].round(3)
        
        return output_df
