import requests
import json
import pandas as pd
import os
from pymongo import MongoClient

#==============================================================================
# API Call function
#==============================================================================

# Create function to call the API
def get_sgm_sportsbet(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')

    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])

    # Create the data for the API
    outcomes_list = []
    for i in range(len(filtered_df)):
        outcomes_dict = {}
        outcomes_dict["marketExternalId"] = int(filtered_df.iloc[i]['marketExternalId'])
        outcomes_dict["outcomeExternalId"] = int(filtered_df.iloc[i]['outcomeExternalId'])
        outcomes_list.append(outcomes_dict)

    data = {"classExternalId": int(filtered_df['classExternalId'].iloc[0]),
            "competitionExternalId": int(filtered_df['competitionExternalId'].iloc[0]),
            "eventExternalId": int(filtered_df['eventExternalId'].iloc[0]),
            "outcomesExternalIds": outcomes_list}

    return data

# Create function to call the API and return the price
def call_sgm_sportsbet(data, player_names, number_of_disposals):
    
    # Get original price information
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Unadjusted price
    unadjusted_price = filtered_df['price'].prod()
    
    data = get_sgm_sportsbet(data, player_names, number_of_disposals)
    url = "https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price"
    headers = {"Content-Type": "application/json",
               'User-Agent': 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36'}
    response = requests.post(url, data=json.dumps(data), headers=headers)
    adjusted_price = 1 + (response.json()['price']['numerator'] / response.json()['price']['denominator'])
    adjustment_factor = adjusted_price / unadjusted_price
    combined_list = [f"{item1}: {item2}" for item1, item2 in zip(player_names, number_of_disposals)]
    player_string = ", ".join(combined_list)
    
    # Create pandas dataframe of output data
    output_data = {
    'Selections': player_string,
    'Unadjusted Price': unadjusted_price,
    'Adjusted Price': adjusted_price,
    'Adjustment Factor': adjustment_factor,
    'Agency': 'Sportsbet'
    }
    
    return pd.DataFrame(output_data, index=[0])
