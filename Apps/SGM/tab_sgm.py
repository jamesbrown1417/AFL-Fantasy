import requests
import json
import pandas as pd
import os
from pymongo import MongoClient

#==============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
def get_sgm_tab(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Get the 'id' column as a list
    id_list = filtered_df['id'].tolist()
    
    # Create the propositions list using the id_list
    propositions = [{"type": "WIN", "propositionId": id} for id in id_list]
    
    return propositions

#==============================================================================
# Make Post Request
#==============================================================================

def call_sgm_tab(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Unadjusted price
    unadjusted_price = filtered_df['price'].prod()
    
    # Get propositions
    propositions = get_sgm_tab(data, player_names, number_of_disposals)
    
    url = "https://api.beta.tab.com.au/v1/pricing-service/enquiry"

    headers = {
        "Content-Type": "application/json",
    }

    payload = {
        "clientDetails": {
            "jurisdiction": "SA",
            "channel": "web"
        },
        "bets": [
            {
                "type": "FIXED_ODDS",
                "legs": [
                    {
                          "type": "SAME_GAME_MULTI",
                        "propositions": propositions
                    }
                ]
            }
        ]
    }

    response = requests.post(url, headers=headers, data=json.dumps(payload))
    
    adjusted_price = response.json()['bets'][0]['legs'][0]['odds']['decimal']
    adjusted_price = float(adjusted_price)
    adjustment_factor = adjusted_price / unadjusted_price
    combined_list = [f"{item1}: {item2}" for item1, item2 in zip(player_names, number_of_disposals)]
    player_string = ", ".join(combined_list)
    
    # Create pandas dataframe of output data
    output_data = {
    'Selections': player_string,
    'Unadjusted Price': unadjusted_price,
    'Adjusted Price': adjusted_price,
    'Adjustment Factor': adjustment_factor,
    'Agency': 'TAB'
    }
    
    return pd.DataFrame(output_data, index=[0])
