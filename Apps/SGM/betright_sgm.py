import requests
import json
import pandas as pd
import os
from pymongo import MongoClient

#==============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
def get_sgm_betright(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Get the 'id' column as a list
    header = filtered_df['group_by_header'].tolist()
    event_id = filtered_df['event_id'].tolist()
    outcome_name = filtered_df['outcome_name'].tolist()
    outcome_id = filtered_df['outcome_id'].tolist()
    fixed_market_id = filtered_df['fixed_market_id'].tolist()
    points = "0"
    fixed_win = filtered_df['price'].tolist()
    
    # Create the payload
    payload = []
    for i in range(len(player_names)):
        payload_dict = {
            "eventId": event_id[i],
            "outcomeId": outcome_id[i],
            "marketType": "WIN",  # This seems to be a constant
            "fixedWin": fixed_win[i],
            "fixedMarketId": fixed_market_id[i],
            "marketTypeDesc": "Win",  # This seems to be a constant
            "groupByHeader": header[i],
            "points": points,
            "outcomeName": outcome_name[i]
        }
        payload.append(payload_dict)
    
    return payload

#==============================================================================
# Make Post Request
#==============================================================================

def call_sgm_betright(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Unadjusted price
    unadjusted_price = filtered_df['price'].prod()
    
    # Get payload
    payload = get_sgm_betright(data, player_names, number_of_disposals)
    
    url = 'https://sgm-api.betright.com.au/Pricing/SgmPrice?'
    
    payload = payload
    
    headers = {
        'User-Agent': 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
        'Content-Type': 'application/json;charset=UTF-8',
        'Origin': 'https://betright.com.au',
        'Referer': 'https://betright.com.au/'
    }

    response = requests.post(url, data=json.dumps(payload), headers=headers)

    
    adjusted_price = response.json()['price']
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
    'Agency': 'Betright'
    }
    
    return pd.DataFrame(output_data, index=[0])
