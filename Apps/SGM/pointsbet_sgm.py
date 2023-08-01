import requests
import json
import pandas as pd
import os

#==============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
def get_sgm_pointsbet(data, player_names, number_of_disposals):
    if len(player_names) != len(number_of_disposals):
        raise ValueError('Both lists should have the same length')
    
    filtered_df = pd.DataFrame()
    for i in range(len(player_names)):
        temp_df = data[(data['player_name'] == player_names[i]) & 
                                  (data['number_of_disposals'] == number_of_disposals[i])]
        filtered_df = pd.concat([filtered_df, temp_df])
        
    # Get the 'id' column as a list
    id_list = filtered_df['outcome_id'].tolist()
    market_id_list = filtered_df['market_id'].tolist()
    event_key = str(filtered_df['match_id'].iloc[0])
    
    # Create the selected_outcomes list using the id_list
    selected_outcomes = [{"MarketKey": str(market_id), "OutcomeKey": str(id)} for market_id, id in zip(market_id_list, id_list)]
    
    # Create the payload
    payload = {
        "EventKey": event_key,
        "SelectedOutcomes": selected_outcomes
    }
    
    return payload


#==============================================================================
# Make Post Request
#==============================================================================

def call_sgm_pointsbet(data, player_names, number_of_disposals):
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
    payload = get_sgm_pointsbet(data, player_names, number_of_disposals)
    
    url = 'https://api.au.pointsbet.com/api/v2/sgm/price'
    
    payload = payload
    
    headers = {
        'User-Agent': 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
        'Content-Type': 'application/json;charset=UTF-8',
        'Origin': 'https://pointsbet.com.au',
        'Referer': 'https://pointsbet.com.au/'
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
    'Agency': 'Pointsbet'
    }
    
    return pd.DataFrame(output_data, index=[0])
