#!/usr/bin/env python3

import json
import urllib3
import urllib

APP_ID  = 'fac90f20'
APP_KEY = 'b35671c6cf9007c984b838074222c83c'

http = urllib3.PoolManager()

def validateResponse(response, data):
    return response.status == 200 and \
        len(data['ingredients']) == 1 and \
        data['totalNutrients']
        
def getNutritionJson(query):
    API_URL = "https://api.edamam.com/api/nutrition-data"
    API_ARGS = "?app_id=%s&app_key=%s&ingr=" % (APP_ID, APP_KEY)
    r = http.request('GET', API_URL + API_ARGS + urllib.parse.quote(query))
    data = json.loads(r.data.decode('ISO-8859-1'))

    if not validateResponse(r, data):
        print(r.getheaders())
        print(json.dumps(data, indent=4, sort_keys=True))
        return None

    return data

foods = [
    '1 shot espresso',
    '3 cup milk',
    '2 medium tomato',
    '1 medium mozarella',
    '1 large carrot',
    '8 slice salami',
    '2 apple',
    '4 cup water',
]

ingredients = []
totalCalories = 0
totalNutrients = {}
for food in foods:
    data = getNutritionJson(food)
    if data == None:
        print('Invalid food: ' + food)
        continue
    totalCalories += data['calories']

    ingredient = data['ingredients'][0]['parsed'][0]
    ingredients.append('%2d: %s' % (ingredient['quantity'], ingredient['food']))

    for nutrient in data['totalDaily'].values():
        name = nutrient['label']
        quantity = nutrient['quantity']

        if name in totalNutrients.keys():
            totalNutrients[name] += quantity
        else:
            totalNutrients[name] = quantity

print('\nIngredients:\n%s' % '\n'.join(ingredients))
print('\nCalories: %d' % totalCalories)
print('\nNutrients: ')

for name in sorted(totalNutrients.keys()):
    title = name.ljust(20)
    print('%s\t%6.2f%%' % (title, totalNutrients[name]))
